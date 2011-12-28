-module(couch_httpd_dav_fs).
-export([options/0, get/2, mkcol/2, delete/1, copy/2, move/3, put/2, is_directory/1, get_children/4, trans_opt/1 ]).
-include_lib("kernel/include/file.hrl").
-include("couch_db.hrl").

-define(DAV_PATH, couch_config:get("web_dav", "dav_dir")).

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "PUT,DELETE,MOVE,MKCOL").


options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.


trans_opt( Dav_Opts ) ->
    { Tail } = Dav_Opts,
    { RelPathBL, [ NameB ] } = lists:split( length(Tail)-1, Tail ), 
    RelPath = lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, RelPathBL) ),
    Name=binary_to_list(NameB),
    {RelPath, Name}.


get( { RelPath, Name }, Req ) ->
    {{Year,Month,Day},Time} = erlang:localtime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    F = element(2, file:read_file_info(?DAV_PATH ++ "/" ++ RelPath ++ Name)),
    Headers = [
        {"Content-Type", "none"},
        {"Accept-Ranges", "bytes"},
        {"Connection", "Keep-Alive"},
        {"Cache-Control", "public, max-age=31536000"},
        {"Etag", 
            binary_to_list( 
                couch_httpd:make_etag(
                    integer_to_list(F#file_info.size) ++ httpd_util:rfc1123_date(F#file_info.mtime) ) )},
        {"Expires", httpd_util:rfc1123_date(OneYearFromNow)}],
    couch_httpd:serve_file( Req, Name, ?DAV_PATH ++ "/" ++ RelPath, Headers).


put( { RelPath, Name}, #httpd{ mochi_req = MochiReq } = Req ) ->
    case maybe_mkdir(RelPath) of
        ok -> 
            Path = ?DAV_PATH ++ "/" ++ RelPath ++ Name,
            case file:open(Path, [raw, read, write, delayed_write, read_ahead]) of
                {ok, Fd} -> 
                    case flush_file( MochiReq, Fd ) of
                        ok ->
                           file:close(Fd),
                           couch_httpd:send_json(Req, 200, {[ {ok, true} ]});
                        Error ->
                           file:close(Fd),
                           couch_httpd:send_error(Req, 409, Error) end;
                Err -> 
                    couch_httpd:send_error(Req, 409, Err) end;
        error -> 
            couch_httpd:send_error(Req, 409, <<"Could not create recursive Directories.">>) end.
flush_file( MochiReq, Fd ) -> 
    MochiReq:stream_body(
        64000,
        fun({0, Footers}, _) ->
            F = mochiweb_headers:from_binary(Footers),
            case mochiweb_headers:get_value("Content-MD5", F) of
                undefined ->
                    ok;
                Md5 ->
                    {md5, base64:decode(Md5)} end;
            ({_Length, Chunk}, _) ->
                file_write( Fd, Chunk) end,
        ok).
file_write(Fd, Bin ) ->      
    try
        ok = file:write(Fd, Bin)
    catch
        _:Err ->
            file:close(Fd),
            Err end.

maybe_mkdir(RelPath) ->
    ThisPath= ?DAV_PATH ++ "/" ++ RelPath,
    RelUp = string:substr( 
        string:strip(RelPath, right, $/) , 
        1, 
        string:rchr(
            string:strip(RelPath, right, $/), 
            $/ ) ),
    UpPath = ?DAV_PATH ++ "/" ++ RelUp,
    case file:read_file_info( ThisPath ) of
        { ok, Dir } -> ok;
        _ -> 
            case file:read_file_info( UpPath ) of
                { ok, Dir } -> file:make_dir( ThisPath ), ok;
                _ -> 
                    case maybe_mkdir( RelUp ) of
                        ok -> file:make_dir( ThisPath ), ok;
                        _ -> error end end end.
        
mkcol({ RelPath, Name }, Req ) ->
    Path = ?DAV_PATH ++ "/" ++ RelPath ++ Name,
    file:make_dir(Path),
    couch_httpd:send_json( Req, 200, {[{successful, <<"created directory">>}]} ).


copy({ RelPath, Name }, Req) ->
    couch_httpd:send_json( Req, 200, {[{successful, <<"copied">>}]} ).


move({ RelPath, Name }, Dest, #httpd{ mochi_req = MochiReq }=Req) ->
    file:rename(?DAV_PATH ++ "/" ++ RelPath ++ Name, ?DAV_PATH ++ Dest).

delete({ RelPath, Name } ) ->
    Path = ?DAV_PATH ++ "/" ++ RelPath ++ Name,
    delete( Path );                    
delete( Path ) ->
    case file:read_file_info(Path) of
        {ok, F} when F#file_info.type == directory ->
            case file:list_dir(Path) of
                {ok, Fs} ->
                    case delete(Path, Fs) of
                        ok ->
                            file:del_dir(Path);
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            end;
        {ok, _} ->
            file:delete(Path);
        Err ->
            Err
    end.
delete(_Dir, []) ->
    ok;
delete(Dir, [H|T]) ->
    F = filename:join(Dir, H),
    case delete(F) of
        ok ->
            delete(Dir, T);
        Err ->
            Err
    end.


is_directory( { RelPath, Name } ) ->
    Path = RelPath ++ Name,
    case file:read_file_info( ?DAV_PATH ++ "/" ++ Path ) of
    { ok, Dir } -> 
        case Dir#file_info.type of
            directory -> true;
            regular -> false end;
    _Else -> no_exist end.

        
get_children( { RelPath, Name }, N, Url, _ ) ->   
    case file:read_file_info( ?DAV_PATH ++ "/" ++ RelPath ++ Name ) of
        { ok, _Info } -> 
            Base = [{ RelPath, Name, Url }],
            Total = if 
                N>0 -> 
                    { ok, L } = file:list_dir( ?DAV_PATH ++ "/" ++ RelPath ++ Name ),
                    Base ++ [{ RelPath ++ Name ++ "/", TempName, Url ++ "/" ++ TempName } || TempName <- L  ];
                true -> Base end,
            lists:map( 
                fun( {RelPath, Name, TempUrl} ) -> make_dav_entry( { RelPath, Name }, TempUrl ) end, Total );
        _Else -> [] end.
make_dav_entry({ RelPath, Name }, Url) ->
    case file:read_file_info(?DAV_PATH ++ "/" ++ RelPath ++ Name) of
        { ok, F } ->
            {response, [], [
                {href, [], [ Url ]},
                {propstat, [], [
                    {prop, [], [
                        {name, [], [Name]},
                        {displayname, [], [Name]},
                        {creationdate, [], [httpd_util:rfc1123_date(F#file_info.ctime)]}, 
                        {getlastmodified, [], [httpd_util:rfc1123_date(F#file_info.mtime)]},
                        {getcontentlength, [], [integer_to_list(F#file_info.size)]},
                        {getetag, [], [
                            binary_to_list( 
                                couch_httpd:make_etag(
                                    integer_to_list(F#file_info.size) ++ httpd_util:rfc1123_date(F#file_info.mtime) ) )]},
                        {resourcetype, [], if F#file_info.type == directory -> [{collection, [], []}]; true ->[] end },
                        %{ishidden, [], [ F#file_info.is_hidden ]}
                        {supportedlock, [], []},
                        {lockdiscovery, [], []} ]},
                    {status, [], ["HTTP/1.1 200 OK"] } ]} ]};
        _Error -> 
            {response, [], [
                {propstat, [], [ 
                    {status, [ RelPath ++ Name ], ["HTTP/1.1 500 Error"] } ]} ]} end.