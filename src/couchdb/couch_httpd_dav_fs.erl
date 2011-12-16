-module(couch_httpd_dav_fs).
-export([options/0, get/2, delete/1, delete/2, put/2, make_dav_entry/2, is_directory/1, get_children/3, trans_opt/1 ]).
-include_lib("/usr/local/lib/erlang/lib/kernel-2.14.5/include/file.hrl").
-include("couch_db.hrl").

-define(DAV_PATH, couch_config:get("web_dav", "dav_dir")).

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "POST,PUT,DELETE,PROPPATCH,MOVE,LOCK,UNLOCK,MKCOL").


options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.


trans_opt( Dav_Opts ) ->
    { Tail } = Dav_Opts,
    { RelPathBL, [ NameB ] } = lists:split( length(Tail)-1, Tail ), 
    RelPath = lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, RelPathBL) ),
    Name=binary_to_list(NameB),
    {RelPath, Name}.


get( { RelPath, Name}, Req ) ->
    {{Year,Month,Day},Time} = erlang:localtime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    Headers = [
        {"Content-Type", "none"},
        {"Accept-Ranges", "bytes"},
        {"Connection", "Keep-Alive"},
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", httpd_util:rfc1123_date(OneYearFromNow)}],
    couch_httpd:serve_file( Req, Name, ?DAV_PATH ++ "/" ++ RelPath, Headers).


put( { RelPath, Name}, #httpd{ mochi_req = MochiReq } = Req ) ->
    Path = ?DAV_PATH ++ "/" ++ RelPath ++ Name,
    Fun = case couch_httpd:body_length(Req) of
                    undefined ->
                        <<"">>;
                    {unknown_transfer_encoding, Unknown} ->
                        exit({unknown_transfer_encoding, Unknown});
                    chunked ->
                        fun(MaxChunkSize, ChunkFun, InitState) ->
                            couch_httpd:recv_chunked(Req, MaxChunkSize,
                                ChunkFun, InitState) end;
                    0 ->
                        <<"">>;
                    TLength when is_integer(TLength) ->
                        Expect = case couch_httpd:header_value(Req, "expect") of
                                     undefined ->
                                         undefined;
                                     Value when is_list(Value) ->
                                         string:to_lower(Value) end,
                        case Expect of
                            "100-continue" ->
                                MochiReq:start_raw_response({100, gb_trees:empty()});
                            _Else ->
                                ok end,
                        fun(Size) -> couch_httpd:recv(Req, Size) end end,
    FileLength = case couch_httpd:header_value(Req,"Content-Length") of
        undefined ->
            undefined;
        Length ->
            list_to_integer(Length)
        end,
    case flush_file( Path, Fun, FileLength ) of
        ok ->
            couch_httpd:send_json(Req, 200, {[ {ok, true} ]});
        Error ->
            couch_httpd:send_error(Req, 409, Error) end.
flush_file(Path, Fun, undefined) ->
    Fun(4096,
        fun({0, Footers}, _) ->
            F = mochiweb_headers:from_binary(Footers),
            case mochiweb_headers:get_value("Content-MD5", F) of
            undefined ->
                ok;
            Md5 ->
                {md5, base64:decode(Md5)}
            end;
        ({_Length, Chunk}, _) ->
            file_write( Path, Chunk)
        end, ok);
flush_file( Path, Fun, AttLen) ->
    write_streamed_file( Path, Fun, AttLen).  
write_streamed_file( _Path, _Fun, 0) ->
    ok;
write_streamed_file( Path, Fun, LenLeft ) when LenLeft > 0 ->
    Bin = read_next_chunk( Fun, LenLeft ),
    ok = file_write( Path, Bin),
    write_streamed_file( Path, Fun, LenLeft - size(Bin) ).
read_next_chunk(Fun, _) when is_function(Fun, 0) ->
    Fun();
read_next_chunk(Fun, LenLeft) when is_function(Fun, 1) ->
    Fun(lists:min([LenLeft, 16#2000])).
file_write(Path, Bin) ->
    case file:open(Path, [raw,write]) of
        {ok, Fd} ->       
            try
                ok = file:write(Fd, Bin),
                file:close(Fd)
            catch
                _:Err2 ->
                    file:close(Fd),
                    Err2 end;
    Err3 ->
        Err3 end.


delete ({ RelPath, Name } ) ->
    Path = ?DAV_PATH ++ "/" ++ RelPath ++ Name,
    delete (Path);                    
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

        
get_children( { RelPath, Name }, N, Url ) ->   
    Base = [{ RelPath, Name, Url }],
    if 
        N>0 -> 
            { ok, L } = file:list_dir( ?DAV_PATH ++ "/" ++ RelPath ++ Name ),
            Base ++ [{ RelPath ++ Name ++ "/", TempName, Url ++ "/" ++ TempName } || TempName <- L  ];
        true -> Base end.

    
make_dav_entry({ RelPath, Name }, Url) ->
    F = element(2, file:read_file_info(?DAV_PATH ++ "/" ++ RelPath ++ Name)),
        {response, [], [
            {href, [], [ Url ]},
            {propstat, [], [
                {prop, [], [
                    {name, [], [Name]},
                    {displayname, [], [Name]},
                    {creationdate, [], [httpd_util:rfc1123_date(F#file_info.ctime)]}, 
                    {getlastmodified, [], [httpd_util:rfc1123_date(F#file_info.mtime)]},
                    {getcontentlength, [], [integer_to_list(F#file_info.size)]},
                    {resourcetype, [], if F#file_info.type == directory -> [{collection, [], []}]; true ->[] end }
                    %{ishidden, [], [ F#file_info.is_hidden ]} 
                    %etag and inspect rest of file_info record!
                ]},
                {status, [], ["HTTP/1.1 200 OK"] } ]} ]}.