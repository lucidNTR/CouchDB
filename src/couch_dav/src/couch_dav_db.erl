-module(couch_dav_db).
-export([options/0, get/2, get_children/4, trans_opt/1 ]).
-include_lib("kernel/include/file.hrl").
-include("couch_db.hrl").

-define(DAV_PATH, couch_config:get("web_dav", "dav_dir")).
-record(server,{root_dir = [],dbname_regexp,max_dbs_open=100,dbs_open=0,start_time=""}).
-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "PUT,DELETE,MOVE,MKCOL").


options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.
trans_opt( { DbPathBL, DbNameB, UserCtx }) ->
   trans_opt( { DbPathBL, DbNameB, UserCtx, [] });
trans_opt( { DbPathBL, DbNameB, UserCtx, DocPathBL }) ->
    DocPath = lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, DocPathBL) ),
    case binary:match( DbNameB, [<<".couch">>],[]) of 
        {Start , _} ->            
            PureDbName = 
                lists:foldr(
                    fun(X, Acc)->
                        <<X/binary, $/, Acc/binary>> end, 
                    binary:part(DbNameB, 0, Start),
                    DbPathBL ),
            case couch_db:open(PureDbName, [{user_ctx, UserCtx}]) of
            {ok, Db} ->
                { lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, DbPathBL) ), binary_to_list(binary:part(DbNameB, 0, Start)), Db, DocPath };
            Error ->
                throw(Error) end;
        Error -> 
            throw(Error) end.

get( { DbPath, DbName, Db, DocPath }, Req ) ->
    {{Year,Month,Day},Time} = erlang:localtime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    NormRoot = couch_util:normpath(Root),
    F = element(2, file:read_file_info(NormRoot ++ "/" ++ DbPath ++ DbName++".couch")),
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
    couch_httpd:serve_file( Req, DbName++".couch", NormRoot ++ "/" ++ DbPath, Headers).

% TODO: port old function to new
%     case lists:member( Id, [ Con_fun(Id) || {row, [{id, Id}| _] } <- DocRows  ]) of
%         true -> false;
%         false -> no_exist end.
doc_con(DocName, SubPath) ->
    case string:tokens( binary_to_list(DocName) -- SubPath, "/" ) of
        [Folder, _Subfolder | Rest] ->
             mochiweb_util:quote_plus(Folder) ;
        [FlatDocName] ->
            mochiweb_util:quote_plus(FlatDocName) ++ ".cdoc" end.
get_children( { DbPath, DbName, Db, DocPath }, Url, N, Req ) ->
    case DocPath of
        [] -> 
            SubPath = "",
            ViewOpts = [];
        DocPath ->
            SubPath = "/" ++ DocPath,
            ViewOpts = [{ start_key, list_to_binary(DocPath) }, { end_key, list_to_binary(DocPath ++ [$~,$~,$~] ) }] end,
    Base = [{ DbPath ++ DbName ++ ".couch" ++ SubPath, Url }],
    Total = if 
        N < 2 ->
            try
               { ok, DocRows} = couch_mrview:query_all_docs(Db, ViewOpts ),
               Base ++ [ { doc_con(DocName, DocPath), Url ++ "/" ++  doc_con(DocName, DocPath)} || {row, [{id, DocName}| _] } <- DocRows  ]
               ++ couch_dav_fs:get_children( {DbPath, DbName},  Url, N, Req)
            after
                catch couch_db:close(Db) end;
        true -> Base end,
    lists:map( fun( { Name, TempUrl} ) -> make_dav_entry( Name , TempUrl ) end, Total ).     
make_dav_entry( Name, Url) ->
    DocString = string:str(Name, ".cdoc"),
    {response, [], [
        {href, [], [ Url ]},
        {propstat, [], [
            {prop, [], [
                {name, [], [Name]},
                {displayname, [], [Name]},
                {creationdate, [], []}, 
                {getlastmodified, [], []},
                {getcontentlength, [], []},
                {getetag, [], []},
                {resourcetype, [], if DocString == 0 -> [{collection, [], []} ]; true -> [] end },
                {supportedlock, [], []},
                {lockdiscovery, [], []} ]},
            {status, [], ["HTTP/1.1 200 OK"] } ]} ]}.