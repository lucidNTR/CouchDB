-module(couch_dav_doc).
-export([options/0, get/2, put/2, trans_opt/1 ]).
-include_lib("kernel/include/file.hrl").
-include("couch_db.hrl").

-define(DAV_PATH, couch_config:get("web_dav", "dav_dir")).

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "PUT,DELETE,MOVE,MKCOL").


options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.

trans_opt( {DbPathBL, DbNameB, UserCtx, DocPathBL, DocNameB} ) ->
    case binary:match( DocNameB, [<<".cdoc">>]) of 
        {DocSfxStart , _} -> 
            PureId = binary:part(DocNameB, 0, DocSfxStart),
            DocPath = lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, DocPathBL) ),
            case binary:match( DbNameB, [<<".couch">>]) of 
                {DbSfxStart, _} ->
                    PureDbName = 
                        lists:foldr(
                            fun(X, Acc)->
                                <<X/binary, $/, Acc/binary>> end, 
                            binary:part(DbNameB, 0, DbSfxStart),
                            DbPathBL ),
                    case couch_db:open(PureDbName, [{user_ctx, UserCtx}]) of
                    {ok, Db} ->
                        { lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, DbPathBL) ), binary_to_list(binary:part(DbNameB, 0, DbSfxStart)), Db, DocPath, binary_to_list(PureId) };
                    Error ->
                        throw(Error) end;
                Error -> 
                    throw(Error) end;
        Error -> 
            throw (Error) end.

get( { _DbPath, _DbName, Db, DocPath, DocName }, Req ) ->
    FullName = list_to_binary(DocPath ++ DocName),
    couch_httpd_db:db_doc_req(Req, Db, FullName ).

put( { _DbPath, _DbName, Db, DocPath, DocName }, Req ) ->
    FullName = list_to_binary(DocPath ++ DocName),
    couch_httpd_db:db_doc_req( Req, Db, FullName ).

make_dav_entry( Name , Url) ->
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
                {resourcetype, [], [] },
                {supportedlock, [], []},
                {lockdiscovery, [], []} ]},
            {status, [], ["HTTP/1.1 200 OK"] } ]} ]}.