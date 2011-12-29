-module(couch_httpd_dav_doc).
-export([options/0, get/2, mkcol/2, delete/1, copy/2, move/3, put/2, is_directory/1, get_children/4, trans_opt/1 ]).
-include_lib("kernel/include/file.hrl").
-include("couch_db.hrl").

-define(DAV_PATH, couch_config:get("web_dav", "dav_dir")).

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "PUT,DELETE,MOVE,MKCOL").


options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.

trans_opt( { DbName, UserCtx, DocType, Id}) ->
    {DbName, Db, nil} = trans_opt( { DbName, UserCtx, nil}),
    {binary_to_list(DbName), Db, DocType, binary_to_list(Id)};
trans_opt( { DbName, UserCtx, DocType}) ->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} ->
        { DbName, Db, DocType };
    Error ->
        throw(Error) end.

get( { _DbName, Db, Handler, Id }, Req ) ->
    case Handler of
        "_design" ->
            FullName = list_to_binary(Handler ++ "/" ++ Id);
        _ -> FullName = list_to_binary(Id) end,
    couch_httpd_db:db_doc_req(Req, Db, FullName ).

put( { _DbName, Db, Handler,  Id }, Req ) ->
    case Handler of
        "_design" ->
            FullName = list_to_binary(Handler ++ "/" ++ Id);
        _ -> FullName = list_to_binary(Id) end,
    couch_httpd_db:db_doc_req( Req, Db, FullName ).

        
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

is_directory( { _Dbname, Db, Handler, Id } ) ->
    case Handler of
        "_design" ->
            ViewOpts = [{start_key, <<"_design">>}, { end_key, <<"_design0">>}],
            Con_fun = fun(Id) -> string:substr(binary_to_list(Id), 9 ) end;
        "_all_docs" ->
            ViewOpts = [{start_key, <<"_design0">>}],
            Con_fun = fun(Id) -> binary_to_list(Id) end end,
    { ok, DocRows} = couch_mrview:query_all_docs(Db, Handler ),
    case lists:member( Id, [ Con_fun(Id) || {row, [{id, Id}| _] } <- DocRows  ]) of
        true -> false;
        false -> no_exist end;
is_directory( { _Dbname, _Db, Handler } ) ->
    case Handler of
        "_design" -> true;
        "_all_docs" -> true;
        _ -> no_exist end.
    
    
get_children( { DbName, Db, Handler, Id }, N, Url, Req ) ->
   [ make_dav_entry( { "/" ++ DbName ++ Handler ++ Id, Id }, Url ) ];
get_children( { DbName, Db, Handler }, N, Url, Req ) ->
    case Handler of
        "_design" ->
            ViewOpts1 = [{start_key, <<"_design">>}, { end_key, <<"_design0">>}],
            ViewOpts2 = undefined,
            Con_fun = fun(Id) -> string:substr(binary_to_list(Id), 9 ) end;
        "_all_docs" ->
            % need to combine the two views to exclude design docs, without loosing
            % some normal docs because in all_docs view, sort order is ASCII not ICU!!!
            ViewOpts1 = [{start_key, <<"_design0">>}],
            ViewOpts2 = [{end_key, <<"_design">>}],
            Con_fun = fun(Id) -> binary_to_list(Id) end end,
    Base = [{ "/" ++ DbName, Handler, Url }],
    Total = if 
        N>0 ->
            try
               { ok, DocRows1} = couch_mrview:query_all_docs(Db, ViewOpts1 ),
               case ViewOpts2 of
                   undefined    -> 
                        DocRows = DocRows1;
                   _            -> 
                        { ok, DocRows2} = couch_mrview:query_all_docs(Db, ViewOpts2 ), 
                        DocRows = DocRows1 ++ DocRows2 end,
               Base ++ [ { "/" ++ Con_fun(Id), Con_fun(Id), Url ++ "/" ++ Con_fun(Id) } || {row, [{id, Id}| _] } <- DocRows  ]
            after
                catch couch_db:close(Db)
            end;
 
        true -> Base end,
    lists:map( fun( {RelPath, Name, TempUrl} ) -> make_dav_entry( { RelPath, Name }, TempUrl ) end, Total )
    ++
    couch_httpd_dav_fs:get_children( {"/" ++ binary_to_list(DbName) ++ "/", Handler}, N, Url, Req).
make_dav_entry({ _RelPath, Name }, Url) ->
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