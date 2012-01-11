-module(couch_dav_srv).
-export([options/0, get/2, get_children/4, trans_opt/1 ]).

-include("couch_db.hrl").

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "DELETE,MKCOL").
-record(server,{root_dir = [],dbname_regexp,max_dbs_open=100,dbs_open=0,start_time=""}).

options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.

trans_opt( { db_folder, DbPathBL } ) ->
    { RelPathBL, [ NameB ] } = lists:split( length(DbPathBL)-1, DbPathBL ),
    DbPath =lists:concat(lists:flatmap(fun(X)->[binary_to_list(X),"/"] end, RelPathBL) ),
    { DbPath, binary_to_list(NameB) };
trans_opt( Dav_Opts ) ->
    Dav_Opts.

get( {}, Req ) ->
    couch_httpd_misc_handlers:handle_welcome_req(Req, <<"CouchDAV says Welcome.">>).


list_db_dir( Path ) ->
    try
        filelib:fold_files(
            Path, "^[a-z0-9\\_\\$()\\+\\-]*[\\.]couch$", true,
            fun(Filename, AccIn) ->
                case couch_util:normpath(Filename) -- Path of
                    [$/ | RelativeFilename] -> ok;
                    RelativeFilename -> ok end,
                case string:tokens( RelativeFilename, "/" ) of
                    [Folder | _Rest] ->
                        [ Folder | AccIn ];
                    [RelativeFilename] ->
                        [ RelativeFilename | AccIn]  end end, 
            [])
    catch
        throw:{stop, Fun, Acc1} ->
          Acc1 end.

get_children( {RelPath, Name}, Url, Depth, _ ) ->
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    NormRoot = couch_util:normpath(Root),
    Path = NormRoot ++ "/" ++ RelPath,
    case Name of 
        []->NormName = "";
        _-> NormName = couch_util:normpath(Name)++"/" end, 
    case lists:member( Name, list_db_dir( Path ) ++ [[]] ) of
        true ->
            Base = [ make_dav_entry( NormName, [], Url ) ],
                case Depth of
                    0 ->
                        Base;
                    N when N < 2 ->
                        lists:foldl(
                            fun(TempName, Sum) ->
                                get_children({ RelPath ++ NormName , TempName }, Url ++ "/" ++ TempName, Depth-1, []) ++ Sum end,
                            Base,
                            list_db_dir( Path ++ Name )) 
                        ++ couch_dav_fs:get_children( {RelPath, Name}, Url, N, []);
                    _ ->
                        wrong_depth end;
        _ -> [] end.


make_dav_entry(Name, _, Url) ->
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
                {resourcetype, [], [{collection, [], []}] },
                {supportedlock, [], []},
                {lockdiscovery, [], []} ]},
            {status, [], ["HTTP/1.1 200 OK"] } ]} ]}.