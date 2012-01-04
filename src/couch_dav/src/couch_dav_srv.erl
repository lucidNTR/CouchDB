-module(couch_httpd_dav_srv).
-export([options/0, get/2, is_directory/1, get_children/4, trans_opt/1 ]).

-define(R_METHODS,  "GET,HEAD,OPTIONS,PROPFIND").
-define(W_METHODS,  "DELETE,MKCOL").

options() ->
    { methods, ?R_METHODS ++ ?W_METHODS, mode, "1,2" }.

trans_opt( Dav_Opts ) ->
    {}.

get( {}, Req ) ->
    couch_httpd_misc_handlers:handle_welcome_req(Req, <<"CouchDAV says Welcome.">>).

is_directory( {} ) ->
    true.
        
get_children( {}, N, Url, _ ) ->   
    Base = [{"/", "CouchDAV", Url}] ,
    Total = if 
        N>0 -> 
            {ok, DbNames} = couch_server:all_databases(),
            Base ++ [ { "/", binary_to_list(TempName), Url ++ "/" ++ binary_to_list(TempName) } || TempName <- DbNames  ];
        true -> Base end,
    lists:map( fun( {RelPath, Name, TempUrl} ) -> make_dav_entry( { RelPath, Name }, TempUrl ) end, Total ) 
    ++  
    couch_httpd_dav_fs:get_children( {"", "/"}, N, Url, []).

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
                {getetag, [], [
                    binary_to_list( 
                        couch_httpd:make_etag(
                            "test" ++ "test" ) )]},
                {resourcetype, [], [{collection, [], []}] },
                %{ishidden, [], [ F#file_info.is_hidden ]}
                {supportedlock, [], []},
                {lockdiscovery, [], []} ]},
            {status, [], ["HTTP/1.1 200 OK"] } ]} ]}.