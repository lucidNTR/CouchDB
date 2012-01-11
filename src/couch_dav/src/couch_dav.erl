% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% This is the WebDAV handler for CouchDB, you can mount a couchdb server
% by pointing your DAV Client to http://server/_dav/<MountPoint>  
%
% This Module was inspired by RackDAV
% Some Parts portet from YAWS DAV module, therefore until this remainder is removed, 
% parts of the source code are licensed under BSD, but will be completely removed 
% and rewritten when this module is finally released (git tag 1.0)
%
% DB Icon design without Couch Overlay:
% Designer: Barry Mieny
% Licence:  Creative Commons Attribution-Noncommercial-Share Alike 3.0 License
% CouchDB overlay by CouchBase (Still to get allowance)
%
%
% Path /                                    -> get all_dbs and generate dav-xml
% Path /<db>/                               -> expose all available database handlers as folders ( _design, _all_docs)
% Path /<db>/_design/                       -> get all design docs in <db> and generate dav-xml (each ddoc is a FOLDER!) 
% Path /<db>/_design/<ddoc>                 -> get ddoc and expose each key as file or folder based on the following rule...
%   key is _rev or _id                      -> hide
%   value is value or array                 -> as file
%   value is object                         -> as folder
% Path /<db>/_design/<ddoc>/<filename>      -> serve value as filestream
% Path /<db>/_design/<ddoc>/<foldername>    -> expose values as folders or files, based on same rule as above
% analog in _all_docs
%
% TODO:
% concurrenting writes?

-module(couch_dav).
-export([handle_dav_req/1]).

-include("couch_db.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(DAV_FILES, couch_config:get("web_dav", "dav_files_on_fs")).
-define(DAV_SUBSTR, couch_config:get("web_dav", "dav_file_name_parts_on_fs")).
-define(DAV_MOUNT, couch_config:get("web_dav", "dav_mount_name")).

-define(XML_PROLOG, "<?xml version=\"1.0\" encoding=\"utf-8\" ?>").

handle_dav_req( #httpd{ user_ctx = UserCtx, mochi_req = MochiReq } = Req ) ->
    
    % TODO: Allow only Admins to access with ok = couch_httpd:verify_is_server_admin(Req),
    % TODO: handle allowed methods first!
    
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Pth, _, _} = mochiweb_util:urlsplit_path(RawUri),
    Path = [ list_to_binary( couch_dav_util:maybe_unquote(Part) ) || Part <- string:tokens(Pth, "/") ],
    case Path of
        % First two segments are _dav handler and MountPoint Name.
        [ _, _ ] ->
             handle_methods( couch_dav_srv, {"", ""}, Req);
        [ _, _ | SrvTail ] ->
            % Does Path contain a segment that is in the configured list of things to be stored on filesystem?
            case is_fs_path( SrvTail ) of
                true ->
                     handle_methods( couch_dav_fs, { SrvTail }, Req );       
                false ->
                    case drop_db_path( SrvTail ) of
                                [] ->
                                    handle_methods(
                                        couch_dav_srv,
                                        {  db_folder, SrvTail },
                                        Req );
                                SrvRest ->
                                    DbPath = SrvTail -- SrvRest,
                                    case SrvRest of
                                        [ <<Dbname/binary>> ] ->
                                            handle_methods(
                                                couch_dav_db,
                                                { DbPath, Dbname, UserCtx },
                                                Req );
                                        [ <<Dbname/binary>> | DbTail ] ->
                                            case drop_doc_path( DbTail ) of
                                                [] ->
                                                    handle_methods( 
                                                        couch_dav_db,
                                                        { DbPath, Dbname, UserCtx, DbTail }, 
                                                        Req );
                                                DbRest ->
                                                    DocPath = DbTail -- DbRest,
                                                    case DbRest of
                                                        [ <<DocName/binary>> ] ->
                                                            handle_methods(
                                                                couch_dav_doc,
                                                                { DbPath, Dbname, UserCtx, DocPath,  DocName }, 
                                                                Req );
                                                        [ <<DocName/binary>> | SubDocPath ] ->
                                                            handle_methods(
                                                                couch_dav_doc,
                                                                { DbPath, Dbname, UserCtx, DocPath, DocName, SubDocPath}, 
                                                                Req ) end end end end end end.
is_fs_path(Path) ->
    lists:any( 
        fun(Name) -> 
            case lists:member(Name, re:split(?DAV_FILES, ", ")) of
            false ->
                case
                    lists:member(
                        1, 
                        [ string:str(binary_to_list(Name), SubString) || SubString <- re:split(?DAV_SUBSTR, ", ",[{return, list}]) ]) of
                    true    -> true;
                    false   -> false end;
            true -> true end end, 
        Path).
        
drop_doc_path( Path ) ->
    drop_path_part( Path, <<".cdoc">> ).
drop_db_path( Path ) ->
    drop_path_part( Path, <<".couch">> ).
drop_path_part( Path, Part ) -> 
    lists:dropwhile(
        fun(Element) -> 
            case binary:match(Element, Part) of 
                nomatch -> true; 
                _ -> false end end, 
        Path ).

handle_methods( Module, Dav_Opts, #httpd{ method = Method } = Req) ->       
    case Method of
        'OPTIONS'   -> handle_options( Module, Dav_Opts, Req);
        'PROPFIND'  -> handle_propfind( Module, Module:trans_opt( Dav_Opts), Req );
        'DELETE'    -> handle_delete( Module, Module:trans_opt(Dav_Opts), Req );
        'PUT'       -> Module:put( Module:trans_opt(Dav_Opts), Req );
        'GET'       -> Module:get( Module:trans_opt(Dav_Opts), Req );
        'MKCOL'     -> Module:mkcol( Module:trans_opt(Dav_Opts), Req );
        'MOVE'      -> handle_move( Module, Dav_Opts, Req );
        'LOCK'      -> couch_httpd:send_error(Req, 200, <<"OK">>, <<"But Not Implemented">>);
        'UNLOCK'    -> couch_httpd:send_error(Req, 200, <<"OK">>, <<"But Not Implemented">>);
        _           -> couch_httpd:send_error(Req, 405, <<"Error">>, <<"Method not Supported">>) end.

handle_move( Module, Dav_Opts , #httpd{ mochi_req = MochiReq } = Req) ->   
    Dest = 
        mochiweb_util:unquote( 
            string:strip( 
                string:substr( 
                    MochiReq:get_header_value("destination") , 
                    string:str( 
                        MochiReq:get_header_value("destination"), ?DAV_MOUNT) 
                    + string:len(?DAV_MOUNT) ), right, $/ )),
    case Module:move( Module:trans_opt(Dav_Opts, Dest, Req )) of
        ok ->
            couch_httpd:send_json( Req, 200, {[{successful, <<"moved object">>}]} );
        Error ->
             couch_httpd:send_error(Req, 409, Error) end. 

handle_propfind( Module, Dav_Opts , #httpd{ mochi_req = MochiReq } = Req ) ->    
    Url = string:strip( couch_httpd:absolute_uri( Req, couch_httpd:path(Req) ), right, $/ ),
    {Depth, _} = string:to_integer( MochiReq:get_header_value(depth) ),
    case Module:get_children( Dav_Opts, Url, Depth, Req) of 
        [] ->
            couch_httpd:send_error( Req, not_found );
        wrong_depth ->
            couch_httpd:send_error(Req, {forbidden, "Wrong Depth (min 0, max 5)"} );
        Dav_struct ->
            Dav_xml = xmerl:export_simple( [{multistatus, [{xmlns, "DAV:"}], Dav_struct }], xmerl_xml, [{ prolog, ?XML_PROLOG }]),
            couch_httpd:send_response( 
                Req, 
                207, 
                [{"Content-Type", "text/xml; charset= \"utf-8\" "},{"Connection", "Keep-Alive"}], 
                Dav_xml ) end.

handle_delete( Module, Dav_Opts, Req ) ->
    case Module:delete( Dav_Opts ) of
        ok -> couch_httpd:send_json( Req, 200, {[{successful, <<"deleted">>}]} );
        Error -> couch_httpd:send_error( Req, {fail_delete, Error} ) end.

handle_options( Module, _Dav_Opts, Req ) ->   
    case Module:options() of
        {methods, Methods, mode, Mode} ->
            couch_httpd:send_response(Req, 200, [ {"Allow", Methods }, {"DAV", Mode} ], <<>>) end.