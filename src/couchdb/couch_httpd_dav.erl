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

% bind_path is based on bind method from Webmachine
% @doc Module for URL rewriting by pattern matching.

% This is the WebDAV handler for CouchDB Design documents, you can mount a databases design doc 
% by pointing your DAV Client to http://server/_dav/(...)  

% Path /                            -> get all_dbs and generate dav-xml
% Path /<db>/                       -> expose all available database handlers as folders (e.g. _design, _view, _list, _show)
% Path /<db>/_design/               -> get all design docs in <db> and generate dav-xml (each ddoc is a FOLDER!) 
% Path /<db>/_design/<ddoc>         -> get ddoc and expose each key as file or folder based on the following rule...
    % key is _rev or _id                        -> hide
    % value is value or array                   -> as file
    % value is object                           -> as folder
% Path /<db>/_design/<ddoc>/<filename>          -> serve value as filestream
% Path /<db>/_design/<ddoc>/<foldername>        -> expose values as folders or files, based on same rule as above
% etc. do this to max. depth that is configurable in dav_max_depth in config file

% This Module was inspired by RackDAV
% Some Parts portet from YAWS DAV module, therefore until this remainder is removed, parts of the source code are licensed under BSD, but will be completely removed and rewritten when this module is released

% DB Icon design without Couch Overlay:
%Designer:
%Barry Mieny
%Lizenz:
%Creative Commons Attribution-Noncommercial-Share Alike 3.0 License
%CouchDB overlay by CouchBase
%Allowance still to be got

% TODO:
% concurrenting writes?


-module(couch_httpd_dav).
-export([handle_dav_req/1]).

-include("couch_db.hrl").
-include("/usr/local/lib/erlang/lib/xmerl-1.2.10/include/xmerl.hrl").

-import(couch_httpd).
-import(couch_httpd_dav_fs).
%-import(couch_httpd_dav_ddoc).
%-import(couch_httpd_dav_db).

-define(DAV_FILES,  couch_config:get("web_dav", "dav_files_on_fs")).
-define(DAV_MOUNT,  couch_config:get("web_dav", "dav_mount_name")).

-define(XML_PROLOG, "<?xml version=\"1.0\" encoding=\"utf-8\" ?>").

handle_dav_req( #httpd{ path_parts = Path, method = Method } = Req ) ->
    %ok = couch_httpd:verify_is_server_admin(Req),
    Mount = list_to_binary(?DAV_MOUNT),
    case Path of
        [ _, Mount ] ->
            couch_httpd_dav_srv;
        [ _, Mount | Tail ] ->
            % Does Path contain a segment that is in the configured list of things to be stored on filesystem?
            case lists:any(
                fun(Name) -> lists:member(binary_to_list(Name), re:split(?DAV_FILES, ", ",[{return, list}])) end,
                Tail) of
                true ->
                     handle_methods( couch_httpd_dav_fs, Method, { Tail }, Req );       
                false ->
                    case Tail of
                        [ <<Dbname/binary>> ] ->
                            handle_methods( couch_httpd_dav_db, Method, { Dbname, show_handlers }, Req );                       
                        [ <<Dbname/binary>>, <<"_design">> ] ->
                            handle_methods( couch_httpd_dav_db, Method, { Dbname, show_ddocs}, Req );
                        [ <<Dbname/binary>>, <<"_design">>, <<DDoc_Name/binary>> ] ->
                            handle_methods(  couch_httpd_dav_ddoc, Method, { Dbname, DDoc_Name, []}, Req );
                        [ <<Dbname/binary>>, <<"_design">>, <<DDoc_Name/binary>>, DDoc_Path ] ->
                            handle_methods(  couch_httpd_dav_ddoc, Method, { Dbname, DDoc_Name, DDoc_Path}, Req );
                        [ <<Dbname/binary>>, <<"_all_docs">> ] ->
                            handle_methods( couch_httpd_dav_db, Method, { Dbname, show_docs }, Req );
                        [ <<Dbname/binary>>, <<"_all_docs">>, <<Doc_Name/binary>> ] ->
                            handle_methods( couch_httpd_dav_doc, Method, { Dbname, Doc_Name, [] }, Req );
                        [ <<Dbname/binary>>, <<"_all_docs">>, <<Doc_Name/binary>>, Doc_Path ] ->
                            handle_methods( couch_httpd_dav_doc, Method, { Dbname, Doc_Name, Doc_Path}, Req ) end end end.
    
handle_methods( Module, Method, Dav_Opts, Req) ->       
    case Method of
        'OPTIONS'   -> handle_options( Module, Dav_Opts, Req );
        'PROPFIND'  -> handle_propfind( Module, Module:trans_opt( Dav_Opts ), Req );
        'DELETE'    -> handle_delete( Module, Module:trans_opt(Dav_Opts), Req );
        'PUT'       -> Module:put( Module:trans_opt(Dav_Opts), Req );
        'GET'       -> Module:get( Module:trans_opt(Dav_Opts), Req );
        'MKCOL'     -> couch_httpd:send_error(Req, 200, <<"OK">>, <<"But Not Implemented">>);
        'PROPPATCH' -> ok;
        'MOVE'      -> ok;
        'COPY'      -> ok;
        'LOCK'      -> couch_httpd:send_error(Req, 200, <<"OK">>, <<"But Not Implemented">>);
        'UNLOCK'    -> couch_httpd:send_error(Req, 200, <<"OK">>, <<"But Not Implemented">>) end.

handle_propfind( Module, Dav_Opts , #httpd{ mochi_req = MochiReq } = Req ) ->    
    Url = couch_httpd:absolute_uri( Req, couch_httpd:path(Req) ),
    Depth = case Module:is_directory( Dav_Opts ) of
        true ->
            case string:to_integer( MochiReq:get_header_value(depth) ) of 
                {0, []} -> 0;
                {1, []} -> 1;
                _ -> wrong_depth end;
        false -> 
            0;
        no_exist -> no_exist end,
    case Depth of 
        no_exist ->
            couch_httpd:send_error( Req, not_found );
        wrong_depth ->
            couch_httpd:send_error(Req, {forbidden, "Wrong Depth (min 0, max 1)"} );
        N ->
            Dav_struct = lists:map( 
                fun( {RelPath, Name, TempUrl} ) -> Module:make_dav_entry( { RelPath, Name }, TempUrl ) end, 
                Module:get_children( Dav_Opts, N, Url) ),
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
    {methods, Methods, mode, Mode} = Module:options(),
    OptionsHeaders = [ {"Allow", Methods }, {"DAV", Mode} ],
    couch_httpd:send_response(Req, 200, OptionsHeaders, <<>>).