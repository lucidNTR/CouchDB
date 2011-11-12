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


% bind_path is based on bind method from Webmachine
% @doc Module for URL rewriting by pattern matching.

% (This Module could be inspired by RackDAV a Rack implementation of DAV)

% This is the WebDAV handler for CouchDB Design documents, you can mount a databases design doc 
% by pointing your DAV Client to http://server/database/_appdav/(...) 
% The nececarry steps are specified as follows:

% 1. Patch couchdb to accept additional methods: OPTIONS, PROPFIND, PROPPATCH, DELETE, MOVE, COPY
% (still not exactly clear where to do that...) 

% 2. Catch mehthod OPTIONS for http://server/_appdav and expose that additional methods are 
% available by generating propper HTTP response

% 3. Catch method PROPFIND ->
% Path /                            -> get all_dbs and generate dav-xml
% Path /<db>/                       -> expose all available database handlers as folders (e.g. _design, _view, _list, _show)
% Path /<db>/_design/               -> get all design docs in <db> and generate dav-xml (each ddoc is a FOLDER!) 
% Path /<db>/<_design>/<ddoc>       -> get ddoc and expose each key as file or folder based on the following rule...
    % key is _rev or _id                        -> hide
    % value is value or array                   -> as file
    % value is object                           -> as folder
% Path /<db>/<_design>/<ddoc>/<filename>        -> serve value as filestream
% Path /<db>/<_design>/<ddoc>/<foldername>      -> expose values as folders or files, based on same rule as above
% etc. do this to max. depth that is configurable in dav_max_depth in config file

% this should be enough to actually mount the ddoc in osx, do testing and additional specs...

-module(couch_httpd_dav).

-export([handle_dav_req/1]).
-export([handle_dav_req/3]).

-include("couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_response/4,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).

-define(SEPARATOR, $\/).
-define(MATCH_ALL, {bind, <<"*">>}).
-define(METHODS, '"GET","HEAD","POST","PUT","DELETE","TRACE","CONNECT","COPY","OPTIONS","PROPPATCH","PROPFIND","MOVE"').
-define(XML_MOCK, ["
<?xml version=\"1.0\" encoding=\"utf-8\"?>

<D:multistatus xmlns:D=\"DAV:\">

	<D:response xmlns:lp1=\"DAV:\" xmlns:lp2=\"http://apache.org/dav/props/\">
		<D:href>/</D:href>
		<D:propstat>
			<D:prop>
				<D:getetag>\"9e07ce-0-658fcf77\"</D:getetag>
				<D:creationdate>2009-05-08T13:13:38Z</D:creationdate>
				<D:getlastmodified>Fri, 08 May 2009 13:13:38 GMT</D:getlastmodified>
				<D:displayname/>
				<D:resourcetype>
					<D:collection/>
				</D:resourcetype>
				<D:getcontentlength>0</D:getcontentlength>
				<D:getcontenttype>httpd/unix-directory</D:getcontenttype>
				<D:getcontentlanguage/>
			</D:prop>
			<D:status>HTTP/1.1 200 OK</D:status>
		</D:propstat>
	</D:response>
	
	<D:response xmlns:ns2=\"http://apache.org/dav/props/\" xmlns:lp1=\"DAV:\" xmlns:lp2=\"http://apache.org/dav/props/\">
		<D:href>/Neuer%20Ordner</D:href>
		<D:propstat>
			<D:prop>
				<ns2:executable>T</ns2:executable>
				<D:getetag>\"9e07db-0-f392ad85\"</D:getetag>
				<D:creationdate>2010-06-08T00:34:25Z</D:creationdate>
				<D:getlastmodified>Tue, 08 Jun 2010 00:34:25 GMT</D:getlastmodified>
				<D:displayname/>
				<D:resourcetype>
					<D:collection/>
				</D:resourcetype>
				<D:getcontentlength>0</D:getcontentlength>
				<D:getcontenttype>httpd/unix-directory</D:getcontenttype>
				<D:getcontentlanguage/>
			</D:prop>
			<D:status>HTTP/1.1 200 OK</D:status>
		</D:propstat>
	</D:response>
	
	<D:response xmlns:ns2=\"http://apache.org/dav/props/\" xmlns:lp1=\"DAV:\" xmlns:lp2=\"http://apache.org/dav/props/\">
		<D:href>/Uni</D:href>
		<D:propstat>
			<D:prop>
				<ns2:executable>T</ns2:executable>
				<D:getetag>\"9e07e0-0-90a19db\"</D:getetag>
				<D:creationdate>2010-08-17T18:22:44Z</D:creationdate>
				<D:getlastmodified>Tue, 17 Aug 2010 18:22:44 GMT</D:getlastmodified>
				<D:displayname/>
				<D:resourcetype>
					<D:collection/>
				</D:resourcetype>
				<D:getcontentlength>0</D:getcontentlength>
				<D:getcontenttype>httpd/unix-directory</D:getcontenttype>
				<D:getcontentlanguage/>
				<D:supportedlock>
					<D:lockentry>
						<D:lockscope>
							<D:exclusive/>
						</D:lockscope>
						<D:locktype>
							<D:write/>
						</D:locktype>
					</D:lockentry>
					<D:lockentry>
						<D:lockscope>
							<D:shared/>
						</D:lockscope>
						<D:locktype>
							<D:write/>
						</D:locktype>
					</D:lockentry>
				</D:supportedlock>
				<D:lockdiscovery/>
			</D:prop>
			<D:status>HTTP/1.1 200 OK</D:status>
		</D:propstat>
	</D:response>
	
	<D:response xmlns:ns2=\"http://apache.org/dav/props/\" xmlns:lp1=\"DAV:\" xmlns:lp2=\"http://apache.org/dav/props/\">
		<D:href>/File</D:href>
		<D:propstat>
			<D:prop>
				<ns2:executable>T</ns2:executable>
				<D:getetag>\"02.16.201\"</D:getetag>
				<D:creationdate>2010-06-09T05:45:10Z</D:creationdate>
				<D:getlastmodified>Wed, 09 Jun 2010 05:45:10 GMT</D:getlastmodified>
				<D:displayname/>
				<D:resourcetype/>
				<D:getcontentlength>0</D:getcontentlength>
				<D:getcontenttype>application/octetstream</D:getcontenttype>
				<D:getcontentlanguage/>
			</D:prop>
			<D:status>HTTP/1.1 200 OK</D:status>
		</D:propstat>
	</D:response>
	
</D:multistatus> "]).

handle_dav_req( #httpd{ method = 'OPTIONS' } = Req ) ->       
    OptionsHeaders = [
        {"Allow", ?METHODS},
        {"DAV", "1"}
    ],
    send_json(Req, 200, OptionsHeaders, {[
        {couchdb, "sends you the _dav OPTIONS"}
    ]});

%Implement Depth up to depth = 3!
handle_dav_req( #httpd{ path_parts =  [<<"_dav">>] , method = 'PROPFIND' } = Req ) ->      
    Headers = [ {"DAV", "1"},{"Content-Type", "text/xml; charset= \"utf-8\" "}],
    send_response(Req, 207, Headers, ?XML_MOCK).


%Implement OS-X Hidden Files serving from Folder, stupid osx request rejection and Windows special fields




handle_dav_req( #httpd{ path_parts = PathOrig, method = Method, mochi_req = MochiReq } = Req, _ ) ->
               
            % normalize final path (fix levels "." and "..")
            RawPath1 ="/", %?b2l(iolist_to_binary(normalize_path(RawPath))),

            ?LOG_DEBUG("rewrite to ~p ~n", [RawPath1]),

            % build a new mochiweb request
            MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                             MochiReq:get(method),
                                             RawPath1,
                                             MochiReq:get(version),
                                             MochiReq:get(headers)),

            % cleanup, It forces mochiweb to reparse raw uri.
            MochiReq1:cleanup(),

            #httpd{
                db_url_handlers = DbUrlHandlers,
                design_url_handlers = DesignUrlHandlers,
                default_fun = DefaultFun,
                url_handlers = UrlHandlers
            } = Req,
            
            couch_httpd:handle_request_int(MochiReq1, DefaultFun, UrlHandlers, DbUrlHandlers, DesignUrlHandlers).





% Only existing couchdb code from url rewriter after this line
% TODO: rewrite this code to match webdav needs and get rid of 
% unnecessary code for generic url rewriting...

handle_dav_req(#httpd{
        path_parts=[DbName, <<"_design">>, DesignName, _Rewrite|PathParts],
        method=Method,
        mochi_req=MochiReq}=Req, _Db, DDoc) ->

    % we are in a design handler
    DesignId = <<"_design/", DesignName/binary>>,
    Prefix = <<"/", DbName/binary, "/", DesignId/binary>>,
    QueryList = lists:map(fun decode_query_value/1, couch_httpd:qs(Req)),

    #doc{body={Props}} = DDoc,

    % get rules from ddoc
    case couch_util:get_value(<<"rewrites">>, Props) of
        undefined ->
            couch_httpd:send_error(Req, 404, <<"rewrite_error">>,
                <<"Invalid path.">>);
        Bin when is_binary(Bin) ->
            couch_httpd:send_error(Req, 400, <<"rewrite_error">>,
                <<"Rewrite rules are a String. They must be a JSON Array.">>);
        Rules ->
            % create dispatch list from rules
            DispatchList =  [make_rule(Rule) || {Rule} <- Rules],
            Method1 = couch_util:to_binary(Method),

            %% get raw path by matching url to a rule.
            RawPath = case try_bind_path(DispatchList, Method1, 
                    PathParts, QueryList) of
                no_dispatch_path ->
                    throw(not_found);
                {NewPathParts, Bindings} ->
                    Parts = [quote_plus(X) || X <- NewPathParts],

                    % build new path, reencode query args, eventually convert
                    % them to json
                    Bindings1 = maybe_encode_bindings(Bindings),
                    Path = binary_to_list(
                        iolist_to_binary([
                                string:join(Parts, [?SEPARATOR]),
                                [["?", mochiweb_util:urlencode(Bindings1)] 
                                    || Bindings1 =/= [] ]
                            ])),
                    
                    % if path is relative detect it and rewrite path
                    case mochiweb_util:safe_relative_path(Path) of
                        undefined ->
                            ?b2l(Prefix) ++ "/" ++ Path;
                        P1 ->
                            ?b2l(Prefix) ++ "/" ++ P1
                    end

                end,

            % normalize final path (fix levels "." and "..")
            RawPath1 = ?b2l(iolist_to_binary(normalize_path(RawPath))),

            ?LOG_DEBUG("rewrite to ~p ~n", [RawPath1]),

            % build a new mochiweb request
            MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                             MochiReq:get(method),
                                             RawPath1,
                                             MochiReq:get(version),
                                             MochiReq:get(headers)),

            % cleanup, It force mochiweb to reparse raw uri.
            MochiReq1:cleanup(),

            #httpd{
                db_url_handlers = DbUrlHandlers,
                design_url_handlers = DesignUrlHandlers,
                default_fun = DefaultFun,
                url_handlers = UrlHandlers
            } = Req,
            couch_httpd:handle_request_int(MochiReq1, DefaultFun,
                    UrlHandlers, DbUrlHandlers, DesignUrlHandlers)
        end.

quote_plus({bind, X}) ->
    mochiweb_util:quote_plus(X);
quote_plus(X) ->
    mochiweb_util:quote_plus(X).

%% @doc Try to find a rule matching current url. If none is found
%% 404 error not_found is raised
try_bind_path([], _Method, _PathParts, _QueryList) ->
    no_dispatch_path;
try_bind_path([Dispatch|Rest], Method, PathParts, QueryList) ->
    [{PathParts1, Method1}, RedirectPath, QueryArgs, Formats] = Dispatch,
    case bind_method(Method1, Method) of
        true ->
            case bind_path(PathParts1, PathParts, []) of
                {ok, Remaining, Bindings} ->
                    Bindings1 = Bindings ++ QueryList,
                    % we parse query args from the rule and fill
                    % it eventually with bindings vars
                    QueryArgs1 = make_query_list(QueryArgs, Bindings1,
                        Formats, []),
                    % remove params in QueryLists1 that are already in
                    % QueryArgs1
                    Bindings2 = lists:foldl(fun({K, V}, Acc) ->
                        K1 = to_binding(K),
                        KV = case couch_util:get_value(K1, QueryArgs1) of
                            undefined -> [{K1, V}];
                            _V1 -> []
                        end,
                        Acc ++ KV
                    end, [], Bindings1),

                    FinalBindings = Bindings2 ++ QueryArgs1,
                    NewPathParts = make_new_path(RedirectPath, FinalBindings,
                                    Remaining, []),
                    {NewPathParts, FinalBindings};
                fail ->
                    try_bind_path(Rest, Method, PathParts, QueryList)
            end;
        false ->
            try_bind_path(Rest, Method, PathParts, QueryList)
    end.

%% rewriting dynamically the quey list given as query member in
%% rewrites. Each value is replaced by one binding or an argument
%% passed in url.
make_query_list([], _Bindings, _Formats, Acc) ->
    Acc;
make_query_list([{Key, {Value}}|Rest], Bindings, Formats, Acc) ->
    Value1 = {Value},
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) when is_binary(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) when is_list(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) ->
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value}|Acc]).

replace_var(<<"*">>=Value, Bindings, Formats) ->
    get_var(Value, Bindings, Value, Formats);
replace_var(<<":", Var/binary>> = Value, Bindings, Formats) ->
    get_var(Var, Bindings, Value, Formats);
replace_var(Value, _Bindings, _Formats) when is_binary(Value) ->
    Value;
replace_var(Value, Bindings, Formats) when is_list(Value) ->
    lists:reverse(lists:foldl(fun
                (<<":", Var/binary>>=Value1, Acc) ->
                    [get_var(Var, Bindings, Value1, Formats)|Acc];
                (Value1, Acc) ->
                    [Value1|Acc]
            end, [], Value));
replace_var(Value, _Bindings, _Formats) ->
    Value.
                    
maybe_json(Key, Value) ->
    case lists:member(Key, [<<"key">>, <<"startkey">>, <<"start_key">>,
                <<"endkey">>, <<"end_key">>, <<"keys">>]) of
        true ->
            ?JSON_ENCODE(Value);
        false ->
            Value
    end.

get_var(VarName, Props, Default, Formats) ->
    VarName1 = to_binding(VarName),
    Val = couch_util:get_value(VarName1, Props, Default),
    maybe_format(VarName, Val, Formats).

maybe_format(VarName, Value, Formats) ->
    case couch_util:get_value(VarName, Formats) of
        undefined ->
             Value;
        Format ->
            format(Format, Value)
    end.

format(<<"int">>, Value) when is_integer(Value) ->
    Value;
format(<<"int">>, Value) when is_binary(Value) ->
    format(<<"int">>, ?b2l(Value));
format(<<"int">>, Value) when is_list(Value) ->
    case (catch list_to_integer(Value)) of
        IntVal when is_integer(IntVal) ->
            IntVal;
        _ ->
            Value
    end;
format(<<"bool">>, Value) when is_binary(Value) ->
    format(<<"bool">>, ?b2l(Value));
format(<<"bool">>, Value) when is_list(Value) ->
    case string:to_lower(Value) of
        "true" -> true;
        "false" -> false;
        _ -> Value
    end;
format(_Format, Value) ->
   Value. 

%% doc: build new patch from bindings. bindings are query args
%% (+ dynamic query rewritten if needed) and bindings found in
%% bind_path step.
make_new_path([], _Bindings, _Remaining, Acc) ->
    lists:reverse(Acc);
make_new_path([?MATCH_ALL], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_new_path([?MATCH_ALL|_Rest], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_new_path([{bind, P}|Rest], Bindings, Remaining, Acc) ->
    P2 = case couch_util:get_value({bind, P}, Bindings) of
        undefined -> << "undefined">>;
        P1 -> 
            iolist_to_binary(P1)
    end,
    make_new_path(Rest, Bindings, Remaining, [P2|Acc]);
make_new_path([P|Rest], Bindings, Remaining, Acc) ->
    make_new_path(Rest, Bindings, Remaining, [P|Acc]).


%% @doc If method of the query fith the rule method. If the
%% method rule is '*', which is the default, all
%% request method will bind. It allows us to make rules
%% depending on HTTP method.
bind_method(?MATCH_ALL, _Method ) ->
    true;
bind_method({bind, Method}, Method) ->
    true;
bind_method(_, _) ->
    false.


%% @doc bind path. Using the rule from we try to bind variables given
%% to the current url by pattern matching
bind_path([], [], Bindings) ->
    {ok, [], Bindings};
bind_path([?MATCH_ALL], [Match|_RestMatch]=Rest, Bindings) ->
    {ok, Rest, [{?MATCH_ALL, Match}|Bindings]};
bind_path(_, [], _) ->
    fail;
bind_path([{bind, Token}|RestToken],[Match|RestMatch],Bindings) ->
    bind_path(RestToken, RestMatch, [{{bind, Token}, Match}|Bindings]);
bind_path([Token|RestToken], [Token|RestMatch], Bindings) ->
    bind_path(RestToken, RestMatch, Bindings);
bind_path(_, _, _) ->
    fail.


%% normalize path.
normalize_path(Path)  ->
    "/" ++ string:join(normalize_path1(string:tokens(Path,
                "/"), []), [?SEPARATOR]).


normalize_path1([], Acc) ->
    lists:reverse(Acc);
normalize_path1([".."|Rest], Acc) ->
    Acc1 = case Acc of
        [] -> [".."|Acc];
        [T|_] when T =:= ".." -> [".."|Acc];
        [_|R] -> R
    end,
    normalize_path1(Rest, Acc1);
normalize_path1(["."|Rest], Acc) ->
    normalize_path1(Rest, Acc);
normalize_path1([Path|Rest], Acc) ->
    normalize_path1(Rest, [Path|Acc]).


%% @doc transform json rule in erlang for pattern matching
make_rule(Rule) ->
    Method = case couch_util:get_value(<<"method">>, Rule) of
        undefined -> ?MATCH_ALL;
        M -> to_binding(M)
    end,
    QueryArgs = case couch_util:get_value(<<"query">>, Rule) of
        undefined -> [];
        {Args} -> Args
        end,
    FromParts  = case couch_util:get_value(<<"from">>, Rule) of
        undefined -> [?MATCH_ALL];
        From ->
            parse_path(From)
        end,
    ToParts  = case couch_util:get_value(<<"to">>, Rule) of
        undefined ->
            throw({error, invalid_rewrite_target});
        To ->
            parse_path(To)
        end,
    Formats = case couch_util:get_value(<<"formats">>, Rule) of
        undefined -> [];
        {Fmts} -> Fmts
    end,
    [{FromParts, Method}, ToParts, QueryArgs, Formats].

parse_path(Path) ->
    {ok, SlashRE} = re:compile(<<"\\/">>),
    path_to_list(re:split(Path, SlashRE), [], 0).

%% @doc convert a path rule (from or to) to an erlang list
%% * and path variable starting by ":" are converted
%% in erlang atom.
path_to_list([], Acc, _DotDotCount) ->
    lists:reverse(Acc);
path_to_list([<<>>|R], Acc, DotDotCount) ->
    path_to_list(R, Acc, DotDotCount);
path_to_list([<<"*">>|R], Acc, DotDotCount) ->
    path_to_list(R, [?MATCH_ALL|Acc], DotDotCount);
path_to_list([<<"..">>|R], Acc, DotDotCount) when DotDotCount == 2 ->
    case couch_config:get("httpd", "secure_rewrites", "true") of
    "false" ->
        path_to_list(R, [<<"..">>|Acc], DotDotCount+1);
    _Else ->
        ?LOG_INFO("insecure_rewrite_rule ~p blocked", [lists:reverse(Acc) ++ [<<"..">>] ++ R]),
        throw({insecure_rewrite_rule, "too many ../.. segments"})
    end;
path_to_list([<<"..">>|R], Acc, DotDotCount) ->
    path_to_list(R, [<<"..">>|Acc], DotDotCount+1);
path_to_list([P|R], Acc, DotDotCount) ->
    P1 = case P of
        <<":", Var/binary>> ->
            to_binding(Var);
        _ -> P
    end,
    path_to_list(R, [P1|Acc], DotDotCount).

maybe_encode_bindings([]) ->
    [];
maybe_encode_bindings(Props) -> 
    lists:foldl(fun 
            ({{bind, <<"*">>}, _V}, Acc) ->
                Acc;
            ({{bind, K}, V}, Acc) ->
                V1 = iolist_to_binary(maybe_json(K, V)),
                [{K, V1}|Acc]
        end, [], Props).
                
decode_query_value({K,V}) ->
    case lists:member(K, ["key", "startkey", "start_key",
                "endkey", "end_key", "keys"]) of
        true ->
            {to_binding(K), ?JSON_DECODE(V)};
        false ->
            {to_binding(K), ?l2b(V)}
    end.

to_binding({bind, V}) ->
    {bind, V};
to_binding(V) when is_list(V) ->
    to_binding(?l2b(V));
to_binding(V) ->
    {bind, V}.
