% based on work from mochiweb_util

-module(couch_dav_util).
-export([maybe_unquote/1]).

-define(PERCENT, 37).  % $\%
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).                 
                    
unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

maybe_unquote(Binary) when is_binary(Binary) ->
    maybe_unquote(binary_to_list(Binary));
maybe_unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    case lists:member( <<Hi, Lo>>,  re:split( couch_config:get("web_dav", "dav_no_decode"), ", " ) ) of
        false -> 
            qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
        true ->
            qs_revdecode(Rest, [ $%, Hi, Lo | Acc]) end;
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).