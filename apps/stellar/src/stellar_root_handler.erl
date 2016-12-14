-module(stellar_root_handler).

-include("nwapi.hrl").
-include("nwapi_errors.hrl").

-export([init/2]).
-export([terminate/3]).
-export([
    is_auth_method/1,
    get_action/4,
    nonauth_get_action/3,
    action/5,
    nonauth_action/5,
    ver/0
]).

init(Req, Opts) -> utils_controller:controller_init(stellar_root_handler, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [<<"login">>, <<"check_session">>]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(A, JSON, Req, Opts, {auth, SData, _SID}) when A =:= <<"login">> ->
    try
        lager:debug("WREF0: ~p", [JSON]),
	    Params = [ 
            {"as",    [<<"user">>, required, undefined ]}
        ],
        [As] = nwapi_utils:get_json_params(JSON, Params),
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("WREF: ~p", [{AccountId, As}]),
    	Ret = #{<<"uid">> => AccountId, <<"ut">> => UT},
        lager:debug("WREF RET: ~p", [Ret]),
        lager:debug("WREF precase: ~p", [{As,UT}]),
        case {As,UT} of
            {<<"user">>, 0} -> ?OKRESP(A, Ret, Req, Opts);
            {<<"user">>, 1} -> ?OKRESP(A, Ret, Req, Opts);
            {<<"user">>, 2} -> ?OKRESP(A, Ret, Req, Opts);
            {<<"admin">>, 2} -> ?OKRESP(A, Ret, Req, Opts);
            {<<"contractor">>, 2} -> ?OKRESP(A, Ret, Req, Opts);
            {<<"contractor">>, 3} -> ?OKRESP(A, Ret, Req, Opts);
            _ -> nwapi_utils:error_reply(A, <<"authorization failed">>, Req)
        end
    catch
        E:R ->
            lager:error("CU WREFERR0 ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, _JSON, Req, Opts, {auth, SData, _SID}) when A =:= <<"check_session">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("WREF: ~p", [AccountId]),
    	Ret = #{<<"uid">> => AccountId, <<"ut">> => UT},
        lager:debug("WREF RET: ~p", [Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR1 ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(<<"logout">> = A, _JSON, Req, Opts, {SS,_,SID}) ->
    try
        case SS of
            auth -> mkh_session:delete_session(SID)
            ;_   -> ok
        end,
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser NA UNKCOMMAND\nNOK">>, Req]), Opts}.



terminate(_Reason, _Req, _State) -> ok.

ver() -> 6.
