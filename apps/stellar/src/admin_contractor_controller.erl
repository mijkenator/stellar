-module(admin_contractor_controller).

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

init(Req, Opts) -> utils_controller:controller_init(admin_contractor_controller, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [
        <<"invite_contractor">> 
    ]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"invite_contractor">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
	    Params = [ 
            {"email",    [<<"">>, required, undefined ]}
        ],
        [Email] = nwapi_utils:get_json_params(JSON, Params),
        model_contractor:invite_contractor(AccountId, Email),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("ACCICERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"get_contractors">> = A, _JSON, Req, Opts, {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
        model_contractor:get_contractors(),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("ACCICERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;

action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser NA UNKCOMMAND\nNOK">>, Req]), Opts}.



terminate(_Reason, _Req, _State) -> ok.

ver() -> 6.

