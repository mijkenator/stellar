-module(admin_user_controller).

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

init(Req, Opts) -> utils_controller:controller_init(admin_user_controller, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [
        <<"get_users">>
    ]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(<<"get_users">> = A, _JSON, Req, Opts, {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
        Ret = model_user:get_users(),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("ACCICERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;

action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(<<"send_support_message">> = A, JSON, Req, Opts, _) ->
    try
        
        Params = [ 
            {"fname",           [<<>>, notrequired, undefined ]},
            {"lname",           [<<>>, notrequired, undefined ]},
            {"email",           [<<>>, notrequired, undefined ]},
            {"message",         [<<>>, notrequired, undefined ]},
            {"as",              [<<"customer">>, notrequired, undefined ]},
            {"phone",           [<<>>, notrequired, undefined ]}
        ],
        lager:debug("~p SSM1", [A]),
        [FName, LName, Email, Msg, As, Phone] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("SSM2: ~p", [{FName, LName, Email, Msg, As, Phone}]),
        Mail = <<"Subject: message from", As/binary, "\n\n", "From:", FName/binary, " ", LName/binary, 
            " email:", Email/binary, "phone:", Phone/binary, " \n",
            "Message:", Msg/binary>>,
	    Ersp = nwapi_utils:send_email(<<"support@stellarmakeover.com">>, Mail), 
        Ret = [],
        lager:debug("SSM3: ~p", [Ersp]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("ACCSSM ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser NA UNKCOMMAND\nNOK">>, Req]), Opts}.



terminate(_Reason, _Req, _State) -> ok.

ver() -> 6.


