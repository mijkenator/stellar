-module(contractor_controller).

-include("nwapi.hrl").
-include("nwapi_errors.hrl").

-export([init/2]).
-export([terminate/3]).
-export([
    is_auth_method/1,
    get_action/4,
    nonauth_get_action/3,
    action/5,
    nonauth_action/5
]).

init(Req, Opts) -> utils_controller:controller_init(contractor_controller, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [<<"get_details">>, <<"set_details">>]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"set_details">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        Params = [ 
            {"fname",           [<<>>, notrequired, undefined ]},
            {"lname",           [<<>>, notrequired, undefined ]},
            {"street",          [undefined, notrequired, undefined ]},
            {"apt",             [undefined, notrequired, undefined ]},
            {"zip",             [undefined, notrequired, undefined ]},
            {"city",            [undefined, notrequired, undefined ]},
            {"state",           [undefined, notrequired, undefined ]},
            {"cell_phone",      [undefined, notrequired, undefined ]},
            {"bank_routing",    [undefined, notrequired, undefined ]},
            {"bank_account",    [undefined, notrequired, undefined ]},
            {"phone",           [undefined, notrequired, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [FName, LName, Street, Apt, City, State, CPhone, BankR, BankA, Phone] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
	    Ret = model_contractor:set_details(AccountId, FName, LName, Street, Apt, City, State, CPhone, BankR, BankA, Phone),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, _JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"get_details">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("UCD: ~p", [AccountId]),
        UT > 1 orelse throw({error, bad_user_type }),
	    Ret = model_contractor:get_details(AccountId),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(<<"signup">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCSIGNUP: ~p", [JSON]),
        Params = [ 
                {"login",       [undefined, required, undefined ]},
                {"refcode",     [undefined, required, undefined ]},
                {"password",    [undefined, required, undefined ]}
            ],
        [Login, Refcode, Pwd] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCSIGNUPs: ~p", []),
        case model_contractor:signup(Login, Pwd, Refcode) of
           {ok,Uid} -> ?OKRESP(A, [Uid], Req, Opts);
           {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
        end
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser NA UNKCOMMAND\nNOK">>, Req]), Opts}.



terminate(_Reason, _Req, _State) -> ok.

