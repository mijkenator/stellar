-module(user_controller).

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

init(Req, Opts) -> utils_controller:controller_init(user_controller, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [<<"delete">>, <<"get_details">>, <<"set_details">>, <<"create_order">>]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(A, _JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"delete">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCD: ~p", [AccountId]),
	Ret = model_user:delete_user(AccountId),
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
        lager:debug("UCD: ~p", [AccountId]),
	    Ret = model_user:get_details(AccountId),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"set_details">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        Params = [ 
            {"name",    [undefined, notrequired, undefined ]},
            {"street",  [undefined, notrequired, undefined ]},
            {"apt",     [undefined, notrequired, undefined ]},
            {"zip",     [undefined, notrequired, undefined ]},
            {"city",    [undefined, notrequired, undefined ]},
            {"state",   [undefined, notrequired, undefined ]},
            {"phone",   [undefined, notrequired, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [Name, Street, Apt, Zip, City, State, Phone] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
	    Ret = model_user:set_details(AccountId, Name, Street, Apt, Zip, City, State, Phone),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"create_order">> = A, JSON, Req, Opts, {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        Params = [ 
            {"contractor_id",           [undefined, required, undefined ]},
            {"service_id",              [undefined, required, undefined ]},
            {"service_ontime",          [undefined, required, undefined ]},
            {"number_of_services",      [undefined, required, undefined ]},
            {"number_of_contractors",   [undefined, required, undefined ]},
            {"cost",                    [undefined, required, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [Cid, Sid, DTime, ServNum, CNum, Cost] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
        Gratuity = 0,
        Tax = 0,
        Ret = model_user:create_order(AccountId, Cid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;

action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(<<"signup-confirm">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCSIGNUP-C: ~p", [JSON]),
	Params = [ 
            {"guid",    [undefined, required, undefined ]}
        ],
        [Guid] = nwapi_utils:get_json_params(JSON, Params),
	case model_user:signup_confirm(Guid) of
	   {ok,Uid} -> ?OKRESP(A, [Uid], Req, Opts);
	   {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
	end
    catch
        E:R ->
            lager:error("CU SC ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(<<"restore-pwd-confirm">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCRPC: ~p", [JSON]),
	Params = [ 
            {"guid",        [undefined, required, undefined ]},
            {"password",    [undefined, required, undefined ]}
        ],
        [Guid, Password] = nwapi_utils:get_json_params(JSON, Params),
	case model_user:rpwd_confirm(Guid, Password) of
	   {ok,Uid} -> ?OKRESP(A, [Uid], Req, Opts);
	   {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
	end
    catch
        E:R ->
            lager:error("CU UCRPC ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(<<"signup">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCSIGNUP: ~p", [JSON]),
	Params = [ 
            {"login",       [undefined, required, undefined ]},
            {"password",    [undefined, required, undefined ]}
        ],
        [Login, Pwd] = nwapi_utils:get_json_params(JSON, Params),
	case model_user:signup(Login, Pwd) of
	   {ok,Uid} -> ?OKRESP(A, [Uid], Req, Opts);
	   {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
	end
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(<<"restore_password">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCSIGNUP: ~p", [JSON]),
	Params = [ 
            {"login",       [undefined, required, undefined ]}
        ],
        [Login] = nwapi_utils:get_json_params(JSON, Params),
	case model_user:login_restore(Login) of
	   {ok,_} -> ?OKRESP(A, [], Req, Opts);
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

ver() -> 6.
