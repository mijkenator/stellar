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
	lists:member(Action, [<<"delete">>, <<"get_details">>, <<"set_details">>, 
                          <<"create_order">>, <<"get_orders">>, <<"update_orders">>, 
                          <<"cancel_order">>, <<"referral_activity">>, <<"invite">>, <<"make_stripe_payment">>]).


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
action(A, _JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"get_orders">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCD: ~p", [AccountId]),
    	Ret = model_order:admin_get_orders(AccountId, undefined),
        lager:debug("UCD RET: ~p", [Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"cancel_order">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCD: ~p", [AccountId]),
        Params = [ 
            {"order_id",    [undefined, required, undefined ]}],
        [Oid] = nwapi_utils:get_json_params(JSON, Params),
    	case model_order:cancel_order(AccountId, Oid) of
            {error, Error} ->
                lager:error("CU cancel order ~p ~p",[Oid, Error]),
                nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
            ;Ret ->
                lager:debug("UCD RET: ~p", [Ret]),
                ?OKRESP(A, [], Req, Opts)
        end
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
            {"phone",   [undefined, notrequired, undefined ]},
            {"lname",   [undefined, notrequired, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [Name, Street, Apt, Zip, City, State, Phone, LName] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
	    Ret = model_user:set_details(AccountId, Name, Street, Apt, Zip, City, State, Phone, LName),
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
            {"service_id",              [undefined, required, undefined ]},
            {"service_ontime",          [undefined, required, undefined ]},
            {"number_of_services",      [1, required, undefined ]},
            {"number_of_contractors",   [1, required, undefined ]},
            {"phone",                   [<<>>, notrequired, undefined ]},
            {"email",                   [<<>>, notrequired, undefined ]},
            {"street",                  [<<>>, notrequired, undefined ]},
            {"apt",                     [<<>>, notrequired, undefined ]},
            {"city",                    [<<>>, notrequired, undefined ]},
            {"state",                   [<<>>, notrequired, undefined ]},
            {"cell_phone",              [<<>>, notrequired, undefined ]},
            {"zip",                     [<<>>, notrequired, undefined ]},
            {"location",                [<<>>, notrequired, undefined ]},
            {"cost",                    [0,    required, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Location, Cost] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
        Gratuity = 0,
        Tax = 0,
        Ret = model_user:create_order(AccountId, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax, Location),
        lager:debug("UCD RET: ~p", [Ret]),
        case Ret of
            {ok_packet,1,1,Oid,_,_,_} ->
                  ?OKRESP(A, [{[ {<<"order_id">>, Oid}, {<<"amount_cents">>,Cost}  ]}], Req, Opts)
            ;_ -> ?OKRESP(A, [], Req, Opts)
        end
    catch
        E:R ->
            lager:error("CUCOERR: ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"make_stripe_payment">> = A, JSON, Req, Opts,  {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCCR: ~p", [JSON]),
        Params = [ 
                {"amount_cents",    [undefined, required, undefined ]},
                {"currency",        [<<"usd">>, required, undefined ]},
                {"token",           [undefined, required, undefined ]},
                {"orderid",         [undefined, required, undefined ]}
            ],
        [AmountCnts, Currency, Token, Orderid] = nwapi_utils:get_json_params(JSON, Params),
        case model_order:save_stripe_payment(AccountId, AmountCnts, Currency, Token, Orderid) of
            true -> ?OKRESP(A, [], Req, Opts);
	        {error, Error} ->
                lager:error("MSP error: ~p", [Error]),
                ?ERRRESP(Error, A, Req, Opts)
        end
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"invite">> = A, JSON, Req, Opts,  {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCCR: ~p", [JSON]),
        Params = [ 
                {"to_email",       [[], required, undefined ]}
            ],
        [Emails] = nwapi_utils:get_json_params(JSON, Params),
        
        lists:foreach(fun({E})-> model_user:send_invite(proplists:get_value(<<"email">>,E), AccountId, proplists:get_value(<<"name">>),E,<<>>) end,Emails),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"referral_activity">> = A, _JSON, Req, Opts,  {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        lager:debug("UCRA: ~p", [AccountId]),
        Ret = model_user:ref_activity(AccountId),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(<<"update_order">> = A, JSON, Req, Opts, {auth, SData, _SID}) ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        Params = [ 
            {"order_id",                [undefined, required, undefined ]},
            {"service_id",              [undefined, nonrequired, undefined ]},
            {"service_ontime",          [undefined, nonrequired, undefined ]},
            {"number_of_services",      [undefined, nonrequired, undefined ]},
            {"number_of_contractors",   [undefined, nonrequired, undefined ]},
            {"phone",                   [undefined, nonrequired, undefined ]},
            {"email",                   [undefined, nonrequired, undefined ]},
            {"street",                  [undefined, nonrequired, undefined ]},
            {"apt",                     [undefined, nonrequired, undefined ]},
            {"city",                    [undefined, nonrequired, undefined ]},
            {"state",                   [undefined, nonrequired, undefined ]},
            {"cell_phone",              [undefined, nonrequired, undefined ]},
            {"zip",                     [undefined, nonrequired, undefined ]},
            {"cost",                    [undefined, nonrequired, undefined ]}
        ],
        lager:debug("~p Params1: ~p", [A, Params]),
        [OrderId, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost] = nwapi_utils:get_json_params(JSON, Params),
        lager:debug("UCD: ~p", [AccountId]),
        Gratuity = 0,
        Tax = 0,
        Ret = model_user:update_order(OrderId, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax),
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
            {"password",       [undefined, required, undefined ]},
            {"refcode",    [<<>>, required, undefined ]}
        ],
        [Login, Pwd, Refcode] = nwapi_utils:get_json_params(JSON, Params),
	case model_user:signup(Login, Pwd, Refcode) of
	   {ok,Uid} -> ?OKRESP(A, [Uid], Req, Opts);
	   {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
	end
    catch
        E:R ->
            lager:error("CU SIGNERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(<<"restore_password">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCRestP1: ~p", [JSON]),
        Params = [ 
                {"login",       [undefined, required, undefined ]}
            ],
        [Login] = nwapi_utils:get_json_params(JSON, Params),
        Headers = cowboy_req:headers(Req),
        RpT = case maps:get(<<"origin">>, Headers, <<"">>) of
            Or when Or == <<"http://stellarmakeover.com">>;
                    Or == <<"https://stellarmakeover.com">>;
                    Or == <<"https://wwww.stellarmakeover.com">>;
                    Or == <<"http://wwww.stellarmakeover.com">> -> <<"user">>;
            Or1 ->
                lager:debug("Nonuser origin ~p", [Or1]),
                <<"pro">>
        end,
        lager:debug("UCRestP2: ~p ~p", [RpT, Headers]),
        case model_user:login_restore(Login, RpT) of
           {ok,_} -> ?OKRESP(A, [], Req, Opts);
           {error, Error} -> ?ERRRESP(Error, A, Req, Opts)
        end
    catch
        E:R ->
            lager:error("CU WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
nonauth_action(<<"check_refcode">> = A, JSON, Req, Opts, _Session) ->
    try
        lager:debug("UCCR: ~p", [JSON]),
        Params = [ 
                {"refcode",       [undefined, required, undefined ]}
            ],
        [Refcode] = nwapi_utils:get_json_params(JSON, Params),
        case model_user:check_refcode(Refcode) of
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
