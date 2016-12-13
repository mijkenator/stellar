-module(admin_service).

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

init(Req, Opts) -> utils_controller:controller_init(admin_service, Req, Opts).

-spec is_auth_method(binary()) -> boolean().
is_auth_method(Action) when is_binary(Action) ->
	lists:member(Action, [
        <<"get_categories">>, <<"create_category">>, 
        <<"delete_category">>, <<"edit_category">>,
        <<"get_services">>
    ]).


get_action(_, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser  UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_get_action(_, Req, Opts) ->
    {ok, ?NWR(reply, [<<"CUser NAGA UNKCOMMAND\nNOK">>, Req]), Opts}.

action(A, _JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"get_categories">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
        Ret = model_service:get_categories(),
        lager:debug("ASC ~p RET: ~p", [A, Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CAC WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"create_category">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
	    Params = [ 
            {"name",    [<<"">>, required, undefined ]}
        ],
        [Name] = nwapi_utils:get_json_params(JSON, Params),
        model_service:create_category(Name),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CAC WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"delete_category">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
	    Params = [ 
            {"id",    [<<"0">>, required, undefined ]}
        ],
        [Id] = nwapi_utils:get_json_params(JSON, Params),
        model_service:delete_category(Id),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CAC WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"edit_category">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
	    Params = [ 
            {"id",      [<<"0">>, required, undefined ]},
            {"name",    [<<"">>, required, undefined ]}
        ],
        [Id, Name] = nwapi_utils:get_json_params(JSON, Params),
        model_service:update_category(Id, Name),
        ?OKRESP(A, [], Req, Opts)
    catch
        E:R ->
            lager:error("CAC WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;
action(A, _JSON, Req, Opts, {auth, SData, _SID}) when  A == <<"get_services">> ->
    try
        AccountId = proplists:get_value(<<"account_id">>, SData),
        UT 	  = proplists:get_value(<<"user_type">>, SData),
        lager:debug("ASC ~p ~p", [A, {AccountId, UT}]),
        UT =:= 2 orelse throw({error, bad_user_type }),
        Ret = model_service:get_services(),
        lager:debug("ASC ~p RET: ~p", [A, Ret]),
        ?OKRESP(A, Ret, Req, Opts)
    catch
        E:R ->
            lager:error("CAC WREFERR ~p ~p",[E,R]),
            nwapi_utils:old_error_resp(?UNKNOWN_ERROR, A, Req, Opts)
    end;

action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUserA UNKCOMMAND\nNOK">>, Req]), Opts}.

nonauth_action(_Action, _, Req, Opts, _) ->
    {ok, ?NWR(reply, [<<"CUser NA UNKCOMMAND\nNOK">>, Req]), Opts}.



terminate(_Reason, _Req, _State) -> ok.

ver() -> 6.
