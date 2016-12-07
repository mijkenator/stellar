-module(utils_controller).

-include("nwapi.hrl").
-export([
    init_get_post/6,
    controller_init/3
    ]).

init_get_post(Req0, Opts, ActionFun, NonauthActionFun, GetActionFun, IsAuthMethodFun) ->
    lager:debug("UCIGP_6 _0", []),
    {SessionStatus, Req, SData, SID} = mkh_session:check(Req0),
    Method = cowboy_req:method(Req),
    lager:debug("UCIGP_6: ~p ~p", [SessionStatus, Method]),
    case {cowboy_req:has_body(Req), Method} of
        {true, <<"POST">>} ->
            {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),
            Request = proplists:get_value(<<"request">>, PostVals), 
            {JSON}  = jiffy:decode(Request),
            Action  = proplists:get_value(<<"type">>, JSON), 
            case IsAuthMethodFun(Action) of
                true -> ActionFun(Action, JSON, Req2, Opts, {SessionStatus, SData, SID})
                ;_   -> NonauthActionFun(Action, JSON, Req2, Opts, {SessionStatus, SData, SID})
            end;
        {_, <<"GET">>} ->
            #{type := T} = cowboy_req:match_qs([{type,[],<<"unknown">>}], Req),
            GetActionFun(T, Req, Opts, {SessionStatus, SData, SID})
        ;_   -> ?NWR(error_reply, [<<"bad request">>, Req])
    end.

-spec controller_init(atom(), cowboy_req:req(),_) -> {'ok',cowboy_req:req(),_} | cowboy_req:req().
controller_init(Module, Req0, Opts) ->
    lager:debug("~p UC INIT", [Module]),
    {SessionStatus, Req, SData, SID} = mkh_session:check(Req0),
    Method = cowboy_req:method(Req),
    lager:debug("UC1 : ~p ~p", [SessionStatus, Method]),
    case {cowboy_req:has_body(Req), Method} of
        {true, <<"POST">>} ->
            {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),
            Request = proplists:get_value(<<"request">>, PostVals), 
            {JSON}  = jiffy:decode(Request),
            lager:debug("UC2 POST: ~p", [JSON]),
            {Action, JSON2} = case proplists:get_value(<<"commands">>, JSON) of
                undefined -> {proplists:get_value(<<"type">>, JSON), JSON}
                ;[{Command}|_] -> {proplists:get_value(<<"type">>, Command), Command}
            end,
            lager:debug("UC POST action: ~p", [Action]),
            Login1 = proplists:get_value(<<"login">>,    JSON2, <<"">>),
            Pwd    = proplists:get_value(<<"password">>, JSON2, <<"">>),
            Login  = nwapi_utils:clean_login(Login1),

            lager:debug("UCAUTHP ~p  -> ~p ~p", [Login1, Login, Login == Login1]),

            case apply(Module, is_auth_method, [Action]) of
                true -> controller_action({post, Login, Pwd}, Module, Action, JSON2, Req2, Opts, {SessionStatus, SData, SID})
                ;_   -> apply(Module, nonauth_action, [Action, JSON2, Req2, Opts, {SessionStatus, SData, SID}])
            end;
        {_, <<"GET">>} ->
            #{type := T, login := Login, password := Pwd} = 
                cowboy_req:match_qs([{type,[],<<"unknown">>}, 
                                    {login,[],<<"">>}, 
                                    {password,[],<<"">>}], Req),
            Login1 = nwapi_utils:clean_login(Login),
            lager:debug("UCAUTHG ~p  -> ~p ~p", [Login1, Login, Login == Login1]),
            case apply(Module, is_auth_method, [T]) of
                true -> controller_action({get, Login1, Pwd}, Module, T, [], Req, Opts, {SessionStatus, SData, SID})
                ;_   -> apply(Module, nonauth_get_action, [T, Req, Opts])
            end
        ;_   -> ?NWR(error_reply, [<<"bad request">>, Req])
    end.

controller_action({Rtype, Login, Pwd}, Module, Action, JSON, Req, Opts, {nonauth,_,SID})     ->
    lager:debug("UCCA T2", []),
    IP = case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        IPT when is_binary(IPT) -> IPT
        ;_                      -> <<"">>
    end,
    case mkh_auth:login(Login, Pwd, SID, IP) of
        {ok, _, _} ->
              {auth, SData} = mkh_session:check_session(SID),
              case Rtype of
                get  -> apply(Module, get_action, [Action, Req, Opts, {auth, SData, SID}]);
                post -> apply(Module, action, [Action, JSON, Req, Opts, {auth, SData, SID}])
              end
        ;_ -> {ok, ?NWR(error_reply, [Action, <<"wrong login or password">>, Req]), Opts}
    end;
controller_action({get, _, _}, Module, Action, _, Req, Opts, {ST, SData, _} = Session) ->
    lager:debug("UCCA T3 ~p", [{Module, Action, ST, SData}]),
    apply(Module, get_action, [Action, Req, Opts, Session]);
controller_action({post, _, _}, Module, Action, JSON, Req, Opts, {ST, SData, _} = Session) ->
    lager:debug("UCCA T4 ~p", [{Module, Action, ST, SData}]),
    apply(Module, action, [Action, JSON, Req, Opts, Session]).



