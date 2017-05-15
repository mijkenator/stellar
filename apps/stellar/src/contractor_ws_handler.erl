-module(contractor_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-export([
    subscribe/1,
    notify/2
]).

init(Req, Opts) ->
    lager:debug("CWH init", []),
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    lager:debug("CWH ws_init ~p", [State]),
    erlang:start_timer(1000, self(), <<"ping">>),
    {ok, State}.

websocket_handle({text, <<"SUBSCRIBE">>},  State) ->
    lager:debug("WSH SUBSCRIBE",[]),
    subscribe(<<"new_orders">>),
    {reply, {text, << "SUBSCRIBED">>}, State};
websocket_handle({text, <<"{\"cmd\":",_/binary>>} = {text, Msg}, State) ->
    lager:debug("WSH CMD: '~p ~p'",[Msg, State]),
    try 
        {JSON} = jiffy:decode(Msg),
        lager:debug("WSH JSON: '~p'",[JSON]),
        case proplists:get_value(<<"cmd">>, JSON) of
            <<"subscribe">> ->
                Uid = proplists:get_value(<<"uid">>, JSON, 0),
                lager:debug("WSH CMD SUBSCR: for uid: ~p",[Uid]),
                subscribe(<<"new_orders">>),
                lager:debug("WSH CMD SUBSCR OK",[]),
                Ret = jiffy:encode({[
                    {<<"orders">>, model_order:get_new_orders(Uid)}
                ]}),
                {reply, {text, Ret}, [Uid]}
        end
    catch
        _E:_R ->
            lager:error("WSH cmd error: ~p ~p", [_E,_R]),
            {reply, {text, <<"{\"status\":\"failed\"}">>}, State}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    lager:debug("PING",[]),
    {reply, {text, <<"pong">>}, State};
websocket_handle({text, Msg}, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(10000, self(), <<"ping">>),
    {reply, {text, Msg}, State};
websocket_info({_Pid, {contractor_ws_handler, <<"new_orders">>}, _Msg}, State) ->
    Uid = case State of
        [Uid0] -> Uid0
        ;_     -> 0
    end,
    NO = model_order:get_new_orders(Uid),
    Ret = jiffy:encode({[
        {<<"orders">>, NO}
    ]}),
    {reply, {text, Ret}, State};
websocket_info(_Info, State) ->
    {ok, State}.

subscribe(EventType) ->
    %% Gproc notation: {p, l, Name} means {(p)roperty, (l)ocal, Name}
    gproc:reg({p, l, {?MODULE, EventType}}).

notify(EventType, Msg) ->
    Key = {?MODULE, EventType},
    gproc:send({p, l, Key}, {self(), Key, Msg}).

