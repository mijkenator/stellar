-module(orders_queue).

-behaviour(gen_server).

-export([start_link/0]).

-export([
     init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3,
     take_order/2,
     update_orders/0
]).

take_order(Cid, Oid) -> gen_server:call(?MODULE, {cmd, take_order, [Cid, Oid]}).
update_orders()      -> gen_server:cast(?MODULE, {cmd, update_orders}).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({cmd, take_order, [Cid, Oid]}, _From, State) ->
    Ret = model_order:take_order(Cid, Oid),
    update_orders(),
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({cmd, update_orders}, State) ->
    _GetNewOrders = model_order:get_new_orders(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
