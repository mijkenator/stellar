%%%-------------------------------------------------------------------
%% @doc stellar top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(stellar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    _MemCached = {mainCluster,
        {mcd_cluster, start_link, [mainCluster, [
            {host1, ["localhost"], 10},
            {host2, ["localhost"], 20}
        ]]},
        permanent, 60000, worker, [mcd_cluster]},

    _OrdersQueue = {orders_queue, {orders_queue, start_link, []}, permanent, 60000, worker, [orders_queue]},

    {ok, { {one_for_all, 5, 10}, [
	_MemCached
    ,_OrdersQueue

   ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
