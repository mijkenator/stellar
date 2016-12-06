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

    {ok, { {one_for_all, 0, 1}, [
	_MemCached

   ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
