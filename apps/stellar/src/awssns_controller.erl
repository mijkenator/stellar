-module(awssns_controller).

-include("nwapi.hrl").
-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
    lager:debug("AWSSNS opts: ~p", [Opts]),
    lager:debug("AWSSNS req:  ~p", [Req]),
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, _Req2} = cowboy_req:read_body(Req),
            lager:debug("AWSSNS body:  ~p", [Body])
        ;_   ->
            lager:debug("AWSSNS no body", [])
    end,
    ?OKRESP(<<"awssms">>, [], Req, Opts).

terminate(_Reason, _Req, _State) -> ok.
