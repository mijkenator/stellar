%%%-------------------------------------------------------------------
%% @doc stellar public API
%% @end
%%%-------------------------------------------------------------------

-module(stellar_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, update/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", stellar_root_handler, []},
			{"/user", user_controller,  []},
			{"/admin/service", admin_service,  []}
		]}
    ]),
    {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
    }),
    emysql:add_pool(mysqlpool, [{size,1},
                     {user,"stellar"},
                     {password,"stellar"},
                     {database,"stellar"},
                     {encoding,utf8}]),
    stellar_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


update() ->
   Fn = fun(E) ->
	case re:run(E, "lib/stellar-\\d.\\d.\\d/ebin$", [{capture, none}]) of
		match -> true
		;_    -> false
	end
   end,
   [Path] = lists:filter(Fn, code:get_path()),
   Fn1 = fun(File) ->
	case re:run(File, "^(.+?)([^/]+).beam$") of
	    {match, [_,{B,L},{B1,L1}]} ->
		P = string:substr(File, B+1, L),
		M = string:substr(File, B1+1, L1),
		code:purge(list_to_existing_atom(M)),
		LR = code:load_abs(P++M),
		lager:debug("Reloaded ~p -> ~p", [M, LR])
	    ;_ -> ok
	end
   end,
   lists:foreach(Fn1, filelib:wildcard(Path ++ "/*.beam")).

%%====================================================================
%% Internal functions
%%====================================================================
