-module(model_user).

-export([
	signup/2
]).

signup(Login, Password) ->
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype) values (?,?,0)">>, [Login,Password]) of
		{ok_packet,_,_,Uid,_,_,[]} -> {ok,Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.
