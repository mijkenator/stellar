-module(model_contractor).

-export([
	signup/3
]).

signup(Login, Password, Refcode) ->
	case emysql:execute(mysqlpool, <<"insert into contractor (login,password,refcode,utype) values (?,?,?,2)">>, [Login,Password,Refcode]) of
		{ok_packet,_,_,Uid,_,_,[]} -> {ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

