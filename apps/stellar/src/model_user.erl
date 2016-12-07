-module(model_user).

-export([
	signup/2
	,login_restore/1
]).

signup(Login, Password) ->
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype) values (?,?,0)">>, [Login,Password]) of
		{ok_packet,_,_,Uid,_,_,[]} -> {ok,Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

login_restore(Login) ->
	case emysql:execute(mysqlpool, <<"select id from user where login=?">>, [Login]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			nwapi_utils:send_email(Login, <<"Subject: password restore\n\n {{ restore_link }} \n">>), {ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.
