-module(model_contractor).

-export([
	signup/3,
    get_details/1
    %,set_details/4
]).

signup(Login, Password, Refcode) ->
	case emysql:execute(mysqlpool, <<"insert into user (login,password,refcode,utype) values (?,?,?,2)">>, [Login,Password,Refcode]) of
		{ok_packet,_,_,Uid,_,_,[]} -> {ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

get_details(Uid) ->
	case emysql:execute(mysqlpool, <<"select ifnull(name,''), ifnull(photo,''), ifnull(phone,''), ifnull(login,'') ",
                "  from user where id=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"name">>, <<"photo">>, <<"phone">>, <<"email">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.
