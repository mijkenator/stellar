-module(model_user).

-export([
	signup/2
	,login_restore/1
	,signup_confirm/1
	,rpwd_confirm/2
	,delete_user/1
    ,get_details/1
    ,set_details/8
    ,get_users/0
    ,create_order/8
    ,create_order/16
]).

signup(Login, Password) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype,confirm_uid) values (?,?,0,?)">>, [Login,Password,GUid]) of
		{ok_packet,_,_,Uid,_,_,[]} -> 
		nwapi_utils:send_email(Login, 
            <<"Subject: signup confirmation\n\n Confirmation link: http://dev.stellarmakeover.com/after-sign-up/", GUid/binary>>),
		{ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

login_restore(Login) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"select id from user where login=?">>, [Login]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"delete from user_pwdrestore where uid=?">>, [Uid]),
			emysql:execute(mysqlpool, <<"insert into user_pwdrestore (uid, guid) values (?,?)">>, [Uid, GUid]),
			nwapi_utils:send_email(Login, 
                <<"Subject: password restore\n\n Restore link: http://dev.stellarmakeover.com/restore-password/", GUid/binary>>), {ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

signup_confirm(Guid) ->
	case emysql:execute(mysqlpool, <<"select id from user where confirm_uid=?">>, [Guid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"update user set confirmed=1 where id=?">>, [Uid]),
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

rpwd_confirm(Guid, Pwd) ->
	case emysql:execute(mysqlpool, <<"select uid from user_pwdrestore where guid=?">>, [Guid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"update user set password=? where id=?">>, [Pwd, Uid]),
			emysql:execute(mysqlpool, <<"delete from user_pwdrestore where uid=?">>, [Uid]),
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

delete_user(Uid) -> emysql:execute(mysqlpool, <<"delete from user where id=?">>, [Uid]).

get_details(Uid) ->
	case emysql:execute(mysqlpool, <<"select ifnull(name,''), ifnull(street,''), ifnull(apt,''), ifnull(zip,''), ifnull(city,''), ",
                "ifnull(state,''), ifnull(phone,''), login from user where id=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"name">>, <<"street">>, <<"apt">>, <<"zip">>, <<"city">>, <<"state">>, <<"phone">>, <<"email">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

set_details(Id, Name, Street, Apt, Zip, City, State, Phone) ->
    P = [{<<"name">>, Name},{<<"street">>, Street},{<<"apt">>,Apt},
         {<<"zip">>, Zip},{<<"city">>, City},{<<"state">>, State},{<<"phone">>, Phone}],
    Fun = fun({_, undefined}, A) -> A;
             ({Fn,V}, {S,Pr})    -> {S++[<<Fn/binary,"=?">>],Pr++[V]} end,
    {SQLl,Pa} = lists:foldl(Fun, {[], []}, P),
    SQLa = list_to_binary(lists:join(<<",">>, SQLl)), 
    SQL = <<"update user set ",SQLa/binary," where id=?">>,
    Params = Pa ++ [Id],
    emysql:execute(mysqlpool, SQL, Params).

get_users() ->
	case emysql:execute(mysqlpool, <<"select ifnull(name,''), ifnull(street,''), ifnull(apt,''), ifnull(zip,''), ifnull(city,''), ",
                "ifnull(state,''), ifnull(phone,''), login from user where utype in (0,1) ">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"name">>, <<"street">>, <<"apt">>, <<"zip">>, <<"city">>, <<"state">>, <<"phone">>, <<"email">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

create_order(Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax) ->
    orders_queue:update_orders(),
    emysql:execute(mysqlpool, 
        <<"insert into orders (uid, sid, order_ontime, number_ofservices, number_ofcontractors, cost, gratuity, tax) ",
           "values (?,?,?,?,?,?,?,?)">>, [Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax]).

create_order(Uid, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax) ->
    orders_queue:update_orders(),
    emysql:execute(mysqlpool, 
        <<"insert into orders (uid, sid, order_ontime, number_ofservices, number_ofcontractors, cost, gratuity, tax, ",
                "phone, email, street, apt, city, state, cell_phone, zip ) ",
           "values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>, [Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax, 
                                                        Phone, Email, Street, Apt, City, State, CPhone, Zip]).

