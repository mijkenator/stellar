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
    ,create_order/17
    ,update_order/16
    ,check_refcode/1
    ,send_invite/2
    ,ref_activity/1
]).

signup(Login, Password) -> signup(Login, Password, <<>>).
signup(Login, Password, Refcode) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype,confirm_uid) values (?,?,0,?)">>, [Login,Password,GUid]) of
		{ok_packet,_,_,Uid,_,_,[]} -> 
	    emysql:execute(mysqlpool, <<"update user set ref_id=CONCAT(?,'-',LPAD(floor(rand()*1000),3,'0') ) where id=?">>, [Uid,Uid]),
        signup_with_refcode(Uid, Refcode),
		nwapi_utils:send_email(Login, 
            <<"Subject: signup confirmation\n\n Confirmation link: http://dev.stellarmakeover.com/after-sign-up/", GUid/binary>>),
		{ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

signup_with_refcode(_, <<>>) -> ok;
signup_with_refcode(Uid, Refcode) ->
    case check_refcode(Refcode) of
        {ok, RUid} when RUid =/= Uid -> insert_or_update_referrals(Uid, RUid)
        ;_ -> ok
    end.

insert_or_update_referrals(Uid, RUid) ->
    [{D}] = get_details(Uid),
    Email = proplists:get_value(<<"email">>, D, <<>>),
    emysql:execute(mysqlpool, <<"update user set bonus = bonus + 2000 where id=?">>, [Uid]),
    emysql:execute(mysqlpool, <<"update user set bonus = bonus + 2000 where id=?">>, [RUid]),
	case emysql:execute(mysqlpool, <<"select id from referrals where to_email=?">>, [Email]) of
		{result_packet,_,_,[[Id]],_} when is_integer(Id), Id>0 ->
              emysql:execute(mysqlpool, <<"update referrals set to_uid=? where id=?">>, [Uid, Id])
        ;_ -> emysql:execute(mysqlpool, <<"insert into referrals (from_uid, to_email, dtime_sent, to_uid, is_sent) ",
                "values (?,?,now(),?,0)">>, [RUid, Email, Uid]) 
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

check_refcode(Refcode) ->
	case emysql:execute(mysqlpool, <<"select uid from user where ref_id=?">>, [Refcode]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

delete_user(Uid) -> emysql:execute(mysqlpool, <<"delete from user where id=?">>, [Uid]).

get_details(Uid) ->
	case emysql:execute(mysqlpool, <<"select ifnull(name,''), ifnull(street,''), ifnull(apt,''), ifnull(zip,''), ifnull(city,''), ",
                "ifnull(state,''), ifnull(phone,''), login, ref_id, bonus from user where id=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"name">>, <<"street">>, <<"apt">>, <<"zip">>, <<"city">>, 
                 <<"state">>, <<"phone">>, <<"email">>, <<"refcode">>, <<"bonus_cents">>],
            [{lists:zip(F,P) ++ [{<<"ref_flag">>, get_ref_flag(Uid)}] }||P<-Ret]
        ;_ -> []
	end.

get_ref_flag(Uid) ->
	case emysql:execute(mysqlpool, <<"select id from referrals where to_uid=?">>, [Uid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 1
		;_ -> 0
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

ref_activity(Uid) ->
	case emysql:execute(mysqlpool, <<"select to_email, cast(dtime_sent as chasr), ifnull(to_uid, 0), ",
                                     "is_sent from referrals where from_uid=? ">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"to_email">>,<<"date">>,<<"to_uid">>,<<"is_sent">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

create_order(Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax) ->
    Ret = emysql:execute(mysqlpool, 
        <<"insert into orders (uid, sid, order_ontime, number_ofservices, number_ofcontractors, cost, gratuity, tax) ",
           "values (?,?,?,?,?,?,?,?)">>, [Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax]),
    orders_queue:update_orders(),
    Ret.


create_order(Uid, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax) ->
    create_order(Uid, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax, <<>>).
create_order(Uid, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax, Location) ->
    Ret = emysql:execute(mysqlpool, 
        <<"insert into orders (uid, sid, order_ontime, number_ofservices, number_ofcontractors, cost, gratuity, tax, ",
                "phone, email, street, apt, city, state, cell_phone, zip, location ) ",
           "values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>, [Uid, Sid, DTime, ServNum, CNum, Cost, Gratuity, Tax, 
                                                        Phone, Email, Street, Apt, City, State, CPhone, Zip, Location]),
    orders_queue:update_orders(),
    Ret.

update_order(OrderId, Sid, DTime, ServNum, CNum, Phone, Email, Street, Apt, City, State, CPhone, Zip, Cost, Gratuity, Tax) ->
    P = [{<<"sip">>, Sid},{<<"order_ontime">>, DTime},{<<"number_ofservices">>, ServNum}, 
         {<<"number_ofcontractors">>, CNum}, {<<"cost">>, Cost}, {<<"gratuity">>, Gratuity}, {<<"tax">>, Tax}, {<<"apt">>,Apt},
         {<<"zip">>, Zip},{<<"city">>, City},{<<"state">>, State},{<<"cell_phone">>, CPhone}, 
         {<<"street">>, Street},{<<"phone">>, Phone}, {<<"email">>, Email}],
    Fun = fun({_, undefined}, A) -> A;
             ({Fn,V}, {S,Pr})    -> {S++[<<Fn/binary,"=?">>],Pr++[V]} end,
    {SQLl,Pa} = lists:foldl(Fun, {[], []}, P),
    SQLa = list_to_binary(lists:join(<<",">>, SQLl)), 
    SQL = <<"update orders set ",SQLa/binary," where id=?">>,
    Params = Pa ++ [OrderId],
    Ret = emysql:execute(mysqlpool, SQL, Params),
    orders_queue:update_orders(),
    Ret.

send_invite(Email, Uid) ->
    [{D}] = get_details(Uid),
    Refcode = proplists:get_value(<<"refcode">>,D,<<>>),

    emysql:execute(mysqlpool, <<"insert into referrals (from_uid, to_email, dtime_sent, is_sent) ",
                                "values (?,?,now(),1)">>, [Uid, Email]),
    nwapi_utils:send_email(Email, 
        <<"Subject: invite!\n\n Signup at http://dev.stellarmakeover.com with referral code:", Refcode/binary>>).

