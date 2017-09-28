-module(model_user).

-export([
	signup/2
	,signup/3
	,login_restore/1
	,login_restore/2
	,signup_confirm/1
	,rpwd_confirm/2
	,delete_user/1
    ,get_details/1
    ,set_details/8
    ,set_details/9
    ,get_users/0
    ,create_order/8
    ,create_order/16
    ,create_order/17
    ,update_order/16
    ,check_refcode/1
    ,send_invite/2
    ,send_invite/3
    ,ref_activity/1
    ,user_get_ref_status/1
    ,fix_bonus/2
    ,back_bonus/2
]).

signup(Login, Password) -> signup(Login, Password, <<>>).
signup(Login, Password, Refcode) ->
    lager:debug("MUS1 ~p", [{Login, Password, Refcode}]),
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype,confirm_uid) values (?,?,0,?)">>, [Login,Password,GUid]) of
		{ok_packet,_,_,Uid,_,_,[]} -> 
        lager:debug("MUS2 ~p", [{Uid, GUid}]),
	    emysql:execute(mysqlpool, <<"update user set ref_id=CONCAT(?,'-',LPAD(floor(rand()*1000),3,'0') ) where id=?">>, [Uid,Uid]),
        lager:debug("MUS3 ", []),
        signup_with_refcode(Uid, Refcode),
        lager:debug("MUS4 ", []),
%		nwapi_utils:send_email(Login, 
%            <<"Subject: signup confirmation\n\n Confirmation link: http://stellarmakeover.com/after-sign-up/", GUid/binary>>),
        nwapi_utils:send_confirm_email({Login, "http://stellarmakeover.com/after-sign-up/"++binary_to_list(GUid), Login}),
		{ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

signup_with_refcode(_, <<>>) -> ok;
signup_with_refcode(Uid, Refcode) ->
    lager:debug("MUS3.1 ", []),
    case check_refcode(Refcode) of
        {ok, RUid} when RUid =/= Uid -> insert_or_update_referrals(Uid, RUid)
        ;_ -> ok
    end.

insert_or_update_referrals(Uid, RUid) ->
    lager:debug("MUS3.2 ", []),
    [{D}] = get_details(Uid),
    Email = proplists:get_value(<<"email">>, D, <<>>),
    emysql:execute(mysqlpool, <<"update user set bonus = bonus + 2000 where id=?">>, [Uid]),
    % will add after UID make payments 
    %emysql:execute(mysqlpool, <<"update user set bonus = bonus + 2000 where id=?">>, [RUid]),
	case emysql:execute(mysqlpool, <<"select id from referrals where to_email=?">>, [Email]) of
		{result_packet,_,_,[[Id]],_} when is_integer(Id), Id>0 ->
              %emysql:execute(mysqlpool, <<"update referrals set to_uid=?, to_bonus=2000, from_bonus=2000 where id=?">>, [Uid, Id])
              emysql:execute(mysqlpool, <<"update referrals set to_uid=?, to_bonus=2000, from_bonus=0 where id=?">>, [Uid, Id])
        ;_ -> emysql:execute(mysqlpool, <<"insert into referrals (from_uid, to_email, dtime_sent, to_uid, is_sent, to_bonus, from_bonus) ",
                "values (?,?,now(),?,0,2000,0)">>, [RUid, Email, Uid]) 
    end.

login_restore(Login) -> login_restore(Login, <<"user">>).
login_restore(Login, RpT) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"select id from user where login=?">>, [Login]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"delete from user_pwdrestore where uid=?">>, [Uid]),
			emysql:execute(mysqlpool, <<"insert into user_pwdrestore (uid, guid) values (?,?)">>, [Uid, GUid]),
            SbD = case RpT of
                <<"pro">> -> <<"pro.">>;
                _ -> <<"">>
            end,
			%nwapi_utils:send_email(Login, 
            %    <<"Subject: password restore\n\n Restore link: http://",SbD/binary,
            %                    "stellarmakeover.com/restore-password/", GUid/binary>>),
            nwapi_utils:restore_password_email(Login, 
                <<"http://",SbD/binary,"stellarmakeover.com/restore-password/", GUid/binary>>),
            {ok, Uid}
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
	case emysql:execute(mysqlpool, <<"select id from user where ref_id=?">>, [Refcode]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

delete_user(Uid) -> emysql:execute(mysqlpool, <<"delete from user where id=?">>, [Uid]).

get_details(Uid) ->
	case emysql:execute(mysqlpool, <<"select ifnull(name,''), ifnull(street,''), ifnull(apt,''), ifnull(zip,''), ifnull(city,''), ",
                "ifnull(state,''), ifnull(phone,''), login, ref_id, bonus, ifnull(lname, '') from user where id=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"name">>, <<"street">>, <<"apt">>, <<"zip">>, <<"city">>, 
                 <<"state">>, <<"phone">>, <<"email">>, <<"refcode">>, <<"bonus_cents">>, <<"lname">>],
            [{lists:zip(F,P) ++ [{<<"ref_flag">>, get_ref_flag(Uid)}] }||P<-Ret]
        ;_ -> []
	end.

get_ref_flag(Uid) ->
	case emysql:execute(mysqlpool, <<"select id from referrals where to_uid=?">>, [Uid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 1
		;_ -> 0
	end.

set_details(Id, Name, Street, Apt, Zip, City, State, Phone) ->
    set_details(Id, Name, Street, Apt, Zip, City, State, Phone, undefined).
set_details(Id, Name, Street, Apt, Zip, City, State, Phone, LName) ->
    P = [{<<"name">>, Name},{<<"street">>, Street},{<<"apt">>,Apt},
         {<<"zip">>, Zip},{<<"city">>, City},{<<"state">>, State},{<<"phone">>, Phone},{<<"lname">>, LName}],
    Fun = fun({_, undefined}, A) -> A;
             ({Fn,V}, {S,Pr})    -> {S++[<<Fn/binary,"=?">>],Pr++[V]} end,
    {SQLl,Pa} = lists:foldl(Fun, {[], []}, P),
    SQLa = list_to_binary(lists:join(<<",">>, SQLl)), 
    SQL = <<"update user set ",SQLa/binary," where id=?">>,
    Params = Pa ++ [Id],
    emysql:execute(mysqlpool, SQL, Params).

get_users() ->
	case emysql:execute(mysqlpool, <<"select id, ifnull(name,''), ifnull(street,''), ifnull(apt,''), ifnull(zip,''), ifnull(city,''), ",
                " ifnull(state,''), ifnull(phone,''), login, cast(time_created as char), ifnull(lname, '') ",
                " from user where utype in (0,1,3) ">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"id">>, <<"name">>, <<"street">>, <<"apt">>, <<"zip">>, <<"city">>, <<"state">>, 
                    <<"phone">>, <<"email">>, <<"registration_time">>, <<"lname">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

ref_activity(Uid) ->
	case emysql:execute(mysqlpool, <<"select id, to_email, cast(dtime_sent as char), ifnull(to_uid, 0), ",
                                     "is_sent, from_bonus from referrals where from_uid=? ">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            lager:debug("RefActivity: ~p", [Ret]),
            F = [<<"id">>,<<"to_email">>,<<"date">>,<<"to_uid">>,<<"is_sent">>,<<"bonus_cents">>],
            %[{lists:zip(F,P)}||P<-Ret]
            lists:map(fun(P)-> 
                lager:debug("_i_RefActivity: ~p", [P]),
                {RefInfo, {StatusMessage, StatusCode}} = get_refuid_info(lists:nth(4, P), lists:nth(5, P)),
                {lists:zip(F,P) ++ [{<<"ref_info">>, {RefInfo}},{<<"status_code">>, StatusCode},{<<"status_message">>, StatusMessage}]}

            end, Ret)

        ;_ -> []
	end.

%get_refuser_membership(_Uid) -> {0, <<>>}.
get_refuser_membership(Uid) ->
	case emysql:execute(mysqlpool, <<"select cast(order_done as char) from orders where status='past' and uid=? limit 1">>, [Uid]) of
		{result_packet,_,_,[[Ret]],_} -> {1, Ret}
        ;_ -> {0, <<>>}
	end.
    
get_refuser_data(Uid) ->
	case emysql:execute(mysqlpool, <<"select cast(time_created as char) from user where id=?">>, [Uid]) of
		{result_packet,_,_,[Ret],_} -> Ret
        ;_ -> []
	end.

get_refuid_info(ToUid, IsSent) when is_integer(ToUid), ToUid>0->
    lager:debug("GRI1 ~p", [{ToUid, IsSent}]),
    RUData = get_refuser_data(ToUid),
    lager:debug("GRI2 ~p", [RUData]),
    {UMSt, UMSD} = get_refuser_membership(ToUid),
    lager:debug("GRI3 ~p", [{UMSt, UMSD}]),
    {Status, Bonus, Mpd} = case {IsSent, UMSt} of
        {0,0} -> {{<<"Somebody used your Referral Code to open an account. Services are not booked yet.">>, 4},<<"NO">>,<<>>}; 
        {1,0} ->{{<<"Referral is sent. Account is created. Services are not booked yet.">>,2},<<"NO">>,<<>>};

        {0,1} ->{{<<"Somebody used your Referral Code to open an account. Services are booked.">>,5},<<"YES">>,UMSD};
        {1,1} ->{{<<"Referral is sent. Account is created. Services are booked.">>, 3},<<"YES">>,UMSD}
    end,
    {[
        {<<"account_created">>, lists:nth(1, RUData)},
        {<<"membership_purchase_date">>, Mpd},
        {<<"bonus_issued">>, Bonus}
    ], Status};
get_refuid_info(undefined, _) -> {[], {<<"Referral is sent. Account not created yet.">>,1}};
get_refuid_info(A,B) ->
    lager:debug("GRI undk: ~p", [{A,B}]), {[], {<<"Referral is sent. Account not created yet.">>,1}}.


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

send_invite(Email, Uid) -> send_invite(Email, Uid, <<>>).
send_invite(Email, Uid, Cname) ->
    [{D}] = get_details(Uid),
    Refcode = proplists:get_value(<<"refcode">>,D,<<>>),
    Lname   = proplists:get_value(<<"lname">>,D,<<>>),
    Fname   = proplists:get_value(<<"fname">>,D,<<>>),
    InvName = <<Fname/binary, " ", Lname/binary>>,

    emysql:execute(mysqlpool, <<"insert into referrals (from_uid, to_email, dtime_sent, is_sent) ",
                                "values (?,?,now(),1)">>, [Uid, Email]),
    nwapi_utils:send_invite_email(Email, Refcode, <<"http://stellarmakeover.com/sign-up?refcode=",Refcode/binary,"&email=",Email/binary>>, InvName, Cname).

user_get_ref_status(Uid) ->
	case emysql:execute(mysqlpool, <<"select id, from_uid from referrals where to_uid=? and from_bonus != 2000 limit 1">>, [Uid]) of
		{result_packet,_,_,[[Id, RUid]],_} -> {0, RUid, Id}
        ;R -> {1, R}
	end.

get_bonus(Uid) ->
	case emysql:execute(mysqlpool, <<"select bonus from user where id=?">>, [Uid]) of
		{result_packet,_,_,[[Bonus]],_} -> Bonus
        ;_R -> 0
	end.
set_bonus(Uid, Amnt)  -> emysql:execute(mysqlpool, <<"update user set bonus=? where id=?">>, [Amnt, Uid]). 
fix_bonus(Uid, Amnt)  ->
    case get_bonus(Uid) of
        0 -> 0;
        B when B =< Amnt -> set_bonus(Uid, 0), B;
        B -> set_bonus(Uid, B-Amnt), Amnt
    end.
back_bonus(Uid, Amnt) -> emysql:execute(mysqlpool, <<"update user set bonus= bonus + ? where id=?">>, [Amnt, Uid]).


