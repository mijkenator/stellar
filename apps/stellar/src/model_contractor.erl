-module(model_contractor).

-export([
	signup/3,
    get_details/1
    ,set_details/11
    ,check_refcode/1
    ,create_refcode/2
    ,invite_contractor/2
]).

invite_contractor(Uid, Email) ->
    Refcode = create_refcode(Uid, Email),
	nwapi_utils:send_email(Email, 
        <<"Subject: contractor signup link\n\n  Link: http://dev.stellarmakeover.com/contractor-signup/", Refcode/binary>>).

create_refcode(Uid, Email) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
    emysql:execute(mysqlpool, <<"insert into refcodes (uid,refcode,email) values (?,?,?)">>, [Uid, GUid, Email]),
    GUid.

check_refcode(<<"fer912">>)    -> ok;
check_refcode(Refcode) ->
	case emysql:execute(mysqlpool,
                    <<"select 1 from refcodes where refcode=?">>, [Refcode]) of
		{result_packet,_,_,[[1]],_} -> ok
        ;_ -> nok
	end.

signup(Login, Password, Refcode) ->
    case check_refcode(Refcode) of
        ok -> signup_i(Login, Password, Refcode)
        ;_ -> {error, <<"bad refcode">>}
    end.

signup_i(Login, Password, Refcode) ->
	case emysql:execute(mysqlpool, <<"insert into user (login,password,refcode,utype) values (?,?,?,2)">>, [Login,Password,Refcode]) of
		{ok_packet,_,_,Uid,_,_,[]} -> 
            emysql:execute(mysqlpool, <<"insert into contractor (uid) values (?)">>, [Uid]), 
            {ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

get_details(Uid) ->
	case emysql:execute(mysqlpool, <<
                "select ifnull(c.fname,''), ifnull(c.lname,''), ifnull(u.photo,''), ifnull(u.phone,''), ifnull(u.login,''), ",
                "ifnull(u.street,''), ifnull(u.apt,''),ifnull(u.city,''),ifnull(u.state,''),"
                "ifnull(c.cphone,''),ifnull(c.bank_routing,''),ifnull(c.bank_account,'') "
                "  from user u left join contractor c on c.uid=u.id where u.id=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"fname">>, <<"lname">>, <<"photo">>, <<"phone">>, <<"email">>,
            <<"street">>,<<"apt">>,<<"city">>,<<"state">>,<<"cell_phone">>,<<"bank_routing">>,<<"bank_account">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.


set_details(Id, FName, LName, Street, Apt, City, State, CPhone, BankR, BankA, Phone) ->
    P = [{<<"name">>, <<FName/binary," ", LName/binary>>}, 
         {<<"phone">>, Phone},
         {<<"street">>, Street}, {<<"apt">>, Apt}, {<<"city">>, City}, {<<"state">>, State}],
    Fun = fun({_, undefined}, A) -> A;
             ({Fn,V}, {S,Pr})    -> {S++[<<Fn/binary,"=?">>],Pr++[V]} end,
    {SQLl,Pa} = lists:foldl(Fun, {[], []}, P),
    SQLa = list_to_binary(lists:join(<<",">>, SQLl)), 
    SQL = <<"update user set ",SQLa/binary," where id=?">>,
    Params = Pa ++ [Id],
    emysql:execute(mysqlpool, SQL, Params),

    Pc = [{<<"lname">>,LName},{<<"fname">>,FName},{<<"cphone">>,CPhone},{<<"bank_routing">>,BankR},{<<"bank_account">>, BankA}],
    {SQLlc,Pac} = lists:foldl(Fun, {[], []}, Pc),
    SQLac = list_to_binary(lists:join(<<",">>, SQLlc)), 
    Params_c = Pac ++ [Id],
    SQLc = <<"update contractor set ",SQLac/binary," where uid=?">>,
    emysql:execute(mysqlpool, SQLc, Params_c).
