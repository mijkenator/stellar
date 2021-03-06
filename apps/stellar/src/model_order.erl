-module(model_order).

-export([
    admin_get_orders/2,
    cancel_order/2
    ,take_order/2
    ,get_new_orders/0
    ,get_new_orders/1
    ,get_order/1
    ,complete_order/2
    ,make_stripe_payment/5
    ,make_stripe_payment/1
    ,make_stripe_payment/2
    ,save_stripe_payment/5
    ,make_stripe_cancel_payment/1
    ,make_stripe_cancel_payment/2
    ,send_contractors_email/1
    ,get_order_info/1
]).

admin_get_orders(Uid, Cid) ->
    {Params, ExtraSQL} = case {Uid, Cid} of
        {undefined, undefined} -> {[], <<>>};
        {Uid, undefined} -> {[Uid], <<" where o.uid=?">>};
        {undefined, Cid} -> {[Cid], <<" where o.cid=?">>};
        {Uid, Cid}       -> {[Uid, Cid], <<" where o.uid=? and o.cid=?">> }
    end,
	case emysql:execute(mysqlpool,
            <<"select o.id, o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status, ",
            " o.street, o.apt, o.city, o.state, o.cell_phone, o.zip, cast(o.order_done as char), s.title, c.name,  ",
            " u.name, u.login, ifnull(o.location,''), ifnull(o.payment_status, 0), ifnull(stripe_id, ''), ifnull(u.lname, '') ",
            " from orders o left join services s on s.id = o.sid left join service_category c on c.id = s.cat_id ",
            " left join user u on u.id = o.uid ",
            ExtraSQL/binary>>, Params) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"order_id">>, <<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>,
            <<"street">>, <<"apt">>, <<"city">>, <<"state">>, <<"cell_phone">>, <<"zip">>, <<"finish_time">>, 
            <<"service_name">>, <<"category_name">>, <<"user_name">>, <<"user_email">>, <<"location">>, <<"payment_status">>, 
            <<"stripe_id">>, <<"user_lname">>],
            
            [{lists:zip(F,P) ++ acs_info(P) }||P<-Ret]
        %;_ -> []
        ;_E -> _E
	end.


acs_info(OrderInfo) ->
    Cid = lists:nth(3, OrderInfo),
    Sid = lists:nth(4, OrderInfo),

    CatName = lists:nth(21, OrderInfo),
    ServiceName = lists:nth(20, OrderInfo),
    lager:debug("ACSI1 ~p ", [{CatName, ServiceName}]),
    %<<CaL:1/binary, _/binary>> = CatName,
    %<<SeL:1/binary, _/binary>> = ServiceName,
    CaL = if is_binary(CatName) -> <<CaL0:1/binary, _/binary>> = CatName, CaL0; true -> <<"">> end,
    SeL = if is_binary(ServiceName) -> <<SeL0:1/binary, _/binary>> = ServiceName, SeL0; true -> <<"">> end,
    OidB = list_to_binary(integer_to_list(lists:nth(1, OrderInfo))),
    OA = <<CaL/binary, SeL/binary, "-", OidB/binary>>,

    CI = case Cid of
        undefined -> []
        ;_ -> model_contractor:get_details(Cid)
    end,
    SI = model_service:get_service(Sid),

    [{<<"service_info">>, SI}, {<<"contractor_info">>, CI}, {<<"order_alias">>, OA}].

get_contractor_scid(Uid) ->
	case emysql:execute(mysqlpool, <<"select service_cat_id from contractor_service where uid=?">>, [Uid]) of
		{result_packet,_,_,Ret,_} -> [Cscid || [Cscid]<-Ret]
        ;_E ->
            lager:error("get_contractor_scid error: ~p", [_E]),
            []
    end.

get_new_orders(0)   -> get_new_orders();
get_new_orders(Uid) ->
    lager:debug("GNO 1 uid ~p", [Uid]),
    Csids = get_contractor_scid(Uid),
    lager:debug("GNO 2 csids ~p ", [Csids]),
    ExtraSQL = case Csids of
        [] -> <<>>
        ;_ -> list_to_binary("and s.cat_id in (" ++  string:join([integer_to_list(C)||C<-Csids],", ") ++ ") ")
    end,
    lager:debug("GNO 3 ~p ", [ExtraSQL]),
	case emysql:execute(mysqlpool,
            <<"select o.id, o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status, ",
            " o.street, o.apt, o.city, o.state, o.cell_phone, o.zip, u.name, u.login, u.phone, ifnull(o.location,''), s.cat_id "
            " from orders o left join user u on u.id = o.uid ",
            " left join services s on s.id=o.sid "
            " where o.cid is null ", ExtraSQL/binary>>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"order_id">>, <<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>,
            <<"street">>, <<"apt">>, <<"city">>, <<"state">>, <<"cell_phone">>, <<"zip">>, <<"name">>, 
            <<"email">>, <<"phone">>, <<"location">>, <<"service_cat_id">>],
            %lager:debug("MOGNO ~p", [Ret]),
            %io:format("GNO 2 ~p ~n", [Ret]),
            [{lists:zip(F,P) ++ acs_info(P)} ||P<-Ret]
        ;_E ->
            %lager:error("MOGNO ~p", [_E]),
            %io:format("GNO 3 ~p ~n", [_E]),
            []
	end.

get_new_orders()    ->
    io:format("GNO 1 ~n", []),
	case emysql:execute(mysqlpool,
            <<"select o.id, o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status, ",
            " o.street, o.apt, o.city, o.state, o.cell_phone, o.zip, u.name, u.login, u.phone, ifnull(o.location,'') "
            " from orders o left join user u on u.id = o.uid where o.cid is null">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"order_id">>, <<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>,
            <<"street">>, <<"apt">>, <<"city">>, <<"state">>, <<"cell_phone">>, <<"zip">>, <<"name">>, 
            <<"email">>, <<"phone">>, <<"location">>],
            %lager:debug("MOGNO ~p", [Ret]),
            %io:format("GNO 2 ~p ~n", [Ret]),
            [{lists:zip(F,P) ++ acs_info(P)} ||P<-Ret]
        ;_E ->
            %lager:error("MOGNO ~p", [_E]),
            %io:format("GNO 3 ~p ~n", [_E]),
            []
	end.

get_order(Oid) ->
	case emysql:execute(mysqlpool,
            <<"select o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status, o.payment_token ",
            " from orders o where o.id = ?">>, [Oid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>, <<"token">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

cancel_order(Uid, Oid) when is_binary(Uid)-> cancel_order(binary_to_integer(Uid), Oid);
cancel_order(Uid, Oid) ->
    lager:debug("CaOrGo: start", []),
    [{ O }] = get_order(Oid),
    lager:debug("CaOrGo: ~p ", [O]),
    OUid   = proplists:get_value(<<"user_id">>, O, 0),
    Status = proplists:get_value(<<"status">>, O, 0),
    lager:debug("CaOr: ~p ", [{OUid, Status, O}]),
    case OUid == Uid of
      true ->
        case lists:member(Status, [<<"pending">>, <<"upcoming">>]) of
            true -> 
                case make_stripe_cancel_payment(Uid, Oid) of
                    {error, Err} -> {error, Err}
                    ;_ ->
                        emysql:execute(mysqlpool,<<"update orders set status = 'cancelled' where uid=? and id=?">>,[Uid, Oid]),
                        try
                            {_Service, _ServiceType, _SDate, _STime, _SLoc, CName, _SPrice, To} = get_order_info(Oid),
                            nwapi_utils:cancel_order_email({To, CName})
                        catch
                            CMErr:CMRea -> lager:error("cancel order email issue: ~p", [{CMErr, CMRea}])
                        end,
                        orders_queue:update_orders()
                end
            ;_   -> {error, non_cancel_status}
        end
      ;_ -> {error, another_user_order} 
    end.

take_order(Cid, Oid) when is_binary(Cid) -> orders_queue:take_order(binary_to_integer(Cid), Oid);
take_order(Cid, Oid) ->
    [{ O }] = get_order(Oid),
    OCid   = proplists:get_value(<<"contractor_id">>, O, undefined),
    case OCid == undefined of
      true -> emysql:execute(mysqlpool,<<"update orders set status = 'upcoming', cid = ? where id=?">>, 
              [Cid, Oid]),
              try
                  {Service, ServiceType, SDate, STime, SLoc, CName, SPrice, To} = get_order_info(Oid),
                  [{CData}] = model_contractor:get_details(Cid),
                  CFName = proplists:get_value(<<"fname">>, CData, <<>>),
                  CLName = proplists:get_value(<<"lname">>, CData, <<>>),
                  CoPhone = case  proplists:get_value(<<"phone">>, CData, <<>>) of
                    <<>> -> proplists:get_value(<<"cell_phone">>, CData, <<>>);
                    CrPh -> CrPh
                  end,
                  {CoName, CoPhoto, CoEmail} = {
                      <<CFName/binary, " ", CLName/binary>>,
                      proplists:get_value(<<"photo">>, CData, <<"/images/default-avatar.png">>),
                      proplists:get_value(<<"email">>, CData, <<>>)
                  },
                  nwapi_utils:take_order_email({To,CName,Oid,ServiceType,Service,SDate,STime,SLoc,CName,SPrice,CoName,CoPhone,CoPhoto,CoEmail,Cid})
              catch
                Err:Rea -> lager:error("take order email error: ~p", [{Err, Rea}])
              end,
              orders_queue:update_orders()
      ;_ ->   {error, already_taken} 
    end.

complete_order(Uid, Oid) ->
    case make_stripe_payment(Uid, Oid) of
        {error, Err} ->
            lager:error("complete order error"),
            emysql:execute(mysqlpool,<<"update orders set payment_status=2, payment_err =? where  id=?">>,[Err, Oid]);
        _Pid        ->
            emysql:execute(mysqlpool,<<"update orders set status = 'past', order_done=NOW()  where cid=? and id=?">>,[Uid, Oid]),
            
            case model_user:user_get_ref_status(Uid) of
                {0, RUid, Id} ->
                    emysql:execute(mysqlpool, <<"update referrals from_bonus=2000 where id=?">>, [Id]),
                    emysql:execute(mysqlpool, <<"update user set bonus = bonus + 2000 where id=?">>, [RUid])
                ;UGRS ->
                    lager:debug("UGRS ~p no bonus to referer", [UGRS])
            end
    end,
    orders_queue:update_orders().

make_stripe_cancel_payment(Oid) -> make_stripe_cancel_payment(0, Oid).
make_stripe_cancel_payment(Uid, Oid) ->
    try
        lager:debug("MSCP ~p", [Oid]),
        case update_order_for_cancel(Oid) of
            ok -> 
                [{Order}] = get_order(Oid),
                OCost = proplists:get_value(<<"cost">>, Order),
                Token = proplists:get_value(<<"token">>, Order),
                case Token of
                    Tkn when is_binary(Tkn), size(Tkn) > 1 ->
                            make_stripe_payment(Uid, OCost, <<"usd">>, Token, Oid);
                    BTkn ->
                        lager:debug("Bad token on cancel order: ~p", [{Oid, BTkn}])
                end;
            ok1 -> ok;
            nok ->
                lager:error("MSCP failed, no stripe payment", []),
                {error, <<"no payment">>}
        end
    catch
        throw:price_mismatch -> {error, <<"wrong price">>};
        _:R                  -> {error, R}
    end.

make_stripe_payment(Oid) -> make_stripe_payment(0, Oid).
make_stripe_payment(Uid, Oid) ->
    try
        [{Order}] = get_order(Oid),
        OCost = proplists:get_value(<<"cost">>, Order),
        Token = proplists:get_value(<<"token">>, Order),
        make_stripe_payment(Uid, OCost, <<"usd">>, Token, Oid) 
    catch
        throw:price_mismatch -> {error, <<"wrong price">>};
        _:R                  -> {error, R}
    end.
make_stripe_payment(AccountId, AmountCnts, _Currency, Token, Orderid) when is_binary(AmountCnts) ->
    make_stripe_payment(AccountId, binary_to_integer(AmountCnts), _Currency, Token, Orderid);
make_stripe_payment(AccountId, AmountCnts, _Currency, Token, Orderid) ->
    Bonus = model_user:fix_bonus(AccountId, AmountCnts),
    try
        lager:debug("MSP ~p ", [{AccountId, AmountCnts, _Currency, Token, Orderid, Bonus}]),
        [{Order}] = get_order(Orderid),
        lager:debug("MSP go ~p", [Order]),
        OCost = proplists:get_value(<<"cost">>, Order),
        OCost == AmountCnts orelse throw(price_mismatch),
        make_stripe_payment_i(AccountId, AmountCnts, _Currency, Token, Orderid, Bonus)
    catch
        throw:price_mismatch -> 
            model_user:back_bonus(AccountId, Bonus),
            {error, <<"wrong price">>};
        _:R                  -> 
            model_user:back_bonus(AccountId, Bonus),
            {error, R}
    end.

make_stripe_payment_i(AccountId, AmountCnts, _Currency, _Token, Orderid, Bonus) when Bonus == AmountCnts ->
        lager:debug("MSP payed by bonus ~p", [{AccountId, Orderid, Bonus, AmountCnts}]),
        update_payed_order(Orderid, <<"ch_bonus">>);
make_stripe_payment_i(AccountId, AmountCnts0, _Currency, Token, Orderid, Bonus) ->
        AmountCnts = AmountCnts0 - Bonus,
        lager:debug("MSP applied bonus ~p", [Bonus]),
        Pcmd0 = io_lib:format("python3 /opt/stellar/stripe/charge.py -a ~p -s ~p -u ~p -o ~p", 
            [AmountCnts, binary_to_list(Token), AccountId, Orderid]),
        Pcmd = binary_to_list(list_to_binary(Pcmd0)),
        lager:debug("MSP pcmd ~p", [Pcmd]),
        case os:cmd(Pcmd) of
            "Error\n" ->
                lager:error("MSP stripe paiment failed ~p", [{AccountId, Orderid, Token}]),
                model_user:back_bonus(AccountId, Bonus),
                {error, <<"failed">>};
            Pid       ->
                lager:debug("MSP Stripe payment ok ~p", [Pid]),
                Pid1 = re:replace(Pid,"\\s+","",[global, {return, list}]),
                lager:debug("MSP Stripe payment ok ~p", [Pid1]),
                update_payed_order(Orderid, Pid1),
                Pid1
        end.

save_stripe_payment(_AccountId, AmountCnts, _Currency, Token, Orderid) when is_binary(AmountCnts) ->
    save_stripe_payment(_AccountId, binary_to_integer(AmountCnts), _Currency, Token, Orderid);
save_stripe_payment(_AccountId, AmountCnts, _Currency, Token, Orderid) ->
    try
        [{Order}] = get_order(Orderid),
        OCost = proplists:get_value(<<"cost">>, Order),
        lager:debug("SSP: ~p", [{_AccountId, AmountCnts, OCost, Orderid}]),
        
        OCost == AmountCnts orelse throw(price_mismatch),
        Pcmd0 = io_lib:format("python3 /opt/stellar/stripe/customer.py -s ~p", [binary_to_list(Token)]),
        Pcmd = binary_to_list(list_to_binary(Pcmd0)),
        lager:debug("SSP pcmd ~p", [Pcmd]),
        Ret = os:cmd(Pcmd),
        lager:debug("SSP pcmdi ret ~p", [Ret]),
        case Ret of
            "Error\n" ->
                lager:error("SSP stripe paiment failed ~p", [{_AccountId, Orderid, Token}]),
                {error, <<"failed">>};
            Caid ->
                lager:debug("SSP Stripe customer ok ~p", [Caid]),
                Caid1 = re:replace(Caid,"\\s+","",[global, {return, list}]),
                lager:debug("SSP Stripe customer ok ~p", [Caid1]),
                update_order_stripe_token(Orderid, Caid1),
                send_contractors_email(Orderid),
                true
        end
    catch
        throw:price_mismatch -> {error, <<"wrong price">>};
        _:R                  -> {error, R}
    end.

update_order_for_cancel(Oid) ->
	case emysql:execute(mysqlpool,
            <<"select o.cost, abs(timestampdiff(hour, now(), order_ontime)) ",
            " from orders o where o.id = ?">>, [Oid]) of
		{result_packet,_,_,[[Amnt, Hrs]],_} when Hrs < 24 -> 
            lager:debug("UOFC < 24 ~p", [Amnt]),
            ok;
		{result_packet,_,_,[[Amnt, Hrs]],_} when Hrs < 48 -> 
            NAmnt = Amnt div 5,
            Ur = emysql:execute(mysqlpool, <<"update orders set cost=? where id=?">>, [NAmnt, Oid]),
            lager:debug("UOFC < 48 ~p", [{Amnt, NAmnt, Ur}]),
            ok;
		{result_packet,_,_,[[Amnt, Hrs]],_} when Hrs > 48 ->
            lager:debug("UOFC > 48 ~p", [Amnt]),
            ok1
        ;_ -> nok
	end.

update_order_stripe_token(Orderid, Pid1) ->
    emysql:execute(mysqlpool,<<"update orders set payment_status = 3, payment_token=? where id=?">>,[Pid1, Orderid]).

update_payed_order(Orderid, Pid1) ->
    emysql:execute(mysqlpool,<<"update orders set payment_status = 1, stripe_id=? where id=?">>,[Pid1, Orderid]).
  
get_order_info(Orderid) ->
	case emysql:execute(mysqlpool,
            <<"select s.title, sc.name, cast(DATE(o.order_ontime) as char), cast(TIME_FORMAT(o.order_ontime, '%H:%i') as char), ",
              "CONCAT_WS(' ', o.street, o.apt, o.city, o.location), CONCAT(u.name,CONCAT(' ', u.lname)), o.cost, u.login from orders o ",
              "left join services s on s.id = o.sid left join user u on u.id = o.uid ",
              "left join service_category sc on sc.id=s.cat_id "
              "where o.id=?">>, [Orderid]) of
		{result_packet,_,_,[Ret],_}  -> list_to_tuple(Ret) 
        ;_ -> {}
	end.

send_contractors_email(Orderid) ->
  try
    lager:debug("SCE1 ~p", [Orderid]),
    Ocid = get_order_catid(Orderid),
    lager:debug("SCE2 ~p", [Ocid]),
    {Service, ServiceType, SDate, STime, SLoc, CName, SPrice, _ULogin} = get_order_info(Orderid),
	case emysql:execute(mysqlpool,
            <<"select distinct u.login, u.name from user u left join contractor_service cs on cs.uid = u.id ",
            " where u.utype=3 and cs.service_cat_id=?">>, [Ocid]) of
		{result_packet,_,_,L,_}  -> 
            lager:debug("SCE3 ~p", [L]),
            Oid = if  is_integer(Orderid) -> integer_to_binary(Orderid); true -> Orderid end,
            Fun = fun([Email, Name]) ->
                %nwapi_utils:send_email(Email, 
                %    <<"Subject: new order ready!\n\n",
                %    " Orderid: ", Oid/binary>>)
                nwapi_utils:new_appointment_email(Email, Oid, {Name, Service, ServiceType, SDate, STime, SLoc, CName, SPrice})
            end,
            lists:foreach(Fun, L),ok
        ;_ -> nok
	end
  catch
    E:R -> lager:debug("SCE error ~p", [{E,R}]), nok
  end.

get_order_catid(Orderid) ->
	case emysql:execute(mysqlpool,
            <<"select sc.id from orders o left join services s on s.id=o.sid left join service_category sc on sc.id=s.cat_id ",
            " where o.id=?">>, [Orderid]) of
		{result_packet,_,_,[[Id]],_}  -> Id 
        ;_ -> 0
	end.

