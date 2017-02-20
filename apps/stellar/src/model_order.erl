-module(model_order).

-export([
    admin_get_orders/2,
    cancel_order/2
    ,take_order/2
    ,get_new_orders/0
    ,get_order/1
    ,complete_order/2
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
            " u.name, u.login ",
            " from orders o left join services s on s.id = o.sid left join service_category c on c.id = s.cat_id ",
            " left join user u on u.id = o.uid ",
            ExtraSQL/binary>>, Params) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"order_id">>, <<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>,
            <<"street">>, <<"apt">>, <<"city">>, <<"state">>, <<"cell_phone">>, <<"zip">>, <<"finish_time">>, 
            <<"service_name">>, <<"category_name">>, <<"user_name">>, <<"user_email">>],
            
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
    <<CaL:1/binary, _/binary>> = CatName,
    <<SeL:1/binary, _/binary>> = ServiceName,
    OidB = list_to_binary(integer_to_list(lists:nth(1, OrderInfo))),
    OA = <<CaL/binary, SeL/binary, "-", OidB/binary>>,

    CI = case Cid of
        undefined -> []
        ;_ -> model_contractor:get_details(Cid)
    end,
    SI = model_service:get_service(Sid),

    [{<<"service_info">>, SI}, {<<"contractor_info">>, CI}, {<<"order_alias">>, OA}].



get_new_orders() ->
	case emysql:execute(mysqlpool,
            <<"select o.id, o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status, ",
            " o.street, o.apt, o.city, o.state, o.cell_phone, o.zip, u.name, u.login, u.phone "
            " from orders o left join user u on u.id = o.uid where o.cid is null">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"order_id">>, <<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>,
            <<"street">>, <<"apt">>, <<"city">>, <<"state">>, <<"cell_phone">>, <<"zip">>, <<"name">>, 
            <<"email">>, <<"phone">>],
            [{lists:zip(F,P) ++ acs_info(P)} ||P<-Ret]
        ;_ -> []
	end.

get_order(Oid) ->
	case emysql:execute(mysqlpool,
            <<"select o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status ",
            " from orders o where o.id = ?">>, [Oid]) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

cancel_order(Uid, Oid) when is_binary(Uid)-> cancel_order(binary_to_integer(Uid), Oid);
cancel_order(Uid, Oid) ->
    [{ O }] = get_order(Oid),
    OUid   = proplists:get_value(<<"user_id">>, O, 0),
    Status = proplists:get_value(<<"status">>, O, 0),
    case OUid == Uid of
      true ->
        case lists:member(Status, [<<"pending">>, <<"upcoming">>]) of
            true -> 
                emysql:execute(mysqlpool,<<"update orders set status = 'cancelled' where uid=? and id=?">>,[Uid, Oid]),
                orders_queue:update_orders()
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
              orders_queue:update_orders()
      ;_ ->   {error, already_taken} 
    end.

complete_order(Uid, Oid) ->
    emysql:execute(mysqlpool,<<"update orders set status = 'past', order_done=NOW() where cid=? and id=?">>,[Uid, Oid]),
    orders_queue:update_orders().

