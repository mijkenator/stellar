-module(model_order).

-export([
    admin_get_orders/2,
    cancel_order/2
]).

admin_get_orders(Uid, Cid) ->
    {Params, ExtraSQL} = case {Uid, Cid} of
        {undefined, undefined} -> {[], <<>>};
        {Uid, undefined} -> {[Uid], <<" where o.uid=?">>};
        {undefined, Cid} -> {[Cid], <<" where o.cid=?">>};
        {Uid, Cid}       -> {[Uid, Cid], <<" where o.uid=? and o.cid=?">> }
    end,
	case emysql:execute(mysqlpool,
            <<"select o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), ",
            " o.number_ofservices, o.number_ofcontractors, o.status ",
            " from orders o ", ExtraSQL/binary>>, Params) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>, <<"status">>],
            [{lists:zip(F,P)}||P<-Ret]
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
    O = get_order(Oid),
    OUid   = proplists:get_value(<<"user_id">>, O, 0),
    Status = proplists:get_value(<<"status">>, O, 0),
    case OUid == Uid of
      true ->
        case lists:member(Status, [<<"pending">>, <<"upcoming">>]) of
            true -> 
                emysql:execute(mysqlpool,<<"update orders set status = 'cancelled' where uid=? and id=?">>,
                [Uid, Oid])
            ;_   -> {error, non_cancel_status}
        end
      ;_ -> {error, another_user_order} 
    end.
