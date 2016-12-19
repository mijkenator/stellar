-module(model_order).

-export([
    admin_get_orders/2
]).

admin_get_orders(Uid, Cid) ->
    {Params, ExtraSQL} = case {Uid, Cid} of
        {undefined, undefined} -> {[], <<>>};
        {Uid, undefined} -> {[Uid], <<" where o.uid=?">>};
        {undefined, Cid} -> {[Cid], <<" where o.cid=?">>};
        {Uid, Cid}       -> {[Uid, Cid], <<" where o.uid=? and o.cid=?">> }
    end,
	case emysql:execute(mysqlpool,
            <<"select o.uid, o.cid, o.sid, o.cost, o.gratuity, o.tax, cast(o.order_time as char), cast(o.order_ontime as char), o.number_ofservices, o.number_ofcontractors ",
            " from orders o ", ExtraSQL/binary>>, Params) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"user_id">>, <<"contractor_id">>, <<"service_id">>, <<"cost">>, <<"gratuity">>,
            <<"tax">>,<<"order_time">>,<<"order_ontime">>,<<"number_of_services">>,<<"number_of_contractors">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

