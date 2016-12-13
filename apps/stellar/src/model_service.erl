-module(model_service).

-export([
	get_categories/0
]).

get_categories() ->
	case emysql:execute(mysqlpool, <<"select id, name from service_category">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"id">>, <<"name">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

