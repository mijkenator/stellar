-module(model_service).

-export([
	get_categories/0,
    create_category/1,
    delete_category/1,
    update_category/2
]).

get_categories() ->
	case emysql:execute(mysqlpool, <<"select id, name from service_category">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"id">>, <<"name">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

create_category(Name) ->
    emysql:execute(mysqlpool, <<"insert into service_category (name) values (?)">>, [Name]).

delete_category(ID) ->
    emysql:execute(mysqlpool, <<"delete from service_category where id=?">>, [ID]).

update_category(ID, Name) ->
    emysql:execute(mysqlpool, <<"update  service_category set name=? where id=?">>, [Name, ID]).
