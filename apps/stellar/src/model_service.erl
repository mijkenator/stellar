-module(model_service).

-export([
	get_categories/0,
    create_category/1,
    delete_category/1,
    update_category/2,

    get_services/0,
    create_service/6,
    delete_service/1,
    update_service/7
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

get_services() ->
	case emysql:execute(mysqlpool, <<"select s.id, s.cat_id, s.title, s.description, s.cost, s.duration, ",
                    "s.note, ifnull(s.img,''),c.name from services s left join service_category c on c.id=s.cat_id">>, []) of
		{result_packet,_,_,Ret,_} ->
            F = [<<"id">>, <<"category">>, <<"title">>, <<"description">>, <<"cost">>, 
                 <<"duration">>, <<"note">>, <<"img">>, <<"category_name">>],
            [{lists:zip(F,P)}||P<-Ret]
        ;_ -> []
	end.

create_service(CatID, Title, Desc, Cost, Dur, Note) ->
    emysql:execute(mysqlpool, <<"insert into services (cat_id, title, description, cost, duration, note) values (?,?,?,?,?,?)">>, 
        [CatID, Title, Desc, Cost, Dur, Note]).

delete_service(ID) ->
    emysql:execute(mysqlpool, <<"delete from services where id=?">>, [ID]).

update_service(Id, CatID, Title, Desc, Cost, Dur, Note) ->
    P = [{<<"cat_id">>, CatID},{<<"title">>, Title},{<<"description">>,Desc},{<<"cost">>, Cost},{<<"duration">>, Dur},{<<"note">>, Note}],
    lager:debug("US1",[]),
    Fun = fun({_, undefined}, A) -> A;
             ({Fn,V}, {S,Pr})    -> {S++[<<Fn/binary,"=?">>],Pr++[V]} end,
    lager:debug("US2",[]),
    {SQLl,Pa} = lists:foldl(Fun, {[], []}, P),
    lager:debug("US3",[]),
    SQLa = list_to_binary(lists:join(<<",">>, SQLl)), 
    lager:debug("US4 ~p",[{SQLa, SQLl}]),
    SQL = <<"update services set ",SQLa/binary," where id=?">>,
    lager:debug("US5",[]),
    Params = Pa ++ [Id],
    lager:debug("SQL: ~p", [SQL]),
    lager:debug("Params: ~p", [Params]),
    emysql:execute(mysqlpool, SQL, Params).

