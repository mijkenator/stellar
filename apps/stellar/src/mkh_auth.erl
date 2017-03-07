-module(mkh_auth).

-export([
	login/2,
	login/4
]).


login(User, Password) -> login(User, Password, <<>>, <<>>).
login(Login, Password, SID, IP) ->
    case emysql:execute(mysqlpool, <<"select u.id,u.utype,u.confirmed from user u where login=? and password=?">>, [Login, Password]) of
        {result_packet,_,_,[[Uid, UType, Confirmed]],_} when is_integer(Uid), Uid > 0 -> 
        case {UType, Confirmed} of
          {0,0} -> 0;
          {1,0} -> 0;
          _     ->
            case SID of
                        S when is_binary(S),size(S) > 5 -> 
                            mkh_session:set_session(SID, [{<<"account_id">>, Uid},{<<"user_type">>, UType},{<<"ip">>, IP}])
                        ;_ -> ok
                    end,
            {ok, Uid, UType}
        end
        ;_                         -> 0 
    end.	
