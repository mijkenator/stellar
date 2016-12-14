-module(mkh_session).

-export([
    check/1,
    check_session/1,
    set_session/2,
    delete_session/1]).

-define('SXP_TIME', 600*3).
-define('MXP_TIME', 1200*3).

-spec check(cowboy_req:req()) -> {'auth',cowboy_req:req(),[any()],binary()} | {'nonauth',cowboy_req:req(),[],binary()}.
check(CReq) ->
    lager:debug("MSC0:",[]),
    #{'MKHSession' := MKHSession} = cowboy_req:match_cookies([{'MKHSession', [], <<>>}], CReq),
    lager:debug("MSC1: ~p",[MKHSession]),
    case MKHSession of
        <<>> -> NSID = list_to_binary(uuid:to_string(uuid:v4())),
                lager:debug("MSC2: ~p",[NSID]),
                {nonauth, cowboy_req:set_resp_cookie(<<"MKHSession">>, NSID, #{max_age => ?SXP_TIME,path => <<"/">>}, CReq), [], NSID}
        ;_   -> case check_session(MKHSession) of
                    {auth, SessionData}   -> 
                        {auth, cowboy_req:set_resp_cookie(<<"MKHSession">>, MKHSession, #{max_age => ?SXP_TIME,path => <<"/">>}, CReq), SessionData, MKHSession}
                    ;_ -> {nonauth, cowboy_req:set_resp_cookie(<<"MKHSession">>, MKHSession, #{max_age => ?SXP_TIME,path => <<"/">>}, CReq), [], MKHSession}
                end
    end.    

-spec check_session(binary()) -> notfound | {auth, list()}.
check_session(SID) ->
    case mcd:get(mainCluster, SID) of
        {error, _ } -> notfound;
        {ok, SData} -> set_session(SID, SData), {auth, SData}
    end.

-spec set_session(binary(), list()) -> any().
set_session(SID, SData) ->
    mcd:set(mainCluster, SID, SData, ?MXP_TIME).

delete_session(SID) ->
    mcd:delete(mainCluster, SID).
