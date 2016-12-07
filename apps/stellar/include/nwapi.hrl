-type session_state()   :: auth | nonauth.

-type session_data()    :: [{binary(), binary()}].

-type session_id()      :: binary().

-type nwapi_session()   :: { session_state(), session_data(), session_id() }.

-type nwapi_json()      :: [{_,_}]. 

-define(NWR(Fun, Args), erlang:apply(nwapi_utils, Fun, Args)).

-define(OLDAPI, 'webapi@webapi2'). % oldapi for communications

-define(ERRRESP(Type, Action, Req, Opts), {ok, erlang:apply(nwapi_utils, error_reply, [Action, Type, Req]), Opts}).

-define(OKRESP(Action, Resp, Req, Opts),  {ok, erlang:apply(nwapi_utils, ok_reply, [
        {[{<<"status">>,  <<"ok">>},{<<"type">>,    Action},{<<"data">>,    Resp}]}, Req, 2]), Opts}).

-define(OKRESP_OLD(Action, Resp, Req, Opts),  {ok, erlang:apply(nwapi_utils, ok_reply, [
       {[ {<<"commands">>, {[{<<"status">>,  <<"ok">>},{<<"type">>,    Action},{<<"data">>,    Resp}]}} ]}, Req, <<"1">>]), Opts}).

-define(JUSTOKRESP_OLD(Action, Req, Opts),  {ok, erlang:apply(nwapi_utils, ok_reply, [
       {[ {<<"commands">>, {[{<<"status">>,  <<"ok">>},{<<"type">>,    Action} ]}} ]}, Req]), Opts}).

-define(NOKRESP_OLD(Action, Msg, Req, Opts),  {ok, erlang:apply(nwapi_utils, ok_reply, [
       {[ {<<"commands">>, {[{<<"status">>,  <<"failed">>},{<<"type">>,    Action},{<<"errors">>, Msg}]}} ]}, Req, <<"1">>]), Opts}).
