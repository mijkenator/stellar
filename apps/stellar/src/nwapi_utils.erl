-module(nwapi_utils).
-include("nwapi.hrl").
-export([
    ba/1,
    reply/2,
    error_reply/2,
    error_reply/3,
    old_error_resp/4,
    ok_reply/2,
    ok_reply/3,
    binary_response/2,
    binary_response_206/2,
    get_coin_cluster_config/0,
    to_list/1,
    to_int/1,
    to_bin/1,
    to_float/1,
    get_json_params/2,
    check_field_param/2,
    external_date/3,
    send_email/2,
    zip_fix/1,
    validate_zip/2,
    get_country/1,
    check_wapi2_session/1,
    clean_login/1,
    zip_ver/2
]).


-spec reply(binary(), cowboy_req:req()) -> cowboy_req:req().
reply(Resp, Req) ->
    cowboy_req:reply(200,  #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Resp, Req).

-spec error_reply(binary(), cowboy_req:req()) -> cowboy_req:req().
error_reply(Resp, Req) -> error_reply(<<"unkown">>, Resp, Req).

-spec error_reply(binary(), binary()|{integer(),binary()}, cowboy_req:req()) -> cowboy_req:req().
error_reply(Action, {ErrCode, ErrTxt}, Req) when is_integer(ErrCode),is_binary(ErrTxt)->
    Resp = {[{<<"type">>, Action},{<<"status">>,<<"failed">>},{<<"error">>, ErrTxt},{<<"error_code">>, ErrCode}]},
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, 
        jiffy:encode(Resp), Req);
error_reply(Action, Txt, Req) ->
    Resp = {[{<<"type">>, Action},{<<"status">>,<<"failed">>},{<<"error">>, Txt}]},
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, 
        jiffy:encode(Resp), Req).

old_error_resp({Msg, Code}, Action, Req, Opts) ->
    {ok, ok_reply(
       {[ {<<"commands">>, {[{<<"status">>,  <<"failed">>},{<<"type">>,    Action},{<<"errors">>, Msg}, {<<"error_code">>, Code}]}} ]}, 
            Req, <<"1">>), Opts}.


-spec ok_reply(_,cowboy_req:req()) -> cowboy_req:req().
ok_reply(Resp, Req) -> ok_reply(Resp, Req, <<"0">>).

ok_reply(Resp, Req, <<"1">>) when is_binary(Resp) -> ok_reply(jiffy:decode(Resp), Req, <<"1">>);
ok_reply({[{<<"type">>,_},{<<"status">>,_}|_] = List}, Req, <<"1">>) when is_list(List) ->
    lager:debug("OR1~p",[List]),
    Vml = proplists:get_value(<<"data">>, List, <<"no_data">>),
    lager:debug("OR2~p",[Vml]),
    case is_binary(Vml) of
        true -> ok_reply(jiffy:decode(Vml), Req, <<"1">>)
        ;_   -> ok_reply(Vml, Req, <<"1">>)
    end;
ok_reply(Resp, Req, <<"1">>) ->
    lager:debug("OR3~p",[Resp]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, 
        jiffy:encode(Resp, [pretty]), Req);
ok_reply(Resp, Req, _) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, 
        jiffy:encode(Resp), Req).

-spec binary_response(binary(), cowboy_req:req()) -> cowboy_req:req().
binary_response(Bin, Req) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"audio/wav">>}, Bin, Req).

-spec binary_response_206(binary(), cowboy_req:req()) -> cowboy_req:req().
binary_response_206(Bin, Req) ->
    Size  = size(Bin),
    Bytes = list_to_binary("0-" ++ integer_to_list(Size-1) ++ "/" ++ integer_to_list(Size)),
    {BR,B}= case cowboy_req:parse_header(<<"range">>, Req) of
        {bytes,[{0, End}]} when End==Size-1 -> {Bytes,Bin};
        {bytes,[{Begin,End}]} when 
            is_integer(Begin),is_integer(End),
            Begin>0,End>0,Begin<Size,End<Size,Begin<End ->
            ChunkSize = End - Begin,
            <<_:Begin/binary,B0:ChunkSize/binary,_/binary >> = Bin,
            {list_to_binary(
                integer_to_list(Begin) ++ "-" ++
                integer_to_list(End) ++ "/" ++ integer_to_list(Size)
            ), B0}    
        ;_ -> {Bytes,Bin}
    end,
    lager:debug("BR206 ~p ~p ~p", [Size, Bytes, BR]),
    cowboy_req:reply(206,  
        #{<<"content-type">> => <<"audio/wav">>,
         <<"Content-Range">> => <<"bytes ",BR/binary>>}
    , B, Req).

-spec ba([binary()]) -> binary().
ba(L)         -> bai(L, <<>> ).

bai([],A)     -> A;
bai([[]|T],A) -> bai(T, A);
bai([B|T],A) when is_integer(B) -> B1 = list_to_binary(B), bai(T, <<A/binary, B1/binary>>);
bai([B|T],A) when is_list(B)    -> B1 = list_to_binary(B), bai(T, <<A/binary, B1/binary>>);
bai([B|T],A) when is_float(B)   -> B1 = float_to_binary(B), bai(T, <<A/binary, B1/binary>>);
bai([B|T], A) -> bai(T, <<A/binary,B/binary>>).


get_coin_cluster_config() ->
    {ok, CoinClusterConfig} = file:consult("../../coinservers.conf"),
    ServersIPList = proplists:get_value(coinservers, CoinClusterConfig, []), 
    User          = list_to_binary(proplists:get_value(rpc_user, CoinClusterConfig, "litecoinrpc")),
    Password      = list_to_binary(proplists:get_value(rpc_pwd, CoinClusterConfig, "")),
    Port          = list_to_binary(proplists:get_value(rpc_port, CoinClusterConfig, "9332")),
    [{list_to_atom(IP), <<"http://", User/binary, ":", Password/binary, "@", (list_to_binary(IP))/binary, ":", Port/binary, "/">>} 
         || IP<-ServersIPList].

to_list(E) when is_binary(E) -> binary_to_list(E);
to_list(E) -> E.

to_int(I) when is_list(I)   -> list_to_integer(I);
to_int(I) when is_binary(I) -> binary_to_integer(I);
to_int(I) -> I.

to_bin(B) when is_list(B)    -> list_to_binary(B);
to_bin(B) when is_integer(B) -> integer_to_binary(B);
to_bin(B) when is_float(B)   -> float_to_binary(B);
to_bin(B) -> B.

to_float(B) when is_binary(B) -> binary_to_float(B);
to_float(B) when is_list(B)   -> list_to_float(B);
to_float(F) when is_float(F)  -> F.

apply_mods(V, [])  -> V;
apply_mods(V, [strip|T]) when is_binary(V)      -> apply_mods(list_to_binary(string:strip(binary_to_list(V))), T);
apply_mods(V, [digitsonly|T]) when is_binary(V) -> apply_mods(re:replace(V,"[^0-9+]", "", [global, {return, binary}]), T);
apply_mods(V, [{cut, N}|T]) when is_binary(V),is_integer(N),N < size(V) -> apply_mods(binary:part(V,0,N), T);
apply_mods(V, [_|T]) -> apply_mods(V, T).

apply_checks(_, []) -> ok;
apply_checks(V, [{regexp, Re}|T]) ->
    case re:run(V, Re, [{capture, none}]) of
        match -> apply_checks(V, T)
        ;_    -> nok
    end;
apply_checks(V, [{minlen, N}|_]) when size(V) < N -> nok;
apply_checks(V, [{maxlen, N}|_]) when size(V) > N -> nok;
apply_checks(V, [{len,    N}|_]) when size(V) =/= N -> nok;
apply_checks(V, [_|T]) -> apply_checks(V,T).

check_field_param(JSON, {FieldName, [DefaultValue, required, Fun]}) when is_function(Fun) ->
    Ret = proplists:get_value(list_to_binary(FieldName), JSON, DefaultValue),
    Ret =/= undefined orelse throw({error, badvalue, FieldName}),
    Fun(Ret) =/= false orelse throw({error, badvalue, FieldName, Ret}),
    Ret;
check_field_param(JSON, {FieldName, [DefaultValue, required, undefined]}) ->
    Ret = proplists:get_value(list_to_binary(FieldName), JSON, DefaultValue),
    Ret =/= undefined orelse throw({error, badvalue, FieldName}),
    Ret;
check_field_param(JSON, {FieldName, [DefaultValue, required, {regexp, Re}]}) ->
    Ret = proplists:get_value(list_to_binary(FieldName), JSON, DefaultValue),
    Ret =/= undefined orelse throw({error, badvalue, FieldName}),
    re:run(Ret, Re, [{capture, none}]) == match orelse throw({error, badvalue, FieldName}),
    Ret;
check_field_param(JSON, {FieldName, [DefaultValue, required, {ModList, CheckList}]}) when is_list(ModList) ->
    Ret = proplists:get_value(list_to_binary(FieldName), JSON, DefaultValue),
    Ret =/= undefined orelse throw({error, badvalue, FieldName}),
    lager:debug("CFP1 ~p ~p",[FieldName, Ret]),
    Ret1 = apply_mods(Ret, ModList),
    lager:debug("CFP2 ~p ~p",[FieldName, Ret1]),
    apply_checks(Ret1, CheckList) == ok orelse throw({error, badvalue, FieldName}),
    lager:debug("CFP3 ~p ~p",[FieldName, Ret1]),
    Ret1;
check_field_param(_JSON, {FieldName, [_DefaultValue, _IsRequired, _CheckFunction]}) ->
    throw({error, check_field_param, FieldName}).

get_json_params(JSON, Params) ->
    lists:map(fun(E)-> check_field_param(JSON, E) end, Params).

external_date(Input, InputTimeZone, OutputTimeZone) ->
    CmdPattern = "date --date 'TZ=\"~s\" ~s' +%Y%m%d%H%M%S",
    CmdDeep = io_lib:format(CmdPattern, [InputTimeZone, Input]),
    Cmd = lists:flatten(CmdDeep),
    Port = open_port({spawn, Cmd}, [{env, [{"TZ", OutputTimeZone}]}, eof, stderr_to_stdout]),
    ResultString = get_stdout(Port),
    case io_lib:fread("~4d~2d~2d~2d~2d~2d", ResultString) of
        {ok, [YearNew, MonthNew, DayNew, HourNew, MinuteNew, SecondNew], _LeftOverChars} ->
            {{YearNew, MonthNew, DayNew}, {HourNew, MinuteNew, SecondNew}}
    end.

get_stdout(Port) ->
    loop(Port, []).
loop(Port, DataAcc) ->
    receive
        {Port, {data, Data}} ->
            loop(Port, DataAcc ++ Data);
        {Port, eof} ->
            DataAcc
    end.

send_email(To, Mail) when is_binary(To) -> send_email([To], Mail);
send_email(To, Mail) ->
    gen_smtp_client:send({<<"support">>, To, Mail},
        [{relay, "127.0.0.1"}, {port, 25}]).

zip_ver(Country, Zip) ->
    try
        lager:debug("ZIPVER0 ~p", [{Country, Zip}]),
        lager:debug("ZIPVER1 ~p", [{Country, Zip, nwapi_utils:validate_zip(Country, Zip)}]),
        Zip0 = zip_fix(Zip),
        lager:debug("ZIPVER2 ~p -> ~p = ~p", [Zip, Zip0, Zip == Zip0]),
        {ok, Zip0}
    catch
        E:R ->
            lager:error("zip_ver error: ~p ~p", [{Country, Zip}, {E,R}]),
            {error, Zip}
    end.
    

zip_fix(Z) when is_list(Z)   -> zip_fix(list_to_binary(Z));
zip_fix(Z) when is_binary(Z) ->
    Z1 = re:replace(Z, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]),
    re:replace(Z1,  "[^0-9a-zA-Z\s-]",  "", [global, {return,binary}]).

validate_zip(C,Z) when is_list(C) -> validate_zip(list_to_binary(C),Z);
validate_zip(C,Z) when is_list(Z) -> validate_zip(C,list_to_binary(Z));
validate_zip(<<"UK">>,Z) when is_list(Z) -> validate_zip(<<"GB">>,list_to_binary(Z));
validate_zip(<<"UK">>,Z) when is_binary(Z) -> validate_zip(<<"GB">>, Z);
validate_zip(<<"HZ">>,_)          -> {error, <<"HZ country">>};
validate_zip(Country, Zip) when is_binary(Country), is_binary(Zip), size(Country) > 2 ->
    validate_zip(get_country(Country), Zip);
validate_zip(Country, Zip) when is_binary(Country), is_binary(Zip)->
%
% validation rules according http://unicode.org/cldr/trac/browser/tags/release-26-0-1/common/supplemental/postalCodeData.xml?rev=11071
%
    ZR = [
    {<<"GB">>, "GIR[ ]?0AA|((AB|AL|B|BA|BB|BD|BH|BL|BN|BR|BS|BT|CA|CB|CF|CH|CM|CO|CR|CT|CV|CW|DA|DD|DE|DG|DH|DL|DN|DT|DY|E|EC|EH|EN|EX|FK|FY|G|GL|GY|GU|HA|HD|HG|HP|HR|HS|HU|HX|IG|IM|IP|IV|JE|KA|KT|KW|KY|L|LA|LD|LE|LL|LN|LS|LU|M|ME|MK|ML|N|NE|NG|NN|NP|NR|NW|OL|OX|PA|PE|PH|PL|PO|PR|RG|RH|RM|S|SA|SE|SG|SK|SL|SM|SN|SO|SP|SR|SS|ST|SW|SY|TA|TD|TF|TN|TQ|TR|TS|TW|UB|W|WA|WC|WD|WF|WN|WR|WS|WV|YO|ZE)(\\d[\\dA-Z]?[ ]?\\d[ABD-HJLN-UW-Z]{2}))|BFPO[ ]?\\d{1,4}"},
    {<<"JE">>, "JE\\d[\\dA-Z]?[ ]?\\d[ABD-HJLN-UW-Z]{2}"},
    {<<"GG">>, "GY\\d[\\dA-Z]?[ ]?\\d[ABD-HJLN-UW-Z]{2}"},
    {<<"IM">>, "IM\\d[\\dA-Z]?[ ]?\\d[ABD-HJLN-UW-Z]{2}"},
    {<<"US">>, "\\d{5}([\s\-]\\d{4})?"},
    {<<"CA">>, "[ABCEGHJKLMNPRSTVXY]\\d[ABCEGHJ-NPRSTV-Z][ ]?\\d[ABCEGHJ-NPRSTV-Z]\\d"},
    {<<"DE">>, "\\d{5}"},
    {<<"JP">>, "\\d{3}-\\d{4}"},
    {<<"FR">>, "\\d{2}[ ]?\\d{3}"},
    {<<"AU">>, "\\d{4}"},
    {<<"IT">>, "\\d{5}"},
    {<<"CH">>, "\\d{4}"},
    {<<"AT">>, "\\d{4}"},
    {<<"ES">>, "\\d{5}"},
    {<<"NL">>, "\\d{4}[ ]?[A-Z]{2}"},
    {<<"BE">>, "\\d{4}"},
    {<<"DK">>, "\\d{4}"},
    {<<"SE">>, "\\d{3}[ ]?\\d{2}"},
    {<<"NO">>, "\\d{4}"},
    {<<"BR">>, "\\d{5}[\-]?\\d{3}"},
    {<<"PT">>, "\\d{4}([\-]\\d{3})?"},
    {<<"FI">>, "\\d{5}"},
    {<<"AX">>, "22\\d{3}"},
    {<<"KR">>, "\\d{3}[\-]\\d{3}"},
    {<<"CN">>, "\\d{6}"},
    {<<"TW">>, "\\d{3}(\\d{2})?"},
    {<<"SG">>, "\\d{6}"},
    {<<"DZ">>, "\\d{5}"},
    {<<"AD">>, "AD\\d{3}"},
    {<<"AR">>, "([A-HJ-NP-Z])?\\d{4}([A-Z]{3})?"},
    {<<"AM">>, "(37)?\\d{4}"},
    {<<"AZ">>, "\\d{4}"},
    {<<"BH">>, "((1[0-2]|[2-9])\\d{2})?"},
    {<<"BD">>, "\\d{4}"},
    {<<"BB">>, "(BB\\d{5})?"},
    {<<"BY">>, "\\d{6}"},
    {<<"BM">>, "[A-Z]{2}[ ]?[A-Z0-9]{2}"},
    {<<"BA">>, "\\d{5}"},
    {<<"IO">>, "BBND 1ZZ"},
    {<<"BN">>, "[A-Z]{2}[ ]?\\d{4}"},
    {<<"BG">>, "\\d{4}"},
    {<<"KH">>, "\\d{5}"},
    {<<"CV">>, "\\d{4}"},
    {<<"CL">>, "\\d{7}"},
    {<<"CR">>, "\\d{4,5}|\\d{3}-\\d{4}"},
    {<<"HR">>, "\\d{5}"},
    {<<"CY">>, "\\d{4}"},
    {<<"CZ">>, "\\d{3}[ ]?\\d{2}"},
    {<<"DO">>, "\\d{5}"},
    {<<"EC">>, "([A-Z]\\d{4}[A-Z]|(?:[A-Z]{2})?\\d{6})?"},
    {<<"EG">>, "\\d{5}"},
    {<<"EE">>, "\\d{5}"},
    {<<"FO">>, "\\d{3}"},
    {<<"GE">>, "\\d{4}"},
    {<<"GR">>, "\\d{3}[ ]?\\d{2}"},
    {<<"GL">>, "39\\d{2}"},
    {<<"GT">>, "\\d{5}"},
    {<<"HT">>, "\\d{4}"},
    {<<"HN">>, "(?:\\d{5})?"},
    {<<"HU">>, "\\d{4}"},
    {<<"IS">>, "\\d{3}"},
    {<<"IN">>, "\\d{6}"},
    {<<"ID">>, "\\d{5}"},
    {<<"IL">>, "\\d{5}"},
    {<<"JO">>, "\\d{5}"},
    {<<"KZ">>, "\\d{6}"},
    {<<"KE">>, "\\d{5}"},
    {<<"KW">>, "\\d{5}"},
    {<<"LA">>, "\\d{5}"},
    {<<"LV">>, "\\d{4}"},
    {<<"LB">>, "(\\d{4}([ ]?\\d{4})?)?"},
    {<<"LI">>, "(948[5-9])|(949[0-7])"},
    {<<"LT">>, "\\d{5}"},
    {<<"LU">>, "\\d{4}"},
    {<<"MK">>, "\\d{4}"},
    {<<"MY">>, "\\d{5}"},
    {<<"MV">>, "\\d{5}"},
    {<<"MT">>, "[A-Z]{3}[ ]?\\d{2,4}"},
    {<<"MU">>, "(\\d{3}[A-Z]{2}\\d{3})?"},
    {<<"MX">>, "\\d{5}"},
    {<<"MD">>, "\\d{4}"},
    {<<"MC">>, "980\\d{2}"},
    {<<"MA">>, "\\d{5}"},
    {<<"NP">>, "\\d{5}"},
    {<<"NZ">>, "\\d{4}"},
    {<<"NI">>, "((\\d{4}-)?\\d{3}-\\d{3}(-\\d{1})?)?"},
    {<<"NG">>, "(\\d{6})?"},
    {<<"OM">>, "(PC )?\\d{3}"},
    {<<"PK">>, "\\d{5}"},
    {<<"PY">>, "\\d{4}"},
    {<<"PH">>, "\\d{4}"},
    {<<"PL">>, "\\d{2}-\\d{3}"},
    {<<"PR">>, "00[679]\\d{2}([ \-]\\d{4})?"},
    {<<"RO">>, "\\d{6}"},
    {<<"RU">>, "\\d{6}"},
    {<<"SM">>, "4789\\d"},
    {<<"SA">>, "\\d{5}"},
    {<<"SN">>, "\\d{5}"},
    {<<"SK">>, "\\d{3}[ ]?\\d{2}"},
    {<<"SI">>, "\\d{4}"},
    {<<"ZA">>, "\\d{4}"},
    {<<"LK">>, "\\d{5}"},
    {<<"TJ">>, "\\d{6}"},
    {<<"TH">>, "\\d{5}"},
    {<<"TN">>, "\\d{4}"},
    {<<"TR">>, "\\d{5}"},
    {<<"TM">>, "\\d{6}"},
    {<<"UA">>, "\\d{5}"},
    {<<"UY">>, "\\d{5}"},
    {<<"UZ">>, "\\d{6}"},
    {<<"VA">>, "00120"},
    {<<"VE">>, "\\d{4}"},
    {<<"ZM">>, "\\d{5}"},
    {<<"AS">>, "96799"},
    {<<"CC">>, "6799"},
    {<<"CK">>, "\\d{4}"},
    {<<"RS">>, "\\d{6}"},
    {<<"ME">>, "8\\d{4}"},
    {<<"CS">>, "\\d{5}"},
    {<<"YU">>, "\\d{5}"},
    {<<"CX">>, "6798"},
    {<<"ET">>, "\\d{4}"},
    {<<"FK">>, "FIQQ 1ZZ"},
    {<<"NF">>, "2899"},
    {<<"FM">>, "(9694[1-4])([ \-]\\d{4})?"},
    {<<"GF">>, "9[78]3\\d{2}"},
    {<<"GN">>, "\\d{3}"},
    {<<"GP">>, "9[78][01]\\d{2}"},
    {<<"GS">>, "SIQQ 1ZZ"},
    {<<"GU">>, "969[123]\\d([ \-]\\d{4})?"},
    {<<"GW">>, "\\d{4}"},
    {<<"HM">>, "\\d{4}"},
    {<<"IQ">>, "\\d{5}"},
    {<<"KG">>, "\\d{6}"},
    {<<"LR">>, "\\d{4}"},
    {<<"LS">>, "\\d{3}"},
    {<<"MG">>, "\\d{3}"},
    {<<"MH">>, "969[67]\\d([ \-]\\d{4})?"},
    {<<"MN">>, "\\d{6}"},
    {<<"MP">>, "9695[012]([ \-]\\d{4})?"},
    {<<"MQ">>, "9[78]2\\d{2}"},
    {<<"NC">>, "988\\d{2}"},
    {<<"NE">>, "\\d{4}"},
    {<<"VI">>, "008(([0-4]\\d)|(5[01]))([ \-]\\d{4})?"},
    {<<"PF">>, "987\\d{2}"},
    {<<"PG">>, "\\d{3}"},
    {<<"PM">>, "9[78]5\\d{2}"},
    {<<"PN">>, "PCRN 1ZZ"},
    {<<"PW">>, "96940"},
    {<<"RE">>, "9[78]4\\d{2}"},
    {<<"SH">>, "(ASCN|STHL) 1ZZ"},
    {<<"SJ">>, "\\d{4}"},
    {<<"SO">>, "\\d{5}"},
    {<<"SZ">>, "[HLMS]\\d{3}"},
    {<<"TC">>, "TKCA 1ZZ"},
    {<<"WF">>, "986\\d{2}"},
    {<<"XK">>, "\\d{5}"},
    {<<"YT">>, "976\\d{2}"}],

    case proplists:get_value(Country, ZR) of
        undefined -> {error, <<"unknown country: ", Country/binary>>}
        ;Regex    -> {ok, re:run(Zip, "^"++ Regex ++ "$", [caseless, {capture, none}]) == match }
    end.

get_country(Country) ->
    C = [
    {<<"AF">>, <<"afghanistan">>},
    {<<"AL">>, <<"albania">>},
    {<<"DZ">>, <<"algeria">>},
    {<<"AS">>, <<"american samoa">>},
    {<<"AD">>, <<"andorra">>},
    {<<"AO">>, <<"angola">>},
    {<<"AI">>, <<"anguilla">>},
    {<<"AG">>, <<"antigua">>},
    {<<"AR">>, <<"argentina">>},
    {<<"AM">>, <<"armenia">>},
    {<<"AW">>, <<"aruba">>},
    {<<"AC">>, <<"ascension island">>},
    {<<"AU">>, <<"australia">>},
    {<<"AT">>, <<"austria">>},
    {<<"AZ">>, <<"azerbaijan">>},
    {<<"BS">>, <<"bahamas">>},
    {<<"BH">>, <<"bahrain">>},
    {<<"BD">>, <<"bangladesh">>},
    {<<"BB">>, <<"barbados">>},
    {<<"BY">>, <<"belarus">>},
    {<<"BE">>, <<"belgium">>},
    {<<"BZ">>, <<"belize">>},
    {<<"BJ">>, <<"benin">>},
    {<<"BM">>, <<"bermuda">>},
    {<<"BT">>, <<"bhutan">>},
    {<<"BO">>, <<"bolivia">>},
    {<<"BA">>, <<"bosnia herzegovina">>},
    {<<"BW">>, <<"botswana">>},
    {<<"BR">>, <<"brazil">>},
    {<<"VG">>, <<"british virgin islands">>},
    {<<"BN">>, <<"brunei">>},
    {<<"BG">>, <<"bulgaria">>},
    {<<"BF">>, <<"burkina faso">>},
    {<<"BI">>, <<"burundi">>},
    {<<"KH">>, <<"cambodia">>},
    {<<"CM">>, <<"cameroon">>},
    {<<"CA">>, <<"canada">>},
    {<<"CV">>, <<"cape verde">>},
    {<<"KY">>, <<"cayman islands">>},
    {<<"CF">>, <<"central african republic">>},
    {<<"TD">>, <<"chad">>},
    {<<"CL">>, <<"chile">>},
    {<<"CN">>, <<"china">>},
    {<<"CO">>, <<"colombia">>},
    {<<"KM">>, <<"comoros">>},
    {<<"CG">>, <<"republic of congo">>},
    {<<"CD">>, <<"congo democratic republic">>},
    {<<"CK">>, <<"cook islands">>},
    {<<"CR">>, <<"costa rica">>},
    {<<"HR">>, <<"croatia">>},
    {<<"CU">>, <<"cuba">>},
    {<<"CY">>, <<"cyprus">>},
    {<<"CZ">>, <<"czech Republic">>},
    {<<"DK">>, <<"denmark">>},
    {<<"DJ">>, <<"djibouti">>},
    {<<"DM">>, <<"dominica">>},
    {<<"DO">>, <<"dominican republic">>},
    {<<"EC">>, <<"ecuador">>},
    {<<"EG">>, <<"egypt">>},
    {<<"SV">>, <<"el salvador">>},
    {<<"GQ">>, <<"equatorial guinea">>},
    {<<"ER">>, <<"eritrea">>},
    {<<"EE">>, <<"estonia">>},
    {<<"ET">>, <<"ethiopia">>},
    {<<"FO">>, <<"faeroe islands">>},
    {<<"FK">>, <<"falkland islands">>},
    {<<"FJ">>, <<"fiji">>},
    {<<"FI">>, <<"finland">>},
    {<<"FR">>, <<"france">>},
    {<<"TF">>, <<"french antilles">>},
    {<<"GF">>, <<"french guiana">>},
    {<<"PF">>, <<"french polynesia">>},
    {<<"GA">>, <<"gabon republic">>},
    {<<"GM">>, <<"gambia">>},
    {<<"GE">>, <<"georgia">>},
    {<<"DE">>, <<"germany">>},
    {<<"GH">>, <<"ghana">>},
    {<<"GI">>, <<"gibraltar">>},
    {<<"GR">>, <<"greece">>},
    {<<"GL">>, <<"greenland">>},
    {<<"GD">>, <<"grenada">>},
    {<<"GP">>, <<"guadeloupe">>},
    {<<"GU">>, <<"guam">>},
    {<<"GT">>, <<"guatemala">>},
    {<<"GN">>, <<"guinea">>},
    {<<"GW">>, <<"guinea bissau">>},
    {<<"GY">>, <<"guyana">>},
    {<<"HT">>, <<"haiti">>},
    {<<"HN">>, <<"honduras">>},
    {<<"HK">>, <<"hong kong">>},
    {<<"HU">>, <<"hungary">>},
    {<<"IS">>, <<"iceland">>},
    {<<"IN">>, <<"india">>},
    {<<"ID">>, <<"indonesia">>},
    {<<"IR">>, <<"iran">>},
    {<<"IQ">>, <<"iraq">>},
    {<<"IE">>, <<"ireland">>},
    {<<"IL">>, <<"israel">>},
    {<<"IT">>, <<"italy">>},
    {<<"CI">>, <<"ivory coast">>},
    {<<"JM">>, <<"jamaica">>},
    {<<"JP">>, <<"japan">>},
    {<<"JO">>, <<"jordan">>},
    {<<"KZ">>, <<"kazakhstan">>},
    {<<"KE">>, <<"kenya">>},
    {<<"KI">>, <<"kiribati">>},
    {<<"KW">>, <<"kuwait">>},
    {<<"KG">>, <<"kyrgyzstan">>},
    {<<"LA">>, <<"laos">>},
    {<<"LV">>, <<"latvia">>},
    {<<"LB">>, <<"lebanon">>},
    {<<"LS">>, <<"lesotho">>},
    {<<"LR">>, <<"liberia">>},
    {<<"LY">>, <<"libya">>},
    {<<"LI">>, <<"liechtenstein">>},
    {<<"LT">>, <<"lithuania">>},
    {<<"LU">>, <<"luxembourg">>},
    {<<"MO">>, <<"macau">>},
    {<<"MK">>, <<"macedonia">>},
    {<<"MG">>, <<"madagascar">>},
    {<<"MW">>, <<"malawi">>},
    {<<"MY">>, <<"malaysia">>},
    {<<"MV">>, <<"maldives">>},
    {<<"ML">>, <<"mali">>},
    {<<"MT">>, <<"malta">>},
    {<<"MH">>, <<"marshall islands">>},
    {<<"MR">>, <<"mauritania">>},
    {<<"MU">>, <<"mauritius">>},
    {<<"YT">>, <<"mayotte island">>},
    {<<"MX">>, <<"mexico">>},
    {<<"FM">>, <<"micronesia">>},
    {<<"MD">>, <<"moldova">>},
    {<<"MC">>, <<"monaco">>},
    {<<"MN">>, <<"mongolia">>},
    {<<"MS">>, <<"montserrat">>},
    {<<"MA">>, <<"morocco">>},
    {<<"MZ">>, <<"mozambique">>},
    {<<"MM">>, <<"myanmar">>},
    {<<"NA">>, <<"namibia">>},
    {<<"NR">>, <<"nauru">>},
    {<<"NP">>, <<"nepal">>},
    {<<"NL">>, <<"netherlands">>},
    {<<"AN">>, <<"netherlands antilles">>},
    {<<"NC">>, <<"new caledonia">>},
    {<<"NZ">>, <<"new zealand">>},
    {<<"NI">>, <<"nicaragua">>},
    {<<"NE">>, <<"niger">>},
    {<<"NG">>, <<"nigeria">>},
    {<<"NU">>, <<"niue island">>},
    {<<"KP">>, <<"north korea">>},
    {<<"NO">>, <<"norway">>},
    {<<"OM">>, <<"oman">>},
    {<<"PK">>, <<"pakistan">>},
    {<<"PW">>, <<"palau">>},
    {<<"PS">>, <<"palestine">>},
    {<<"PA">>, <<"panama">>},
    {<<"PG">>, <<"papua new guinea">>},
    {<<"PY">>, <<"paraguay">>},
    {<<"PE">>, <<"peru">>},
    {<<"PH">>, <<"philippines">>},
    {<<"PL">>, <<"poland">>},
    {<<"PT">>, <<"portugal">>},
    {<<"PR">>, <<"puerto rico">>},
    {<<"QA">>, <<"qatar">>},
    {<<"RE">>, <<"reunion island">>},
    {<<"RO">>, <<"romania">>},
    {<<"RU">>, <<"russian federation">>},
    {<<"RW">>, <<"rwanda">>},
    {<<"MP">>, <<"saipan">>},
    {<<"SM">>, <<"san marino">>},
    {<<"ST">>, <<"sao tome">>},
    {<<"SA">>, <<"saudi arabia">>},
    {<<"SN">>, <<"senegal">>},
    {<<"CS">>, <<"serbia">>},
    {<<"SC">>, <<"seychelles island">>},
    {<<"SL">>, <<"sierra leone">>},
    {<<"SG">>, <<"singapore">>},
    {<<"SK">>, <<"slovakia">>},
    {<<"SI">>, <<"slovenia">>},
    {<<"SB">>, <<"solomon islands">>},
    {<<"SO">>, <<"somalia">>},
    {<<"ZA">>, <<"south africa">>},
    {<<"KR">>, <<"south korea">>},
    {<<"ES">>, <<"spain">>},
    {<<"LK">>, <<"sri lanka">>},
    {<<"SH">>, <<"st. helena">>},
    {<<"KN">>, <<"st. kitts & nevis">>},
    {<<"LC">>, <<"st. lucia">>},
    {<<"PM">>, <<"st. pierre & miquelon">>},
    {<<"VC">>, <<"st. vincent">>},
    {<<"SD">>, <<"sudan">>},
    {<<"SR">>, <<"suriname">>},
    {<<"SZ">>, <<"swaziland">>},
    {<<"SE">>, <<"sweden">>},
    {<<"CH">>, <<"switzerland">>},
    {<<"SY">>, <<"syria">>},
    {<<"TW">>, <<"taiwan">>},
    {<<"TJ">>, <<"tajikistan">>},
    {<<"TZ">>, <<"tanzania">>},
    {<<"TH">>, <<"thailand">>},
    {<<"TG">>, <<"togo">>},
    {<<"TK">>, <<"tokelau">>},
    {<<"TO">>, <<"tonga">>},
    {<<"TT">>, <<"trinidad & tobago">>},
    {<<"TN">>, <<"tunisia">>},
    {<<"TR">>, <<"turkey">>},
    {<<"TM">>, <<"turkmenistan">>},
    {<<"TC">>, <<"turks & caicos">>},
    {<<"TV">>, <<"tuvalu">>},
    {<<"UG">>, <<"uganda">>},
    {<<"UA">>, <<"ukraine">>},
    {<<"AE">>, <<"united arab emirates">>},
    {<<"UK">>, <<"united kingdom">>},
    {<<"UY">>, <<"uruguay">>},
    {<<"VI">>, <<"us virgin islands">>},
    {<<"US">>, <<"usa">>},
    {<<"UZ">>, <<"uzbekistan">>},
    {<<"VU">>, <<"vanuatu">>},
    {<<"VA">>, <<"vatican city">>},
    {<<"YV">>, <<"venezuela">>},
    {<<"VN">>, <<"vietnam">>},
    {<<"WF">>, <<"wallis & futuna">>},
    {<<"WS">>, <<"western samoa">>},
    {<<"YD">>, <<"yemen arab republic">>},
    {<<"ZM">>, <<"zambia">>},
    {<<"ZW">>, <<"zimbabwe">>},
    {<<"AX">>, <<"aland islands">>},
    {<<"AQ">>, <<"antarctica">>},
    {<<"GG">>, <<"guernsey">>},
    {<<"IM">>, <<"isle of man">>},
    {<<"JE">>, <<"jersey">>},
    {<<"MQ">>, <<"martinique">>},
    {<<"NF">>, <<"norfolk island">>},
    {<<"PN">>, <<"pitcairn">>},
    {<<"SJ">>, <<"svalbard and jan mayen">>},
    {<<"SS">>, <<"south sudan">>},
    {<<"MF">>, <<"st. martin">>},
    {<<"ME">>, <<"montenegro">>},
    {<<"A1">>, <<"abkhazia">>}],
    case lists:keysearch(list_to_binary(string:to_lower(binary_to_list(Country))), 2, C) of
        {value, {ISO, _}} -> ISO
        ;_                -> 
            lager:error("ZIPVER unknown country: ~p", [Country]),
            <<"HZ">>
    end.

wapi2() -> 'webapi@webapi2'.

-spec check_wapi2_session(binary()) -> {ok, integer()}|{error, atom}.
check_wapi2_session(Cookie) ->
    case rpc:call(wapi2(),e_session,get_session,[binary_to_list(Cookie)]) of
        {ok, L} when is_list(L) ->
            case proplists:get_value("account_id", L, 0) of
                0 -> {error, notauthorized};
                A when is_integer(A),A > 0 -> {ok, A}
            end
        ;_ -> {error, notfound}
    end.

clean_login(Login) when is_binary(Login) ->
    case binary:match(Login, <<"@">>) of
        nomatch   -> clean_login_i(Login)
        ;_        -> Login
    end;
clean_login(Login) -> Login.

clean_login_i(<<"+",   Login/binary>>) -> Login;
clean_login_i(<<"011", Login/binary>>) -> Login;
clean_login_i(<<"00",  Login/binary>>) -> Login;
clean_login_i(Login) -> Login.

