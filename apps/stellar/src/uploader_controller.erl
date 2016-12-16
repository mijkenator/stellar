-module(uploader_controller).

-export([init/2]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(Req0, Opts) ->
    lager:debug("UC! HANDLE: ~p ~p", [Req0, Opts]),
    handle(Req0, Opts).


handle(Req0, State) ->
    lager:debug("BpH HANDLE: ~p ~p", [Req0, State]),
    {_SessionStatus, Req, SData, _SID} = mkh_session:check(Req0),

    {Type, Ret, Req2} = case cowboy_req:path(Req) of
        <<"/upload/service/category">> ->
            {Ret1, Req02} = multipart(Req, [], <<"upload-service-category">>),
            Ret0 = Ret1 ++ SData, 
            upload_service_category(Ret0),
            {<<"upload-service-category">>, Ret0, Req02};
        <<"/upload/service">> ->
            {Ret1, Req02} = multipart(Req, [], <<"upload-service">>),
            Ret0 = Ret1 ++ SData, 
            upload_service(Ret0),
            {<<"upload-service">>,Ret0, Req02};
        <<"/upload/contractor">> ->
            {Ret1, Req02} = multipart(Req, [], <<"upload-service">>),
            Ret0 = Ret1 ++ SData, 
            upload_contractor(Ret0),
            {<<"upload-service">>,Ret0, Req02}
        ;_UU -> 
            lager:debug("UU: ~p ", [_UU]),
            {<<"unk_upload">>,[],Req}
    end,
    lager:debug("BpH multipart ret: ~p", [Ret]),

    Img = case proplists:get_value(<<"mkh_image">>, Ret) of
            <<"/usr/share/nginx/html", R/binary>> -> R
            ;R -> R
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>},
                {<<"image">>,   Img}]}), Req2),
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

upload_service_category(Params) -> 
    [Img, Id] = [proplists:get_value(X, Params, <<>>) || X <- [<<"mkh_image">>, <<"id">> ]],
    Img0 = case Img of
            <<"/usr/share/nginx/html", R/binary>> -> R
            ;R -> R
    end,
    emysql:execute(mysqlpool, <<"update service_category set img=? where id=?">>, [Img0, Id]).

upload_contractor(Params) -> 
    [Img, Id] = [proplists:get_value(X, Params, <<>>) || X <- [<<"mkh_image">>, <<"account_id">>]],
    Img0 = case Img of
            <<"/usr/share/nginx/html", R/binary>> -> R
            ;R -> R
    end,
    emysql:execute(mysqlpool, <<"update user set photo=? where id=?">>, [Img0, Id]).

upload_service(Params) -> 
    [Img, Id] = [proplists:get_value(X, Params, <<>>) || X <- [<<"mkh_image">>, <<"id">> ]],
    Img0 = case Img of
            <<"/usr/share/nginx/html", R/binary>> -> R
            ;R -> R
    end,
    emysql:execute(mysqlpool, <<"update services set img=? where id=?">>, [Img0, Id]).

multipart(Req, A, Type) ->
    lager:debug("UC MULT1", []),
    %case cowboy_req:part(Req) of
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            lager:debug("UC MULT2", []),
            {Req4,AP} = case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    lager:debug("UC MULT3 ~p", [FieldName]),
                    {ok, Body, Req3} = cowboy_req:read_part_body(Req2),
                    {Req3, [{FieldName, Body}]};
                {file, FileInpName, Filename, _CType, _CTransferEncoding} ->
                    lager:debug("mp BCH FNL: ~p ~p", [FileInpName, Filename]),
                    case FileInpName of
                        <<"image">> ->
                            SaveFileName1 = get_file_name(Filename, FileInpName, Type),
                            lager:debug("temporary file name: ~p", [SaveFileName1]),
                            Req3 = save_file(Req2, SaveFileName1),
                            {Req3 ,[{<<"mkh_image">>, SaveFileName1}]};
                        _ -> {Req2 ,[]}
                    end
                ;Unk ->
                    lager:debug("UC MULT UNK ~p", [Unk])
            end,
            multipart(Req4, A++AP, Type);
        {done, Req2} -> {A, Req2}
    end.

save_file(Req, FileName) ->
    lager:debug("SAVEFILE1: ~p", [FileName]),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    lager:debug("SAVEFILE2", []),
    {ok, _, Req2}     = get_part_body(Req, IoDevice),
    lager:debug("SAVEFILE3", []),
    file:close(IoDevice),
    lager:debug("SAVEFILE DONE."),
    Req2.

-spec get_file_name(binary(), binary(), binary()) -> binary().
get_file_name(UploadFileName0, InputName, Type) ->
    UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = get_tmp_dir(InputName, Type),
    lager:debug("GFN: ~p ~p", [InputName, TempDirectory]),
    TN1 = <<TempDirectory/binary, UploadFileName/binary>>,
    case filelib:is_regular(TN1) of
        true  -> get_file_name(modify_ufn(UploadFileName), InputName, Type);
        false -> TN1
    end.

get_tmp_dir(_,<<"upload-service">>) -> <<"/usr/share/nginx/html/mijkuploads/">>;
get_tmp_dir(_,<<"upload-service-category">>) -> <<"/usr/share/nginx/html/mijkuploads/">>.

-spec modify_ufn(binary()) -> binary().
modify_ufn(UploadFileName) ->
    RN  = filename:rootname(UploadFileName),
    RN1 = case re:run(RN, "_c_(\\d+)$", []) of
        {match,[_,{B,L}]} ->
              L1 = B - 3,
              <<H:L1/binary,"_c_",N:L/binary,_/binary>> = RN,
              Extra = integer_to_binary(binary_to_integer(N) + 1),
              <<H/binary, "_c_", Extra/binary>>
        ;_ -> <<RN/binary, "_c_1">>
    end,
    EX = filename:extension(UploadFileName),
    <<RN1/binary, EX/binary>>.

get_part_body(Req, IoDevice) ->
    lager:debug("GPB PH HANDLE1: ", []),
    case cowboy_req:read_part_body(Req) of
        {ok, D, R }  ->
            ok = file:write(IoDevice, D),
            {ok, D, R};
        {more, A, R} ->
            lager:debug("more", []),
            ok = file:write(IoDevice, A),
            get_part_body(R, IoDevice);
        M ->
            lager:debug("LALALALALA ~p", [M]),
            M
    end.

reply(Response, Req) ->
        cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Response, Req).

terminate(Reason, _Req, _State) ->
    lager:debug("PH TERMINATE: ~p", [Reason]),
        ok.

