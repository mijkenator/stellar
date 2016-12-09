-module(model_user).

-export([
	signup/2
	,login_restore/1
	,signup_confirm/1
	,rpwd_confirm/2
	,delete_user/1
]).

signup(Login, Password) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"insert into user (login,password,utype,confirm_uid) values (?,?,0,?)">>, [Login,Password,GUid]) of
		{ok_packet,_,_,Uid,_,_,[]} -> 
		nwapi_utils:send_email(Login, <<"Subject: signup confirmation\n\n Confirmation code: ", GUid/binary>>),
		{ok, Uid};
		{error_packet,1,1062,<<"23000">>,_} -> {error, <<"already exists">>}
	end.

login_restore(Login) ->
	GUid = list_to_binary(uuid:to_string(uuid:v4())),
	case emysql:execute(mysqlpool, <<"select id from user where login=?">>, [Login]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"delete from user_pwdrestore where uid=?">>, [Uid]),
			emysql:execute(mysqlpool, <<"insert into user_pwdrestore (uid, guid) values (?,?)">>, [Uid, GUid]),
			nwapi_utils:send_email(Login, <<"Subject: password restore\n\n Restore code: ", GUid/binary>>), {ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

signup_confirm(Guid) ->
	case emysql:execute(mysqlpool, <<"select id from user where confirm_uid=?">>, [Guid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"update user set confirmed=1 where id=?">>, [Uid]),
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

rpwd_confirm(Guid, Pwd) ->
	case emysql:execute(mysqlpool, <<"select uid from user_pwdrestore where guid=?">>, [Guid]) of
		{result_packet,_,_,[[Uid]],_} when is_integer(Uid), Uid>0 -> 
			emysql:execute(mysqlpool, <<"update user set password=? where id=?">>, [Pwd, Uid]),
			emysql:execute(mysqlpool, <<"delete from user_pwdrestore where uid=?">>, [Uid]),
			{ok, Uid}
		;_ -> {error, <<"user not found">>}
	end.

delete_user(Uid) -> emysql:execute(mysqlpool, <<"delete from user where id=?">>, [Uid]).

