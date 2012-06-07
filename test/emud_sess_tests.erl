-module (emud_sess_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_a_user_connects_the_first_time_test_() ->
    {"when a user connects the first time",setup,
    fun start_session/0,
    fun stop_session/1, 
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
         fun unsupported_cmd_returns_error/1,
         fun user_can_create_a_new_user/1
       ])
    end}.

when_a_user_already_has_an_account_test_() ->
    {"when a user already has an account",setup,
    fun setup_account/0,
    fun remove_account/1, 
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
         fun user_can_not_login_with_bad_username/1,
         fun user_can_not_login_with_bad_password/1,
         fun user_can_login/1,
         fun user_can_logout/1
       ])
    end}.


start_session() ->
    helper:start_deps(),
    application:start(emud),
    {ok, SessId, Sess} = emud_srv:connect(),
    {SessId, Sess}.

setup_account() ->
    {SessId, Sess} = start_session(),
    NewAccount = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"password">>}]},
    {ok, Username} = emud_sess:handle_cmd(Sess, NewAccount),
    {SessId, Sess, Username}.

stop_session({_SessId, _Sess}) ->
    emud_user_db:remove(<<"test">>),
    application:stop(emud).

remove_account({SessId, Sess, _Username}) ->
    stop_session({SessId, Sess}).


unsupported_cmd_returns_error({SessId, Sess}) ->
    Cmd = #cmd{type=unsupported_cmd, sessid = SessId},
    ?_assertMatch({error, invalid_cmd}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_create_a_new_user({SessId, Sess}) ->
    Cmd = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"password">>}]},
    ?_assertMatch({ok, <<"test">>}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_not_login_with_bad_username({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=login, sessid = SessId, props = [
                    {username, <<"nope">>}, 
                    {password, <<"password">>}]},
    ?_assertMatch({error, invalid_creds}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_not_login_with_bad_password({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=login, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"nope">>}]},
    ?_assertMatch({error, invalid_creds}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_login({SessId, Sess, Username}) ->
    Cmd = #cmd{type=login, sessid = SessId, props = [
                    {username, Username},
                    {password, <<"password">>}]},
    ?_assertMatch({ok, Username}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_logout({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=logout, sessid = SessId},
    ?_assertMatch(ok, emud_sess:handle_cmd(Sess, Cmd)).
