-module (emud_sess_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_session_first_started_test_() ->
    {setup,
    fun start_session/0,
    fun stop_session/1, 
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
         fun unsupported_cmd_returns_error/1,
         fun user_can_create_a_new_user/1,
         fun user_can_not_login_with_bad_username/1,
         fun user_can_not_login_with_bad_password/1,
         fun user_can_login_with_created_account/1
       ])
    end}.


start_session() ->
    SessId = emud_session_db:generate_session_id(),
    {ok, Sess} = emud_sess:start_link(SessId, self()),
    {SessId, Sess}.

stop_session({_SessId, Sess}) ->
    exit(Sess, normal).


unsupported_cmd_returns_error({SessId, Sess}) ->
    Cmd = #cmd{type=unsupported_cmd, sessid = SessId},
    ?_assertMatch({error, invalid_cmd_or_session}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_create_a_new_user({SessId, Sess}) ->
    Cmd = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"password">>}]},
    ?_assertMatch({ok, <<"test">>}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_not_login_with_bad_username({SessId, Sess}) ->
    Cmd = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"nope">>}, 
                    {password, <<"password">>}]},
    ?_assertMatch({error, invalid_creds}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_not_login_with_bad_password({SessId, Sess}) ->
    Cmd = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"nope">>}]},
    ?_assertMatch({error, invalid_creds}, emud_sess:handle_cmd(Sess, Cmd)).

user_can_login_with_created_account({SessId, Sess}) ->
    Cmd = #cmd{type=login, sessid = SessId, props = [
                    {username, <<"test">>},
                    {password, <<"password">>}]},
    ?_assertMatch(ok, emud_sess:handle_cmd(Sess, Cmd)).
% new_user -> new_character, login -> pick_character?

% need user record and storage, emud_srv could keep track of active/logged in?