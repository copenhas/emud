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

when_a_user_logins_first_time_test_() ->
    {"when a user logins for the first time", setup,
    fun setup_login/0,
    fun remove_account/1,
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
        fun can_not_pick_a_non_existent_character/1,
        fun can_start_the_new_character_workflow/1
       ])
    end}.

when_a_user_is_create_a_new_character_test_() ->
    {"when a user is creating a character", setup,
    fun setup_new_char/0,
    fun remove_account/1,
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
        fun first_they_pick_a_name/1,
        fun finally_they_can_pick_the_character_to_join_the_game/1
       ])
    end}.

when_a_user_joins_the_game_test_() ->
    {"when a user joins the game", setup,
    fun join_game/0,
    fun remove_account/1,
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
        fun room_has_them_as_an_occupant/1,
        fun current_state_is_in_game/1,
        fun logging_out_also_removes_them_from_game/1
       ])
    end}.


when_a_user_is_in_game_test_() ->
    {"when a user is in game", setup,
    fun join_game/0,
    fun remove_account/1,
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
        fun a_bad_command_does_not_kill_the_session/1
       ])
    end}.


can_execute_a_cmd_and_recieve_msg_test() ->
    % had to put all the setup and clean in the test 
    %due to mismatched Pids
    Ctxt = {SessId, Sess, _Username} = join_game(),
    Look = #cmd{type=look, sessid = SessId},
    ?assertEqual(ok, emud_sess:handle_cmd(Sess, Look)),
    receive 
        {emud_msg, _Ref, Msg} -> ?assertMatch(#msg{type=look}, Msg)
    after 1000 ->
        throw(no_msg_found)
    end,
    remove_account(Ctxt).


start_session() ->
    helper:start_deps(),
    application:start(emud),
    {ok, SessId, Sess} = emud_srv:connect(),
    receive 
        {emud_msg, _Ref, Msg} -> ?assertMatch(#msg{type=welcome}, Msg)
    after 1000 ->
        throw(no_msg_found)
    end,
    {SessId, Sess}.

setup_account() ->
    {SessId, Sess} = start_session(),
    NewAccount = #cmd{type=new_user, sessid = SessId, props = [
                    {username, <<"test">>}, 
                    {password, <<"password">>}]},
    {ok, Username} = emud_sess:handle_cmd(Sess, NewAccount),
    {SessId, Sess, Username}.

setup_login() ->
    {SessId, Sess, Username} = setup_account(),
    Login = #cmd{type=login, sessid = SessId, props = [
                    {username, Username},
                    {password, <<"password">>}]},   
    emud_sess:handle_cmd(Sess, Login),
    {SessId, Sess, Username}.

setup_new_char() ->
    {SessId, Sess, Username} = setup_login(),
    NewChar = #cmd{type=new_character, sessid = SessId},
    ok = emud_sess:handle_cmd(Sess, NewChar),
    {SessId, Sess, Username}.

join_game() ->
    {SessId, Sess, Username} = setup_new_char(),
    PickName = #cmd{type=character_name, sessid = SessId, props = [
                    {name, <<"test character">>}]},   
    {ok, <<"test character">>} = emud_sess:handle_cmd(Sess, PickName),
    JoinGame = #cmd{type=pick_character, sessid = SessId, props = [
                    {character, <<"test character">>}]},
    {ok, <<"test character">>} = emud_sess:handle_cmd(Sess, JoinGame),
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
    fun () ->
        ?assertMatch(ok, emud_sess:handle_cmd(Sess, Cmd)),
        ?assertNot(erlang:is_process_alive(Sess))
    end.

can_not_pick_a_non_existent_character({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=pick_character, sessid = SessId, props = [
                    {character, <<"doesn't exist">>}]},
    ?_assertMatch({error, no_character}, emud_sess:handle_cmd(Sess, Cmd)).

can_start_the_new_character_workflow({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=new_character, sessid = SessId},
    ?_assertMatch(ok, emud_sess:handle_cmd(Sess, Cmd)).

first_they_pick_a_name({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=character_name, sessid = SessId, props = [
                    {name, <<"test character">>}]},
    ?_assertMatch({ok, <<"test character">>}, emud_sess:handle_cmd(Sess, Cmd)).

finally_they_can_pick_the_character_to_join_the_game({SessId, Sess, _Username}) ->
    fun () ->
        Cmd = #cmd{type=pick_character, sessid = SessId, props = [
                    {character, <<"test character">>}]},
        ?assertMatch({ok, <<"test character">>}, emud_sess:handle_cmd(Sess, Cmd)),
        ?assertMatch(in_game, emud_sess:get_state(Sess, SessId))
    end.

room_has_them_as_an_occupant(_) ->
    Char = emud_char:get(<<"test character">>),
    RmId = Char#char.room,
    ?_assertMatch([<<"test character">>], emud_room:occupants(RmId)).

current_state_is_in_game({SessId, Sess, _Username}) ->
    ?_assertMatch(in_game, emud_sess:get_state(Sess, SessId)).

logging_out_also_removes_them_from_game({SessId, Sess, _Username}) ->
    Cmd = #cmd{type=logout, sessid = SessId},
    Char = emud_char:get(<<"test character">>),
    RmId = Char#char.room,
    fun () ->
        ?assertMatch(ok, emud_sess:handle_cmd(Sess, Cmd)),
        ?assertMatch([], emud_room:occupants(RmId))
    end.

a_bad_command_does_not_kill_the_session({SessId, Sess, _Username}) ->
    fun () ->
        ?assertMatch(in_game, emud_sess:get_state(Sess, SessId)),
        emud_sess:handle_cmd(Sess, #cmd{type=move, sessid=SessId, props=[{exit, none}]}),
        ?assertMatch(in_game, emud_sess:get_state(Sess, SessId))
    end.
