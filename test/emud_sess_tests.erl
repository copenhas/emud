-module (emud_sess_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_session_first_started_test_() ->
    {setup,
    fun start_session/0,
    fun stop_session/1, 
    fun (SessInfo) ->
       lists:map(fun (T) -> T(SessInfo) end, [
         fun unsupported_cmd_returns_error/1
       ])
    end}.


start_session() ->
    SessId = emud_session_db:generate_session_id(),
    {ok, Sess} = emud_sess:start_link(SessId, self()),
    {SessId, Sess}.

stop_session({_SessId, Sess}) ->
    exit(Sess, normal).


% what if bad request?
unsupported_cmd_returns_error({SessId, Sess}) ->
    Cmd = #cmd{type=unsupported_cmd, sessid = SessId},
    ?_assertMatch({error, invalid_cmd}, emud_sess:handle_cmd(Sess, Cmd)).

% only allows a login or new_user cmd

% ok, then what once logged in?
% new_user -> new_character, login -> pick_character?

% need user record and storage, emud_srv could keep track of active/logged in?