-module (emud_sup_tests).
-include_lib("eunit/include/eunit.hrl").

when_emud_sup_is_started_test_()->
    {setup,
    fun start_sup/0,
    fun terminate_sup/1, [
        fun emud_sup_is_alive/0,
        fun emud_game_srv_is_alive/0
    ]}.

start_sup() ->
    emud_sup:start_link().

terminate_sup(_) ->
    exit(whereis(emud_sup), normal).


emud_sup_is_alive() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_sup)).

emud_game_srv_is_alive() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_game_srv)).
