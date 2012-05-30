-module (emud_sup_tests).
-include_lib("eunit/include/eunit.hrl").

when_emud_sup_is_started_test_()->
    {setup,
    fun start_sup/0,
    fun terminate_sup/1, [
        fun emud_sup_is_alive/0,
        fun emud_game_srv_is_alive/0,
        fun emud_game_srv_gets_restarted_on_error/0,
        fun emud_game_srv_never_dies/0
    ]}.


start_sup() ->
    emud_sup:start_link().

terminate_sup(_) ->
    exit(whereis(emud_sup), normal).


emud_sup_is_alive() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_sup)).

emud_game_srv_is_alive() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_srv)).

emud_game_srv_gets_restarted_on_error() ->
    exit(whereis(emud_srv), test_crash),
    emud_game_srv_is_alive(). 

emud_game_srv_never_dies() ->
    emud_game_srv_never_dies(10).

emud_game_srv_never_dies(0) -> ok;
emud_game_srv_never_dies(Count) ->
    emud_game_srv_gets_restarted_on_error(),
    emud_game_srv_never_dies(Count - 1).
