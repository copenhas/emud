-module (emud_srv_tests).

-include_lib("eunit/include/eunit.hrl").

when_srv_is_started_test() ->
    {setup,
    fun start_srv/0,
    fun terminate_srv/1, [
        fun emud_srv_is_registered/0
    ]}.



start_srv() ->
    emud_srv:start_link().

terminate_srv(_) ->
    exit(whereis(emud_srv), normal).


emud_srv_is_registered() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_srv)).
