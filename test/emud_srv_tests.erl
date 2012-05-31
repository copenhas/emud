-module (emud_srv_tests).

-include_lib("eunit/include/eunit.hrl").

when_srv_is_started_test_() ->
    {"when the emud server is started", setup,
    fun start_srv/0,
    fun terminate_srv/1, [
        fun emud_srv_is_registered/0,
        fun a_user_can_connect/0,
        fun user_can_retrieve_their_session/0
    ]}.


start_srv() ->
    emud_srv:start_link().

terminate_srv(_) ->
    emud_srv:terminate().


emud_srv_is_registered() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_srv)).

a_user_can_connect() ->
    ?assertMatch({ok, _SessionId}, emud_srv:connect()).

user_connects() ->
    ?assertMatch({ok, _SessionId}, emud_srv:connect()).

user_can_retrieve_their_session() ->
    {ok, Id} = emud_srv:connect(),
    ?_assertMatch(Id, emud_srv:get_session()).
