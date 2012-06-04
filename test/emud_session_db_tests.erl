-module (emud_session_db_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").


when_a_session_is_added_test_() ->
    {"when a session is added", setup,
    fun init_session/0,
    fun cleanup_db/1, 
    fun (Session) ->
        lists:map(fun (F) -> F(Session) end, [
            fun you_can_look_up_by_session_id_/1,
            fun you_can_look_up_by_pid_/1
        ])
    end}.

when_empty_db_test_() ->
    {"when the db is empty", setup,
    fun init_db/0,
    fun cleanup_db/1, [
        fun looking_up_a_session_gives_no_session/0,
        fun looking_up_by_pid_gives_no_session/0
    ]}.


init_db() ->
    emud_session_db:init().

init_session() ->
    init_db(),
    SessionId = emud_session_db:generate_session_id(),
    emud_session_db:create_session(SessionId, self(), self()).

cleanup_db(_) ->
    emud_session_db:cleanup().


you_can_look_up_by_session_id_(Sess) ->
    ?_assertMatch(Sess, emud_session_db:get_session(Sess#session.id)).

you_can_look_up_by_pid_(Sess) ->
    ?_assertMatch(Sess, emud_session_db:get_session(Sess#session.conn)).

looking_up_by_pid_gives_no_session() ->
    ?assertMatch(no_session, emud_session_db:get_session(self())).

looking_up_a_session_gives_no_session() ->
    MadeUp = emud_session_db:generate_session_id(),
    ?assertMatch(no_session, emud_session_db:get_session(MadeUp)).
