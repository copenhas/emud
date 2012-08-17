-module (emud_srv_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_srv_is_started_test_() ->
    {"when the emud server is started", setup,
    fun start_srv/0,
    fun terminate_srv/1, [
        fun emud_srv_is_registered/0,
        fun a_user_can_connect/0,
        fun random_process_can_not_create_users/0
    ]}.

when_user_connected_test_() ->
    {"when a user connects to emud server", setup,
    fun setup_session/0,
    fun terminate_srv/1,
    fun (SessInfo) ->
        lists:map(fun (T) -> T(SessInfo) end, [
            fun connected_user_can_not_create_new_account_directly/1
        ])
    end}.


start_srv() ->
    helper:start_deps(),
    application:start(emud).

setup_session() ->
    start_srv(),
    {ok, Id, Sess} = emud_srv:connect(),
    {Id, Sess}.

terminate_srv(_) ->
    application:stop(emud).


emud_srv_is_registered() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_srv)).

a_user_can_connect() ->
    ?assertMatch({ok, _SessionId, _Sess}, emud_srv:connect()).

random_process_can_not_create_users() ->
    SessId = emud_session_db:generate_session_id(),
    User = #usr{name = <<"test">>, password = <<"password">>},
    ?assertMatch({error, unauthorized}, emud_srv:new_user(SessId, User)).

connected_user_can_not_create_new_account_directly({Id, _Sess}) ->
    User = #usr{name = <<"test">>, password = <<"password">>},
    ?_assertMatch({error, unauthorized}, emud_srv:new_user(Id, User)).