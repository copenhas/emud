-module (emud_app_tests).
-include_lib("eunit/include/eunit.hrl").

when_emud_has_not_started_test_() ->
    {"when the application has not been started", [fun emud_sup_is_not_alive/0]}.

when_emud_starts_test_() ->
    {"when the applicaiton has been started",setup,
    fun start_app/0,
    fun shutdown_app/1, [
        fun emud_sup_is_alive/0
    ]}.

when_emud_is_shutdown_test_() ->
    {"when the applicatio has been stopped", setup,
    fun () ->
        S = start_app(),
        shutdown_app(S)
    end,
    fun emud_sup_is_not_alive/0}.

start_app() ->
    helper:start_deps().

shutdown_app(_) ->
    application:stop(emud).

emud_sup_is_alive() ->
    ?assertMatch(Pid when is_pid(Pid), whereis(emud_sup)).

emud_sup_is_not_alive() ->
    ?assertEqual(undefined, whereis(emud_sup)).
