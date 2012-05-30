-module (emud_app_tests).
-include_lib("eunit/include/eunit.hrl").

when_emud_has_not_started_test_() ->
    [fun emud_sup_is_not_alive/0].

when_emud_starts_test() ->
    {setup,
    fun start_app/0,
    fun shutdown_app/1, 
    fun emud_sup_is_alive/0}.

when_emud_is_shutdown_test() ->
    {setup,
    fun () ->
        S = start_app(),
        shutdown_app(S)
    end,
    fun emud_sup_is_not_alive/0}.

start_app() ->
    application:start(emud_app).

shutdown_app(_) ->
    application:stop(emud_app).

emud_sup_is_alive() ->
    Resolved = whereis(emud_sup),
    ?assert(is_process_alive(Resolved)).

emud_sup_is_not_alive() ->
    ?assertEqual(undefined, whereis(emud_sup)).
