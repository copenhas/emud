-module (helper).

-export([start_deps/0]).

start_deps() ->
    error_logger:tty(false),
    application:load(emud_db),
    application:start(emud_db),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([usr, char, room], 5000).
    
