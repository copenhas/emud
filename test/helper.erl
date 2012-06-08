-module (helper).

-export([start_deps/0]).

start_deps() ->
    error_logger:tty(false),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([usr, char], 5000).