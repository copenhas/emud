-module (helper).

-export([start_deps/0]).

start_deps() ->
    error_logger:tty(false),
    application:load(emud_db),
    application:start(emud_db),
    emud_db:ready(5000).    
