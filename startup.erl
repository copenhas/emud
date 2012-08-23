-module(startup).

-export([all/0]).

all() ->
    application:start(mnesia),
    application:start(cowboy),
    application:start(emud),
    application:start(emud_http).
