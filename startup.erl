-module(startup).

-export([all/0]).

all() ->
    application:start(mnesia),
    application:start(cowboy),
    applicatoin:start(emud_db),
    application:start(emud),
    application:start(emud_http).
