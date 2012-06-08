-module (emud_room).

-include("../include/emud.hrl").

-export([retrieve/1]).


retrieve(RmId) when is_binary(RmId) ->
    {atomic, Rm} = mnesia:transaction(fun () ->
        [Rm] = mnesia:read({room, RmId}),
        Rm
    end),
    Rm.