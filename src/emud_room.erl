-module (emud_room).

-include("../include/emud.hrl").

-export([init/0]).

-export([get/1,
         occupants/1,
         enter/2,
         leave/2]).

init() ->
    case ets:info(emud_room2Chars) of
        undefined ->
            Opts = [named_table, public, bag, 
                    {read_concurrency, true}, 
                    {write_concurrency, true}],
            ets:new(emud_room2Chars, Opts);
        _ -> emud_room2Chars
    end.

get(RmId) when is_binary(RmId) ->
    {atomic, Rm} = mnesia:transaction(fun () ->
        [Rm] = mnesia:read({room, RmId}),
        Rm
    end),
    Rm.

occupants(RmId) when is_binary(RmId) ->
    [CharName || {_RmId, CharName} <- ets:lookup(emud_room2Chars, RmId)].

enter(RmId, Char) when is_binary(RmId), is_record(Char, char) ->
    true = ets:insert(emud_room2Chars, {RmId, Char#char.name}).

leave(RmId, Char) when is_binary(RmId), is_record(Char, char) ->
    true = ets:delete_object(emud_room2Chars, {RmId, Char#char.name}).
