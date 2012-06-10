-module (emud_char).

-include("../include/emud.hrl").

-export([get/1,
         update/1,
         remove/1,
         join_game/1,
         move/2]).

get(CharName) when is_binary(CharName) ->
    {atomic, Records} = mnesia:transaction(fun () ->
        mnesia:read({char, CharName})
    end),
    case Records of
        [] -> no_character;
        [Char] -> Char 
    end.

update(Char) when is_record(Char, char) ->
    case mnesia:dirty_read({char, Char#char.name}) of
        [] -> throw(no_character);
        _ -> ok
    end,
    {atomic, ok} = mnesia:transaction(fun () ->
            mnesia:write(Char) 
        end),
    ok.

remove(undefined) ->
    ok;
remove(CharName) when is_binary(CharName) ->
    {atomic, ok} = mnesia:transaction(fun () ->
        mnesia:delete({char, CharName})
    end),
    ok.

join_game(Char) when is_record(Char, char) ->
    emud_room:enter(Char#char.room, Char),
    Char.

move(Char, Exit) ->
    Rm = emud_room:get(Char#char.room),
    NextRmId = ?EXITTO(Rm, Exit),
    emud_room:leave(Rm#room.id, Char),
    UChar = Char#char{room=NextRmId},
    ok = emud_char:update(UChar),
    emud_room:enter(NextRmId, UChar),
    UChar.
