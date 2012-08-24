-module (emud_char).

-include("../include/emud.hrl").

-export([get/1,
         update/1,
         remove/1,
         join_game/1,
         leave_game/1,
         move/2]).

get(CharName) when is_binary(CharName) ->
    emud_db:transaction(fun () ->
                emud_db:lookup({char, CharName})
            end).

update(Char) when is_record(Char, char) ->
    %%% |-
    %%% Comment: Should this check and throw or call
    %%%          emud_retrieve, which will fail if no
    %%%          is found?
    case emud_db:lookup({char, Char#char.name, dirty}) of
        not_found -> throw(not_found);
        _ -> ok
    end,
    %%% -|
    emud_db:transaction(fun () -> emud_db:save(Char) end).

remove(undefined) ->
    ok;

remove(CharName) when is_binary(CharName) ->
    emud_db:transaction(fun () -> emud_db:remove({char, CharName}) end).
    
join_game(Char) ->
    emud_room:enter(Char#char.room, Char),
    Char.

leave_game(Char) ->
    emud_room:leave(Char#char.room, Char),
    Char.

move(Char, Exit) ->
    Rm = emud_room:get(Char#char.room),
    NextRmId = ?EXITTO(Rm, Exit),
    emud_room:leave(Rm#room.id, Char),
    UChar = Char#char{room=NextRmId},
    ok = emud_char:update(UChar),
    emud_room:enter(NextRmId, UChar),
    UChar.
