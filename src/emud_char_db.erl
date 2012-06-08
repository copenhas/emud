-module (emud_char_db).

-include("../include/emud.hrl").

-export([get/1,
         save/1,
         remove/1]).

get(CharName) when is_binary(CharName) ->
    {atomic, Records} = mnesia:transaction(fun () ->
        mnesia:read({char, CharName})
    end),
    case Records of
        [] -> no_character;
        [Char] -> Char 
    end.

save(Char) when is_record(Char, char) ->
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