-module (emud_user).

-include("../include/emud.hrl").

-export([insert/1,
         save/1,
         remove/1,
         get/1,
         add_char/2]).


insert(Usr) when is_record(Usr, usr) ->
    case ?MODULE:get(Usr#usr.name) of
        not_found -> save(Usr);
        _ -> username_taken
    end.

save(Usr) when is_record(Usr, usr) ->
    emud_db:transaction(fun () -> emud_db:save(Usr) end).
    
remove(Username) when is_binary(Username) ->
    case emud_db:transaction(fun () -> emud_db:lookup({usr, Username}) end) of
        not_found -> ok;
        Usr -> 
            emud_db:transaction(fun () -> emud_char:remove(Usr#usr.character) end)
    end,
    ok.

get(Username) when is_binary(Username) ->
    emud_db:transaction(fun () -> emud_db:lookup({usr, Username}) end).
    
add_char(Usr, #char{name=CharName} = Char) when is_record(Usr, usr) ->
    UUsr = case Usr#usr.character of
        CharName -> Usr;
        _ -> 
            ok = emud_db:transaction(fun () -> emud_char:remove(Usr#usr.character) end),
            Usr#usr{character = Char#char.name}
    end,
    emud_db:transaction(fun () -> emud_db:save(UUsr),
                                  emud_db:save(Char) end),
    {ok, UUsr, Char}.
