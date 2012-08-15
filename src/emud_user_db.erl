-module (emud_user_db).

-include("../include/emud.hrl").

-export([insert/1,
         save/1,
         remove/1,
         get/1,
         add_char/2]).


insert(Usr) when is_record(Usr, usr) ->
    case ?MODULE:get(Usr#usr.name) of
        no_user -> save(Usr);
        _ -> username_taken
    end.

save(Usr) when is_record(Usr, usr) ->
    emud_db:save(Usr).
    
remove(Username) when is_binary(Username) ->
    case emud_db:lookup({usr, Username}) of
        no_user -> ok;
        Usr -> 
            emud_char:remove(Usr#usr.character)
    end,
    ok.

get(Username) when is_binary(Username) ->
    emud_db:lookup({usr, Username}).
    
add_char(Usr, #char{name=CharName} = Char) when is_record(Usr, usr) ->
    UUsr = case Usr#usr.character of
        CharName -> Usr;
        _ -> 
            ok = emud_char:remove(Usr#usr.character),
            Usr#usr{character = Char#char.name}
    end,
    emud_db:save([UUsr,Char]),
    {ok, UUsr, Char}.
