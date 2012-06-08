-module (emud_user_db).

-include("../include/emud.hrl").

-export([insert/1,
         save/1,
         remove/1,
         get/1]).


insert(Usr) when is_record(Usr, usr) ->
    case ?MODULE:get(Usr#usr.name) of
        no_user -> save(Usr);
        _ -> username_taken
    end.

save(Usr) when is_record(Usr, usr) ->
    {atomic, ok} = mnesia:transaction(fun () ->
            mnesia:write(Usr) 
        end),
    ok.

remove(Username) when is_binary(Username) ->
    {atomic, ok} = mnesia:transaction(fun () ->
        case mnesia:read({usr, Username}) of
            [] -> ok;
            [Usr] -> emud_char_db:remove(Usr#usr.character)
        end,
        mnesia:delete({usr, Username})
    end),
    ok.

get(Username) when is_binary(Username) ->
    {atomic, Records} = mnesia:transaction(fun () ->
        mnesia:read({usr, Username})
    end),
    case Records of
        [] -> no_user;
        [Usr] -> Usr 
    end.