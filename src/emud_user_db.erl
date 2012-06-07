-module (emud_user_db).

-include("../include/emud.hrl").

-export([save/1,
         remove/1,
         get/1]).

save(Usr) when is_record(Usr, usr) ->
    {atomic, ok} = mnesia:transaction(fun () ->
            mnesia:write(Usr) 
        end),
    ok.

remove(Username) when is_binary(Username) ->
    {atomic, ok} = mnesia:transaction(fun () ->
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