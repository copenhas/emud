-module (emud_db).

-include("../../emud/include/emud.hrl").

-export([save/1,
         save/2,
         transaction/1,
         remove/1,
         lookup/1,
         retrieve/1,
         ready/1]).

%% ---------- Trans ---------

transaction(Trans) when is_function(Trans, 0) ->
    {atomic, Resp} = mnesia:transaction(Trans),
    Resp.
    

%% ---------- Save ---------- 
save(Rec, dirty) ->
    mnesia:dirty_write(Rec),
    ok.

save(Rec) -> 
    mnesia:write(Rec),
    ok.

%% ---------- Retrieve ---------- 

retrieve({Table, Value}) ->
    case mnesia:read({Table, Value}) of
        [] -> throw(not_found); 
        [R] ->  R
    end;

retrieve({Table, Value, dirty}) ->
    case mnesia:dirty_read({Table, Value}) of
         [] -> throw(not_found);
         [R] ->  R
    end.

%% ---------- Lookup ---------- 

lookup({Table, Value})  ->
    case mnesia:read({Table, Value}) of
        [] -> not_found;
        [R] ->  R
    end;

lookup({Table, Value, dirty}) ->
    case mnesia:dirty_read({Table, Value}) of
        [] -> not_found;
        [R] ->  R
    end;

lookup({Table, Pos, Value}) -> 
    mnesia:index_read({Table, Value, Pos}).

%% ---------- Remove ---------- 

remove({Table, Value}) ->
    mnesia:delete({Table, Value}),
    ok;

remove({Table, Value, dirty}) ->
    mnesia:dirty_delete({Table, Value}),
    ok.

%% ---------- Ready -----------

ready(Timeout) when is_integer(Timeout) ->
    mnesia:start(),
    mnesia:wait_for_tables([usr, char, room], Timeout),
    ok.

