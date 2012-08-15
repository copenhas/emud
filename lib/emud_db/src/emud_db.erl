-module (emud_db).

-include("../include/emud.hrl").

-export([save/1,
         remove/1,
         lookup/1,
         retrieve/1]).

%% ---------- Save ---------- 

%% Case where multiple saves will be in one transaction.
%% found a case in source.
save(Rec) when is_list(Rec) -> 
    {atomic, ok} = mnesia:transaction(fun () ->
            [Head|_] = [mnesia:write(R) || R <- Rec],
            Head
        end),
    ok;

save(Rec) -> 
    {atomic, ok} = mnesia:transaction(fun () ->
            mnesia:write(Rec) 
        end),
    ok.

%% ---------- Retrieve ---------- 

retrieve(Crit) -> 
    NotFoundResponse = [{char, no_character}, {room, no_room},{usr, no_user}],
    case Crit of
        {Table, Value} ->
            {atomic, Records} = mnesia:transaction(fun () ->
                mnesia:read({Table, Value})
            end),
            case Records of
                [] -> 
                    {_, Resp} = lists:keyfind(Table,1,NotFoundResponse),
                    throw(Resp);
                [R] ->  R
            end;
        {Table, Value, dirty} ->
            Records = mnesia:dirty_read({Table, Value}),
            case Records of
                [] -> 
                    {_, Resp} = lists:keyfind(Table,1,NotFoundResponse),
                    throw(Resp);
                [R] ->  R
            end;
        {Table, Index, Value} -> 
            throw(not_impl);
        _ -> throw(op_notrecognized)
    end. 

%% ---------- Lookup ---------- 

lookup(Crit) ->
    NotFoundResponse = [{char, no_character}, {room, no_room},{usr, no_user}],
    case Crit of
        {Table, Value} ->
            {atomic, Records} = mnesia:transaction(fun () ->
                mnesia:read({Table, Value})
            end),
            case Records of
                [] -> 
                    {_, Resp} = lists:keyfind(Table,1,NotFoundResponse),
                    Resp;
                [R] ->  R
            end;
        {Table, Value, dirty} ->
            Records = mnesia:dirty_read({Table, Value}),
            case Records of
                [] -> 
                    {_, Resp} = lists:keyfind(Table,1,NotFoundResponse),
                    throw(Resp);
                [R] ->  R
            end;
        {Table, Index, Value} -> 
            throw(not_impl);
         _ -> throw(op_notrecognized)
    end. 

%% ---------- Remove ---------- 

remove(Crit) ->
    case Crit of
        {Table, Index, Value} -> 
            throw(not_impl);
        {Table, Value} ->
            {atomic, ok} = mnesia:transaction(fun () ->
                mnesia:delete({Table, Value})
            end),
            ok;
        _ -> throw(op_notrecognized)
    end.
