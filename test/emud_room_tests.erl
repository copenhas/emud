-module (emud_room_tests).  

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").


when_there_are_rooms_in_the_game_test_() ->
    {"when there are rooms in the game", setup,
    fun add_room/0,
    fun remove_room/1,
    fun (RmId) ->
        lists:map(fun (T) -> T(RmId) end, [
            fun can_retrieve_a_room_by_id/1,
            fun room_keeps_track_of_its_occupants/1
        ])
    end}.


add_room() ->
    emud_room:init(),
    Id = crypto:rand_bytes(8),
    ok = mnesia:dirty_write(#room{id = Id}),
    Id.

remove_room(Id) ->
    ok = mnesia:dirty_delete({room, Id}).


can_retrieve_a_room_by_id(Id) ->
    ?_assertMatch(#room{id = Id}, emud_room:get(Id)).

room_keeps_track_of_its_occupants(Id) ->
    fun () ->
        Char = #char{name= <<"test">>},
        ?assertMatch([], emud_room:occupants(Id)),
        emud_room:enter(Id, Char),
        ?assertMatch([<<"test">>], emud_room:occupants(Id)),
        emud_room:leave(Id, Char),
        ?assertMatch([], emud_room:occupants(Id))
    end.