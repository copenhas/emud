-module (emud_char_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_there_are_no_characters_test_() ->
    {"when there are no characters", setup, 
    fun setup/0, [
        fun get_a_char_returns_no_char/0,
        fun can_not_update_new_char/0,
        fun can_remove_non_existent_user/0
    ]}.

when_a_character_is_not_in_the_game_test_() ->
    {"when a character is not in the game", setup, 
    fun setup_char_and_world/0, 
    fun remove/1,[
        fun can_get_existing_char/0,
        fun can_update_existing_char/0,
        fun character_can_join_game/0
    ]}.

when_a_character_joins_the_game_test_() ->
    {"when a character joins the game", setup, 
    fun setup_and_join/0, 
    fun remove/1,[
        fun char_can_move_between_rooms/0
    ]}.


setup() ->
    helper:start_deps(),
    emud_room:init().

setup_char_and_world() ->
    setup(),
    Start = #room{id= <<"game start">>, exits= [{left, <<"west start">>}]},
    West = #room{id= <<"west start">>, exits= [{right, <<"west start">>}]},
    Char = #char{name= <<"test">>, room= <<"game start">>},
    mnesia:transaction(fun () ->
        mnesia:write(Start),
        mnesia:write(West),
        mnesia:write(Char)
    end),
    Char.

setup_and_join() ->
    setup_char_and_world(),
    Char = emud_char:get(<<"test">>),
    UChar = emud_char:join_game(Char),
    UChar.

remove(Char)->
    emud_char:remove(Char#char.name).


get_a_char_returns_no_char() ->
    ?assertEqual(no_character, emud_char:get(<<"nobody">>)).

can_not_update_new_char() ->
    ?assertThrow(no_character, emud_char:update(#char{name= <<"new">>})).

can_remove_non_existent_user() ->
    ?assertEqual(ok, emud_char:remove(<<"nobody">>)).

can_get_existing_char() ->
    ?assertMatch(#char{name = <<"test">>}, emud_char:get(<<"test">>)).

can_update_existing_char() ->
   ?assertEqual(ok, emud_char:update(#char{name= <<"test">>, room= <<"something">>})).

character_can_join_game() ->
    Char = emud_char:get(<<"test">>),
    ?assertEqual([], emud_room:occupants(Char#char.room)),
    emud_char:join_game(Char),
    ?assertEqual([Char#char.name], emud_room:occupants(Char#char.room)).

char_can_move_between_rooms() ->
    Char = emud_char:get(<<"test">>),
    UChar = emud_char:move(Char, left),
    ?assertEqual(<<"west start">>, UChar#char.room),
    ?assertEqual([UChar#char.name], emud_room:occupants(UChar#char.room)).
