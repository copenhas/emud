-module (emud_char_db_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_the_db_is_empty_test_() ->
    {"when the character db is empty", setup, 
    fun setup/0, 
    fun remove/1,[
        fun get_a_char_returns_no_char/0,
        fun can_not_update_new_char/0,
        fun can_remove_non_existent_user/0,
        fun can_get_existing_char/0,
        fun can_update_existing_char/0
    ]}.


setup() ->
    helper:start_deps(),
    mnesia:dirty_write(#char{name= <<"test">>}).

remove(_) ->
    emud_char_db:remove(<<"test">>).


get_a_char_returns_no_char() ->
    ?assertEqual(no_character, emud_char_db:get(<<"nobody">>)).

can_not_update_new_char() ->
    ?assertThrow(no_character, emud_char_db:update(#char{name= <<"new">>})).

can_remove_non_existent_user() ->
    ?assertEqual(ok, emud_char_db:remove(<<"nobody">>)).

can_get_existing_char() ->
    ?assertEqual(#char{name = <<"test">>}, emud_char_db:get(<<"test">>)).

can_update_existing_char() ->
   ?assertEqual(ok, emud_char_db:update(#char{name= <<"test">>, room= <<"something">>})).