-module (emud_char_db_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_the_db_is_empty_test_() ->
    {"when the character db is empty", setup, 
    fun setup/0, 
    fun remove/1,[
        fun get_a_char_returns_no_char/0,
        fun can_add_a_new_char/0,
        fun can_remove_non_existent_user/0,
        fun can_get_existing_char/0
    ]}.


setup() ->
    helper:start_deps().

remove(_) ->
    emud_char_db:remove(<<"test">>).


get_a_char_returns_no_char() ->
    ?assertEqual(no_character, emud_char_db:get(<<"nobody">>)).

can_add_a_new_char() ->
    ?assertEqual(ok, emud_char_db:save(#char{name= <<"test">>})).

can_remove_non_existent_user() ->
    ?assertEqual(ok, emud_char_db:remove(<<"nobody">>)).

can_get_existing_char() ->
    ?assertEqual(#char{name = <<"test">>}, emud_char_db:get(<<"test">>)).
