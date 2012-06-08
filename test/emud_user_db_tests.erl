-module (emud_user_db_tests).

-include("../include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

when_the_db_is_empty_test_() ->
    {"when the user db is empty", setup, 
    fun setup/0, [
        fun get_a_user_returns_no_user/0,
        fun can_add_a_new_user/0,
        fun can_remove_non_existent_user/0
    ]}.

when_the_db_has_a_user_test_() ->
    {"when the user db has a user", setup,
    fun add_user/0,
    fun remove_user/1, 
    fun (Usr) ->
        lists:map(fun (T) -> T(Usr) end, [
            fun can_add_a_character/1,
            fun can_not_insert_same_user/1,
            fun can_remove_a_user/1,
            fun removes_users_character_too/1
        ])
    end}.


setup() ->
    helper:start_deps().


add_user() ->
    Usr = #usr{name= <<"test">>, password= <<"password">>},
    ok = emud_user_db:save(Usr),
    Usr.

remove_user(Usr) ->
    ok = emud_user_db:remove(Usr#usr.name).


get_a_user_returns_no_user() ->
    ?assertEqual(no_user, emud_user_db:get(<<"test">>)).

can_add_a_new_user() ->
    ?assertEqual(ok, emud_user_db:save(#usr{name= <<"test">>, password= <<"password">>})).

can_remove_non_existent_user() ->
    ?assertEqual(ok, emud_user_db:remove(<<"nothing to see here">>)).

can_add_a_character(Usr) ->
    fun () ->
        Char = #char{name= <<"character">>},
        {ok, UUsr, UChar} = emud_user_db:add_char(Usr, Char),
        ?assertEqual(UChar, emud_char_db:get(UChar#char.name)),
        ?assertEqual(UChar#char.name, UUsr#usr.character)
    end.

can_not_insert_same_user(Usr) ->
    ?_assertEqual(username_taken, emud_user_db:insert(Usr)).

can_remove_a_user(#usr{name=Username}) ->
    ?_assertEqual(ok, emud_user_db:remove(Username)).

removes_users_character_too(_) ->
    ?_assertEqual(no_character, emud_char_db:get(<<"character">>)).