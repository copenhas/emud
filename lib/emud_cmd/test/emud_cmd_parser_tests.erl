-module(emud_cmd_parser_tests).

-export([test/1]).

-include_lib("eunit/include/eunit.hrl").

test(Bin) ->
    emud_cmd_parser:parse(Bin).

empty_binary_does_not_kill_the_parser_test() ->
    ?assertMatch(invalid_input, test(<<"">>)).

incomplete_command_does_not_kill_the_parser_test() ->
    ?assertMatch(invalid_cmd, test(<<"login">>)).

a_non_binary_does_not_kill_the_parser_test_() ->
    [?_assertMatch(invalid_input, test("login")),
     ?_assertMatch(invalid_input, test(login)),
     ?_assertMatch(invalid_input, test(42))].

can_parse_a_login_command_test() ->
    ?assertMatch({cmd,login,undefined,[{user,<<"user">>},{pass,<<"pass">>}]},
                 test(<<"login user pass">>)).

can_parse_a_look_command_test_() ->
    [?_assertMatch({cmd,look, undefined, []}, test(<<"look">>)),
     ?_assertMatch({cmd,look, undefined, [{target, <<"orc">>}]},
                 test(<<"look orc">>)),
     ?_assertMatch({cmd,look, undefined, [{target, <<"orc">>}]},
                   test(<<"look at orc">>))].

can_parse_a_move_command_test_() ->
    [?_assertMatch({cmd,move,undefined,[{exit, <<"north">>}]},
                   test(<<"move north">>)),
     ?_assertMatch({cmd,move,undefined,[{exit, <<"w">>}]},
                   test(<<"w">>))].
