-module(emud_http_data_tests).

-include_lib("emud/include/emud.hrl").
-include_lib("eunit/include/eunit.hrl").

decode_cmd_only_accepts_json_test() ->
    ?assertError(function_clause, emud_http_data:decode_cmd(<<"not json">>)),
    ?assertError(function_clause, emud_http_data:decode_cmd({text, "just a list"})).

decode_cmd_valid_json_bad_structure_blows_test() ->
    Json = <<"{ \"something\": \"whatever\" }">>,
    ?assertThrow(invalid_cmd, emud_http_data:decode_cmd({text, Json})).

decode_cmd_can_parse_login_test() ->
    Json = <<"{ \"command\": \"login username user password pass\" }">>,
    Result = emud_http_data:decode_cmd({text, Json}),
    ?assertMatch(login, Result#cmd.type),
    ?assertMatch(<<"pass">>, ?CMDPROP(Result, password)),
    ?assertMatch(<<"user">>, ?CMDPROP(Result, username)).

encode_msg_generates_json_test() ->
    Msg = #msg {
        type = welcome,
        source = server,
        text = <<"hello world">>
    },
    emud_http_data:encode_msg(Msg).
