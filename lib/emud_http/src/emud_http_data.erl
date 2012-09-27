-module(emud_http_data).

-include_lib("emud/include/emud.hrl").

-export([decode_cmd/1, encode_msg/1]).

decode_cmd({text, Json}) when is_binary(Json) ->
    {ok, {CmdProps}} = json:decode(Json),
    case proplists:lookup(<<"command">>, CmdProps) of
        none -> throw(invalid_cmd);
        {_, Text} ->
            emud_cmd_parser:parse(Text)
    end.


encode_msg(Msg) when is_record(Msg, msg) ->
    Fields = lists:map(fun to_json_safe/1, record_info(fields, msg)),
    Values = lists:map(fun to_json_safe/1, tl(tuple_to_list(Msg))),
    Props = lists:zip(Fields, Values),
    JsonReady = {safeify(Props)},
    {ok, Json} = json:encode(JsonReady),
    Json.


safeify(Props) ->
    safeify(Props, []).

safeify([], Safe) -> Safe;
safeify([{Key, Value} | More], Safe) ->
    safeify(More, [{to_json_safe(Key), to_json_safe(Value)} | Safe]).


to_json_safe(undefined) -> undefined;
to_json_safe(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_json_safe(Value) when is_list(Value) -> lists:map(fun to_json_safe/1, Value);
to_json_safe(Value) -> Value.
