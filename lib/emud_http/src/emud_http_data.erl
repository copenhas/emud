-module(emud_http_data).

-include_lib("emud/include/emud.hrl").

-export([decode_cmd/1, encode_msg/1]).

decode_cmd({text, Json}) when is_binary(Json) ->
    {ok, {CmdProps}} = json:decode(Json),
    case proplists:lookup(<<"command">>, CmdProps) of
        none -> throw(invalid_cmd);
        {_, Text} ->
            Tokens = binary:split(Text, <<" ">>),
            {Type, Props} = parse(Tokens),
            #cmd{ 
                type = Type,
                props = Props
            }
    end.


encode_msg(Msg) when is_record(Msg, msg) ->
    Props = [{type, atom_to_binary(Msg#msg.type, utf8)} | 
            [{text, Msg#msg.text} | Msg#msg.props]],
    JsonReady = {safeify(Props)},
    {ok, Json} = json:encode(JsonReady),
    Json.


parse([CmdType | Binary]) ->
    Type = list_to_atom(binary_to_list(CmdType)),
    parse(Binary, {Type, []}).
        
parse([], Parsed) -> Parsed; 
parse([Binary], {Type, Props}) ->
    [Key | Rest] = binary:split(Binary, <<" ">>),
    parse(Key, Rest, {Type, Props}).

parse(Key, [Binary], {Type, Props}) ->
    [Value | Rest] = binary:split(Binary, <<" ">>),
    parse(Rest, {Type, [{list_to_atom(binary_to_list(Key)), Value} | Props]}).


safeify(Props) ->
    safeify(Props, []).

safeify([], Safe) -> Safe;
safeify([{Key, Value} | More], Safe) ->
    safeify(More, [{to_binary(Key), to_binary(Value)} | Safe]).

to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_binary(Value) -> Value.
