-module (emud_db_install).
-include_lib("emud/include/emud.hrl").

-export([install/0,
         reset/0]).

install() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(usr, [{attributes, record_info(fields, usr)}]),
    mnesia:create_table(char, [{attributes, record_info(fields, char)}]),
    mnesia:create_table(room, [{attributes, record_info(fields, room)}]),
    ok.

reset() ->
    mnesia:clear_table(usr),
    mnesia:clear_table(char),
    mnesia:clear_table(room).

