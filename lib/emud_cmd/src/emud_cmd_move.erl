-module (emud_cmd_move).

-include_lib("emud/include/emud.hrl").

-export ([execute/4]).


execute(Sess, Char, Cmd, Ref) ->
    emud_char:move(Char, ?CMDPROP(Cmd, exit)),
    emud_cmd_look:execute(Sess, Char, Cmd, Ref).
