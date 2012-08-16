-module (emud_conn).

-include("../include/emud.hrl").

-export([send/2]).


send(Conn, #msg{cmdref=Ref} = Msg) when is_pid(Conn) ->
    Conn ! {emud_msg, Ref, Msg}.

