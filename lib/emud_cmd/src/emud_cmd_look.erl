-module (emud_cmd_look).

-include_lib("emud/include/emud.hrl").

-export ([execute/4]).


execute(Sess, Char, #cmd{type=look}, Ref) ->
    Rm = emud_room:get(Char#char.room),
    Msg = #msg{type=look, 
               text=Rm#room.desc, 
               cmdref=Ref,
               source=Rm#room.id,
               props=[{room, Rm#room.name}]},
    emud_conn:send(Sess#session.conn, Msg).
