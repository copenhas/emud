-module(emud_cmd_new_user).

-include_lib("emud/include/emud.hrl").

-export ([execute/2]).

execute(#cmdctxt{sessid=Id, sendmsg=SendMsg}, Cmd) ->
    Usr = #usr{name = ?CMDPROP(Cmd, username), 
               password = ?CMDPROP(Cmd, password)},
    case emud_user:insert(Usr) of
        {error, Reason} ->
            SendMsg(#msg{type=Reason});
        Ok ->
            SendMsg(#msg{
                type=success, 
                source=server,
                text= <<"New user was created successfully\nPlease login.">>
            })
    end.
