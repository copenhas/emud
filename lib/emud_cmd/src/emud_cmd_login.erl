-module(emud_cmd_login).

-include_lib("emud/include/emud.hrl").

-export([execute/2]).

execute(Ctxt, Cmd) ->
    #cmdctxt{
        sessid=Id, 
        sendmsg=SendMsg, 
        progress=Progress} = Ctxt,

    Response = emud_srv:login(Id,
                              ?CMDPROP(Cmd, username), 
                              ?CMDPROP(Cmd, password)),
    case Response of
        {ok, _} -> 
            SendMsg(#msg{
                type=success, 
                source=server,
                text= <<"Pick an existing character or create a new one">>
            }),
            Progress();
        {error, Reason} -> 
            SendMsg(#msg{
                type=Reason, 
                source=server,
                text= <<"Login failed">>
            })
    end.
