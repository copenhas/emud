-module(emud_cmd).

-include_lib("emud/include/emud.hrl").

%% API
-export([run/3]).
-export([spawn_link/3]).

%% Callbacks
-export ([init/3]).


% context data for the command
-record(ctxt, {
        sessid,
        ref,
        cmd,
        dbg
    }).

%%%===================================================================
%%% API
%%%===================================================================

run(SessId, Ref, Cmd) ->
    emud_cmd_sup:start_cmd(SessId, Ref, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
spawn_link(SessId, Ref, Cmd) ->
    {ok, proc_lib:spawn_link(emud_cmd, init, [SessId, Ref, Cmd])}.


%%%===================================================================
%%% Callbacks
%%%===================================================================

init(SessId, Ref, Cmd) ->
    Context = #ctxt{sessid=SessId, 
                   ref=Ref, 
                   cmd=Cmd,
                   dbg=sys:debug_options([])},

    run(Context).


%%%===================================================================
%%% Internal functions
%%%===================================================================

run(#ctxt{sessid=SessId, cmd=Cmd, ref=Ref}) ->
    Mod = look_up_cmd(Cmd#cmd.type),
    Sess = emud_session_db:get_session(SessId),
    Char = emud_char:get(Sess#session.character),

    Mod:execute(Sess, Char, Cmd, Ref).


look_up_cmd(CmdType) ->
    CmdName = atom_to_list(CmdType),
    ModName = "emud_cmd_" ++ CmdName,
    list_to_atom(ModName).
