-module(emud_cmd).

-include_lib("emud/include/emud.hrl").

%% API
-export([run/2]).
-export([spawn_link/2]).

%% Callbacks
-export ([init/2]).


% context data for the command
-record(state, {
        ctxt,
        cmd,
        dbg
    }).

%%%===================================================================
%%% API
%%%===================================================================

run(Ctxt, Cmd) when is_record(Ctxt, cmdctxt), is_record(Cmd, cmd) ->
    {ok, Pid} = emud_cmd_sup:start_cmd(Ctxt, Cmd),
    Pid.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
spawn_link(Ctxt, Cmd) ->
    {ok, proc_lib:spawn_link(emud_cmd, init, [Ctxt, Cmd])}.


%%%===================================================================
%%% Callbacks
%%%===================================================================

init(Ctxt, Cmd) ->
    Context = #state{
                   ctxt=Ctxt,
                   cmd=Cmd,
                   dbg=sys:debug_options([])},

    execute(Ctxt, Cmd).


%%%===================================================================
%%% Internal functions
%%%===================================================================

execute(Ctxt, Cmd) ->
    Mod = look_up_cmd(Cmd#cmd.type),
    Mod:execute(Ctxt, Cmd).


look_up_cmd(CmdType) ->
    CmdName = atom_to_list(CmdType),
    ModName = "emud_cmd_" ++ CmdName,
    list_to_existing_atom(ModName).
