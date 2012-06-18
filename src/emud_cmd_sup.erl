-module(emud_cmd_sup).

-behaviour(supervisor).
-include("../include/emud.hrl").

%% API
-export([start_link/0,
         start_cmd/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_cmd(SessId, Ref, Cmd) when is_reference(Ref), is_record(Cmd, cmd) ->
    supervisor:start_child(?SERVER, [SessId, Ref, Cmd]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        RestartStrategy = simple_one_for_one,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = temporary,
        Shutdown = brutal_kill,
        Type = worker,

        AChild = {emud_cmd, {emud_cmd, spawn_link, []},
                          Restart, Shutdown, Type, [emud_cmd]},

        {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

