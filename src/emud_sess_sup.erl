-module(emud_sess_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_sess/2,
         stop_sess/1]).

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

start_sess(SessId, Conn) when is_pid(Conn) ->
    supervisor:start_child(?SERVER, [SessId, Conn]).

stop_sess(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

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
        Shutdown = 500,
        Type = worker,

        AChild = {emud_sess, {emud_sess, start_link, []},
                          Restart, Shutdown, Type, [emud_sess]},

        {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

