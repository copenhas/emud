-module(emud_srv).

-include("../include/emud.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
         terminate/0,
         connect/0,
         get_session/0,
         login/3,
         logout/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

terminate() ->
    gen_server:cast(?SERVER, terminate).

connect() ->
    gen_server:call(?SERVER, connect).

get_session() ->
    gen_server:call(?SERVER, get_session).

login(SessId, Username, Password) when is_binary(Username), is_binary(Password) ->
    gen_server:call(?SERVER, {login, SessId, Username, Password}).

logout(SessId) ->
    gen_server:call(?SERVER, {logout, SessId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    emud_session_db:init(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(connect, {Pid, _Tag}, State) ->
    SessId = emud_session_db:generate_session_id(),
    {ok, Sess} = emud_sess_sup:start_sess(SessId, Pid),
    Session = emud_session_db:create_session(SessId, Pid, Sess),
    Reply = {ok, Session#session.id, Sess},
    emud_conn:send(Pid, #msg{
        type= welcome, 
        source= server, 
        text= <<"Welcome to EMUD\n Login or create new user: ">>
    }),
    {reply, Reply, State};

handle_call(get_session, {Pid, _Tag}, State) ->
    case emud_session_db:get_session(Pid) of
        no_session -> {reply, no_session, State};
        Session -> {reply, {ok, Session#session.id}, State}
    end;

handle_call({login, SessId, Username, Password}, {Pid, _Tag}, State) ->
    case emud_session_db:get_session(SessId) of
        no_session -> 
            {reply, {error, unauthorized}, State};
        Session ->
            case emud_user:get(Username) of
                no_user -> 
                    {reply, {error, invalid_creds}, State};
                #usr{name=Username, password=Password} = Usr ->
                    LoggedIn = Session#session{user = Usr},
                    emud_session_db:update_session(LoggedIn),
                    {reply, {ok, Username}, State};
                _ ->
                    {reply, {error, invalid_creds}, State}
            end;
        _ ->
            {reply, {error, unauthorized}, State}
    end;

handle_call({logout, SessId}, _From, State) ->
    Reply = emud_session_db:remove_session(SessId),
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(terminate, State) ->
        {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    emud_session_db:cleanup(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


