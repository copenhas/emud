-module(emud_sess).

-behaviour(gen_fsm).
-include("../include/emud.hrl").

%% API
-export([start_link/2,
         handle_cmd/2,
         get_state/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([auth/3,
         pick_character/3,
         new_character/3,
         in_game/3]).

-define(SERVER, ?MODULE).

-define(HANDLES_INVALID(StateName), StateName(_Cmd, _From, State) -> {reply, {error, invalid_cmd}, StateName, State}).

-record(state, { id, conn}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SessId, Conn) ->
    gen_fsm:start_link(?MODULE, [SessId, Conn], []).

handle_cmd(Sess, #cmd{type=logout} = Cmd) when is_pid(Sess) ->
    gen_fsm:sync_send_all_state_event(Sess, Cmd);

handle_cmd(Sess, Cmd) when is_pid(Sess), is_record(Cmd, cmd) ->
    gen_fsm:sync_send_event(Sess, Cmd).

get_state(Sess, SessId) when is_pid(Sess) ->
    gen_fsm:sync_send_all_state_event(Sess, {get_state, SessId}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([SessId, Conn]) ->
    {ok, auth, #state{id = SessId, conn = Conn}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%state_name(_Event, State) ->
%%        {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
auth(Cmd = #cmd{type=new_user, sessid=Id}, _From, #state{id=Id} = State) ->
    Usr = #usr{name = ?CMDPROP(Cmd, username), 
               password = ?CMDPROP(Cmd, password)},
    Reply = emud_srv:new_user(Id, Usr),
    {reply, Reply, auth, State};

auth(Cmd = #cmd{type=login, sessid=Id}, _From, #state{id=Id} = State) ->
    Response = emud_srv:login(Id,
                           ?CMDPROP(Cmd, username), 
                           ?CMDPROP(Cmd, password)),
    case Response of
        {ok, _} -> {reply, Response, pick_character, State};
        _ -> {reply, Response, auth, State}
    end;

?HANDLES_INVALID(auth).


pick_character(_Cmd = #cmd{type=new_character, sessid=Id}, _From, #state{id=Id} = State) ->
    {reply, ok, new_character, State}; 

pick_character(Cmd = #cmd{type=pick_character, sessid=Id}, _From, #state{id=Id} = State) ->
    Sess = emud_session_db:get_session(Id),
    Usr = Sess#session.user,
    Char = ?CMDPROP(Cmd, character),
    case {Usr#usr.character, emud_char_db:get(Char)} of
        {Char, #char{}} ->
            JoinedGame = Sess#session{character = Char},
            emud_session_db:update_session(JoinedGame),
            {reply, {ok, Char}, in_game, State};
        _ ->
            {reply, {error, no_character}, pick_character, State}
    end;

?HANDLES_INVALID(pick_character).


new_character(#cmd{type=character_name, sessid = Id} = Cmd, _From, #state{id=Id} = State) ->
    Char = #char{name= ?CMDPROP(Cmd, name), room= <<"game start">>},
    Sess = emud_session_db:get_session(Id),
    Usr = Sess#session.user,
    UpdatedUsr = Usr#usr{character = Char#char.name},
    UpdatedSess = Sess#session{user = UpdatedUsr},
    emud_user_db:save(UpdatedUsr),
    emud_char_db:save(Char),
    emud_session_db:update_session(UpdatedSess),
    {reply, {ok, Char#char.name}, pick_character, State};

?HANDLES_INVALID(new_character).


in_game(#cmd{type=look, sessid=Id}, {Pid, _Tag}, #state{id=Id} = State) ->
    emud_conn:send(Pid, #msg{type=look}),
    {reply, ok, in_game, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(none, StateName, State) ->
        {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%   
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(#cmd{type=logout} = _Cmd, _From, _StateName, State) ->
    Reply = emud_srv:logout(State#state.id),
    {stop, normal, Reply, State};

handle_sync_event({get_state, _SessId}, _From, StateName, State) ->
    {reply, StateName, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(none, StateName, State) ->
        {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
        {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

