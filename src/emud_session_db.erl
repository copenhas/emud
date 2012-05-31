-module (emud_session_db).

-include("../include/emud.hrl").

-export([init/0,
         cleanup/0,
         generate_session_id/0]).

-export([create_session/1,
         get_session/1]).


init() ->
    ets:new(emud_sessions, [named_table, {keypos, 2}, {read_concurrency, true}]),
    ets:new(emud_pid2session, [named_table, {read_concurrency, true}]).

cleanup() ->
    ets:delete(emud_sessions),
    ets:delete(emud_pid2session).

generate_session_id() ->
    {crypto:rand_bytes(6), now()}.

create_session(Conn) when is_pid(Conn) ->
    SessionId = generate_session_id(),
    Session = #session{ id = SessionId, conn = Conn },
    ets:insert(emud_sessions, Session),
    ets:insert(emud_pid2session, {Conn, SessionId}),
    Session.

get_session(Conn) when is_pid(Conn) ->
    case ets:lookup(emud_pid2session, Conn) of
        [] -> no_session;
        [{Conn, SessionId}] -> get_session(SessionId)
    end;
get_session(SessionId) when is_tuple(SessionId) ->
    case ets:lookup(emud_sessions, SessionId) of
        [] -> no_session;
        [Session] -> Session
    end.
