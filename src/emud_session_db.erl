-module (emud_session_db).

-include("../include/emud.hrl").

-export([init/0,
         cleanup/0]).

-export([generate_session_id/0,
         create_session/3,
         get_session/1,
         remove_session/1]).


init() ->
    ets:new(emud_sessions, [named_table, {keypos, 2}, {read_concurrency, true}]),
    ets:new(emud_conn2session, [named_table, {read_concurrency, true}]).

cleanup() ->
    ets:delete(emud_sessions),
    ets:delete(emud_conn2session).

generate_session_id() ->
    {crypto:rand_bytes(6), now()}.

create_session(SessId, Conn, Sess) when is_tuple(SessId), is_pid(Conn), is_pid(Sess) ->
    Session = #session{ id = SessId, conn = Conn, sess = Sess },
    ets:insert(emud_sessions, Session),
    ets:insert(emud_conn2session, {Conn, SessId}),
    Session.

get_session(Conn) when is_pid(Conn) ->
    case ets:lookup(emud_conn2session, Conn) of
        [] -> no_session;
        [{Conn, SessionId}] -> get_session(SessionId)
    end;
get_session(SessionId) when is_tuple(SessionId) ->
    case ets:lookup(emud_sessions, SessionId) of
        [] -> no_session;
        [Session] -> Session
    end.

remove_session(SessionId) when is_tuple(SessionId) ->
    case get_session(SessionId) of
        no_session -> ok;
        Session -> 
            ets:delete(emud_sessions, SessionId),
            ets:delete(emud_conn2session, Session#session.conn),
            ok
    end.