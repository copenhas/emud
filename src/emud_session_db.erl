-module (emud_session_db).

-include("../include/emud.hrl").

-export([init/0,
         cleanup/0]).

-export([generate_session_id/0,
         create_session/3,
         get_session/1,
         update_session/1,
         remove_session/1]).


init() ->
    ets:new(emud_sessions, [named_table, {keypos, 2}, public, {read_concurrency, true}]),
    ets:new(emud_conn2session, [named_table, public, {read_concurrency, true}]),
    ets:new(emud_char2session, [named_table, public, {read_concurrency, true}]).

cleanup() ->
    ets:delete(emud_sessions),
    ets:delete(emud_conn2session),
    ets:delete(emud_char2session).

generate_session_id() ->
    {crypto:rand_bytes(6), now()}.

create_session(SessId, Conn, Sess) when is_tuple(SessId), is_pid(Conn), is_pid(Sess) ->
    Session = #session{ id = SessId, conn = Conn, sess = Sess },
    ets:insert(emud_sessions, Session),
    ets:insert(emud_conn2session, {Conn, SessId}),
    % they just connected no character to add with
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
    end;
get_session(Char) when is_binary(Char) ->
    case ets:lookup(emud_char2session, Char) of
        [] -> no_session;
        [{Char, SessionId}] -> get_session(SessionId)
    end.

update_session(#session{id = SessId, conn = Conn, character = Char} = Session) ->
    ets:insert(emud_sessions, Session),
    ets:insert(emud_conn2session, {Conn, SessId}),
    case Char of
        undefined -> ok;
        Name when is_binary(Name) -> ets:insert(emud_char2session, {Name, SessId})
    end,
    ok.

remove_session(SessionId) when is_tuple(SessionId) ->
    case get_session(SessionId) of
        no_session -> ok;
        Session -> 
            ets:delete(emud_sessions, SessionId),
            ets:delete(emud_conn2session, Session#session.conn),
            case Session#session.character of
                undefined -> ok;
                Name when is_binary(Name) -> ets:delete(emud_char2session, Name)
            end,
            ok
    end.