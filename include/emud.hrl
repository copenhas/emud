
-record(session, {
        id, % sessionid()
        conn, % pid()
        sess % pid()
    }).

-record(cmd, {
        type, % atom()
        sessid % sessiondid()
    }).