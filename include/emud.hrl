
-record(session, {
        id, % sessionid()
        conn, % pid()
        cmder % pid()
    }).

-record(cmd, {
        type % atom()
    }).