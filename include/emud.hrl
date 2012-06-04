
-record(session, {
        id, % sessionid()
        conn, % pid()
        sess % pid()
    }).

-record(cmd, {
        type, % atom()
        sessid, % sessiondid()
        props % list({key,value})
    }).