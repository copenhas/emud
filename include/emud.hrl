
-record(session, {
        id, % sessionid()
        conn, % pid()
        sess % pid()
    }).

% cmd's are how the user causes effects to the system
-record(cmd, {
        type, % atom()
        sessid, % sessiondid()
        props % list({key,value})
    }).

-define(CMDPROP(Cmd, Key), proplists:get_value(Key, Cmd#cmd.props)).

% the user record mostly for auth and tracking if someone's online
-record(usr, {
        name, % binary()
        password, % binary()
        character, % binary()
        conn % none | pid()
    }).

% system msg which can be generated from anything, 
% gets sent to the connection process for rendering 
% to the user. source is atom() for system generated 
% and binary() for character
-record(msg, {
        name, % atom()
        source, % atom() | binary()
        props % list({key, value})
    }).