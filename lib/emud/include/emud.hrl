
-record(session, {
        id, % sessionid()
        conn, % pid()
        sess, % pid()
        user, % usr(), only if logged in
        character % binary(), only if they are actively in game
    }).


-define(CMDPROP(Cmd, Key), proplists:get_value(Key, Cmd#cmd.props)).

% cmd's are how the user causes effects to the system
-record(cmd, {
        type, % atom()
        sessid, % sessionid()
        props=[] % list({key,value})
    }).

-record(cmdctxt, {
        cmdref, % reference()
        sessid, % sessionid()
        sendmsg, % fun/1
        progress % fun/0
    }).

% system msg which can be generated from anything, 
% gets sent to the connection process for rendering 
% to the user. 
-record(msg, {
        type, % atom()
        cmdref, % reference(), if the msg is a direct result of a user's command
        source, % atom() | binary(), atom() if it's system; binary() for user/room
        text, % binary(), base text or message
        props=[] % list({key, value})
    }).


% the user record mostly for auth and tracking if someone's online
-record(usr, {
        name, % binary()
        password, % binary()
        character % binary()
    }).

-record(char, {
        name, % binary()
        room % binary()
    }).

-define(EXITTO(Rm, Exit), proplists:get_value(Exit, Rm#room.exits)).

-record(room, {
        id, % binary() 
        name, % binary()
        desc, % binary()
        exits=[] % list({atom(), binary()})
    }).
