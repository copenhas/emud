grammar Mud;

options {
    language = C;
}

@header {
    #include <erl_nif.h>
}

command[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : login[env] { $value = $login.value; } 
    | look[env] { $value = $look.value; }
    | move[env] { $value = $move.value; }
    ;

login[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'login' WS TEXT WS TEXT {
            ERL_NIF_TERM tag = enif_make_atom(env, "cmd");
            ERL_NIF_TERM cmd = enif_make_atom(env, "login");
            ERL_NIF_TERM sessid = enif_make_atom(env, "undefined");
            ERL_NIF_TERM props = enif_make_list(env, 0);

            $value = enif_make_tuple(env, 4, tag, cmd, sessid, props);
        }
    ;

look[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'look'  
    | 'look' WS 'around' 
    | 'look' WS 'at' WS target=TEXT 
    ;

move[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'move' WS DIRECTION 
    | DIRECTION 
    ;


WS: (' ' | '\t')+;

DIRECTION: 'north' | 'n'
         | 'east' | 'e'
         | 'south' | 's'
         | 'west' | 'w' 
         ;

TEXT: ('a'..'z'|'A'..'Z'|'1'..'0'|'!'|'@'|'#'|'$'|'%'|'^'|'&'|'*'|'('|')')+;

