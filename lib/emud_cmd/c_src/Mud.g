grammar Mud;

options {
    language = C;
}

@includes {
    #include <string.h>
    #include <antlr3.h>
    #include <erl_nif.h>

    #define A(text) enif_make_atom(env, text)
    #define PROP(key, value) enif_make_tuple(env, 2, A(key), value)
    #define BIN(token) make_binary(env, token)

    ERL_NIF_TERM make_binary(ErlNifEnv *env, pANTLR3_COMMON_TOKEN token);
}

@members {
    ERL_NIF_TERM make_binary(ErlNifEnv *env, pANTLR3_COMMON_TOKEN token) {
        ErlNifBinary bin;
        pANTLR3_STRING text = token->getText(token);
        pANTLR3_STRING unicode = text->toUTF8(text);

        enif_alloc_binary(unicode->len, &bin);
        memcpy(bin.data, unicode->chars, unicode->len); 
        return enif_make_binary(env, &bin);
    }
}

command[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : login[env] { $value = $login.value; } 
    | look[env] { $value = $look.value; }
    | move[env] { $value = $move.value; }
    ;

login[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'login' WS user=TEXT WS pass=TEXT {
            ERL_NIF_TERM userProp = PROP("user", BIN($user));
            ERL_NIF_TERM passProp = PROP("pass", BIN($pass));

            ERL_NIF_TERM props = enif_make_list(env, 2, userProp, passProp);

            $value = enif_make_tuple(env, 4, A("cmd"), A("login"), 
                                     A("undefined"), props);
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

