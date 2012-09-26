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
    #define LIST(count, ...) enif_make_list(env, count, ##__VA_ARGS__)

    ERL_NIF_TERM make_binary(ErlNifEnv *env, pANTLR3_COMMON_TOKEN token);

    ERL_NIF_TERM make_cmd(ErlNifEnv *env, char *cmd, ERL_NIF_TERM props);
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

    ERL_NIF_TERM make_cmd(ErlNifEnv *env, char *cmd, ERL_NIF_TERM props){
        return enif_make_tuple(env, 4, A("cmd"), A(cmd), 
                               A("undefined"), props);
    }
}

command[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : cmd=login[env] { $value = $cmd.value; } 
    | cmd=look[env] { $value = $cmd.value; }
    | cmd=move[env] { $value = $cmd.value; }
    ;
    catch[] {
        $value = A("invalid_cmd");
    }

login[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'login' WS user=TEXT WS pass=TEXT {
            ERL_NIF_TERM userProp = PROP("user", BIN($user));
            ERL_NIF_TERM passProp = PROP("pass", BIN($pass));

            ERL_NIF_TERM props = LIST(2, userProp, passProp);

            $value = make_cmd(env, "login", props);
        }
    ;
    catch[] {}

look[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : 'look' (WS 'around')? {
            $value = make_cmd(env, "look", LIST(0));
        }
    | 'look' (WS 'at')? WS target=TEXT {
            ERL_NIF_TERM obj = PROP("target", BIN($target));
            $value = make_cmd(env, "look", LIST(1, obj));
        }
    ;
    catch[] {}

move[ErlNifEnv *env] returns [ERL_NIF_TERM value]
    : ('move' WS)? dir=TEXT {
            ERL_NIF_TERM direction = PROP("exit", BIN($dir));
            $value = make_cmd(env, "move", LIST(1, direction));
        }
    ;
    catch[] {}


WS: (' ' | '\t')+;

TEXT: ('a'..'z'|'A'..'Z'|'1'..'0'|'!'|'@'|'#'|'$'|'%'|'^'|'&'|'*'|'('|')')+;

