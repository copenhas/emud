#include <string.h>
#include <antlr3.h>
#include <erl_nif.h>

#include "MudLexer.h"
#include "MudParser.h"

static ERL_NIF_TERM 
emud_cmd_parser_parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"parse", 1, emud_cmd_parser_parse}
};

static ERL_NIF_TERM 
emud_cmd_parser_parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ 
    ERL_NIF_TERM record;
    ErlNifBinary input;

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_atom(env, "invalid_input");
    }

    if (input.size == 0) {
        return enif_make_atom(env, "invalid_input");
    }

    pANTLR3_INPUT_STREAM inputStream = antlr3StringStreamNew(
                (pANTLR3_UINT8)input.data,
                ANTLR3_ENC_UTF8,
                (ANTLR3_UINT32)input.size,
                (pANTLR3_UINT8)"command string"
            );

    if (inputStream == NULL) {
        return enif_make_atom(env, "stream_error");
    }

    pMudLexer lexer = MudLexerNew(inputStream);

    if (lexer == NULL) {
        record = enif_make_atom(env, "lexer_error");
        goto inputStreamClean;
    }

    pANTLR3_COMMON_TOKEN_STREAM tokenStream = antlr3CommonTokenStreamSourceNew(
                ANTLR3_SIZE_HINT, TOKENSOURCE(lexer)
            );

    if (tokenStream == NULL) {
        record = enif_make_atom(env, "token_stream_error");
        goto lexerClean;
    }

    pMudParser parser = MudParserNew(tokenStream);

    if (parser == NULL) {
        record = enif_make_atom(env, "parser_error");
        goto tokenStreamClean;
    }

    record = parser->command(parser, env);

    if (record == NULL) {
        record = enif_make_atom(env, "invalid_input");
    }

    parser->free(parser);
tokenStreamClean:
    tokenStream->free(tokenStream);
lexerClean:
    lexer->free(lexer);
inputStreamClean:
    inputStream->free(inputStream);

    return record;
}

int upgrade(ErlNifEnv *env, void **priv_data, 
            void **old_priv_data, ERL_NIF_TERM load_info){
    // no private data to worry about
    return 0;
}


ERL_NIF_INIT(emud_cmd_parser, nif_funcs, NULL, NULL, &upgrade, NULL);
