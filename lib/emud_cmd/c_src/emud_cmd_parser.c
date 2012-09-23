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
    ErlNifBinary input;
    enif_inspect_binary(env, argv[0], &input);

    pANTLR3_INPUT_STREAM inputStream = antlr3StringStreamNew(
                (pANTLR3_UINT8)input.data,
                ANTLR3_ENC_UTF8,
                (ANTLR3_UINT32)input.size,
                (pANTLR3_UINT8)"command string"
            );

    pMudLexer lexer = MudLexerNew(inputStream);

    pANTLR3_COMMON_TOKEN_STREAM tokenStream = antlr3CommonTokenStreamSourceNew(
                ANTLR3_SIZE_HINT, TOKENSOURCE(lexer)
            );

    pMudParser parser = MudParserNew(tokenStream);

    ERL_NIF_TERM record = parser->command(parser, env);

    parser->free(parser);
    tokenStream->free(tokenStream);
    lexer->free(lexer);
    inputStream->free(inputStream);

    return record;
}

ERL_NIF_INIT(emud_cmd_parser, nif_funcs, NULL, NULL, NULL, NULL);
