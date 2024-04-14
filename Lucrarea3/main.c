#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"

int main() {
    char *input = loadFile("tests/testad.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    pushDomain();
    parse(tokenList);
    showDomain(symTable,"global");
    dropDomain();
    return 0;
}
