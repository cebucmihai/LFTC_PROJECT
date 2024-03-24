#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"

int main() {
    char *input = loadFile("tests1/testparser.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    parse(tokenList);
    return 0;
}
