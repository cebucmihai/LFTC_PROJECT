#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"

int main() {
    char *input = loadFile("tests/testlex.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    return 0;
}
