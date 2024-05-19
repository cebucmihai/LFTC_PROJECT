#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main() {
    char *input = loadFile("tests/testat.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    pushDomain();
    vmInit(); 
    parse(tokenList);
    Instr *testCode = temaMasinaVirtuala();
    run(testCode);
    //showDomain(symTable,"global");
    dropDomain();
    return 0;
}
