#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main() {
    char *input = loadFile("tests/testgc.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    pushDomain();
    vmInit(); 
    parse(tokenList);
   // Instr *testCode = temaMasinaVirtuala();
   // run(testCode);
    //showDomain(symTable,"global");
    Symbol *symMain = findSymbolInDomain(symTable, "main");
    if (!symMain) {
        err("missing main function");
    }
     Instr *entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
    run(entryCode);
    //dropDomain();
    return 0;
}
