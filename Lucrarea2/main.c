#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"

int main() {
    // Încarcăm conținutul fișierului de intrare
    char *input = loadFile("tests/testlex.c");

    // Tokenizează conținutul și obține lista de token-uri
    Token *tokenList = tokenize(input);

    // Afișează lista de token-uri
    showTokens(tokenList);

    // Eliberăm memoria alocată pentru conținutul fișierului și pentru lista de token-uri
    free(input);
    // Eliberarea memoriei pentru lista de token-uri trebuie să fie implementată în funcția showTokens()

    return 0;
}
