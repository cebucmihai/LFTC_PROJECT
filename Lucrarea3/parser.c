#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;         // iteratorul in lista de token-uri
Token *consumedTk;  // ultimul token consumat

void tkerr(const char *fmt, ...) {
    fprintf(stderr, "eroare in linia %d: ", iTk->line);
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

bool consume(int code) {
    if (iTk->code == code) {
        consumedTk = iTk;
        iTk = iTk->next;
        return true;
    }
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(TYPE_INT)) {
        return true;
    }
    if (consume(TYPE_DOUBLE)) {
        return true;
    }
    if (consume(TYPE_CHAR)) {
        return true;
    }
    if (consume(STRUCT)) {
        if (consume(ID)) {
            return true;
        } else {
            tkerr("Lipseste identificatorul structurii dupa 'struct'");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(LBRACKET)) {
        if (consume(INT)) {
            // Optional, consuma INT
        }
        if (consume(RBRACKET)) {
            return true;
        } else {
            tkerr("Lipseste paranteza dreapta ']' in declaratia de array");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() {
    Token *start = iTk; // Salvam pozitia curenta
    if (typeBase()) {
        if (consume(ID)) {
            if (arrayDecl()) {
                // Declaratie de array gasita
            }
            if (consume(SEMICOLON)) {
                return true;
            } else {
                tkerr("Lipseste punctul si virgula ';' dupa declaratia variabilei");
            }
        } else {
            tkerr("Se asteapta un identificator dupa specificatorul de tip");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(STRUCT)) {
        if (consume(ID)) {
            if (consume(LACC)) {
                while (varDef()) {} // Zero sau mai multe varDefs
                if (consume(RACC)) {
                    if (consume(SEMICOLON)) {
                        return true;
                    } 
                }
            }
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() {
    Token *start = iTk; // Salvam pozitia curenta
    if (typeBase()) {
        if (consume(ID)) {
            if (arrayDecl()) {}
            return true;
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound
bool fnDef() {
    Token *start = iTk; // Salvam pozitia curenta
    if (typeBase() || consume(VOID)) {
        if (consume(ID)) {
            if (consume(LPAR)) {
                if (fnParam()) {
                    while (consume(COMMA)) {
                        if (!fnParam()) {
                            tkerr("Eroare de sintaxa in parametrii functiei");
                        }
                    }
                }
                if (consume(RPAR)) {
                    if (stmCompound()) {
                        return true;
                    } else {
                        tkerr("Lipseste corpul functiei dupa parametrii");
                    }
                } else {
                    tkerr("Lipseste paranteza dreapta ')' in declaratia functiei");
                }
            } 
            
        } else {
            tkerr("Se asteapta un identificator dupa tipul functiei");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(LACC)) {
        while (varDef() || stm()) {} // Zero sau mai multe varDefs sau instructiuni
        if (consume(RACC)) {
            return true;
        } else {
            tkerr("Lipseste acolada dreapta '}' in instructiunea compusa");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON
bool stm() {
    Token *start = iTk; // Salvam pozitia curenta
    if (stmCompound()) {
        return true;
    }
    if (consume(IF)) {
        if (consume(LPAR)) {
            if (expr()) {
                if (consume(RPAR)) {
                    if (stm()) {
                        if (consume(ELSE)) {
                            if (stm()) {}
                            else {
                                tkerr("Lipseste ramura 'else' dupa instructiunea 'if'");
                            }
                        }
                        return true;
                    } else {
                        tkerr("Lipseste instructiunea in ramura 'if'");
                    }
                } else {
                    tkerr("Lipseste paranteza dreapta ')' in instructiunea 'if'");
                }
            } else {
                tkerr("Lipseste conditia in instructiunea 'if'");
            }
        } else {
            tkerr("Lipseste paranteza stanga '(' in instructiunea 'if'");
        }
    }
    if (consume(WHILE)) {
        if (consume(LPAR)) {
            if (expr()) {
                if (consume(RPAR)) {
                    if (stm()) {
                        return true;
                    } else {
                        tkerr("Lipseste instructiunea in bucla 'while'");
                    }
                } else {
                    tkerr("Lipseste paranteza dreapta ')' in bucla 'while'");
                }
            } else {
                tkerr("Lipseste conditia in bucla 'while'");
            }
        } else {
            tkerr("Lipseste paranteza stanga '(' in bucla 'while'");
        }
    }
    if (consume(RETURN)) {
        if (expr()) {}
        if (consume(SEMICOLON)) {
            return true;
        } else {
            tkerr("Lipseste punctul si virgula ';' dupa instructiunea return");
        }
    }
    if (expr()) {
        if (consume(SEMICOLON)) {
            return true;
        } else {
            tkerr("Lipseste punctul si virgula ';' dupa expresie");
        }
    } else {
        if (consume(SEMICOLON)) {
            return true;
        } else {
            tkerr("Lipseste punctul si virgula ';' dupa instructiunea goala");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// expr: exprAssign
bool expr() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprAssign()) {
        return true;
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprUnary()) {
        if (consume(ASSIGN)) {
            if (exprAssign()) {
                return true;
            } else {
                tkerr("Eroare de sintaxa in expresia de atribuire");
            }
        }
    }
    if (exprOr()) {
        return true;
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(SUB) || consume(NOT)) {
        if (exprUnary()) {
            return true;
        } else {
            tkerr("Eroare de sintaxa in expresia unara");
        }
    }
    if (exprPostfix()) {
        return true;
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprPostfix: exprPrimary postfixPrim
bool exprPostfix() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprPrimary()) {
        return exprPostfixPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// postfixPrim = LBRACKET expr RBRACKET postfixPrim
// | DOT ID postfixPrim
// | ε
bool exprPostfixPrim() {
    if (consume(LBRACKET)) {
        if (expr()) {
            if (consume(RBRACKET)) {
                if (exprPostfixPrim()) {
                    return true;
                }
            } else {
                tkerr("Lipseste paranteza de inchidere ']' in expresia postfixata");
            }
        } else {
            tkerr("Eroare de sintaxa in expresia intre paranteze in expresia postfixata");
        }
    }

    if (consume(DOT)) {
        if (consume(ID)) {
            if (exprPostfixPrim()) {
                return true;
            }
        }
    }
    return true;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(ID)) {
        printf("LINE : %d \n", iTk->line);
        if (consume(LPAR)) {
            printf("LINE : %d \n", iTk->line);
            if (expr()) {
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr()) {} 
                        else return false;
                    }
                }
                if (consume(RPAR)) {}
                else return false;
            }
        }
        return true;
    }
    if (consume(INT)) {
        return true;
    }
    if (consume(DOUBLE)) {
        return true;
    }
    if (consume(CHAR)) {
        return true;
    }
    if (consume(STRING)) {
        return true;
    }
    if (consume(LPAR)) {
        if (expr()) {
            if (consume(RPAR)) {
                return true;
            }
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast() {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(LPAR)) {
        if (typeBase()) {
            if (arrayDecl()) {}
            if (consume(RPAR)) {
                return exprCast();
            }
        }
    }
    if (exprUnary()) {
        return true;
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

bool exprMul() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprCast()) {
        return exprMulPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

bool exprMulPrim() {
    if (consume(MUL) || consume(DIV)) {
        if (exprCast()) {
            if (exprMulPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}

// exprAdd: exprMul exprAddPrim
bool exprAdd() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprMul()) {
        return exprAddPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprAddPrim: ( ADD | SUB ) exprMul exprAddPrim | ε
bool exprAddPrim() {
    if (consume(ADD) || consume(SUB)) {
        if (exprMul()) {
            if (exprAddPrim()) {
                return true;
            }
        }
    }
    return true; // ε
}

// exprRel: exprAdd exprRelPrim
bool exprRel() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprAdd()) {
        return exprRelPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | ε
bool exprRelPrim() {
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
        }
    }
    return true; // ε
}

// exprEq: exprRel exprEqPrim
bool exprEq() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprRel()) {
        return exprEqPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprEqPrim: ( EQUAL | NOTEQ ) exprRel exprEqPrim | ε
bool exprEqPrim() {
    if (consume(EQUAL) || consume(NOTEQ)) {
        if (exprRel()) {
            if (exprEqPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}

// exprAnd: exprEq exprAndPrim
bool exprAnd() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprEq()) {
        return exprAndPrim();
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprAndPrim: AND exprEq exprAndPrim | ε
bool exprAndPrim() {
    if (consume(AND)) {
        if (exprEq()) {
            if (exprAndPrim()) {
                return true;
            }
        }
    }
    return true; //  ε
}

// exprOr: exprAnd exprOrPrim
bool exprOr() {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprAnd()) {
        if (exprOrPrim()) {
            return true; // am ajuns la capatul regulii
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprOrPrim: OR exprAnd exprOrPrim | ε
bool exprOrPrim() { // este recursiva
    if (consume(OR)) {
        if (exprAnd()) {
            if (exprOrPrim()) {
                return true;
            }
        }
    }
    return true; // ε
}

// unit: ( structDef | fnDef | varDef )* END
bool unit() {
    Token *start = iTk; // Salvam pozitia curenta
    for (;;) {
        if (structDef() || fnDef() || varDef()) {}
        else break;
    }
    if (consume(END)) {
        return true;
    } else {
        tkerr("Lipseste token-ul 'END' la sfarsitul codului");
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

void parse(Token *tokens) {
    iTk = tokens;
    if (!unit()) tkerr("Eroare de sintaxa in unitate");
}
