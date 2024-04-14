#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

typedef struct Token {
    int code;
    int line;
    union {
        char *text;
        int i;
        char c;
        double d;
    };
    struct Token *next;
} Token;

void tkerr(const char *fmt,...);
bool consume(int code);
bool typeBase();
bool arrayDecl();
bool varDef();
bool structDef();
bool fnParam();
bool fnDef();
bool stmCompound();
bool stm();
bool expr();
bool exprAssign();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();
bool exprCast();
bool exprMul();
bool exprMulPrim();
bool exprAdd();
bool exprAddPrim();
bool exprRel();
bool exprRelPrim();
bool exprEq();
bool exprEqPrim();
bool exprAnd();
bool exprAndPrim();
bool exprOr();
bool exprOrPrim();
bool unit();
void parse(Token *tokens);

enum {
    ID,
    TYPE_CHAR,
    TYPE_DOUBLE,  // Tipuri noi
    ELSE,
    IF,
    TYPE_INT,
    RETURN,
    STRUCT,
    VOID,
    WHILE,
    INT,
    CHAR,
    DOUBLE,
    STRING,
    SPACE,
    LINECOMMENT,  
    COMMA,
    SEMICOLON,
    LPAR,
    RPAR,
    LBRACKET,
    RBRACKET,
    LACC,
    RACC,
    END,
    ADD,
    SUB,
    MUL,
    DIV,
    DOT,
    AND,
    OR,
    NOT,
    ASSIGN,
    EQUAL,
    NOTEQ,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
};



Token *tokenize(const char *pch);
void showTokens(const Token *tokens);

void tkerr(const char *fmt,...);
bool consume(int code);
bool typeBase();
bool arrayDecl();
bool varDef();
bool structDef();
bool fnParam();
bool fnDef();
bool stmCompound();
bool stm();
bool expr();
bool exprAssign();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();
bool exprCast();
bool exprMul();
bool exprMulPrim();
bool exprAdd();
bool exprAddPrim();
bool exprRel();
bool exprRelPrim();
bool exprEq();
bool exprEqPrim();
bool exprAnd();
bool exprAndPrim();
bool exprOr();
bool exprOrPrim();
bool unit();
void parse(Token *tokens);

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
        iTk = start;
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
// | INC postfixPrim
// | DEC postfixPrim
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
        iTk = start;
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
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
        return true;
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

void err(const char *fmt,...){
	fprintf(stderr,"error: ");
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

void *safeAlloc(size_t nBytes){
	void *p=malloc(nBytes);
	if(!p)err("not enough memory");
	return p;
	}

char *loadFile(const char *fileName){
	FILE *fis=fopen(fileName,"rb");
	if(!fis)err("unable to open %s",fileName);
	fseek(fis,0,SEEK_END);
	size_t n=(size_t)ftell(fis);
	fseek(fis,0,SEEK_SET);
	char *buf=(char*)safeAlloc((size_t)n+1);
	size_t nRead=fread(buf,sizeof(char),(size_t)n,fis);
	fclose(fis);
	if(n!=nRead)err("cannot read all the content of %s",fileName);
	buf[n]='\0';
	return buf;
	}
Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line = 1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
    Token *tk = safeAlloc(sizeof(Token));
    tk->code = code;
    tk->line = line;
    tk->next = NULL;
    if(lastTk){
        lastTk->next = tk;
    }else{
        tokens = tk;
    }
    lastTk = tk;
    return tk;
}

char *extract(const char *begin, const char *end){
    // Calculate the length of the substring to be extracted
    size_t length = end - begin;
    
    // Allocate memory for the extracted substring
    char *extracted = (char *)safeAlloc(length + 1); // +1 for NULL-termination
    
    // Copy the substring from the original string to the extracted substring
    strncpy(extracted, begin, length);
    
    // Add NULL-termination
    extracted[length] = '\0';
    
    // Return the extracted substring
    return extracted;
}

Token *tokenize(const char *pch){
    const char *start;
    Token *tk;
    char *endptr; // Pointer for strtol / strtod functions
    for(;;){
        switch(*pch){
            case ' ':case '\t':pch++;break;
            case '\r':
                if(pch[1]=='\n')pch++;
            case '\n':
                line++;
                pch++;
                break;
            case '\0':addTk(END);return tokens;
            case ',':addTk(COMMA);pch++;break;
            case '=':
                if(pch[1]=='='){
                    addTk(EQUAL);
                    pch+=2;
                }else{
                    addTk(ASSIGN);
                    pch++;
                }
                break;
            case '(': addTk(LPAR); pch++; break;
            case ')': addTk(RPAR); pch++; break;
            case '{': addTk(LACC); pch++; break;
            case '}': addTk(RACC); pch++; break;
            case '[': addTk(LBRACKET); pch++; break;
            case ']': addTk(RBRACKET); pch++; break;
            case ';': addTk(SEMICOLON); pch++; break;
            case '+': addTk(ADD); pch++; break;
            case '-': addTk(SUB); pch++; break;
            case '*': addTk(MUL); pch++; break;
            case '/':
                if (pch[1] == '/') {
                    while (*pch && *pch != '\n') pch++;
                } else if (pch[1] == '*') {
                    pch += 2; 
                    while (*pch && !(*pch == '*' && pch[1] == '/')) pch++; 
                    pch += 2; 
                } else {
                    addTk(DIV);
                    pch++;
                }
                break;
            case '.': addTk(DOT); pch++; break;
            case '&':
                if(pch[1] == '&'){
                    addTk(AND);
                    pch += 2;
                }else{
                    err("invalid char: %c (%d)", *pch, *pch);
                }
                break;
            case '|':
                if(pch[1] == '|'){
                    addTk(OR);
                    pch += 2;
                }else{
                    err("invalid char: %c (%d)", *pch, *pch);
                }
                break;
            case '!':
                if(pch[1]=='='){
                    addTk(NOTEQ);
                    pch+=2;
                }else{
                    addTk(NOT);
                    pch++;
                }
                break;
            case '<':
                if(pch[1] == '='){
                    addTk(LESSEQ);
                    pch += 2;
                }else{
                    addTk(LESS);
                    pch++;
                }
                break;
            case '>':
                if(pch[1] == '='){
                    addTk(GREATEREQ);
                    pch += 2;
                }else{
                    addTk(GREATER);
                    pch++;
                }
                break;
            case '\'':
                if (pch[2] == '\'') {
                    addTk(CHAR)->c = pch[1];
                    pch += 3;
                } else {
                    err("invalid char: %c (%d)", *pch, *pch);
                }
                break;
            case '"':
                for (start = ++pch; *pch && *pch != '"'; ++pch) {}
                if (*pch == '"') {
                    addTk(STRING)->text = extract(start, pch);
                    ++pch; 
                } else {
                    err("unclosed string literal");
                }
                break;
            default:
                if(isalpha(*pch)||*pch=='_'){
                    for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
                    char *text = extract(start, pch);
                    if(strcmp(text,"char")==0) addTk(TYPE_CHAR);
                    else if(strcmp(text,"double")==0) addTk(TYPE_DOUBLE);
                    else if(strcmp(text,"else")==0) addTk(ELSE);
                    else if(strcmp(text,"if")==0) addTk(IF);
                    else if(strcmp(text,"int")==0) addTk(TYPE_INT);
                    else if(strcmp(text,"return")==0) addTk(RETURN);
                    else if(strcmp(text,"struct")==0) addTk(STRUCT);
                    else if(strcmp(text,"void")==0) addTk(VOID);
                    else if(strcmp(text,"while")==0) addTk(WHILE);
                    else if(strcmp(text,"else")==0) addTk(ELSE);
                    else{
                        tk = addTk(ID);
                        tk->text = text;
                    }
                } else if (isdigit(*pch) || *pch == '-' || *pch == '.') {
                    int hasDot = 0;
                    int hasExp = 0;
                    for (start = pch; isdigit(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E' || *pch == '+' || *pch == '-'; pch++) {
                        if (*pch == '.') {
                            if (hasDot || hasExp) {
                                err("invalid number");
                            }
                            hasDot = 1;
                        } else if (*pch == 'e' || *pch == 'E') {
                            if (hasExp) {
                                err("invalid number");
                            }
                            hasExp = 1;
                        }
                    }
                    char *text = extract(start, pch);
                    if (hasDot || hasExp) {
                        double val = strtod(text, &endptr);
                        if (endptr != text) {
                            addTk(DOUBLE)->d = val;
                        } else {
                            err("invalid number");
                        }
                    } else {
                        long val = strtol(text, &endptr, 10);
                        if (endptr != text) {
                            addTk(INT)->i = (int) val;
                        } else {
                            err("invalid number");
                        }
                    }
                } else {
                    err("invalid char: %c (%d)", *pch, *pch);
                }
        }
    }
}


void showTokens(const Token *tokens) {
    FILE *file = fopen("lex.txt", "w"); 
    if (file == NULL) {
        printf("Eroare la deschiderea fișierului.");
        return;
    }

    for (const Token *tk = tokens; tk; tk = tk->next) {
        fprintf(file, "%d\t", tk->line); 
        switch (tk->code) {
            case ID: fprintf(file, "ID: %s\n", tk->text); break;
            case TYPE_CHAR: fprintf(file, "TYPE_CHAR\n"); break;
            case COMMA: fprintf(file, "COMMA\n"); break;
            case END: fprintf(file, "END\n"); break;
            case ASSIGN: fprintf(file, "ASSIGN\n"); break;
            case EQUAL: fprintf(file, "EQUAL\n"); break;
            case NOTEQ: fprintf(file, "NOTEQ\n"); break;
            case INT: fprintf(file, "INT: %d\n", tk->i); break;
            case TYPE_INT: fprintf(file, "TYPE_INT\n"); break;
            case CHAR: fprintf(file, "CHAR: %c\n", tk->c); break;
            case DOUBLE: fprintf(file, "DOUBLE: %lf\n", tk->d); break;
            case TYPE_DOUBLE: fprintf(file, "TYPE_DOUBLE\n"); break;
            case STRING: fprintf(file, "STRING: %s\n", tk->text); break;
            case IF: fprintf(file, "IF\n"); break;
            case ELSE: fprintf(file, "ELSE\n"); break;
            case WHILE: fprintf(file, "WHILE\n"); break;
            case RETURN: fprintf(file, "RETURN\n"); break;
            case STRUCT: fprintf(file, "STRUCT\n"); break;
            case VOID: fprintf(file, "VOID\n"); break;
            case ADD: fprintf(file, "ADD\n"); break;
            case SUB: fprintf(file, "SUB\n"); break;
            case MUL: fprintf(file, "MUL\n"); break;
            case DIV: fprintf(file, "DIV\n"); break;
            case DOT: fprintf(file, "DOT\n"); break;
            case AND: fprintf(file, "AND\n"); break;
            case OR: fprintf(file, "OR\n"); break;
            case NOT: fprintf(file, "NOT\n"); break;
            case LESSEQ: fprintf(file, "LESSEQ\n"); break;
            case GREATEREQ: fprintf(file, "GREATEREQ\n"); break;
            case LESS: fprintf(file, "LESS\n"); break;
            case GREATER: fprintf(file, "GREATER\n"); break;
            case LPAR: fprintf(file, "LPAR\n"); break;
            case RPAR: fprintf(file, "RPAR\n"); break;
            case LBRACKET: fprintf(file, "LBRACKET\n"); break;
            case RBRACKET: fprintf(file, "RBRACKET\n"); break;
            case LACC: fprintf(file, "LACC\n"); break;
            case RACC: fprintf(file, "RACC\n"); break;
            case SEMICOLON: fprintf(file, "SEMICOLON\n"); break;
            default: fprintf(file, "Unknown token\n");
        }
    }

    fclose(file); // Închide fișierul
}


int main() {
    char *input = loadFile("tests1/testparser.c");
    Token *tokenList = tokenize(input);
    showTokens(tokenList);
    free(input);
    parse(tokenList);
    return 0;
}
