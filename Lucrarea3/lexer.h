#pragma once
#include <stdlib.h>

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

Token *tokenize(const char *pch);
void showTokens(const Token *tokens);
