#pragma once

#include "lexer.h"
#include <stdbool.h>

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