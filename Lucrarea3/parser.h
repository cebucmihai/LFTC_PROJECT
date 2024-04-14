#pragma once

#include "lexer.h"
#include <stdbool.h>
#include "ad.h"

void tkerr(const char *fmt,...);
bool consume(int code);
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool varDef();
bool structDef();
bool fnParam();
bool fnDef();
bool stmCompound(bool newDomain);
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