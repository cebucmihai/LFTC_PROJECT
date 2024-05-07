#pragma once

#include "lexer.h"
#include <stdbool.h>
#include "ad.h"
#include "at.h"

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
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);
bool exprCast(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool unit();
void parse(Token *tokens);