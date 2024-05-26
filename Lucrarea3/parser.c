#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

#include "ad.h"
#include "lexer.h"
#include "utils.h"
#include "at.h"
#include "gc.h"

Symbol *owner = NULL;

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
bool typeBase(Type *t) {
    t->n = -1;
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(TYPE_INT)) {
        t->tb=TB_INT;
        return true;
    }
    if (consume(TYPE_DOUBLE)) {
        t->tb=TB_DOUBLE;
        return true;
    }
    if (consume(TYPE_CHAR)) {
        t->tb=TB_CHAR;
        return true;
    }
    if (consume(STRUCT)) {
        if (consume(ID)) {
            Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if(!t->s) tkerr("structura nedefinita: %s",tkName->text);
			
            return true;
        } else {
            tkerr("Lipseste numele structurii dupa identificatorul 'struct'");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t) {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(LBRACKET)) {
        if (consume(INT)) {
            Token *tkSize=consumedTk;
			t->n=tkSize->i;
		} else {
			t->n=0; // array fara dimensiune: int v[]
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
    Type t;
    Token *start = iTk; // Salvam pozitia curenta
    if (typeBase(&t)) {
        if (consume(ID)) {
            Token *tkName = consumedTk;
            if (arrayDecl(&t)) {
                if (t.n == 0) tkerr("a vector variable must have a specified dimension");
            }
            if (consume(SEMICOLON)) {
                Symbol *var=findSymbolInDomain(symTable,tkName->text);
				if(var)tkerr("symbol redefinition: %s",tkName->text);
				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable,var);
				if (owner) {
					switch(owner->kind) {
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
						default:
							break;
					}
				} else {
					var->varMem=safeAlloc(typeSize(&t));
				}
                return true;
            } else {
                tkerr("Lipseste punctul si virgula ';' dupa declaratia variabilei");
            }
        } else {
            tkerr("Lipseste numele variabilei");
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
            Token *tkName = consumedTk;
            if (consume(LACC)) {
                Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if(s) tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable,newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;

                while (varDef()) {} // Zero sau mai multe varDefs
                if (consume(RACC)) {
                    if (consume(SEMICOLON)) {
                        owner = NULL;
						dropDomain();
                        return true;
                    } else {
						tkerr("Lipseste ';' dupa definerea structurii");
                    } 
                }else {
					tkerr("Lipseste '}' la finalul structurii");
				}
            }
        }else {
			tkerr("Lipseste numele structurii");
		}
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() {
    Token *start = iTk;
	Type t;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (arrayDecl(&t)) {
				t.n=0;
			}
			Symbol *param=findSymbolInDomain(symTable,tkName->text);
			if(param)tkerr("symbol redefinition: %s",tkName->text);
			param=newSymbol(tkName->text,SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable,param);
			addSymbolToList(&owner->fn.params,dupSymbol(param));
			return true;
		} else {
			tkerr("Lipseste numele parametrului");
		}
	}
	iTk = start;
	return false;
}

// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound
bool fnDef() {
    Type t;
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    if (typeBase(&t) || consume(VOID)) {
        if (consume(ID)) {
            Token *tkName = consumedTk;
            if (consume(LPAR)) {
                Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn)tkerr("symbol redefinition: %s",tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
                if (fnParam()) {
                    while (consume(COMMA)) {
                        if (!fnParam()) {
                            tkerr("Lipseste parametrul functiei dupa ,");
                        }
                    }
                }
                if (consume(RPAR)) {
					addInstr(&fn->fn.instr, OP_ENTER);
                    if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals); 
                        if (fn->type.tb == TB_VOID)
                            addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
                        dropDomain();
						owner=NULL;
						return true;
                    } else {
                        tkerr("Lipseste corpul functiei dupa parametrii");
                    }
                } else {
                    tkerr("Lipseste ')' la finalul functiei");
                }
            } 
            
        } else {
            tkerr("Lipseste numele functiei");
        }
    }else if (consume(VOID)) {
		t.tb=TB_VOID;
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn)tkerr("symbol redefinition: %s",tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam()) {}
							else tkerr("Lipseste parametrul functiei dupa ,");
						} else break;
					}
				}
				if (consume(RPAR)) {
					addInstr(&fn->fn.instr, OP_ENTER);
					if (stmCompound(false)) {
						fn->fn.instr->arg.i = symbolsLen(fn->fn.locals); 
                        if (fn->type.tb == TB_VOID) {
                            addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
                        }
						dropDomain();
						owner=NULL;
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		} else {
			tkerr("Lipseste numele functiei");
		}
    }
    iTk = start; // Revenim la pozitia initiala
	if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain) {
    Token *start = iTk; // Salvam pozitia curenta
    if (consume(LACC)) {
        if(newDomain)pushDomain();
        while (varDef() || stm()) {} // Zero sau mai multe varDefs sau instructiuni
        if (consume(RACC)) {
            if(newDomain)dropDomain();
            return true;
        } else {
            tkerr("Lipseste '}' in instructiunea compusa");
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON
bool stm() {
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    Ret rCond, rExpr;
    if (stmCompound(true)) {
        return true;
    }
    if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr(&rCond)) {
                if (!canBeScalar(&rCond)) {
					tkerr("Conditia if-ului trebuie sa fie scalar");
				}
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *ifJF = addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
						if (consume(ELSE)){
                            Instr *ifJMP = addInstr(&owner->fn.instr, OP_JMP);
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);

                            if (stm()){
                                ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                            } else{
                                tkerr("Lipseste statement-ul dupa 'else'");
                            }
                        } else{
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                        }
                        return true;
                    } else {
                        tkerr("Lipseste statement-ul dupa 'if'");
                    }
				} else {
					tkerr("Lipseste ')' dupa expresie");
				}
			} else {
				tkerr("Lipseste expresia");
			} 
		} else {
			tkerr("Lipseste '(' inainte de expresie");
		}
		iTk = start;
    }
    if (consume(WHILE)) {
		Instr *beforeWhileCond = lastInstr(owner->fn.instr);
		if (consume(LPAR)) {
			if (expr(&rCond)) {
                if (!canBeScalar(&rCond)) {
					tkerr("conditia trebuie sa fie scalar");
				}
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *whileJF = addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
                        addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
                        whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                        return true;
                    } else{
                        tkerr("Lipseste statement-ul dupa 'while'");
                    }
				} else {
					tkerr("Lipseste ')' dupa expresie");
				}
			}  else {
				tkerr("Lipseste expresia");
			} 
		} else {
			tkerr("Lipseste '(' inainte de expresie");
		}
		iTk = start;
    }
    if (consume(RETURN)) {
        if (expr(&rExpr)) {
            if (owner->type.tb == TB_VOID) {
                tkerr("A void function cannot return a value");
            }
            if (!canBeScalar(&rExpr)) {
                tkerr("The return value must be a scalar value");
            }
            if (!convTo(&rExpr.type, &owner->type)) {
                tkerr("Cannot convert the return expression type to the function return type");
            }
            addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
            addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
        } else {
            if (owner->type.tb != TB_VOID) {
                tkerr("A non-void function must return a value");
            }
            addInstr(&owner->fn.instr, OP_RET_VOID);
        }
        if (consume(SEMICOLON)){
            return true;
        } else{
            tkerr("Missing ';' after 'return'");
        }
    } else {
        if (expr(&rExpr)){
            if (rExpr.type.tb != TB_VOID){
                addInstr(&owner->fn.instr, OP_DROP);
            }
        }
        if (consume(SEMICOLON)){
            return true;
        }
    }
    iTk = start;
    if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// expr: exprAssign
bool expr(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprAssign(r)) {
        return true;
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret *r) {
    Ret rDst;
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    if (exprUnary(&rDst)) {
		if (consume(ASSIGN)) {
			if (exprAssign(r)) {
				if (!rDst.lval) {
					tkerr("Destinatia trebuie sa fie left-value");
				}
				if (rDst.ct) {
					tkerr("Destinatia nu trebuie sa fie constanta");
				}
				if (!canBeScalar(&rDst)) {
					tkerr("Destinatia trebuie sa fie scalar");
				}
				if (!canBeScalar(r)) {
					tkerr("Sursa trebuie sa fie scalar");
				}
				if (!convTo(&r->type,&rDst.type)) {
					tkerr("Sursa trebuie sa fie convertibila la destinatie");
				}
				r->lval = false;
				r->ct = true;
                addRVal(&owner->fn.instr, r->lval, &r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
                switch (rDst.type.tb) {
                    case TB_INT:
                        addInstr(&owner->fn.instr, OP_STORE_I);
                        break;
                    case TB_DOUBLE:
                        addInstr(&owner->fn.instr, OP_STORE_F);
                        break;
                    default : break;
                }
                return true;
            } else {
                tkerr("Lipseste expresia dupa semnul =");
            }
        }
        iTk = start;
    }
	iTk = start;
    if (owner) {
        delInstrAfter(startInstr);
    }
    if (exprOr(r)) {
        return true;
    }
    return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(Ret *r) {
    Token *start = iTk;
	if (consume(SUB) || consume(NOT)) {
		if (exprUnary(r)) {
			if (!canBeScalar(r)) {
				tkerr("Minus unar si Not trebuie sa aiba un operand scalar");
			}
			r->lval = false;
			r->ct = true;
			return true;
		} else {
			tkerr("Expresie invalida dupa '-' sau '!'");
		}
		iTk = start;
	}
	if (exprPostfix(r)) {
		return true;
	}
	iTk = start;
	return false;
}

// exprPostfix: exprPrimary postfixPrim
bool exprPostfix(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprPrimary(r)) {
        return exprPostfixPrim(r);
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// postfixPrim = LBRACKET expr RBRACKET postfixPrim
// | DOT ID postfixPrim
// | ε
bool exprPostfixPrim(Ret *r) {
	Token *start = iTk;
    if (consume(LBRACKET)) {
        Ret idx;
        if (expr(&idx)) {
            if (consume(RBRACKET)) {
                if (r->type.n < 0) {
					tkerr("Doar un array poate fi indexat");
				}
				Type tInt = {TB_INT,NULL,-1};
                if (!convTo(&idx.type, &tInt)) {
					tkerr("Indexul in array trebuie sa fie convertibil la int");
				}
				r->type.n = -1;
                r->lval = true;
                r->ct = false;
                if (exprPostfixPrim(r)) {
                    return true;
                } else {
					tkerr("Expresie invalida dupa ']'");
				}
            } else {
				tkerr("Lipseste ']' dupa expresie");
			}
        }
		iTk = start;
    }

   if (consume(DOT)) {
        if (consume(ID)) {
			Token *tkName = consumedTk;
            if (r->type.tb != TB_STRUCT) {
				tkerr("Operatorul de selectie a unui camp de structura se poate aplica doar structurilor");
			}
			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if (!s) {
				tkerr("Structura %s nu are un camp %s",r->type.s->name,tkName->text);
			}
			*r = (Ret){s->type,true,s->type.n>=0};
            if (exprPostfixPrim(r)) {
                return true;
            } else {
				tkerr("Lipseste expresia dupa nume campului");
			}
        } else {
			tkerr("Lipseste numele campului ce se doreste a fi cautat");
		}
		iTk = start;
    }

    return true; // epsilon-ul nostru
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r) {
   Token *start = iTk;
   Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(ID)) {
        Token *tkName = consumedTk;
        Symbol *s = findSymbol(tkName->text);
        if (!s) {
            tkerr("Undefined id: %s", tkName->text);
        }
        if (consume(LPAR)) {
            if (s->kind != SK_FN) {
                tkerr("Only a function can be called");
            }
            Ret rArg;
            Symbol *param = s->fn.params;
            if (expr(&rArg)) {
                if (!param) {
                    tkerr("Too many arguments in function call");
                }
                if (!convTo(&rArg.type, &param->type)) {
                    tkerr("In call, cannot convert the argument type to the parameter type");
                }
                addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
                param = param->next;
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr(&rArg)) {
                            if (!param) {
                                tkerr("Too many arguments in function call");
                            }
                            if (!convTo(&rArg.type, &param->type)) {
                                tkerr("In call, cannot convert the argument type to the parameter type");
                            }
                            addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                            insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
                            param = param->next;
                        } else {
                            tkerr("Missing expression after ',' in function call");
                        }
                    } else {
                        break;
                    }
                }
            }
            if (consume(RPAR)) {
                if (param) {
                    tkerr("Too few arguments in function call");
                }
                *r = (Ret){s->type, false, true};
                if (s->fn.extFnPtr) {
                    addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
                } else {
                    addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
                }
                return true;
            } else {
                tkerr("Missing ')' in function call");
            }
        } else {
            if (s->kind == SK_FN) {
                tkerr("A function can only be called");
            }
            *r = (Ret){s->type, true, s->type.n >= 0};

            if (s->kind == SK_VAR) {
                if (s->owner == NULL) {// global variables
                    addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
                } else {// local variables
                    switch (s->type.tb) {
                        case TB_INT:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1);
                            break;
                        case TB_DOUBLE:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1);
                            break;
                        default : break;
                    }
                }
            }

            if (s->kind == SK_PARAM) {
                switch (s->type.tb) {
                    case TB_INT:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                    case TB_DOUBLE:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                    default : break;
                }
            }
        }
        return true;
    } else if (consume(INT)) {
        *r = (Ret){{TB_INT, NULL, -1}, false, true};
        Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
        return true;
    } else if (consume(DOUBLE)) {
        *r = (Ret){{TB_DOUBLE, NULL, -1}, false, true};
        Token *ct = consumedTk;
        addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
        return true;
    } else if (consume(CHAR)) {
        *r = (Ret){{TB_CHAR, NULL, -1}, false, true};
        return true;
    } else if (consume(STRING)) {
        *r = (Ret){{TB_CHAR, NULL, 0}, false, true};
        return true;
    } else if (consume(LPAR)) {
        if (expr(r)) {
            if (consume(RPAR)) {
                return true;
            } else {
                tkerr("Missing ')' after expression");
            }
        }
    }
    iTk = start;
    if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret *r) {
    Token *start = iTk;
	if (consume(LPAR)) {
        Type t;
        Ret op;
		if (typeBase(&t)) {
			if (arrayDecl(&t)) {}
			if (consume(RPAR)) {
				if(exprCast(&op)) {
					if (t.tb == TB_STRUCT) {
						tkerr("Tipul la care se converteste nu poate fi structura"); 
					}
					if (op.type.tb == TB_STRUCT) {
						tkerr("Structurile nu se pot converti");
					} 
					if (op.type.n >= 0 && t.n < 0) {
						tkerr(" Un array se poate converti doar la alt array"); 
					}
					if (op.type.n < 0 && t.n >= 0) {
						tkerr("Un scalar se poate converti doar la alt scalar"); 
					}
					*r = (Ret){t,false,true};
					return true;
				} else {
					tkerr("Lipseste expresia de la Type Cast");
				}
			} else {
				tkerr("Lipseste ')'");
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '}'");
		}
		iTk = start;
	}
	if (exprUnary(r)) {
		return true;
	}
	iTk = start;
	return false;
}

bool exprMul(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    if (exprCast(r)) {
        return exprMulPrim(r);
    }
    iTk = start; // Revenim la pozitia initiala
	if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

bool exprMulPrim(Ret *r) {
    Ret right;
	Token *op;
	Instr *lastLeft;
    switch (iTk->code) {
        case MUL:
            consume(MUL);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
            if (exprCast(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '*'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
                case MUL:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                        default : break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }
            *r = (Ret){tDst, false, true};
                if (exprMulPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '*'");
            }
            break;
        case DIV:
            consume(DIV);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
             if (exprCast(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '/'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
                case MUL:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                        default : break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }
            *r = (Ret){tDst, false, true};
                if (exprMulPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '/'");
            }
            break;
        default:
            break;
    }
    return true; // epsilon-ul nostru
}


// exprAdd: exprMul exprAddPrim
bool exprAdd(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    if (exprMul(r)) {
        return exprAddPrim(r);
    }
    iTk = start; // Revenim la pozitia initiala
	if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// exprAddPrim: ( ADD | SUB ) exprMul exprAddPrim | ε
bool exprAddPrim(Ret *r) {
    Ret right;
	Token *op;
	Instr *lastLeft;
    switch (iTk->code) {
        case ADD:
            consume(ADD);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal (&owner->fn.instr, r->lval, &r->type);
            if (exprMul(&right)) {
			    Type tDst;
                if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '+'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case ADD:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_ADD_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_ADD_F);
                            break;
                        default : break;
                    }
                    break;
                case SUB:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_SUB_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_SUB_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }
            *r = (Ret){tDst, false, true};
                if (exprAddPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '+'");
            }
            break;
        case SUB:
            consume(SUB);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal (&owner->fn.instr, r->lval, &r->type);
             if (exprMul(&right)) {
			    Type tDst;
                if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '-'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case ADD:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_ADD_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_ADD_F);
                            break;
                        default : break;
                    }
                    break;
                case SUB:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_SUB_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_SUB_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }
            *r = (Ret){tDst, false, true};
                if (exprAddPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '-'");
            }
            break;
        default:
            break;
    }
    return true; // epsilon-ul nostru
}


// exprRel: exprAdd exprRelPrim
bool exprRel(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    if (exprAdd(r)) {
        return exprRelPrim(r);
    }
     iTk = start;
    if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | ε
bool exprRelPrim(Ret *r) {
	Token *op;
    Ret right;
	Instr *lastLeft;
    switch (iTk->code) {
        case LESS:
            consume(LESS);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
		    if (exprAdd(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '<'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case LESS:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};
                if (exprRelPrim(r)) {
                    return true;
                }
            } else {
                tkerr("Lipseste expresia dupa semnul '<'");
            }
            break;
        case LESSEQ:
            consume(LESSEQ);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
            if (exprAdd(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '<='");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case LESS:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};
                if (exprRelPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '<='");
            }
            break;
        case GREATER:
            consume(GREATER);
			op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
             if (exprAdd(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '>'");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case LESS:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};
                if (exprRelPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '>'");
            }
            break;
        case GREATEREQ:
		op = consumedTk;
        lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
            consume(GREATEREQ);
             if (exprAdd(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '>='");
			    }
			    addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case LESS:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};
                if (exprRelPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa semnul '>='");
            }
            break;
        default:
            //tkerr("Se astepta unul dintre semnele '<', '<=', '>', '>='");
            break;
    }
    return true; // epsilon-ul nostru
}


// exprEq: exprRel exprEqPrim
bool exprEq(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprRel(r)) {
        return exprEqPrim(r);
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprEqPrim: ( EQUAL | NOTEQ ) exprRel exprEqPrim | ε
bool exprEqPrim(Ret *r) {
    Ret right;
    switch (iTk->code) {
        case EQUAL:
            consume(EQUAL);
            if (exprRel(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '=='");
			    }
			    *r = (Ret){{TB_INT,NULL,-1},false,true};
                if (exprEqPrim(r)) {
                    return true;
                } 
            } else {
                tkerr("Lipseste expresia dupa '=='");
            }
            break;
        case NOTEQ:
            consume(NOTEQ);
            if (exprRel(&right)) {
			    Type tDst;
                 if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				    tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '!='");
			    }
			    *r = (Ret){{TB_INT,NULL,-1},false,true};
                if (exprEqPrim(r)) {
                    return true;
                } 
            } else{
                tkerr("Lipseste expresia dupa '!='");
            }
            break;
        default:
            break;
    }
    return true; // epsilon-ul nostru
}


// exprAnd: exprEq exprAndPrim
bool exprAnd(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprEq(r)) {
        return exprAndPrim(r);
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprAndPrim: AND exprEq exprAndPrim | ε
bool exprAndPrim(Ret *r) {
    if (consume(AND)) {
        Ret right;
        if (exprEq(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '&&'");
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprAndPrim(r)) {
                return true;
            }
        } else {
			tkerr("Lipseste expresia dupa semnul '&&'");
		}
    }
    return true; // epsilon-ul nostru
}

// exprOr: exprAnd exprOrPrim
bool exprOr(Ret *r) {
    Token *start = iTk; // Salvam pozitia curenta
    if (exprAnd(r)) {
        if (exprOrPrim(r)) {
            return true; // am ajuns la capatul regulii
        }
    }
    iTk = start; // Revenim la pozitia initiala
    return false;
}

// exprOrPrim: OR exprAnd exprOrPrim | ε
bool exprOrPrim(Ret *r) { // este recursiva
    if (consume(OR)) {
		Ret right;
		if(exprAnd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("Ambii operanzi trebuie sa fie scalari si sa nu fie structuri in operatia '||'");
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
			if(exprOrPrim(r)) {
				return true;
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '||'");
		}
	}
	return true; //epsilon-ul nostru
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
    if (!unit()) {}
}
