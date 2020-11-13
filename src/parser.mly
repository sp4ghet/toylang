%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT MINUS LT GT LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ 


%token <int> INTV
%token <Syntax.id> ID

%start <Syntax.program> toplevel

%%

toplevel :
    e=Expr SEMISEMI { Exp ($loc, e) }
  | e=Decl SEMISEMI { Decl ($loc, e) }

Decl :
  LET x=ID EQ e=Expr  { (x,e) }
  | LET x=ID e=Args { (x,e) }

Args :
  x=ID EQ e=Expr { FunExp' ($loc, x, e) }
  | x=ID rest=Args { FunExp' ($loc, x, rest) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=LExpr { e }

LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { LetExp' ($loc, x,e1,e2) }
  | LET x=ID e1=Args IN e2=Expr { LetExp' ($loc, x,e1,e2) }

LExpr :
     l=CmpExpr LOR r=LExpr {BinOp' ($loc, Or, l, r)}
    | l=CmpExpr LAND r=LExpr { BinOp' ($loc, And, l, r) }
    | e=CmpExpr { e }

CmpExpr :
    l=MathExpr LT r=MathExpr { BinOp' ($loc, Lt, l, r) }
  | l=MathExpr GT r=MathExpr { BinOp' ($loc, Gt, l, r) }
  | e=MathExpr { e }

MathExpr :
    l=AppExpr PLUS r=AppExpr { BinOp' ($loc, Plus, l, r) }
  | l=AppExpr MINUS r=AppExpr { BinOp' ($loc, Mult, l, r) }
  | l=AppExpr MULT r=AppExpr { BinOp' ($loc, Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp' ($loc, e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit' ($loc,i) }
  | TRUE   { BLit' ($loc,true) }
  | FALSE  { BLit' ($loc, false) }
  | i=ID   { Var' ($loc,i) }
  | LPAREN e=Expr RPAREN { e }


IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp' ($loc, c, t, e) }
