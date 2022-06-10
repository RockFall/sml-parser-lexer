%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC
    | IF | ELSE | MATCH
    | EXCLA | NEG
    | HD | TL | ISE | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | NEQ
    | LESS | LESSEQ | COL | DBCOLON | SEMIC
    | LBRACK | RBRACK
    | LBRACE | RBRACE
    | LPAR | RPAR
    | NAME of string | CINT of int
    | EOF
    | COMMA
    | NIL

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr
    | Comps of expr | MatchExpr of expr | CondExpr of expr
    | Args of expr | Params of expr | TypedVar of expr | Type of expr | AtomType of expr | Types of expr

%prefer

%right SEMIC DBCOLON
%left ELSE AND EQ NEQ LESS LESSEQ PLUS MINUS MULTI DIV LBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
(*
Decl : VAR NAME EQ Expr ()
     | FUN NAME Args EQ Expr ()
    | FUNREC NAME Args COL Type EQ Expr (makeFun(NAME, Args, Type, Expr, Prog))*)

Expr : AtomExpr (AtomExpr)
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAR Expr RPAR (Expr)

Const : CINT (ConI CINT)

(*Args : Params ()
Params : TypedVar ()
    | TypedVar COMMA TypedVar ()
TypedVar : Type NAME ()
Type :  AtomType ()
    |   LBRACK AtomType RBRACK ()
AtomType :  NIL ()*)
