%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC
    | IF | THEN | ELSE | MATCH
    | EXCLA | NEG
    | HD | TL | ISE | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | NEQ
    | LESS | LESSEQ | COL | DBCOLON | SEMIC
    | LBRACK | RBRACK
    | LBRACE | RBRACE
    | LPAR | RPAR
    | NAME of string | CONSTI of int | CONSTB of bool
    | COMMA
    | NIL | BOOL | INT
    | EOF

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr
    | Comps of expr | MatchExpr of expr | CondExpr of expr
    | Args of expr | Params of expr | TypedVar of expr | Type of plcType | AtomType of plcType | Types of plcType list

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

Const : CONSTB (ConB CONSTB)
    | CONSTI (ConI CONSTI)
    | LPAR RPAR (List [])
    | LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Type :  AtomType (AtomType)
    | LPAR Types RPAR (ListT (Types))
    | LBRACK AtomType RBRACK (SeqT AtomType)

AtomType : NIL (ListT [])
    | BOOL (BoolT)
    | INT  (IntT)
    | LPAR Type RPAR (Type)

Types : Type COMMA Type ([Type, Type])
    | Type COMMA Types ([Type]@Types)

(*Args : Params ()
Params : TypedVar ()
    | TypedVar COMMA TypedVar ()
TypedVar : Type NAME ()

*)
