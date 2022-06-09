%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC
    | IF | MATCH
    | EXCLA | NEG
    | HD | TL | ISE | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | NEQ
    | LESS | LESSEQ | DBCOLON | SEMIC
    | LBRACK | RBRACK
    | LBRACE | RBRACE
    | LPAR | RPAR
    | NAME of string | CINT of int
    | EOF

%nonterm Prog of expr | Decl | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr

%prefer

%right SEMIC DBCOLON
%left ELSE AND EQ NEQ LESS LESSEQ PLUS MINUS MULTI DIV LBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Decl : VAR NAME EQ Expr ()
    | FUN NAME Args EQ Expr ()
    | FUN REC NAME Args COL Type EQ Expr (makeFun(NAME, Args, TYPE, Expr, Prog))
Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr ()
    | Expr PLUS Expr (Prim1("+", Expr1, Expr2))
