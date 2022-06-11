%%

%name PlcParser

%pos int

%term VAR | FUN | FN | REC
    | IF | THEN | ELSE | MATCH | WITH | END
    | EXCLA | NEG
    | HD | TL | ISE | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | NEQ
    | LESS | LESSEQ
    | LBRACK | RBRACK
    | LBRACE | RBRACE
    | LPAR | RPAR
    | NAME of string | CONSTI of int | CONSTB of bool
    | COMMA | COL | DBCOLON | SEMIC
    | ARROW | DARROW
    | NIL | BOOL | INT
    | BAR | UNDERBAR
    | EOF

%nonterm Start of expr | Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr
    | Comps of expr list | MatchExpr of (expr option * expr) list | CondExpr of expr option
    | Args of (plcType * string) list | Params of (plcType * string) list | TypedVar of (plcType * string) | Type of plcType | AtomType of plcType | Types of plcType list

%prefer

%right SEMIC ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LESS LESSEQ
%right DBCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc EXCLA HD TL ISE PRINT
%left LBRACK

%eop EOF

%noshift EOF

%start Start

%%

Start   : Prog (Prog)

Prog    : Expr (Expr)
        | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
        | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
        | FUN REC NAME Args COL Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr    : AtomExpr (AtomExpr)
        | AppExpr (AppExpr)
        | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
        | MATCH Expr WITH MatchExpr   (Match(Expr, MatchExpr))
        | EXCLA Expr                (Prim1("!", Expr))
        | MINUS Expr                (Prim1("-", Expr))
        | HD Expr                   (Prim1("hd", Expr))
        | TL Expr                   (Prim1("tl", Expr))
        | ISE Expr                  (Prim1("ise", Expr))
        | PRINT Expr                (Prim1("print", Expr))
        | Expr AND Expr             (Prim2("&&", Expr1, Expr2))
        | Expr PLUS Expr            (Prim2("+", Expr1, Expr2))
        | Expr MINUS Expr           (Prim2("-", Expr1, Expr2))
        | Expr MULTI Expr           (Prim2("*", Expr1, Expr2))
        | Expr DIV Expr             (Prim2("/", Expr1, Expr2))
        | Expr EQ Expr              (Prim2("=", Expr1, Expr2))
        | Expr NEQ Expr             (Prim2("!=", Expr1, Expr2))
        | Expr LESS Expr            (Prim2("<", Expr1, Expr2))
        | Expr LESSEQ Expr          (Prim2("<=", Expr1, Expr2))
        | Expr DBCOLON Expr         (Prim2("::", Expr1, Expr2))
        | Expr SEMIC Expr           (Prim2(";", Expr1, Expr2))
        | Expr LBRACK CONSTI RBRACK (Item(CONSTI, Expr))

AtomExpr: Const  (Const)
        | NAME  (Var(NAME))
        | LBRACE Prog RBRACE (Prog)
        | LPAR Expr RPAR  (Expr)
        | LPAR Comps RPAR (List Comps)
        | FN Args DARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
        | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const   : CONSTB (ConB CONSTB)
        | CONSTI (ConI CONSTI)
        | LPAR RPAR (List [])
        | LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Comps   : Expr COMMA Expr ([Expr1, Expr2])
        | Expr COMMA Comps (Expr::Comps)

MatchExpr:END ([])
        | BAR CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr (SOME(Expr))
        | UNDERBAR (NONE)

Args    : LPAR RPAR ([(ListT[], "x")])
        | LPAR Params RPAR (Params)

Params  : TypedVar ([TypedVar])
        | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME ((Type, NAME))

Type    : AtomType (AtomType)
        | LPAR Types RPAR (ListT (Types))
        | LBRACK Type RBRACK (SeqT Type)
        | Type ARROW Type (FunT(Type1, Type2))

AtomType: NIL (ListT [])
        | BOOL (BoolT)
        | INT  (IntT)
        | LPAR Type RPAR (Type)

Types   : Type COMMA Type ([Type1, Type2])
        | Type COMMA Types ([Type]@Types)
