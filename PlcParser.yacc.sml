functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\010\000\030\000\009\000\032\000\008\000\033\000\007\000\
\\034\000\006\000\000\000\
\\001\000\015\000\011\000\025\000\042\000\000\000\
\\001\000\015\000\011\000\031\000\024\000\000\000\
\\001\000\019\000\029\000\000\000\
\\001\000\026\000\020\000\030\000\019\000\031\000\018\000\032\000\008\000\
\\033\000\007\000\034\000\006\000\036\000\017\000\037\000\016\000\
\\038\000\015\000\000\000\
\\001\000\026\000\020\000\030\000\036\000\036\000\017\000\037\000\016\000\
\\038\000\015\000\000\000\
\\001\000\026\000\023\000\000\000\
\\001\000\026\000\023\000\031\000\033\000\035\000\032\000\000\000\
\\001\000\027\000\030\000\000\000\
\\001\000\027\000\034\000\000\000\
\\001\000\030\000\009\000\032\000\008\000\033\000\007\000\034\000\006\000\000\000\
\\001\000\030\000\028\000\036\000\017\000\037\000\016\000\038\000\015\000\000\000\
\\001\000\031\000\031\000\000\000\
\\001\000\031\000\033\000\000\000\
\\001\000\031\000\033\000\035\000\032\000\000\000\
\\001\000\031\000\038\000\000\000\
\\001\000\032\000\021\000\000\000\
\\001\000\039\000\000\000\000\000\
\\045\000\015\000\011\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\035\000\032\000\000\000\
\\064\000\000\000\
\"
val actionRowNumbers =
"\000\000\022\000\020\000\018\000\
\\025\000\026\000\023\000\004\000\
\\016\000\010\000\029\000\006\000\
\\002\000\034\000\033\000\032\000\
\\027\000\004\000\011\000\003\000\
\\021\000\008\000\024\000\012\000\
\\007\000\009\000\005\000\010\000\
\\015\000\030\000\005\000\035\000\
\\031\000\013\000\005\000\001\000\
\\028\000\037\000\036\000\014\000\
\\000\000\019\000\017\000"
val gotoT =
"\
\\001\000\042\000\003\000\003\000\004\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\012\000\004\000\002\000\006\000\001\000\013\000\011\000\
\\014\000\010\000\000\000\
\\000\000\
\\003\000\020\000\004\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\012\000\004\000\002\000\006\000\001\000\013\000\024\000\
\\014\000\010\000\015\000\023\000\000\000\
\\014\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\033\000\014\000\010\000\000\000\
\\003\000\035\000\004\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\013\000\038\000\014\000\010\000\015\000\037\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\039\000\014\000\010\000\015\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\041\000\003\000\003\000\004\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 43
val numrules = 20
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | CONSTB of unit ->  (bool) | CONSTI of unit ->  (int)
 | NAME of unit ->  (string) | Types of unit ->  (plcType list)
 | AtomType of unit ->  (plcType) | Type of unit ->  (plcType)
 | TypedVar of unit ->  (expr) | Params of unit ->  (expr)
 | Args of unit ->  (expr) | CondExpr of unit ->  (expr)
 | MatchExpr of unit ->  (expr) | Comps of unit ->  (expr)
 | Const of unit ->  (expr) | AppExpr of unit ->  (expr)
 | AtomExpr of unit ->  (expr) | Expr of unit ->  (expr)
 | Decl of unit ->  (expr) | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 38) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "FUNREC"
  | (T 3) => "IF"
  | (T 4) => "THEN"
  | (T 5) => "ELSE"
  | (T 6) => "MATCH"
  | (T 7) => "EXCLA"
  | (T 8) => "NEG"
  | (T 9) => "HD"
  | (T 10) => "TL"
  | (T 11) => "ISE"
  | (T 12) => "PRINT"
  | (T 13) => "AND"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "MULTI"
  | (T 17) => "DIV"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LESS"
  | (T 21) => "LESSEQ"
  | (T 22) => "COL"
  | (T 23) => "DBCOLON"
  | (T 24) => "SEMIC"
  | (T 25) => "LBRACK"
  | (T 26) => "RBRACK"
  | (T 27) => "LBRACE"
  | (T 28) => "RBRACE"
  | (T 29) => "LPAR"
  | (T 30) => "RPAR"
  | (T 31) => "NAME"
  | (T 32) => "CONSTI"
  | (T 33) => "CONSTB"
  | (T 34) => "COMMA"
  | (T 35) => "NIL"
  | (T 36) => "BOOL"
  | (T 37) => "INT"
  | (T 38) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.NAME NAME1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Prog (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(NAME, Expr, Prog))
end)
 in ( LrTable.NT 0, ( result, VAR1left, Prog1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, 
AtomExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (AtomExpr)
end)
 in ( LrTable.NT 2, ( result, AtomExpr1left, AtomExpr1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 3, ( result, Const1left, Const1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var(NAME))
end)
 in ( LrTable.NT 3, ( result, NAME1left, NAME1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.CONSTB CONSTB1, CONSTB1left, CONSTB1right))
 :: rest671)) => let val  result = MlyValue.Const (fn _ => let val  (
CONSTB as CONSTB1) = CONSTB1 ()
 in (ConB CONSTB)
end)
 in ( LrTable.NT 5, ( result, CONSTB1left, CONSTB1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.CONSTI CONSTI1, CONSTI1left, CONSTI1right))
 :: rest671)) => let val  result = MlyValue.Const (fn _ => let val  (
CONSTI as CONSTI1) = CONSTI1 ()
 in (ConI CONSTI)
end)
 in ( LrTable.NT 5, ( result, CONSTI1left, CONSTI1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => (List []))
 in ( LrTable.NT 5, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAR1right)) :: _ :: _ :: ( _, ( MlyValue.Type
 Type1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => let val  (Type as Type1) = Type1 ()
 in (ESeq(Type))
end)
 in ( LrTable.NT 5, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.AtomType AtomType1, AtomType1left, 
AtomType1right)) :: rest671)) => let val  result = MlyValue.Type (fn _
 => let val  (AtomType as AtomType1) = AtomType1 ()
 in (AtomType)
end)
 in ( LrTable.NT 12, ( result, AtomType1left, AtomType1right), rest671
)
end
|  ( 12, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Types Types1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT (Types))
end)
 in ( LrTable.NT 12, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.AtomType 
AtomType1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let
 val  result = MlyValue.Type (fn _ => let val  (AtomType as AtomType1)
 = AtomType1 ()
 in (SeqT AtomType)
end)
 in ( LrTable.NT 12, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 14, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (ListT []))
 in ( LrTable.NT 13, ( result, NIL1left, NIL1right), rest671)
end
|  ( 15, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (BoolT))
 in ( LrTable.NT 13, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 16, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (IntT))
 in ( LrTable.NT 13, ( result, INT1left, INT1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomType (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 13, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  Type2 = Type2 ()
 in ([Type, Type])
end)
 in ( LrTable.NT 14, ( result, Type1left, Type2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Types as Types1) = Types1 ()
 in ([Type]@Types)
end)
 in ( LrTable.NT 14, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun COL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun DBCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun CONSTI (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.CONSTI (fn () => i),p1,p2))
fun CONSTB (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.CONSTB (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
end
end
