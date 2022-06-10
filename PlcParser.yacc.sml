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
\\001\000\001\000\007\000\004\000\006\000\000\000\
\\001\000\004\000\006\000\000\000\
\\001\000\014\000\008\000\024\000\014\000\000\000\
\\001\000\018\000\012\000\000\000\
\\001\000\031\000\010\000\000\000\
\\001\000\033\000\000\000\000\000\
\\017\000\014\000\008\000\000\000\
\\018\000\000\000\
\\022\000\000\000\
\\023\000\000\000\
\\024\000\014\000\008\000\000\000\
\\025\000\000\000\
\"
val actionRowNumbers =
"\000\000\009\000\008\000\006\000\
\\001\000\004\000\001\000\010\000\
\\003\000\011\000\001\000\002\000\
\\000\000\007\000\005\000"
val gotoT =
"\
\\001\000\014\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\007\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\003\000\009\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\011\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\001\000\013\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 15
val numrules = 16
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
 | CINT of unit ->  (int) | NAME of unit ->  (string)
 | Params of unit ->  (expr) | TypedVar of unit ->  (expr)
 | Type of unit ->  (expr) | AtomType of unit ->  (expr)
 | Args of unit ->  (expr) | Const of unit ->  (expr)
 | AppExpr of unit ->  (expr) | AtomExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Prog of unit ->  (expr)
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
fn (T 32) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "FUNREC"
  | (T 3) => "IF"
  | (T 4) => "ELSE"
  | (T 5) => "MATCH"
  | (T 6) => "EXCLA"
  | (T 7) => "NEG"
  | (T 8) => "HD"
  | (T 9) => "TL"
  | (T 10) => "ISE"
  | (T 11) => "PRINT"
  | (T 12) => "AND"
  | (T 13) => "PLUS"
  | (T 14) => "MINUS"
  | (T 15) => "MULTI"
  | (T 16) => "DIV"
  | (T 17) => "EQ"
  | (T 18) => "NEQ"
  | (T 19) => "LESS"
  | (T 20) => "LESSEQ"
  | (T 21) => "COL"
  | (T 22) => "DBCOLON"
  | (T 23) => "SEMIC"
  | (T 24) => "LBRACK"
  | (T 25) => "RBRACK"
  | (T 26) => "LBRACE"
  | (T 27) => "RBRACE"
  | (T 28) => "LPAR"
  | (T 29) => "RPAR"
  | (T 30) => "NAME"
  | (T 31) => "CINT"
  | (T 32) => "EOF"
  | (T 33) => "COMMA"
  | (T 34) => "NIL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
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
|  ( 2, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: ( _, ( 
MlyValue.NAME NAME1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  NAME1 = NAME1
 ()
 val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, VAR1left, Expr1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: ( _, ( 
MlyValue.Args Args1, _, _)) :: ( _, ( MlyValue.NAME NAME1, _, _)) :: (
 _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  NAME1 = NAME1 ()
 val  Args1 = Args1 ()
 val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, FUN1left, Expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _))
 :: ( _, ( MlyValue.NAME NAME1, _, _)) :: ( _, ( _, FUNREC1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (NAME as NAME1) = NAME1 ()
 val  (Args as Args1) = Args1 ()
 val  (Type as Type1) = Type1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (makeFun(NAME, Args, Type, Expr, Prog))
end; ()))
 in ( LrTable.NT 1, ( result, FUNREC1left, Expr1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, 
AtomExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (AtomExpr)
end)
 in ( LrTable.NT 2, ( result, AtomExpr1left, AtomExpr1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 2, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  Expr1 = Expr1 ()
 in ()
end)
 in ( LrTable.NT 2, ( result, IF1left, Expr1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim1("+", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Params Params1, Params1left, Params1right))
 :: rest671)) => let val  result = MlyValue.Args (fn _ => let val  
Params1 = Params1 ()
 in ()
end)
 in ( LrTable.NT 6, ( result, Params1left, Params1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  TypedVar1 = TypedVar1 ()
 in ()
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 11, ( ( _, ( MlyValue.TypedVar TypedVar2, _, TypedVar2right)) ::
 _ :: ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: 
rest671)) => let val  result = MlyValue.Params (fn _ => let val  
TypedVar1 = TypedVar1 ()
 val  TypedVar2 = TypedVar2 ()
 in ()
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, TypedVar2right), rest671
)
end
|  ( 12, ( ( _, ( MlyValue.NAME NAME1, _, NAME1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  Type1 = Type1 ()
 val  NAME1 = NAME1 ()
 in ()
end)
 in ( LrTable.NT 9, ( result, Type1left, NAME1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.AtomType AtomType1, AtomType1left, 
AtomType1right)) :: rest671)) => let val  result = MlyValue.Type (fn _
 => let val  AtomType1 = AtomType1 ()
 in ()
end)
 in ( LrTable.NT 8, ( result, AtomType1left, AtomType1right), rest671)

end
|  ( 14, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.AtomType 
AtomType1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let
 val  result = MlyValue.Type (fn _ => let val  AtomType1 = AtomType1
 ()
 in ()
end)
 in ( LrTable.NT 8, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 15, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => ())
 in ( LrTable.NT 7, ( result, NIL1left, NIL1right), rest671)
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
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun COL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun DBCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun CINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.CINT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
