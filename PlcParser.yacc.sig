signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val NIL:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val CINT: (int) *  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val SEMIC:  'a * 'a -> (svalue,'a) token
val DBCOLON:  'a * 'a -> (svalue,'a) token
val COL:  'a * 'a -> (svalue,'a) token
val LESSEQ:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MULTI:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val ISE:  'a * 'a -> (svalue,'a) token
val TL:  'a * 'a -> (svalue,'a) token
val HD:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val EXCLA:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val FUNREC:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end