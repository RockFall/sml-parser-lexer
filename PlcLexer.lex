(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Converts a String to Int/Bool*)
fun stoi s =
    case Int.fromString s of
          SOME i => i
        | NONE => raise Fail ("Could not convert string to int")

fun stob "true"  = true
  | stob "false" = false
  | stob _       = raise Fail ("Could not convert string to bool")

(* Initialize the lexer. *)
fun init() = ()

%%


%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
letter      = [A-Za-z];
symbol      = [+-/*<>=|\\~^`?!$%&#@:];
digit       = [0-9];

name        = [a-zA-Z_][a-zA-Z_'0-9]*;
symbol      = {symbol}+;

whitespace  = [\ \t];

%%


"\n"              => (lineNumber := !lineNumber + 1; lex());
{whitespace}+     => (lex());


{digit}+          => (CONSTI  (stoi(yytext), yypos, yypos));
"true"            => (CONSTB  (stob(yytext), yypos, yypos));
"false"           => (CONSTB  (stob(yytext), yypos, yypos));

"Nil"             => (NIL     (yypos, yypos));
"Bool"            => (BOOL    (yypos, yypos));
"Int"             => (INT     (yypos, yypos));

"if"              => (IF      (yypos, yypos));
"then"            => (THEN    (yypos, yypos));
"else"            => (ELSE    (yypos, yypos));

"match"           => (MATCH   (yypos, yypos));
"with"            => (WITH    (yypos, yypos));

"var"             => (VAR      (yypos, yypos));
"fn"              => (FN      (yypos, yypos));
"fun"             => (FUN     (yypos, yypos));
"rec"             => (REC     (yypos, yypos));
"end"             => (END     (yypos, yypos));

"hd"              => (HD      (yypos, yypos));
"tl"              => (TL      (yypos, yypos));
"ise"             => (ISE     (yypos, yypos));
"print"           => (PRINT   (yypos, yypos));

"|"               => (BAR     (yypos, yypos));
"_"               => (UNDERBAR(yypos, yypos));
"("               => (LPAR    (yypos, yypos));
")"               => (RPAR    (yypos, yypos));
"["               => (LBRACK  (yypos, yypos));
"]"               => (RBRACK  (yypos, yypos));
"{"               => (LBRACE  (yypos, yypos));
"}"               => (RBRACE  (yypos, yypos));

"->"              => (ARROW   (yypos, yypos));
"=>"              => (DARROW  (yypos, yypos));

","               => (COMMA   (yypos, yypos));
"!"               => (EXCLA(yypos, yypos));
"&&"              => (AND(yypos, yypos));
"+"               => (PLUS(yypos, yypos));
"-"               => (MINUS(yypos, yypos));
"*"               => (MULTI(yypos, yypos));
"/"               => (DIV(yypos, yypos));
"="               => (EQ(yypos, yypos));
"!="              => (NEQ(yypos, yypos));
"<"               => (LESS(yypos, yypos));
"<="              => (LESSEQ(yypos, yypos));
"::"              => (DBCOLON(yypos, yypos));
":"              => (COL(yypos, yypos));
";"               => (SEMIC(yypos, yypos));

{name}            => (NAME(yytext, yypos, yypos));



. => (error("\n %% ERROR %%: Lexer error: invalid character"); raise
Fail("Lexer error: invalid character: " ^yytext));
