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

fun stoi s =
    case Int.fromString s of
          SOME i => i
        | NONE => raise Fail ("Could not convert string to int")

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
name=[a-zA-Z_][a-zA-Z_0-9]*;
whitespace=[\ \t];

%%
\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(stoi(yytext), yypos, yypos));

"+" => (PLUS(yypos, yypos));
. => (error("\n %% ERROR %%: Lexer error: invalid character"); raise
Fail("Lexer error: invalid character: " ^yytext));
