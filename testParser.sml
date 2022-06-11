(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fromString "2+2";
fromString "true";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end";
fromFile ("example.plc");
fromFile ("testing.plc");

fromString "var x = 4; 2 * x";
fromString "(6,false)";
fromString "1-3; {var x = 4; 2 * x}";
fromString "fn (Int x) => -x end";
fromString "2*2+2";
fromString "2*(2+2)";
fromString "fun f(Int x) = x; f(1)";

use "testParserCases.sml"

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)

fun fromStringWithHandle s =
   (fromString(s))
   handle PlcParser.ParseError => Var "Error found";

fun  test ([], n) = ("ALL WORKING!", "", ConI n)
   | test (((s, e : expr)::t), n) = if fromStringWithHandle(s)=e then test(t, n+1) else ("FAILED!", s, Prim2("expected/caseN", e, ConI n));

test(cases, 1);
cases
("ALL WORKING!", "", ConI 0)

fromString "fun f(Int x, Int y, Int z) = x - y * z ; f(5,4,2)";
fromStringWithHandle "([[Int]] [])";
