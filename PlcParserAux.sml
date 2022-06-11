(* Plc Parser Aux *)

(* Creat the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr =
  case xs of
    ((t, xn)::[]) => Let(xn, Item(n, Var "$list"), e)
  | ((t, xn)::tl) => Let(xn, Item(n, Var "$list"), makeFunAux(n+1, tl, e));

(* Create the list of arguments of a function. *)
fun separateTypes (args: (plcType * string) list) =
  case args of
      (t, s)::[] => [t]
    | (t, s)::tl => (t::separateTypes(tl));

fun makeType (args: (plcType * string) list): plcType =
    ListT(separateTypes(args));

(* Create a function expression. *)
(* [Int "x", String "s"] *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType xs
        val e1' = makeFunAux (1, xs, e1)
      in
        Letrec(f, t, "$list", rt, e1', e2)
      end;

(* Create a Anonymus function expression. *)
fun makeAnon (xs:(plcType * string) list, e:expr):expr =
  case xs of
      [] => Anon(ListT [], "()", e)
    | (t,x)::[] => Anon(t,x,e)
    | _ =>
      let
        val t = makeType xs
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;
