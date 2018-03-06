(* $Id: mixmod3.ml,v 1.2 2005/02/22 10:31:08 garrigue Exp $ *)

(* Similar to mixin2.ml, but use recursive modules in place of classes *)

(* Basic interfaces *)

(* The types involved in our recursion *)
module type ET = sig type exp end
(* The recursive operations on our our types *)
module type E =
  sig
    include ET
    val eval : (string * exp) list -> exp -> exp
  end

(* Variables are common to lambda and expr *)

module VarT = struct
  type exp = [`Var of string]
end
module Var(E : E with type exp as [> VarT.exp]) =
  struct
    type exp0 = VarT.exp
    type exp = E.exp
    let eval sub (`Var s as v : exp0) : exp =
      try List.assoc s sub with Not_found -> v
  end

(* The lambda language: free variables, substitutions, and evaluation *)
let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

module LamT = struct
  type 'e exp = [VarT.exp | `Abs of string * 'e | `App of 'e * 'e]
end
module Lam(E : E with type exp as ([> 'e LamT.exp] as 'e)) =
  struct
    type exp0 = E.exp LamT.exp
    type exp = E.exp
    module LVar = Var(E)

    let eval subst : exp0 -> exp = function
        #LVar.exp0 as v -> LVar.eval subst v
      | `App(l1, l2) ->
          let l2' = E.eval subst l2 in
          let l1' = E.eval subst l1 in
          begin match l1' with
            `Abs (s, body) ->
              E.eval [s,l2'] body
          | _ ->
              `App (l1', l2')
          end
      | `Abs(s, l1) ->
          let s' = gensym () in
          `Abs(s', E.eval ((s,`Var s')::subst) l1)
  end

module rec LamF : E with type exp = LamF.exp LamT.exp = Lam(LamF)
let e1 = LamF.eval [] (`App(`Abs("x",`Var"x"), `Var"y"));;

(* The expr language of arithmetic expressions *)

module ExprT = struct
  type 'e exp =
      [ `Var of string | `Num of int | `Add of 'e * 'e | `Mult of 'e * 'e]
end

module Expr(E : E with type exp as ([> 'e ExprT.exp] as 'e)) =
  struct
    type exp0 = E.exp ExprT.exp
    type exp = E.exp
    module LVar = Var(E)

    let map f : exp0 -> exp = function
        #LVar.exp0 | `Num _ as e -> e
      | `Add(e1, e2) -> `Add (f e1, f e2)
      | `Mult(e1, e2) -> `Mult (f e1, f e2)

    let eval subst (e : exp0) =
      let e' = map (E.eval subst) e in
      match e' with
        #LVar.exp0 as v -> LVar.eval subst v
      | `Add(e1, e2) ->
          begin match e1, e2 with
            `Num m, `Num n -> `Num (m+n)
          | _ -> e'
          end
      | `Mult(e1, e2) ->
          begin match e1, e2 with
            `Num m, `Num n -> `Num (m*n)
          | _ -> e'
          end
      | _ -> e'
  end

module rec ExprF : E with type exp = ExprF.exp ExprT.exp = Expr(ExprF)
let e2 = ExprF.eval [] (`Add(`Mult(`Num 3, `Num 2), `Var"x"));;

(* The lexpr language, reunion of lambda and expr *)

module LExprT = struct
  type 'e exp = [ 'e LamT.exp | 'e ExprT.exp ]
end
module LExpr(E : E with type exp as ([> 'e LExprT.exp] as 'e)) =
  struct
    type exp0 = E.exp LExprT.exp
    type exp = E.exp
    module SLam = Lam(E)
    module SExpr = Expr(E)

    let eval subst = function
        #SLam.exp0 as x -> SLam.eval subst x
      | #SExpr.exp0 as x -> SExpr.eval subst x
  end

module rec LExprF : E with type exp = LExprF.exp LExprT.exp = LExpr(LExprF)
let e3 =
  LExprF.eval [] (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
