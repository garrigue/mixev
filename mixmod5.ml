(* $Id: mixmod5.ml,v 1.3 2013/08/15 06:53:58 garrigue Exp $ *)

(* Basic interfaces *)

(* The types involved in our recursion *)
module type ET = sig type exp end
(* The recursive operations on our our types *)
module type E =
  sig
    include ET
    val eval : (string * exp) list -> exp -> exp
  end
(* Utility functor to extract useful types in its argument *)
module Types(X : sig type exp type a end) =
  struct type exp = X.exp type a = X.a end

(* Variables are common to lambda and expr *)

module VarT = struct
  type exp = [`Var of string]
end
module type VarS = sig
  type exp = private [> VarT.exp]
  include E with type exp := exp
end
module Var(E : VarS) =
  struct
    type exp0 = VarT.exp
    type exp = E.exp
    let eval sub (`Var s as v : exp0) : exp =
      try List.assoc s sub with Not_found -> v
  end


(* The lambda language: free variables, substitutions, and evaluation *)

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

(* Concrete definition of types in this language, with parameters to
   allow extension *)
module LamT = struct
  (* Define types using only one parameter for recursion; *)
  (* actual types are then extracted through the constraint *)
  type 'a exp = [VarT.exp | `Abs of string * 'e | `App of 'e * 'e]
  constraint 'a = <exp:'e;..>
end
(* Signature for parameters to the language construction functor *)
module type LamS = sig
  (* Close the recursion creating row-abstract types *)
  type exp = private [> a LamT.exp]
  and a = <exp:exp>
  include E with type exp := exp
end
module Lam(E : LamS) =
  struct
    type exp0 = E.a LamT.exp
    include Types(E)
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

(* Signature for an actual module *)
module type LamF = sig
  (* Close the recursion creating concrete types *)
  type exp = a LamT.exp
  and a = <exp:exp>
  include E with type exp := exp
end
(* The actual language is a fix-point of the construction functor *)
module rec LamF : LamF = Lam(LamF)
let e1 = LamF.eval [] (`App(`Abs("x",`Var"x"), `Var"y"));;


(* The expr language of arithmetic expressions *)

(* Define the concrete types in the language *)
module ExprT = struct
  type 'a exp =
      [ `Var of string | `Num of int | `Add of 'e * 'e | `Mult of 'e * 'e]
  constraint 'a = <exp:'e;..>
end
(* Exactly the same boilerplate as for Lam *)
module type ExprS = sig
  type exp = private [> a ExprT.exp]
  and a = <exp:exp>
  include E with type exp := exp
end
module Expr(E : ExprS) =
  struct
    type exp0 = E.a ExprT.exp
    include Types(E)
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

module type ExprF = sig
  type exp = a ExprT.exp
  and a = <exp:exp>
  include E with type exp := exp
end
module rec ExprF : ExprF = Expr(ExprF)
let e2 = ExprF.eval [] (`Add(`Mult(`Num 3, `Num 2), `Var"x"));;


(* The lexpr language, reunion of lambda and expr *)

module LExprT = struct
  (* We don't need to write the constraint here, as we don't use
     'e specifically *)
  type 'a exp = [ 'a LamT.exp | 'a ExprT.exp ]
end
module type LExprS = sig
  type exp = private [> a LExprT.exp]
  and a = <exp:exp>
  include E with type exp := exp
end
module LExpr(E : LExprS) =
  struct
    include Types(E)
    type exp0 = E.a LExprT.exp
    module SLam = Lam(E)
    module SExpr = Expr(E)

    let eval subst : exp0 -> exp = function
        #SLam.exp0 as x -> SLam.eval subst x
      | #SExpr.exp0 as x -> SExpr.eval subst x
  end

module type LExprF = sig
  type exp = a LExprT.exp
  and a = <exp:exp>
  include E with type exp := exp
end
module rec LExprF : LExprF = LExpr(LExprF)
let e3 =
  LExprF.eval [] (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
