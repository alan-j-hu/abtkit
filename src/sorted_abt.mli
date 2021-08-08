include module type of Intf (** @inline *)

module Make(Sig : Signature) : S
  with type 'sort sort = 'sort Sig.sort
   and type ('arity, 'sort) operator = ('arity, 'sort) Sig.operator
   and type name = Sig.name
(** Functor building an implementation of abstract binding trees given a
    signature. *)

(**

{1 Example: Simply-typed lambda calculus}

Here is a simple example of how to use this library.

First, create an input module for the language signature:
{[
module STLC = struct
]}

The STLC has two sorts, types and terms:
{[
  type ty = Ty
  type tm = Tm

  type _ sort =
    | Term : tm sort
    | Type : ty sort

  let equal_sorts
    : type s1 s2 any.
      s1 sort
      -> s2 sort
      -> ((s1, s2) Sorted_abt.eq, (s1, s2) Sorted_abt.eq -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)
]}

The sorts are represented as phantom types. The type [_ sort] is a proxy that
holds the type-level sort.

The operators of the language will be [unit] (the unit type), [arrow] (the
function type), [ax] (the unique inhabitant of the unit type), [app] (function
application), and [lam] (function introduction). The operators are listed as a
GADT that contains their sorts and arities.
{[
  type ('arity, 'sort) operator =
    | Unit : (ty Sorted_abt.out, ty) operator
    | Arrow : (ty Sorted_abt.out -> ty Sorted_abt.out -> ty Sorted_abt.out, ty) operator
    | Ax : (tm Sorted_abt.out, tm) operator
    | App : (tm Sorted_abt.out -> tm Sorted_abt.out -> tm Sorted_abt.out, tm) operator
    | Lam : (ty Sorted_abt.out -> (tm -> tm Sorted_abt.out) -> tm Sorted_abt.out, tm) operator

  let equal_sorts
    : type s1 s2 any.
      s1 sort
      -> s2 sort
      -> ((s1, s2) Sorted_abt.eq, (s1, s2) Sorted_abt.eq -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)

  let equal_ops
    : type a1 a2 s.
      (a1, s) operator -> (a2, s) operator -> (a1, a2) Sorted_abt.eq option =
    fun op1 op2 -> match op1, op2 with
      | App, App -> Some Refl
      | Arrow, Arrow -> Some Refl
      | Ax, Ax -> Some Refl
      | Lam, Lam -> Some Refl
      | Unit, Unit -> Some Refl
      | _, _ -> None

  let pp_print_op : type a s. Format.formatter -> (a, s) operator -> unit =
    fun ppf op ->
    Format.pp_print_string ppf begin match op with
      | Unit -> "unit"
      | Arrow -> "arrow"
      | Ax -> "ax"
      | App -> "app"
      | Lam -> "lam"
    end
]}

Finally, variable names are strings:
{[
  type name = string

  let pp_print_name = Format.pp_print_string
end
]}

The [STLCSig] module can be passed to {!module:Make} to implement ABTs for the
STLC.
{[
module Abt = Sorted_abt.Make(STLCSig)

open STLCSig
]}

Create some utility functions for working with results:
{[
let ( let+ ) opt f = Result.map f opt

let ( and+ ) opt1 opt2 = match opt1, opt2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error _ -> Error e

let ( and* ) = ( and+ )

let ( let* ) = Result.bind
]}

This is a function that performs type inference:
{[
let rec infer
    (gamma : (tm Abt.var * ty Sorted_abt.out Abt.t) list)
    (term : tm Sorted_abt.out Abt.t)
  : (ty Sorted_abt.out Abt.t, unit) result =
  match Abt.out term with
  | Op(Ax, Abt.[]) -> Ok (Abt.into (Op(Unit, Abt.[])))
  | Op(Lam, Abt.[in_ty; body]) ->
    let Abs(var, body) = Abt.out body in
    let+ out_ty = infer ((var, in_ty) :: gamma) body in
    Abt.into (Op(Arrow, Abt.[in_ty; out_ty]))
  | Op(App, Abt.[f; arg]) ->
    let* f_ty = infer gamma f
    and* arg_ty = infer gamma arg in
    begin match Abt.out f_ty with
      | Op(Arrow, Abt.[in_ty; out_ty]) ->
        if Abt.equal in_ty arg_ty then
          Ok out_ty
        else
          Error ()
      | _ -> Error ()
    end
  | Var v ->
    match List.assoc_opt v gamma with
    | Some ty -> Ok ty
    | None -> Error ()

let has_ty (term : tm Sorted_abt.out Abt.t) (ty : ty Sorted_abt.out Abt.t) =
  match infer [] term with
  | Ok ty' -> Abt.equal ty ty'
  | Error _ -> false
]}
*)
