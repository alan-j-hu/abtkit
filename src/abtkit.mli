(** Many-sorted abstract binding trees.

    Abtkit is an implementation of many-sorted abstract binding trees.
    Abstract binding trees (ABTs) are similar to abstract syntax trees, but
    also keep track of variable scopes. Many-sorted ABTs support multiple
    syntactic classes, known as sorts. This library uses GADTs and phantom
    types to statically ensure that only syntactically valid ABTs are
    representable. *)

include module type of Intf (** @inline *)

module Make(Sig : Signature) : S
  with type 'sort Sort.t = 'sort Sig.Sort.t
   and type ('arity, 'sort) Operator.t = ('arity, 'sort) Sig.Operator.t
   and type Name.t = Sig.Name.t
(** Functor building an implementation of abstract binding trees given a
    signature. *)

(**

{1 Example: Simply Typed Lambda Calculus}

Here is an example of using this library to create binding trees for the
simply typed lambda calculus (STLC).

{2 Signature}

First, create an input module for the language signature:
{[
module Sig = struct
]}

The STLC has two sorts, types and terms:
{[
  type ty = Ty
  type tm = Tm

  module Sort = struct
    type 'sort t =
      | Term : tm t
      | Type : ty t

    let equal
      : type s1 s2 any.
        s1 t -> s2 t
        -> ((s1, s2) Abtkit.eq, (s1, s2) Abtkit.eq -> any) Either.t =
      fun s1 s2 -> match s1, s2 with
        | Term, Term -> Left Refl
        | Term, Type -> Right (function _ -> .)
        | Type, Type -> Left Refl
        | Type, Term -> Right (function _ -> .)
  end
]}

The sorts are represented as phantom types. The type [_ sort] is a proxy that
holds the type-level sort.

The operators of the language are [unit] (the unit type), [arrow] (the
function type), [ax] (the unique inhabitant of the unit type), [app] (function
application), and [lam] (function introduction). The operators are listed as a
GADT that contains their sorts and arities.
{[
  module Operator = struct
    type ('arity, 'sort) t =
      | Unit : (ty Abtkit.ar, ty) t
      | Arrow : (ty Abtkit.va -> ty Abtkit.va -> ty Abtkit.ar, ty) t
      | Ax : (tm Abtkit.ar, tm) t
      | App : (tm Abtkit.va -> tm Abtkit.va -> tm Abtkit.ar, tm) t
      | Lam : (ty Abtkit.va -> (tm -> tm Abtkit.va) -> tm Abtkit.ar, tm) t

    let equal
      : type a1 a2 s. (a1, s) t -> (a2, s) t -> (a1, a2) Abtkit.eq option =
      fun op1 op2 -> match op1, op2 with
        | App, App -> Some Refl
        | Arrow, Arrow -> Some Refl
        | Ax, Ax -> Some Refl
        | Lam, Lam -> Some Refl
        | Unit, Unit -> Some Refl
        | _, _ -> None

    let to_string : type a s. (a, s) t -> string = function
      | Unit -> "unit"
      | Arrow -> "arrow"
      | Ax -> "ax"
      | App -> "app"
      | Lam -> "lam"
  end

Finally, variable names are strings:
{[
  module Name = struct
    type t = string

    let to_string = Fun.id
  end
end
]}

The [Sig] module can be passed to {!module:Make} to implement ABTs for the
STLC.
{[
module Syn = Abtkit.Make(Sig)

open Sig
]}

{2 Static Semantics}

The following are utility functions for working with results:
{[
let ( let+ ) res f = Result.map f res

let ( and+ ) res1 res2 = match res1, res2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error _ -> Error e

let ( and* ) = ( and+ )

let ( let* ) = Result.bind
]}

Create a function for performing type inference:
{[
let to_string term =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Syn.pp_print ppf term;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let rec infer
    (gamma : (tm Syn.var * ty Abtkit.va Syn.t) list)
    (term : tm Abtkit.va Syn.t)
  : (ty Abtkit.va Syn.t, string) result =
  match Syn.out term with
  | Op(Ax, Syn.[]) -> Ok (Syn.op Unit Syn.[])
  | Op(Lam, Syn.[in_ty; abstr]) ->
    let Abs(var, body) = Syn.out abstr in
    let+ out_ty = infer ((var, in_ty) :: gamma) body in
    Syn.op Arrow Syn.[in_ty; out_ty]
  | Op(App, Syn.[f; arg]) ->
    let* f_ty = infer gamma f
    and* arg_ty = infer gamma arg in
    begin match Syn.out f_ty with
      | Op(Arrow, Syn.[in_ty; out_ty]) ->
        if Syn.aequiv in_ty arg_ty then
          Ok out_ty
        else
          Error ("Expected argument of type " ^ to_string in_ty ^
                 ", got argument of type " ^ to_string arg_ty ^ "!")
      | _ ->
        Error ("Expected function, got term of type " ^ to_string f_ty ^ "!")
    end
  | Var v -> Ok (List.assoc v gamma)
]}

{2 Dynamic Semantics}

For the dynamic semantics, we will define a small-step interpreter. An
interpreter result can either be a step, a value, or an error:
{[
type progress = Step of tm Abtkit.va Syn.t | Val | Err
]}

The interpreter shall use call-by-value (CBV), meaning that function arguments
are evaluated to values before being substituted into the function.
{[
let rec cbv (term : tm Abtkit.va Syn.t) =
  match Syn.out term with
  | Op(Ax, Syn.[]) -> Val
  | Op(Lam, Syn.[_; _]) -> Val
  | Op(App, Syn.[f; arg]) ->
    begin match cbv f with
      | Step next -> Step (Syn.op App Syn.[next; arg])
      | Val ->
        begin match cbv arg with
          | Step next -> Step (Syn.op App Syn.[f; next])
          | Val ->
            begin match Syn.out f with
              | Op(Lam, Syn.[_; abstr]) ->
                let Abs(var, body) = Syn.out abstr in
                Step (body |> Syn.subst Term begin fun var' ->
                    match Syn.equal_vars var var' with
                    | Some Refl -> Some arg
                    | None -> None
                  end)
              | _ -> Err
            end
          | Err -> Err
        end
      | Err -> Err
    end
  | Var _ -> Err
]}
*)
