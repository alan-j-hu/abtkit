(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module STLC_Sig = struct
  type ty = Ty
  type tm = Tm

  type 'sort sort =
    | Term : tm sort
    | Type : ty sort

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

  type name = string

  let pp_print_name = Format.pp_print_string
end

module Abt = Sorted_abt.Make(STLC_Sig)

open STLC_Sig

let ( let+ ) opt f = Result.map f opt

let ( and+ ) opt1 opt2 = match opt1, opt2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error _ -> Error e

let ( and* ) = ( and+ )

let ( let* ) = Result.bind

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

type progress = Step of tm Sorted_abt.out Abt.t | Val | Err

let rec cbv (term : tm Sorted_abt.out Abt.t) =
  match Abt.out term with
  | Op(Ax, Abt.[]) -> Val
  | Op(Lam, Abt.[_; _]) -> Val
  | Op(App, Abt.[f; arg]) ->
    begin match cbv f with
      | Step next -> Step (Abt.op App Abt.[next; arg])
      | Val ->
        begin match cbv arg with
          | Step next -> Step (Abt.op App Abt.[f; next])
          | Val ->
            begin match Abt.out f with
              | Op(Lam, Abt.[_; abs]) ->
                let Abs(var, body) = Abt.out abs in
                Step (body |> Abt.subst Term begin fun var' ->
                    match Abt.equal_vars var var' with
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

let unit_type = Abt.into (Abt.Op(Unit, Abt.[]))

let to_string margin term =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Abt.pp_print ppf term;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let () =
  let unit_arr_unit = Abt.into (Abt.Op(Arrow, Abt.[unit_type; unit_type])) in
  let ax = Abt.op Ax Abt.[] in
  let id_unit =
    Abt.op Lam Abt.[ Abt.op Unit Abt.[]
                   ; let x = Abt.fresh_var Term "x" in
                     Abt.abs x (Abt.var x) ]
  in
  let ret_id_unit =
    Abt.op Lam Abt.[ Abt.op Unit Abt.[]
                   ; let y = Abt.fresh_var Term "y" in
                     Abt.abs y id_unit ]
  in
  assert (has_ty id_unit unit_arr_unit);
  assert (cbv ax = Val);
  assert (cbv (Abt.op App Abt.[id_unit; ax]) = Step ax);
  assert (cbv (Abt.op App Abt.[ret_id_unit; ax]) = Step id_unit);
  assert (to_string 78 unit_arr_unit = "arrow(unit();unit())");
  assert (to_string 16 unit_arr_unit = "arrow(unit();\n      unit())");
  assert (to_string 78 id_unit = "lam(unit();x.x)");
  assert (to_string 78 ret_id_unit = "lam(unit();y.lam(unit();x.x))")
