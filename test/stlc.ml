(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Sig = struct
  type ty = Ty
  type tm = Tm

  type 'sort sort =
    | Term : tm sort
    | Type : ty sort

  let equal_sorts
    : type s1 s2 any.
      s1 sort
      -> s2 sort
      -> ((s1, s2) Abtkit.eq, (s1, s2) Abtkit.eq -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)

  type ('arity, 'sort) operator =
    | Unit : (ty Abtkit.ar, ty) operator
    | Arrow : (ty Abtkit.va -> ty Abtkit.va -> ty Abtkit.ar, ty) operator
    | Ax : (tm Abtkit.ar, tm) operator
    | App : (tm Abtkit.va -> tm Abtkit.va -> tm Abtkit.ar, tm) operator
    | Lam : (ty Abtkit.va -> (tm -> tm Abtkit.va) -> tm Abtkit.ar, tm) operator

  let equal_ops
    : type a1 a2 s.
      (a1, s) operator -> (a2, s) operator -> (a1, a2) Abtkit.eq option =
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

module Syn = Abtkit.Make(Sig)

open Sig

let ( let+ ) res f = Result.map f res

let ( and+ ) res1 res2 = match res1, res2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error _ -> Error e

let ( and* ) = ( and+ )

let ( let* ) = Result.bind

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

type progress = Step of tm Abtkit.va Syn.t | Val | Err

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

let has_ty (term : tm Abtkit.va Syn.t) (ty : ty Abtkit.va Syn.t) =
  match infer [] term with
  | Ok ty' -> Syn.aequiv ty ty'
  | Error _ -> false

let () =
  let unit_type = Syn.op Unit Syn.[] in
  let unit_arr_unit = Syn.op Arrow Syn.[unit_type; unit_type] in
  let ax = Syn.op Ax Syn.[] in
  let id_unit =
    Syn.op Lam Syn.[ Syn.op Unit Syn.[]
                   ; let x = Syn.fresh_var Term "x" in
                     Syn.abs x (Syn.var x) ]
  in
  let ret_id_unit =
    Syn.op Lam Syn.[ Syn.op Unit Syn.[]
                   ; let y = Syn.fresh_var Term "y" in
                     Syn.abs y id_unit ]
  in
  assert (has_ty ax unit_type);
  assert (has_ty id_unit unit_arr_unit);
  assert (cbv ax = Val);
  assert (cbv (Syn.op App Syn.[id_unit; ax]) = Step ax);
  assert (cbv (Syn.op App Syn.[ret_id_unit; ax]) = Step id_unit);
  assert (to_string unit_arr_unit = "arrow(unit();unit())");
  assert (to_string id_unit = "lam(unit();x.x)");
  assert (to_string ret_id_unit = "lam(unit();y.lam(unit();x.x))")
