(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Stlc_sig = struct
  type ty = Ty
  type tm = Tm

  type 'sort sort =
    | Term : tm sort
    | Type : ty sort

  type ('arity, 'sort) operator =
    | Unit : (ty Sorted_abt.out, ty) operator
    | Arrow : (ty -> ty -> ty Sorted_abt.out, ty) operator
    | Ax : (tm Sorted_abt.out, tm) operator
    | App : (tm -> tm -> tm Sorted_abt.out, tm) operator
    | Lam : (ty -> (tm -> tm) -> tm Sorted_abt.out, tm) operator

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
end

module Abt = Sorted_abt.Make(Stlc_sig)

open Stlc_sig

let unit_type = Abt.into (Abt.Op(Unit, Abt.[]))

let unit_arr_unit =
  Abt.into (Abt.Op(Arrow, Abt.[unit_type; unit_type]))

let create_unit_id () =
  let x = Abt.fresh_var Term in
  let xv = Abt.into (Abt.Var x) in
  let abstr = Abt.into (Abt.Abs(x, xv)) in
  Abt.into (Abt.Op(Lam, Abt.[unit_type; abstr]))

let rec equal_types (ty1 : ty Abt.t) (ty2 : ty Abt.t) =
  match Abt.out ty1, Abt.out ty2 with
  | Op(Arrow, Abt.[a; b]), Op(Arrow, Abt.[c; d]) ->
    equal_types a c && equal_types b d
  | Op(Arrow, Abt.[_; _]), Op(Unit, Abt.[]) -> false
  | Op(Unit, Abt.[]), Op(Arrow, Abt.[_; _]) -> false
  | Op(Unit, Abt.[]), Op(Unit, Abt.[]) -> true
  | Var _, Op _ -> false
  | Op _, Var _ -> false
  | Var _, Var _ -> failwith "Unreachable!"

let to_string margin term =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Abt.pp_print ppf term;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let () =
  assert (create_unit_id () = create_unit_id ());
  assert (equal_types unit_type unit_type);
  assert (equal_types unit_arr_unit unit_arr_unit);
  assert (equal_types unit_arr_unit unit_type = false);
  assert (equal_types unit_type unit_arr_unit = false);
  let x = Abt.fresh_var Term in
  let xv = Abt.into (Abt.Var x) in
  assert (Abt.subst Term (fun var ->
      match Abt.equal_vars var x with
      | Some Refl -> Some (create_unit_id ())
      | None -> None
    ) xv = create_unit_id ());
  assert (Abt.equal (create_unit_id ()) (create_unit_id ()));
  assert (to_string 78 unit_arr_unit = "arrow(unit();unit())");
  assert (to_string 16 unit_arr_unit = "arrow(unit();\n      unit())")
