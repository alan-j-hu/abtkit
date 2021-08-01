(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Input = struct
  type ty = Ty
  type tm = Tm

  type 'sort sort =
    | Term : tm sort
    | Type : ty sort

  type ('arity, 'sort) operator =
    | Unit : (ty, ty) operator
    | Arrow : (ty -> ty -> ty, ty) operator
    | Ax : (tm, tm) operator
    | App : (tm -> tm -> tm, tm) operator
    | Lam : (ty -> (tm -> tm) -> tm, tm) operator

  let sort_eq
    : type s1 s2 any
    . s1 sort
      -> s2 sort
      -> ((s1, s2) Sorted_abt.eq, (s1, s2) Sorted_abt.eq -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)
end

module Abt = Sorted_abt.Make(Input)

open Input

let unit_type = Abt.into (Abt.Op(Unit, Nil))

let unit_arr_unit =
  Abt.into (Abt.Op(Arrow, Cons(unit_type, Cons(unit_type, Nil))))

let create_unit_id () =
  let x = Abt.fresh_var Term in
  let xv = Abt.into (Abt.Var x) in
  let abs = Abt.into (Abt.Abs(x, xv)) in
  Abt.into (Abt.Op(Lam, Cons(unit_type, Cons(abs, Nil))))

let rec equal_types (ty1 : ty Abt.t) (ty2 : ty Abt.t) =
  match Abt.out ty1, Abt.out ty2 with
  | Op(Arrow, Cons(a, Cons(b, Nil))), Op(Arrow, Cons(c, Cons(d, Nil))) ->
    equal_types a c && equal_types b d
  | Op(Arrow, Cons(_, Cons(_, Nil))), Op(Unit, Nil) -> false
  | Op(Unit, Nil), Op(Arrow, Cons(_, Cons(_, Nil))) -> false
  | Op(Unit, Nil), Op(Unit, Nil) -> true
  | Var _, Op _ -> false
  | Op _, Var _ -> false
  | Var _, Var _ -> failwith "Unreachable!"

let () =
  assert (create_unit_id () = create_unit_id ());
  assert (equal_types unit_type unit_type);
  assert (equal_types unit_arr_unit unit_arr_unit);
  assert (equal_types unit_arr_unit unit_type = false);
  assert (equal_types unit_type unit_arr_unit = false)
