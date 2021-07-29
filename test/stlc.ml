(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type ty = |
type tm = |

type 'sort sort =
  | Term : tm sort
  | Type : ty sort

type ('arity, 'sort) operator =
  | Unit
    : (unit, ty) operator
  | Arrow
    : (((unit, ty) Sorted_abt.pair * ((unit, ty) Sorted_abt.pair * unit))
      , ty) operator
  | Ax
    : (unit, tm) operator
  | App
    : ((unit, tm) Sorted_abt.pair * ((unit, tm) Sorted_abt.pair * unit)
      , tm) operator
  | Lam
    : ((unit, ty) Sorted_abt.pair * ((tm * unit, tm) Sorted_abt.pair * unit)
      , tm) operator

module Input = struct
  type nonrec 'sort sort = 'sort sort

  type nonrec ('arity, 'sort) operator = ('arity, 'sort) operator

  let sort_eq
    : type s1 s2 any
    . s1 sort
      -> s2 sort
      -> ((s1, s2) Sorted_abt.Eq.t, (s1, s2) Sorted_abt.Eq.t -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)
end

module Abt = Sorted_abt.Make(Input)

let unit_type = Abt.into (Abt.VOP(Unit, Nil))

let unit_arr_unit =
  Abt.into (Abt.VOP(Arrow, Cons(unit_type, Cons(unit_type, Nil))))

let unit_id =
  let x = Abt.fresh_var Term in
  let xv = Abt.into (Abt.VAR x) in
  let abs = Abt.into (Abt.VABS(x, xv)) in
  Abt.into (Abt.VOP(Lam, Cons(unit_type, Cons(abs, Nil))))
