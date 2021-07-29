(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Input (Lib : Sorted_abt.LIB) = struct
  open Lib

  type ty = |
  type tm = |

  type 'sort sort =
    | Term : tm sort
    | Type : ty sort

  type ('arity, 'sort) operator =
    | Unit : (unit, ty) operator
    | Ax : (unit, tm) operator
    | App : ((unit, tm) t * ((unit, tm) t * unit), tm) operator
    | Lam : ((tm * unit, tm) t * unit, tm) operator

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
