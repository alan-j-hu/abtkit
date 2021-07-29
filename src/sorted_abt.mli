(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Eq : sig
  type (_, _) t = Refl : ('a, 'a) t
  (** [('a, 'b) t] is the proposition that ['a] and ['b] are equal. *)
end

module type INPUT = sig
  type 'sort sort
  (** ['sort sort] is a proxy type that represents a sort of the language. *)

  type ('arity, 'sort) operator
  (** An operator is a function symbol. *)

  val sort_eq
    : 'a sort -> 'b sort -> (('a, 'b) Eq.t, ('a, 'b) Eq.t -> 'any) Either.t
    (** Decides the equality of two sorts. Iff the sorts are equal, it returns
        a proof that their types are equal. Iff the sorts are unequal, it
        returns a proof that their types are not equal. *)
end

module Make (M : INPUT) : sig
  type 'sort var

  type ('valence, 'sort) t

  and 'arity arity =
    | Nil : unit arity
    | Cons : ('a, 'b) t * 'c arity -> (('a -> 'b) * 'c) arity

  type ('valence, 'sort) view =
    | VABS : 's var * ('valence, 'sort) t -> ('s * 'valence, 'sort) view
    | VOP : ('arity, 'sort) M.operator * 'arity arity -> (unit, 'sort) view
    | VAR : 'sort var -> (unit, 'sort) view

  val fresh_var : 'sort M.sort -> 'sort var

  val into : ('v, 's) view -> ('v, 's) t

  val out : ('v, 's) t -> ('v, 's) view
end
