(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type (_, _) eq =
  | Refl : ('a, 'a) eq (** Proof that ['a] and ['a] are equal. *)
(** [('a, 'b) eq] is the proposition that ['a] and ['b] are equal. *)

module type Signature = sig
  type 'sort sort
  (** A sort is the syntactic class that an operator belongs to. The type
      parameter is a phantom type that represents the sort. *)

  type ('arity, 'sort) operator
  (** An operator is a function symbol. The operator's type contains two
      phantom type parameters, the first for the operator's arity and the second
      for the operator's sort.

      The arity of an operator consists of a sequence of sorts {i s{_ 1 }, ...,
      s{_ n } } describing the operator's parameters, and a sort {i s }
      describing the sort that the operator belongs to. The arity usually takes
      the form {i s{_ 1 } × ... × s{_ n } → s }.

      The ['arity] type parameter is a sequence of arrow types, ['s1 -> 's2 ->
      ... -> 'sort]. The output type must be the sort type. An operator of zero
      arity should have type [('sort, 'sort) operator]. *)

  val sort_eq
    : 'a sort -> 'b sort -> (('a, 'b) eq, ('a, 'b) eq -> 'any) Either.t
  (** Decides the equality of two sorts. Iff the sorts are equal, it returns
      a proof that their types are equal. Iff the sorts are unequal, it
      returns a proof that their types are not equal. *)
end
(** Input signature of the functor {!Make}.

    A signature describes the symbols and syntax of some mathematical theory,
    whether it be a logic, language, or algebraic structure. *)

module type S = sig
  type 'sort sort

  type ('arity, 'sort) operator

  type 'sort var

  type 'valence t
  (** An abstract binding tree (ABT). ['valence] is a phantom type parameter
      representing the valence of the ABT. *)

  type ('arity, 'sort) arity =
    | Nil : ('sort, 'sort) arity
    | Cons : 'valence t * ('a, 'sort) arity -> ('valence -> 'a, 'sort) arity

  type 'valence view =
    | VABS : 'sort var * 'valence t -> ('sort -> 'valence) view
    | VOP : ('arity, 'sort) operator * ('arity, 'sort) arity -> 'sort view
    | VAR : 'sort var -> 'sort view

  val fresh_var : 'sort sort -> 'sort var

  val into : 'v view -> 'v t

  val out : 'v t -> 'v view
end
(** Output signature of the functor {!Make}. *)

module Make(Sig : Signature) : S
  with type 'sort sort = 'sort Sig.sort
   and type ('arity, 'sort) operator = ('arity, 'sort) Sig.operator
