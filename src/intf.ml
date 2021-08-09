(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type (_, _) eq =
  | Refl : ('a, 'a) eq (** Proof that ['a] and ['a] are equal. *)
(** [('a, 'b) eq] is the proposition that ['a] and ['b] are equal. *)

type 'a out = Out of 'a [@@ocaml.unbox]
(** A helper type for specifying arities and valences. *)

module type Signature = sig
  type 'sort sort
  (** A sort is the syntactic class that an operator belongs to. The type
      parameter is a phantom type that represents the sort. *)

  type ('arity, 'sort) operator
  (** An operator is a function symbol. The operator's type contains two
      phantom type parameters, the first for the operator's arity and the second
      for the operator's sort.

      The arity of an operator consists of a sequence of sorts {i s{_ 1}, ...,
      s{_ n}} describing the operator's parameters, and the sort {i s} that the
      operator belongs to. The arity usually takes the form {i s{_ 1} × ... ×
      s{_ n} → s}.

      Abstract binding trees record variables that are bound in the scope of a
      term. Therefore, operands have a {i valence}, which lists the sorts of
      the variables bound in the operand in addition to the sort of the operand
      itself. The valence takes the form {i s{_ 1} × ... × s{_ k} → s}, where
      {i s{_ 1}, ..., s{_ k}} are the sorts of the variables and {i s} is the
      sort of the operand.

      As a result, the arity for an operator of an abstract binding tree really
      takes the form {i v{_ 1} × ... × v{_ n} → s} where each {i v{_ i}} is a
      valence.

      The ['arity] type parameter is a sequence of arrow types, ['v1 -> ... ->
      'vn -> 'sort out] where each ['vi] has the form ['s1 -> ... 'sk ->
      's out]. The output type ['sort] must be the sort type of the operator. An
      operator of arity zero has type [('sort out, 'sort) operator]. *)

  type name
  (** The human-readable name of variables. *)

  val equal_sorts
    : 'a sort -> 'b sort -> (('a, 'b) eq, ('a, 'b) eq -> 'any) Either.t
  (** Decides the equality of two sorts. Iff the sorts are equal, returns
      a proof that their types are equal. Iff the sorts are unequal, it
      returns a proof that their types are not equal. *)

  val equal_ops
    : ('arity1, 'sort) operator -> ('arity2, 'sort) operator
    -> ('arity1, 'arity2) eq option
  (** Checks the equality of two operators from the same sort. Iff the
      operators are equal, returns a proof that their equalities are equal. *)

  val pp_print_op : Format.formatter -> ('arity, 'sort) operator -> unit
  (** Pretty-prints an operator. *)

  val pp_print_name : Format.formatter -> name -> unit
  (** Pretty-prints a name. *)
end
(** Input signature of the functor {!module:Make}.

    A signature describes the symbols and syntax of some mathematical theory,
    whether it be a logic, language, or algebraic structure. It is not to be
    confused with the "signatures" of the metalanguage (OCaml), which were
    influenced by the same mathematical concepts. *)

module type S = sig
  type 'sort sort
  (** An alias of {!type:Signature.sort}. *)

  type ('arity, 'sort) operator
  (** An alias of {!type:Signature.operator}. *)

  type name
  (** An alias of {!type:Signature.name}. *)

  type 'sort var
  (** A variable annotated by its sort. *)

  type 'valence t
  (** An abstract binding tree (ABT). ['valence] is a phantom type parameter
      representing the valence of the ABT. *)

  type ('arity, 'sort) operands =
    | [] : ('sort out, 'sort) operands
    (** An empty list of operands. *)
    | (::) : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands
    (** An operand followed by a list of operands. *)
  (** A list of operands. *)

  type 'valence view =
    | Abs : 'sort var * 'valence t -> ('sort -> 'valence) view
    (** An abstraction, which binds a variable within a term. *)
    | Op : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort out view
    (** An operator applied to operands. *)
    | Var : 'sort var -> 'sort out view
    (** A variable. *)
  (** A view of an ABT.*)

  val fresh_var : 'sort sort -> name -> 'sort var
  (** Generates a fresh variable of the given sort. The variable is unique from
      any other variable generated from the function. *)

  val name : _ var -> name
  (** Retrieves the name of the variable. *)

  val equal_vars : 'sort1 var -> 'sort2 var -> ('sort1, 'sort2) eq option
  (** Checks two variables for equality. Iff the variables are equal, returns
      [Some proof] that their sorts are the same. *)

  val abs : 'sort var -> 'valence t -> ('sort -> 'valence) t
  (** Constructs an abstraction ABT. *)

  val op : ('arity, 'sort) operator -> ('arity, 'sort) operands -> 'sort out t
  (** Constructs an operation ABT. *)

  val var : 'sort var -> 'sort out t
  (** Constructs a variable ABT. *)

  val into : 'valence view -> 'valence t
  (** Constructs an ABT from a view. *)

  val out : 'valence t -> 'valence view
  (** Views an ABT. *)

  val subst : 'sort sort -> ('sort var -> 'sort out t option) -> 'valence t -> 'valence t
  (** Applies a substitution to the ABT. *)

  val equal : 'valence t -> 'valence t -> bool
  (** Checks two ABTs for alpha-equivalence. *)

  val pp_print : Format.formatter -> 'valence t -> unit
  (** Pretty-prints an ABT. *)
end
(** Output signature of the functor {!module:Make}. *)
