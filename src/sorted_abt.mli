include module type of Intf (** @inline *)

module Make(Sig : Signature) : S
  with type 'sort sort = 'sort Sig.sort
   and type ('arity, 'sort) operator = ('arity, 'sort) Sig.operator
(** Functor building an implementation of abstract binding trees given a
    signature. *)
