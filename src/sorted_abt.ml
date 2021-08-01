(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type (_, _) eq = Refl : ('a, 'a) eq

module type Signature = sig
  type 'sort sort

  type ('arity, 'sort) operator

  val sort_eq
    : 'a sort -> 'b sort -> (('a, 'b) eq, ('a, 'b) eq -> 'any) Either.t
end

module type S = sig
  type 'sort sort

  type ('arity, 'sort) operator

  type 'sort var

  type 'valence t

  type ('arity, 'sort) operands =
    | Nil : ('sort, 'sort) operands
    | Cons : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands

  type 'valence view =
    | Abs : 'sort var * 'valence t -> ('sort -> 'valence) view
    | Op : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort view
    | Var : 'sort var -> 'sort view

  val fresh_var : 'sort sort -> 'sort var

  val into : 'valence view -> 'valence t

  val out : 'valence t -> 'valence view
end

let counter = ref 0

module Make(Sig : Signature) = struct
  include Sig

  type 'sort var = {
    id : int;
    sort : 'sort sort;
  }

  let fresh_var sort =
    let id = !counter in
    counter := id + 1;
    { id; sort }

  let var_eq : type s1 s2 . s1 var -> s2 var -> (s1, s2) eq option =
    fun v1 v2 -> match sort_eq v1.sort v2.sort with
      | Left Refl when v1.id = v2.id -> Some Refl
      | _ -> None

  type 'valence t =
    | FV : 'sort var -> 'sort t
    | BV : int * 'sort sort -> 'sort t
    | ABS : 'sort sort * 'valence t -> ('sort -> 'valence) t
    | OPER : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort t

  and ('arity, 'sort) operands =
    | Nil : ('sort, 'sort) operands
    | Cons : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands

  type 'valence view =
    | Abs : 'sort var * 'valence t -> ('sort -> 'valence) view
    | Op : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort view
    | Var : 'sort var -> 'sort view

  type poly = { f : 'v 's . 's var -> 'v t -> 'v t } [@@ocaml.unboxed]

  let rec map_operands
    : type a s1 s2 . poly -> s1 var -> (a, s2) operands -> (a, s2) operands =
    fun poly v operands -> match operands with
      | Nil -> Nil
      | Cons(x, xs) -> Cons(poly.f v x, map_operands poly v xs)

  let rec bind : type s v . s var -> v t -> v t =
    fun v t -> match t with
      | FV v' ->
        begin match var_eq v v' with
          | Some Refl -> BV(0, v.sort)
          | None -> FV v'
        end
      | BV(i, sort) -> BV(i + 1, sort)
      | ABS(sort, body) -> ABS(sort, bind v body)
      | OPER(ator, ands) -> OPER(ator, map_operands { f = bind } v ands)

  let into : type v . v view -> v t = function
    | Abs(v, body) -> ABS(v.sort, bind v body)
    | Op(ator, ands) -> OPER(ator, ands)
    | Var v -> FV v

  let rec unbind : type s v . s var -> v t -> v t =
    fun v t -> match t with
      | FV v' -> FV v'
      | BV(0, sort) ->
        begin match sort_eq v.sort sort with
          | Left Refl -> FV v
          | Right _ -> failwith "Sort mismatch!"
        end
      | BV(n, sort) -> BV(n - 1, sort)
      | ABS(sort, body) -> ABS(sort, unbind v body)
      | OPER(ator, ands) -> OPER(ator, map_operands { f = unbind } v ands)

  let out : type v . v t -> v view = function
    | FV v -> Var v
    | BV _ -> failwith "Unbound variable!"
    | ABS(sort, body) ->
      let v = fresh_var sort in
      Abs(v, unbind v body)
    | OPER(ator, ands) -> Op(ator, ands)
end
