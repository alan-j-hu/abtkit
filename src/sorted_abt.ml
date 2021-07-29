(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type (_, _) eq = Refl : ('a, 'a) eq

module type INPUT = sig
  type 'sort sort

  type ('arity, 'sort) operator

  val sort_eq
    : 'a sort -> 'b sort -> (('a, 'b) eq, ('a, 'b) eq -> 'any) Either.t
end

let counter = ref 0

module Make (M : INPUT) = struct
  type 'sort var = {
    id : int;
    sort : 'sort M.sort;
  }

  let fresh_var sort =
    let id = !counter in
    counter := id + 1;
    { id; sort }

  let var_eq : type s1 s2 . s1 var -> s2 var -> (s1, s2) eq option =
    fun v1 v2 -> match M.sort_eq v1.sort v2.sort with
      | Left Refl when v1.id = v2.id -> Some Refl
      | _ -> None

  type ('valence, 'sort) t =
    | FV : 'sort var -> (unit, 'sort) t
    | BV : int * 'sort M.sort -> (unit, 'sort) t
    | ABS : 'a M.sort * ('valence, 'sort) t -> ('a * 'valence, 'sort) t
    | OPER : ('arity, 'sort) M.operator * 'arity arity -> (unit, 'sort) t

  and 'arity arity =
    | Nil : unit arity
    | Cons : ('a, 'b) t * 'c arity -> (('a -> 'b) * 'c) arity

  type ('valence, 'sort) view =
    | VABS : 's var * ('valence, 'sort) t -> ('s * 'valence, 'sort) view
    | VOP : ('arity, 'sort) M.operator * 'arity arity -> (unit, 'sort) view
    | VAR : 'sort var -> (unit, 'sort) view

  type poly = { f : 'v 's1 's2 . 's1 var -> ('v, 's2) t -> ('v, 's2) t }

  let rec map_rands : type a s . poly -> s var -> a arity -> a arity =
    fun poly v rands -> match rands with
      | Nil -> Nil
      | Cons(x, xs) -> Cons(poly.f v x, map_rands poly v xs)

  let rec bind : type s1 s2 v . s1 var -> (v, s2) t -> (v, s2) t =
    fun v t -> match t with
      | FV v' ->
        begin match var_eq v v' with
          | Some Refl -> BV(0, v.sort)
          | None -> FV v'
        end
      | BV(i, sort) -> BV(i + 1, sort)
      | ABS(sort, body) -> ABS(sort, bind v body)
      | OPER(ator, ands) -> OPER(ator, map_rands { f = bind } v ands)

  let into : type s v . (v, s) view -> (v, s) t = function
    | VABS(v, body) -> ABS(v.sort, bind v body)
    | VOP(ator, ands) -> OPER(ator, ands)
    | VAR v -> FV v

  let rec unbind : type s1 s2 v . s1 var -> (v, s2) t -> (v, s2) t =
    fun v t -> match t with
      | FV v' -> FV v'
      | BV(0, sort) ->
        begin match M.sort_eq v.sort sort with
          | Left Refl -> FV v
          | Right _ -> failwith "Sort mismatch!"
        end
      | BV(n, sort) -> BV(n - 1, sort)
      | ABS(sort, body) -> ABS(sort, unbind v body)
      | OPER(ator, ands) -> OPER(ator, map_rands { f = unbind } v ands)

  let out : type s v . (v, s) t -> (v, s) view = function
    | FV v -> VAR v
    | BV _ -> failwith "Unbound variable!"
    | ABS(sort, body) ->
      let v = fresh_var sort in
      VABS(v, unbind v body)
    | OPER(ator, ands) -> VOP(ator, ands)
end
