(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

include Intf

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
    | Bound : int * 'sort sort -> 'sort t
    | Free : 'sort var -> 'sort t
    | Abstr : 'sort sort * 'valence t -> ('sort -> 'valence) t
    | Oper : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort t

  and ('arity, 'sort) operands =
    | [] : ('sort, 'sort) operands
    | (::) : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands

  type 'valence view =
    | Abs : 'sort var * 'valence t -> ('sort -> 'valence) view
    | Op : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort view
    | Var : 'sort var -> 'sort view

  type poly = { f : 'v . 'v t -> 'v t } [@@ocaml.unboxed]

  let rec map_operands
    : type a s . poly -> (a, s) operands -> (a, s) operands =
    fun poly operands -> match operands with
      | [] -> []
      | x :: xs -> poly.f x :: map_operands poly xs

  let rec bind : type s v . s var -> v t -> v t =
    fun v t -> match t with
      | Free v' ->
        begin match var_eq v v' with
          | Some Refl -> Bound(0, v.sort)
          | None -> Free v'
        end
      | Bound(i, sort) -> Bound(i + 1, sort)
      | Abstr(sort, body) -> Abstr(sort, bind v body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> bind v x } ands)

  let abs v body = Abstr(v.sort, bind v body)

  let op ator ands = Oper(ator, ands)

  let var v = Free v

  let into : type v . v view -> v t = function
    | Abs(v, body) -> abs v body
    | Op(ator, ands) -> op ator ands
    | Var v -> var v

  let rec unbind : type s v . s var -> v t -> v t =
    fun v t -> match t with
      | Free v' -> Free v'
      | Bound(0, sort) ->
        begin match sort_eq v.sort sort with
          | Left Refl -> Free v
          | Right _ -> failwith "Sort mismatch!"
        end
      | Bound(n, sort) -> Bound(n - 1, sort)
      | Abstr(sort, body) -> Abstr(sort, unbind v body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> unbind v x } ands)

  let out : type v . v t -> v view = function
    | Free v -> Var v
    | Bound _ -> failwith "Unbound variable!"
    | Abstr(sort, body) ->
      let v = fresh_var sort in
      Abs(v, unbind v body)
    | Oper(ator, ands) -> Op(ator, ands)

  let rec subst : type s1 s2 . s1 sort -> (s1 var -> s1 t option) -> s2 t -> s2 t =
    fun sort sub abt -> match abt with
      | Free var as abt ->
        begin match sort_eq sort var.sort with
          | Left Refl ->
            begin match sub var with
              | Some abt -> abt
              | None -> abt
            end
          | Right _ -> abt
        end
      | Bound _ as abt -> abt
      | Abstr(sort', body) -> Abstr(sort', subst sort sub body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> subst sort sub x } ands)

  let pp_print_var ppf var =
    Format.pp_print_char ppf 'v';
    Format.pp_print_int ppf var.id

  let rec pp_print : type s . Format.formatter -> s t -> unit =
    fun ppf t ->
    match out t with
    | Var var -> pp_print_var ppf var
    | Abs(var, body) ->
      Format.fprintf
        ppf
        "%a.%a"
        pp_print_var var
        pp_print body
    | Op(ator, []) -> Format.fprintf ppf "%a()" pp_print_op ator
    | Op(ator, abt :: ands) ->
      Format.fprintf
        ppf
        "%a(@[<hv>%a%a)@]"
        pp_print_op ator
        pp_print abt
        pp_print_operands ands

  and pp_print_operands
    : type a s . Format.formatter -> (a, s) operands -> unit =
    fun ppf operands ->
    match operands with
    | [] -> ()
    | abt :: next ->
      Format.fprintf
        ppf
        ";@,%a%a"
        pp_print abt
        pp_print_operands next
end
