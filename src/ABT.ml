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
    name : name;
    sort : 'sort sort;
  }

  let fresh_var sort name =
    let id = !counter in
    counter := id + 1;
    { id; name; sort }

  let name var = var.name

  let equal_vars : type s1 s2 . s1 var -> s2 var -> (s1, s2) eq option =
    fun v1 v2 -> match equal_sorts v1.sort v2.sort with
      | Left Refl when v1.id = v2.id -> Some Refl
      | _ -> None

  type 'valence t =
    | Bound : int * 'sort sort -> 'sort va t
    | Free : 'sort var -> 'sort va t
    | Abstr : name * 'sort sort * 'valence t -> ('sort -> 'valence) t
    | Oper : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort va t

  and ('arity, 'sort) operands =
    | [] : ('sort ar, 'sort) operands
    | (::) : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands

  type 'valence view =
    | Abs : 'sort var * 'valence t -> ('sort -> 'valence) view
    | Op : ('arity, 'sort) operator * ('arity, 'sort) operands -> 'sort va view
    | Var : 'sort var -> 'sort va view

  type poly = { f : 'v. 'v t -> 'v t } [@@ocaml.unboxed]

  let rec map_operands
    : type a s. poly -> (a, s) operands -> (a, s) operands =
    fun poly operands -> match operands with
      | [] -> []
      | x :: xs -> poly.f x :: map_operands poly xs

  let rec bind : type s v. s var -> v t -> v t =
    fun v t -> match t with
      | Free v' ->
        begin match equal_vars v v' with
          | Some Refl -> Bound(0, v.sort)
          | None -> t
        end
      | Bound(i, sort) -> Bound(i + 1, sort)
      | Abstr(name, sort, body) -> Abstr(name, sort, bind v body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> bind v x } ands)

  let abs v body = Abstr(v.name, v.sort, bind v body)

  let op ator ands = Oper(ator, ands)

  let var v = Free v

  let into : type v. v view -> v t = function
    | Abs(v, body) -> abs v body
    | Op(ator, ands) -> op ator ands
    | Var v -> var v

  let rec unbind : type s v. s var -> v t -> v t =
    fun v t -> match t with
      | Free _ -> t
      | Bound(0, sort) ->
        begin match equal_sorts v.sort sort with
          | Left Refl -> Free v
          | Right _ -> failwith "unbind: Sort mismatch!"
        end
      | Bound(n, sort) -> Bound(n - 1, sort)
      | Abstr(name, sort, body) -> Abstr(name, sort, unbind v body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> unbind v x } ands)

  let out : type v. v t -> v view = function
    | Free v -> Var v
    | Bound _ -> failwith "out: Unbound variable!"
    | Abstr(name, sort, body) ->
      let v = fresh_var sort name in
      Abs(v, unbind v body)
    | Oper(ator, ands) -> Op(ator, ands)

  let rec subst
    : type s1 s2. s1 sort -> (s1 var -> s1 va t option) -> s2 t -> s2 t =
    fun sort sub abt -> match abt with
      | Free var as abt ->
        begin match equal_sorts sort var.sort with
          | Left Refl ->
            begin match sub var with
              | Some abt -> abt
              | None -> abt
            end
          | Right _ -> abt
        end
      | Bound _ as abt -> abt
      | Abstr(name, sort', body) -> Abstr(name, sort', subst sort sub body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> subst sort sub x } ands)

  let rec aequiv : type v. v t -> v t -> bool = fun t1 t2 ->
    match t1, t2 with
    | Free var1, Free var2 ->
      begin match equal_vars var1 var2 with
        | Some Refl -> true
        | None -> false
      end
    | Bound(var1, _), Bound(var2, _) -> var1 = var2
    | Abstr(_, _, body1), Abstr(_, _, body2) -> aequiv body1 body2
    | Oper(ator1, ands1), Oper(ator2, ands2) ->
      begin match equal_ops ator1 ator2 with
        | Some Refl -> aequiv_operands ands1 ands2
        | None -> false
      end
    | _, _ -> false

  and aequiv_operands
    : type a s. (a, s) operands -> (a, s) operands -> bool =
    fun ands1 ands2 -> match ands1, ands2 with
      | [], [] -> true
      | x :: xs, y :: ys -> aequiv x y && aequiv_operands xs ys

  let pp_print_var ppf var =
    pp_print_name ppf var.name

  let rec pp_print : type s. Format.formatter -> s t -> unit =
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
    : type a s. Format.formatter -> (a, s) operands -> unit =
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
