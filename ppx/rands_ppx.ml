(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Ppxlib

let ext_pat =
  Extension.V3.declare
    "rands"
    Extension.Context.pattern
    Ast_pattern.(ppat __ none)
    begin fun ~ctxt:_ ->
      let rec f pat =
        let loc = pat.ppat_loc in
        match pat with
        | [%pat? []] -> [%pat? Nil]
        | [%pat? [%p? pat] :: [%p? pats]] ->
          [%pat? Cons([%p pat], [%p (f pats)])]
        | _ -> Location.raise_errorf ~loc "Only list constructors are permitted"
      in f
    end

let ext_expr =
  Extension.V3.declare
    "rands"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    begin fun ~ctxt:_ ->
      let rec f expr =
        let loc = expr.pexp_loc in
        match expr with
        | [%expr []] -> [%expr Nil]
        | [%expr [%e? expr] :: [%e? exprs]] ->
          [%expr Cons([%e expr], [%e (f exprs)])]
        | _ -> Location.raise_errorf ~loc "Only list constructors are permitted"
      in f
    end

let () =
  Driver.register_transformation
    ~rules:[ Context_free.Rule.extension ext_expr
           ; Context_free.Rule.extension ext_pat ]
    "rands_ppx"
