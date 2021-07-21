(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the Q Public License  *)
(*  version 1.0.                                                      *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* file: lco_misc.ml *)
(* created: 2005-08-03  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Functions on Lco AST *)

open Lco_ast

let rec is_value e =
  match e.coexpr_desc with
  | Coexpr_local _  | Coexpr_global _ | Coexpr_constant _
  | Coexpr_function _ | Coexpr_process _ ->
      true

  | Coexpr_let(_, patt_expr_list, expr) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Coexpr_tuple expr_list ->
      List.for_all is_value expr_list

  | Coexpr_construct (_, None) -> true
  | Coexpr_construct (_, Some expr) -> is_value expr

  | Coexpr_constraint (expr, _) -> is_value expr

  | Coexpr_trywith (expr, _) ->
      (is_value expr)

  | Coexpr_assert expr -> false

  | Coexpr_ifthenelse (e1, e2, e3) ->
      (is_value e1) && (is_value e2) && (is_value e3)

  | Coexpr_match (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all
         (fun (_, when_opt, e) -> when_opt = None && is_value e)
         patt_expr_list)

  | _ -> false
