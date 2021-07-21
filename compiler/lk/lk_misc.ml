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

(* file: lk_misc.ml *)
(* created: 2007-06-11  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Functions on Lk AST *)

open Lk_ast

let rec is_value e =
  match e.kexpr_desc with
  | Kexpr_local _ | Kexpr_global _ | Kexpr_constant _
  | Kexpr_function _ | Kexpr_process _ ->
      true

  | Kexpr_let (_, patt_expr_list, expr) ->
      (is_value expr)
      &&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Kexpr_tuple expr_list ->
      List.for_all is_value expr_list

  | Kexpr_construct (_, None) -> true
  | Kexpr_construct (_, Some expr) -> is_value expr

  | Kexpr_constraint (expr, _) -> is_value expr
  | Kexpr_trywith (expr, patt_expr_list) ->
      (is_value expr)

  | Kexpr_assert expr -> false
  | Kexpr_ifthenelse  (e1, e2, e3) ->
      (is_value e1) && (is_value e2) && (is_value e3)

  | Kexpr_match (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all
         (fun (_, when_opt, e) -> when_opt = None && is_value e)
         patt_expr_list)


  | _ -> false
