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

(* file: wf_rec.ml *)
(* created: 2009-05-06  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Asttypes
open Reac_ast
open Reac_misc

(* Checks that expression as right-hand side of `let rec' are well formed *)

let error e =
  Format.fprintf !Misc.err_fmt
   "%aThis kind of expression is not allowed as right-hand side of `let rec'.\n"
   Location.print e.expr_loc;
  raise Misc.Error

let rec empty_intersection l1 l2 =
  match l1 with
  | [] -> true
  | x::xs ->
      if is_free x l2 then empty_intersection xs l2
      else false

let rec check_patt_expr (patt, expr) =
  let vars = vars_of_patt patt in
  let free_vars = expr_free_vars expr in
  if empty_intersection vars free_vars then ()
  else
    begin match expr.expr_desc with
    | Rexpr_pause _
    | Rexpr_halt _
    | Rexpr_constant _
    | Rexpr_function _
    | Rexpr_construct _
    | Rexpr_array _
    | Rexpr_tuple _
    | Rexpr_record _
    | Rexpr_process _
    | Rexpr_nothing ->
	  ()
    | Rexpr_local _
    | Rexpr_global _
    | Rexpr_let _
    | Rexpr_apply _
    | Rexpr_record_access _
    | Rexpr_record_with _
    | Rexpr_record_update _
    | Rexpr_assert _
    | Rexpr_trywith _
    | Rexpr_ifthenelse _
    | Rexpr_match _
    | Rexpr_while _
    | Rexpr_for _
    | Rexpr_seq _
    | Rexpr_pre _
    | Rexpr_last _
    | Rexpr_default _
    | Rexpr_emit _
    | Rexpr_loop _
    | Rexpr_fordopar _
    | Rexpr_par _
    | Rexpr_merge _
    | Rexpr_signal _
    | Rexpr_run _
    | Rexpr_until _
    | Rexpr_when _
    | Rexpr_control _
    | Rexpr_present _
    | Rexpr_await _
    | Rexpr_await_val _
    | Rexpr_get _ ->
	error expr
    | Rexpr_constraint (e, _) ->
	check_patt_expr (patt, e)
  end

let check_expr expr =
  begin match expr.expr_desc with
  | Rexpr_let (Recursive, patt_expr_list, _) ->
      List.iter check_patt_expr patt_expr_list;
      expr
  | _ -> expr
  end

let check impl =
  ignore (Reac2reac.impl_map check_expr impl);
  begin match impl.impl_desc with
  | Rimpl_let (Recursive, patt_expr_list) ->
      List.iter check_patt_expr patt_expr_list
  | _ -> ()
  end
