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

(* file: reac_misc.ml *)
(* created: 2005-05-05  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Functions on Reac AST *)

open Asttypes
open Reac_ast
open Def_types
open Types
open Reactivity_effects

let make_expr_all e typ static reactivity reactivity_effect loc =
  { expr_desc = e;
    expr_loc = loc;
    expr_type = typ;
    expr_static = static;
    expr_reactivity = reactivity;
    expr_reactivity_effect = reactivity_effect; }

let make_expr e loc =
  { expr_desc = e;
    expr_loc = loc;
    expr_type = no_type_expression;
    expr_static = Def_static.Process, Def_static.Dynamic Def_static.Dontknow;
    expr_reactivity = [];
    expr_reactivity_effect = no_react; }

let make_patt p loc =
  { patt_desc = p;
    patt_loc = loc;
    patt_type = no_type_expression; }

let make_conf c loc =
  { conf_desc = c;
    conf_loc = loc; }

let make_te t loc =
  { te_desc = t;
    te_loc = loc; }

let make_impl it loc =
  { impl_desc = it;
    impl_loc = loc; }

let make_intf it loc =
  { intf_desc = it;
    intf_loc = loc; }


let string_of_varpatt x =
  begin match x with
  | Varpatt_local id -> Ident.unique_name id
  | Varpatt_global x -> Global.little_name_of_global x
  end


(* Compute the list of variables introduce in a pattern *)
let rec vars_of_patt p =
  match p.patt_desc with
  | Rpatt_any -> []

  | Rpatt_var x -> [x]

  | Rpatt_alias (patt,x) -> x :: (vars_of_patt patt)

  | Rpatt_constant _ -> []

  | Rpatt_tuple patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list

  | Rpatt_construct (_, None) -> []

  | Rpatt_construct (_, Some patt) -> vars_of_patt patt

  | Rpatt_or (patt1, patt2) -> (vars_of_patt patt1) @ (vars_of_patt patt2)

  | Rpatt_record label_patt_list ->
      List.fold_left
	(fun vars (_,patt) -> (vars_of_patt patt)@vars)
	[] label_patt_list

  | Rpatt_array patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list

  | Rpatt_constraint (patt, _) -> vars_of_patt patt


(* Compute the list of variables introduce in an event configuration *)
let rec vars_of_config config =
  match config.conf_desc with
  | Rconf_present (e, None) -> []

  | Rconf_present (e, Some p) -> vars_of_patt p

  | Rconf_and (cfg1, cfg2) -> (vars_of_config cfg1) @ (vars_of_config cfg2)

  | Rconf_or (cfg1, cfg2) -> (vars_of_config cfg1) @ (vars_of_config cfg2)

(* Checks that a variable is not in a variables list *)
let rec is_free x vars =
  begin match vars with
  | [] -> true
  | x' :: vars' ->
      begin match x, x' with
      | Varpatt_local id1, Varpatt_local id2 ->
	  if Ident.same id1 id2 then
	    false
	  else
	    is_free x vars'
      | Varpatt_global gl1, Varpatt_global gl2 ->
	  if Global_ident.same gl1.Global.gi gl2.Global.gi then
	    false
	  else
	    is_free x vars'
      | _ ->
	  is_free x vars'
      end
  end

(* Compute the list of free variables of an expression *)
let expr_free_vars e =
  let fv = ref [] in
  let rec expr_free_vars vars expr =
    begin match expr.expr_desc with
    | Rexpr_local x ->
	if is_free (Varpatt_local x) vars then
	  fv := (Varpatt_local x) :: !fv

    | Rexpr_global x ->
	if is_free (Varpatt_global x) vars then
	  fv := (Varpatt_global x) :: !fv

    | Rexpr_constant _ -> ()

    | Rexpr_let (rec_flag, patt_expr_list, expr) ->
	let vars' =
	  List.fold_left
	    (fun vars' (p, _) -> (vars_of_patt p) @ vars')
	    vars
	    patt_expr_list
	in
	if rec_flag = Recursive then
	  List.iter
	    (fun (_, e) -> expr_free_vars vars' e)
	    patt_expr_list
	else
	  List.iter
	    (fun (_, e) -> expr_free_vars vars e)
	    patt_expr_list;
	expr_free_vars vars' expr

    | Rexpr_function patt_expr_list ->
	List.iter
	  (fun (p,when_opt,e) ->
	    let vars' = (vars_of_patt p) @ vars in
            Misc.opt_iter (expr_free_vars vars') when_opt;
	    expr_free_vars vars' e)
	  patt_expr_list

    | Rexpr_apply (e, expr_list) ->
	expr_free_vars vars e;
	List.iter (expr_free_vars vars) expr_list

    | Rexpr_tuple expr_list ->
	List.iter (expr_free_vars vars) expr_list

    | Rexpr_construct (const, None) -> ()

    | Rexpr_construct (const, Some e) ->
	expr_free_vars vars e

    | Rexpr_array expr_list ->
	List.iter (expr_free_vars vars) expr_list

    | Rexpr_record lbl_expr_list ->
	List.iter (fun (_,e) -> expr_free_vars vars e) lbl_expr_list

    | Rexpr_record_access (e, lbl) ->
	expr_free_vars vars e

    | Rexpr_record_with (e, lbl_expr_list) ->
        expr_free_vars vars e;
        List.iter (fun (_,e) -> expr_free_vars vars e) lbl_expr_list

    | Rexpr_record_update (e1, lbl, e2) ->
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_constraint (e, ty) ->
	expr_free_vars vars e

    | Rexpr_trywith (e, patt_expr_list) ->
	expr_free_vars vars e;
	List.iter
	  (fun (p,when_opt,e) ->
	    let vars' = (vars_of_patt p) @ vars in
            Misc.opt_iter (expr_free_vars vars') when_opt;
	    expr_free_vars vars' e)
	  patt_expr_list

    | Rexpr_assert e ->
	expr_free_vars vars e

    | Rexpr_ifthenelse(e,e1,e2) ->
	expr_free_vars vars e;
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_match (e, patt_expr_list) ->
	expr_free_vars vars e;
	List.iter
	  (fun (p,when_opt,e) ->
	    let vars' = (vars_of_patt p) @ vars in
            Misc.opt_iter (expr_free_vars vars') when_opt;
	    expr_free_vars vars' e)
	  patt_expr_list

    | Rexpr_while (e1,e2) ->
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	let vars' = (Varpatt_local ident) :: vars in
	expr_free_vars vars' e1;
	expr_free_vars vars' e2;
	expr_free_vars vars' e

    | Rexpr_seq e_list ->
	List.iter (expr_free_vars vars) e_list

    | Rexpr_process e ->
	expr_free_vars vars e

    | Rexpr_pre (pre_kind, e) ->
	expr_free_vars vars e

    | Rexpr_last e ->
	expr_free_vars vars e

    | Rexpr_default e ->
	expr_free_vars vars e

    | Rexpr_nothing -> ()

    | Rexpr_pause _ -> ()

    | Rexpr_halt _ -> ()

    | Rexpr_emit (e, None) ->
	expr_free_vars vars e

    | Rexpr_emit (e1, Some e2) ->
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_loop (n_opt, e) ->
	Misc.opt_iter (expr_free_vars vars) n_opt;
	expr_free_vars vars e

    | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	let vars' = (Varpatt_local ident) :: vars in
	expr_free_vars vars' e1;
	expr_free_vars vars' e2;
	expr_free_vars vars' e

    | Rexpr_par e_list ->
	List.iter (expr_free_vars vars) e_list

    | Rexpr_merge (e1, e2) ->
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_signal ((ident, tyexpr_opt), None, e) ->
	let vars' = (Varpatt_local ident) :: vars in
	expr_free_vars vars' e
    | Rexpr_signal ((ident, tyexpr_opt), Some(kind, e1,e2), e) ->
	let vars' = (Varpatt_local ident) :: vars in
	expr_free_vars vars' e1;
	expr_free_vars vars' e2;
	expr_free_vars vars' e

    | Rexpr_run e ->
	expr_free_vars vars e

    | Rexpr_until (e, config_when_opt_e_opt_list) ->
	expr_free_vars vars e;
        List.iter
          (fun (config, when_opt, e_opt) ->
            config_free_vars vars config;
            let vars' = (vars_of_config config) @ vars in
            Misc.opt_iter (expr_free_vars vars') when_opt;
            Misc.opt_iter (expr_free_vars vars') e_opt)
          config_when_opt_e_opt_list

    | Rexpr_when (config, e) ->
	config_free_vars vars config;
	expr_free_vars vars e

    | Rexpr_control (config, None, e) ->
	config_free_vars vars config;
	expr_free_vars vars e
    | Rexpr_control (config, Some e1, e) ->
	config_free_vars vars config;
	expr_free_vars vars e;
	let vars' = (vars_of_config config) @ vars in
	expr_free_vars vars' e1

    | Rexpr_get (e,patt,e1) ->
	expr_free_vars vars e;
	let vars' = (vars_of_patt patt) @ vars in
	expr_free_vars vars' e1

    | Rexpr_present (config, e1, e2) ->
	config_free_vars vars config;
	expr_free_vars vars e1;
	expr_free_vars vars e2

    | Rexpr_await (immediate_flag, config) ->
	config_free_vars vars config

    | Rexpr_await_val (immediate, kind, config, when_opt, e1) ->
	config_free_vars vars config;
	let vars' = (vars_of_config config) @ vars in
        Misc.opt_iter (expr_free_vars vars') when_opt;
	expr_free_vars vars' e1

    end

  and config_free_vars vars config =
    match config.conf_desc with
    | Rconf_present (e, _) ->
	expr_free_vars vars e

    | Rconf_and (c1, c2) ->
	config_free_vars vars c1;
	config_free_vars vars c2

    | Rconf_or (c1, c2) ->
	config_free_vars vars c1;
  	config_free_vars vars c2
  in
  expr_free_vars [] e;
  !fv

let int_of_expr expr =
  match expr.expr_desc with
  | Rexpr_constant (Const_int n) -> Some n
  | _ -> None

(* Test is a pattern is a partial matching *)
let rec partial_match patt =
  match patt.patt_desc with
  | Rpatt_any -> false
  | Rpatt_var _ -> false
  | Rpatt_alias (p, _) -> partial_match p
  | Rpatt_constant Const_unit -> false
  | Rpatt_constant _ -> true
  | Rpatt_tuple patt_list -> List.exists partial_match patt_list
  | Rpatt_construct _ -> true
  | Rpatt_or (p1, p2) -> (partial_match p1) && (partial_match p2)
  | Rpatt_record _ -> true
  | Rpatt_array _ -> true
  | Rpatt_constraint (p, _) -> partial_match p
