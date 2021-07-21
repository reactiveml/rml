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

(* file: reac2reac.ml *)
(* created: 2005-09-07  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Source to source transformations *)

open Asttypes
open Reac_ast
open Def_types
open Reac_misc

(* Generic source to source transformation for Reac_ast *)
let expr_map  =
  let rec config_map f config =
    let loc = config.conf_loc in
    match config.conf_desc with
    | Rconf_present (e, patt_opt) ->
	let e' = f e in
	make_conf (Rconf_present (e', patt_opt)) loc
    | Rconf_and (c1, c2) ->
	let c1' = config_map f c1 in
	let c2' = config_map f c2 in
	make_conf (Rconf_and (c1', c2')) loc
    | Rconf_or (c1, c2) ->
	let c1' = config_map f c1 in
	let c2' = config_map f c2 in
	make_conf (Rconf_or (c1', c2')) loc
  in
  let rec expr_map f expr =
    let typ = expr.expr_type in
    let static = expr.expr_static in
    let reactivity = expr.expr_reactivity in
    let reactivity_effect = expr.expr_reactivity_effect in
    let loc = expr.expr_loc in
    let expr' =
      match expr.expr_desc with
      | Rexpr_local _ -> f expr

      | Rexpr_global _ -> f expr

      | Rexpr_constant _ -> f expr

      | Rexpr_let (rec_flag, patt_expr_list, expr) ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  let expr' = expr_map f expr in
	  f (make_expr_all (Rexpr_let (rec_flag, patt_expr_list', expr'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_function patt_expr_list ->
	  let patt_expr_list' =
	    List.map (fun (p, when_opt, e) ->
              (p, Misc.opt_map (expr_map f) when_opt , expr_map f e))
              patt_expr_list
	  in
	  f (make_expr_all
	       (Rexpr_function patt_expr_list')
	       typ static reactivity reactivity_effect loc)

      | Rexpr_apply (e, expr_list) ->
	  let e' = expr_map f e in
	  let expr_list' =
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all
	       (Rexpr_apply (e', expr_list'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_tuple expr_list ->
	  let expr_list' =
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all
	       (Rexpr_tuple expr_list')
               typ static reactivity reactivity_effect loc)

      | Rexpr_construct (const, None) ->
	  f expr
      | Rexpr_construct (const, Some e) ->
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_construct (const, Some e'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_array expr_list ->
	  let expr_list' =
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all
	       (Rexpr_array expr_list')
               typ static reactivity reactivity_effect loc)

      | Rexpr_record lbl_expr_list ->
	  let lbl_expr_list' =
	    List.map (fun (lbl,e) -> (lbl, expr_map f e)) lbl_expr_list
	  in
	  f (make_expr_all
	       (Rexpr_record lbl_expr_list')
               typ static reactivity reactivity_effect loc)

      | Rexpr_record_access (e, lbl) ->
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_record_access (e', lbl))
               typ static reactivity reactivity_effect loc)

      | Rexpr_record_with (e, lbl_expr_list) ->
          let e' = expr_map f e in
          let lbl_expr_list' =
	    List.map (fun (lbl,e) -> (lbl, expr_map f e)) lbl_expr_list
	  in
          f (make_expr_all
               (Rexpr_record_with (e', lbl_expr_list'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_record_update (e1, lbl, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_record_update (e1', lbl, e2'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_constraint (e, ty) ->
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_constraint (e',ty))
               typ static reactivity reactivity_effect loc)

      | Rexpr_trywith (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p, when_opt, e) ->
              (p, Misc.opt_map (expr_map f) when_opt , expr_map f e))
              patt_expr_list
	  in
	  f (make_expr_all
	       (Rexpr_trywith(e', patt_expr_list'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_assert e ->
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_assert e')
               typ static reactivity reactivity_effect loc)

      | Rexpr_ifthenelse(e,e1,e2) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all
	       (Rexpr_ifthenelse(e',e1',e2'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_match (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p, when_opt, e) ->
              (p, Misc.opt_map (expr_map f) when_opt , expr_map f e))
              patt_expr_list
	  in
	  f (make_expr_all
	       (Rexpr_match (e', patt_expr_list'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_while (e1,e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_while (e1', e2'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all ( Rexpr_for (ident, e1', e2', direction_flag, e'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_seq e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr_all (Rexpr_seq e_list')
               typ static reactivity reactivity_effect loc)

      | Rexpr_process e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_process e')
               typ static reactivity reactivity_effect loc)

      | Rexpr_pre (pre_kind, e) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_pre (pre_kind, e'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_last e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_last e')
               typ static reactivity reactivity_effect loc)

      | Rexpr_default e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_default e')
               typ static reactivity reactivity_effect loc)

      | Rexpr_nothing ->
	  f expr

      | Rexpr_pause _ ->
	  f expr

      | Rexpr_halt _ ->
	  f expr

      | Rexpr_emit (e, None) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_emit (e', None))
               typ static reactivity reactivity_effect loc)

      | Rexpr_emit (e1, Some e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all
	       (Rexpr_emit (e1', Some e2'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_loop (n_opt, e) ->
	  let n_opt' = Misc.opt_map (expr_map f) n_opt in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_loop (n_opt', e'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_fordopar (ident, e1', e2', direction_flag, e'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_par e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr_all (Rexpr_par e_list')
               typ static reactivity reactivity_effect loc)

      | Rexpr_merge (e1, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_merge (e1',e2'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_signal (id_tyexpr_opt, None, e) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_signal (id_tyexpr_opt, None, e'))
	       typ static reactivity reactivity_effect loc)
      | Rexpr_signal (id_tyexpr_opt, Some(k, e1,e2), e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_signal (id_tyexpr_opt, Some(k,e1',e2'), e'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_run e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_run e')
               typ static reactivity reactivity_effect loc)

      | Rexpr_until (e, config_when_opt_e_opt_list) ->
	  let e' = expr_map f e in
          let config_when_opt_e_opt_list' =
            List.map
              (fun (config, when_opt, e_opt) ->
	        let config' = config_map f config in
                let when_opt' = Misc.opt_map (expr_map f) when_opt in
	        let e_opt' = Misc.opt_map (expr_map f) e_opt in
                (config', when_opt', e_opt'))
              config_when_opt_e_opt_list
          in
	  f (make_expr_all
	       (Rexpr_until (e', config_when_opt_e_opt_list'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_when (config, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_when (config',e'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_control (config, None, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_control (config', None, e'))
               typ static reactivity reactivity_effect loc)
      | Rexpr_control (config, Some e1, e) ->
	  let config' = config_map f config in
	  let e1' = expr_map f e1 in
	  let e' = expr_map f e in
	  f (make_expr_all
	       (Rexpr_control (config', Some e1', e'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_get (e,patt,e1) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr_all (Rexpr_get (e',patt,e1'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_present (config, e1, e2) ->
	  let config' = config_map f config in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all
	       (Rexpr_present (config', e1', e2'))
               typ static reactivity reactivity_effect loc)

      | Rexpr_await (immediate_flag, config) ->
	  let config' = config_map f config in
	  f (make_expr_all (Rexpr_await (immediate_flag, config'))
	       typ static reactivity reactivity_effect loc)

      | Rexpr_await_val (immediate, kind, config, when_opt, e1) ->
	  let config' = config_map f config in
          let when_opt' = Misc.opt_map (expr_map f) when_opt in
	  let e1' = expr_map f e1 in
	  f (make_expr_all
               (Rexpr_await_val (immediate, kind, config', when_opt', e1'))
	       typ static reactivity reactivity_effect loc)
    in
    expr'.expr_type <- expr.expr_type;
    expr'.expr_static <- expr.expr_static;
    expr'.expr_reactivity <- expr.expr_reactivity;
    expr'.expr_reactivity_effect <- expr.expr_reactivity_effect;
    expr'
  in
  expr_map

let impl_map f impl =
  let loc = impl.impl_loc in
  match impl.impl_desc with
  | Rimpl_expr e -> make_impl (Rimpl_expr (expr_map f e)) loc
  | Rimpl_let (rec_flag, patt_expr_list) ->
      let patt_expr_list' =
	List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
      in
      make_impl (Rimpl_let (rec_flag, patt_expr_list')) loc

  | Rimpl_signal decl_list ->
      let decl_list' =
	List.map
	  (function
	    | (id_tyexpr_opt, Some(k,e1,e2)) ->
		let e1' = expr_map f e1 in
		let e2' = expr_map f e2 in
		(id_tyexpr_opt, Some(k,e1',e2'))
	    | decl -> decl)
	  decl_list
      in
      make_impl (Rimpl_signal decl_list') loc

  | _ -> impl




(* Print static information *)
let print_static e =
  Format.fprintf !Misc.err_fmt
    "%a%s@."
    Location.print e.expr_loc
    (Def_static.string_of_static (snd e.expr_static));
  e


(* Check left branche of |> operator and annotate pause statement *)
let translate_merge =
  let merge_error exp =
    Format.fprintf !Misc.err_fmt
      "%aThis expression is not allowed on the left of a |> operator.\n"
      Location.print exp.expr_loc;
    raise Misc.Error
  in

  let annotate_pause expr =
    begin match expr.expr_desc with
    | Rexpr_pause _ ->
	{ expr with expr_desc = Rexpr_pause K_boi }
    | Rexpr_halt _ ->
	{ expr with expr_desc = Rexpr_halt K_boi }
    | Rexpr_local _
    | Rexpr_global _
    | Rexpr_constant _
    | Rexpr_let _
    | Rexpr_function _
    | Rexpr_apply _
    | Rexpr_tuple _
    | Rexpr_construct _
    | Rexpr_array _
    | Rexpr_record _
    | Rexpr_record_access _
    | Rexpr_record_with _
    | Rexpr_record_update _
    | Rexpr_constraint _
    | Rexpr_trywith _
    | Rexpr_assert _
    | Rexpr_ifthenelse _
    | Rexpr_match _
    | Rexpr_while _
    | Rexpr_for _
    | Rexpr_seq _
    | Rexpr_process _
    | Rexpr_pre _
    | Rexpr_last _
    | Rexpr_default _
    | Rexpr_nothing
    | Rexpr_emit _
    | Rexpr_loop _
    | Rexpr_fordopar _
    | Rexpr_par _
    | Rexpr_merge _
    | Rexpr_signal _ ->
	expr
    | Rexpr_run _
    | Rexpr_until _
    | Rexpr_when _
    | Rexpr_control _
    | Rexpr_present _
    | Rexpr_await _
    | Rexpr_await_val _ ->
	merge_error expr
    | Rexpr_get _ ->
	merge_error expr
    end
  in
  fun expr ->
    begin match expr.expr_desc with
    | Rexpr_merge (e1, e2) ->
	{ expr with expr_desc = Rexpr_merge (expr_map annotate_pause e1, e2) }
    | _ -> expr
    end


