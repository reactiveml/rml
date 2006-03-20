(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac2reac.ml                                               *)
(*  Date de creation : 07/09/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Source to source transformations *)

open Reac_ast
open Def_types
open Reac_misc

(* Generic source to source transformation for Reac_ast *)
let expr_map  =
  let rec config_map f config =
    let loc = config.conf_loc in
    match config.conf_desc with
    | Rconf_present e -> 
	let e' = f e in
	make_conf (Rconf_present e') loc 
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
	  f (make_expr (Rexpr_let (rec_flag, patt_expr_list', expr')) loc)
	    
      | Rexpr_function patt_expr_list ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr (Rexpr_function patt_expr_list') loc)
	    
      | Rexpr_apply (e, expr_list) ->
	  let e' = expr_map f e in
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr (Rexpr_apply (e', expr_list')) loc)
	    
      | Rexpr_tuple expr_list ->
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr (Rexpr_tuple expr_list') loc)
	    
      | Rexpr_construct (const, None) ->
	  f expr
      | Rexpr_construct (const, Some e) ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_construct (const, Some e')) loc)
	    
      | Rexpr_array expr_list ->
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr (Rexpr_array expr_list') loc)
	    
      | Rexpr_record lbl_expr_list ->
	  let lbl_expr_list' =
	    List.map (fun (lbl,e) -> (lbl, expr_map f e)) lbl_expr_list
	  in
	  f (make_expr (Rexpr_record lbl_expr_list') loc)
	    
      | Rexpr_record_access (e, lbl) -> 
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_record_access (e', lbl)) loc)
	    
      | Rexpr_record_update (e1, lbl, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_record_update (e1', lbl, e2')) loc)
	    
      | Rexpr_constraint (e, ty) ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_constraint (e',ty)) loc)
	    
      | Rexpr_trywith (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr (Rexpr_trywith(e', patt_expr_list')) loc)
	    
      | Rexpr_assert e ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_assert e') loc)
	    
      | Rexpr_ifthenelse(e,e1,e2) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_ifthenelse(e',e1',e2')) loc)
	    
      | Rexpr_match (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr (Rexpr_match (e', patt_expr_list')) loc)
	    
      | Rexpr_when_match (e1,e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_when_match (e1', e2')) loc)
	    
      | Rexpr_while (e1,e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_while (e1', e2')) loc)
	    
      | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr ( Rexpr_for (ident, e1', e2', direction_flag, e')) loc)
	    
      | Rexpr_seq e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr (Rexpr_seq e_list') loc)
	    
      | Rexpr_process e ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_process e') loc)
	    
      | Rexpr_pre (pre_kind, e) ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_pre (pre_kind, e')) loc)
	    
      | Rexpr_nothing ->
	  f expr
	    
      | Rexpr_pause ->
	  f expr

      | Rexpr_halt ->
	  f expr
	    
      | Rexpr_emit e ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_emit e') loc)
	    
      | Rexpr_emit_val (e1, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_emit_val (e1', e2')) loc)
	    
      | Rexpr_loop e ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_loop e') loc)
	    
      | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr 
	       (Rexpr_fordopar (ident, e1', e2', direction_flag, e')) 
	       loc)
	    
      | Rexpr_par e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr (Rexpr_par e_list') loc)
	    
      | Rexpr_merge (e1, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_merge (e1',e2')) loc)
	    
      | Rexpr_signal (id_tyexpr_opt, None, e) ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_signal (id_tyexpr_opt, None, e')) loc)
      | Rexpr_signal (id_tyexpr_opt, Some(e1,e2), e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_signal (id_tyexpr_opt, Some(e1',e2'), e')) loc)
	    
      | Rexpr_run e ->
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_run e') loc)
	    
      | Rexpr_until (config, e, None) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr ( Rexpr_until (config', e', None)) loc)
      | Rexpr_until (config, e, Some(p,e1)) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr ( Rexpr_until (config', e', Some(p,e1'))) loc)
	    
      | Rexpr_when (config, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_when (config',e')) loc)
	    
      | Rexpr_control (config, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr (Rexpr_control (config', e')) loc)
	    
      | Rexpr_get (e,patt,e1) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr (Rexpr_get (e',patt,e1')) loc)
	    
      | Rexpr_present (config, e1, e2) ->
	  let config' = config_map f config in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr (Rexpr_present (config', e1', e2')) loc)
	    
      | Rexpr_await (immediate_flag, config) ->
	  let config' = config_map f config in
	  f (make_expr (Rexpr_await (immediate_flag, config')) loc)
	    
      | Rexpr_await_val (immediate, kind, e, patt, e1) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr (Rexpr_await_val (immediate, kind, e', patt, e1')) loc)
    in
    expr'.expr_type <- expr.expr_type;
    expr'.expr_static <- expr.expr_static;
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
	    | (id_tyexpr_opt, Some(e1,e2)) ->
		let e1' = expr_map f e1 in
		let e2' = expr_map f e2 in
		(id_tyexpr_opt, Some(e1',e2'))
	    | decl -> decl)
	  decl_list
      in
      make_impl (Rimpl_signal decl_list') loc

  | _ -> impl



(* Translate binary seq and par to n-ary operators *)
let binary2nary e =
  let e' =
    match e.expr_desc with
    | Rexpr_seq e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_seq e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
      in Rexpr_seq (f [] e_list)
	  
    | Rexpr_par e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_par e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
	in Rexpr_par (f [] e_list)
    | e -> e
  in make_expr e' e.expr_loc


