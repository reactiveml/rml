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

(* file: remove_when.ml *)
(* created: 2005-07-27  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Replace de "when" constructs by "await" *)

open Reac_ast
open Reac_misc



let tr =
  let rec tr_config ctrl config =
    let c =
      match config.conf_desc with
      | Rconf_present e ->
	  let e' = tr ctrl e in
	  Rconf_present e'
      | Rconf_and (c1, c2) ->
	  let c1' = tr_config ctrl c1 in
	  let c2' = tr_config ctrl c2 in
	  Rconf_and (c1', c2')
      | Rconf_or (c1, c2) ->
	  let c1' = tr_config ctrl c1 in
	  let c2' = tr_config ctrl c2 in
	  Rconf_or (c1', c2')
    in
    make_conf c config.conf_loc
  and tr ctrl expr =
    let rexpr =
      match expr.expr_desc with
      | Rexpr_local _ as e -> e

      | Rexpr_global _ as e -> e

      | Rexpr_constant _ as e -> e

      | Rexpr_let (rec_flag, patt_expr_list, expr) ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, tr ctrl e)) patt_expr_list
	  in
	  let expr' = tr ctrl expr in
	  Rexpr_let (rec_flag, patt_expr_list', expr')

      | Rexpr_function patt_expr_list ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, tr ctrl e)) patt_expr_list
	  in
	  Rexpr_function patt_expr_list'

      | Rexpr_apply (e, expr_list) ->
	  let e' = tr ctrl e in
	  let expr_list' =
	    List.map (fun e -> tr ctrl e) expr_list
	  in
	  Rexpr_apply (e', expr_list')

      | Rexpr_tuple expr_list ->
	  let expr_list' =
	    List.map (fun e -> tr ctrl e) expr_list
	  in
	  Rexpr_tuple expr_list'

      | Rexpr_construct (_, None) as e-> e
      | Rexpr_construct (const, Some e) ->
	  let e' = tr ctrl e in
	  Rexpr_construct (const, Some e')

      | Rexpr_array expr_list ->
	  let expr_list' =
	    List.map (fun e -> tr ctrl e) expr_list
	  in
	  Rexpr_array expr_list'

      | Rexpr_record lbl_expr_list ->
	  let lbl_expr_list' =
	    List.map (fun (lbl,e) -> (lbl, tr ctrl e)) lbl_expr_list
	  in
	  Rexpr_record lbl_expr_list'

      | Rexpr_record_access (e, lbl) ->
	  let e' = tr ctrl e in
	  Rexpr_record_access (e', lbl)

      | Rexpr_record_update (e1, lbl, e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_record_update (e1', lbl, e2')

      | Rexpr_constraint (e, ty) ->
	  let e' = tr ctrl e in
	  Rexpr_constraint (e',ty)

      | Rexpr_trywith (e, patt_expr_list) ->
	  let e' = tr ctrl e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, tr ctrl e)) patt_expr_list
	  in
	  Rexpr_trywith (e', patt_expr_list')

      | Rexpr_assert e ->
	  let e' = tr ctrl e in
	  Rexpr_assert e'

      | Rexpr_ifthenelse (e,e1,e2) ->
	  let e' = tr ctrl e in
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_ifthenelse (e',e1',e2')

      | Rexpr_match (e, patt_expr_list) ->
	  let e' = tr ctrl e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, tr ctrl e)) patt_expr_list
	  in
	  Rexpr_match (e', patt_expr_list')

      | Rexpr_when_match (e1,e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_when_match (e1', e2')

      | Rexpr_while (e1,e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_while (e1', e2')

      | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  let e' = tr ctrl e in
	  Rexpr_for (ident, e1', e2', direction_flag, e')

      | Rexpr_seq (e1, e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_seq (e1',e2')

      | Rexpr_process e ->
	  let id =
	    Ident.create Initialization.gen_ident "__ctrl" Ident.Internal
	  in
	  let c =
	    make_conf
	      (Rconf_present (make_expr (Rexpr_local id) Location.none))
	      Location.none
	  in
	  let e' = tr c e in
	  Rexpr_function
	    [ make_patt (Rpatt_var (Varpatt_local id)) Location.none,
	      make_expr (Rexpr_process e') Location.none ]

      | Rexpr_pre (pre_kind, e) ->
	  let e' = tr ctrl e in
	  Rexpr_pre (pre_kind, e')

      | Rexpr_nothing -> Rexpr_nothing

      | Rexpr_pause ->
	  begin match ctrl with
	  | None -> Rexpr_pause
	  | Some c ->
	      Rexpr_seq
		(make_expr Rexpr_pause Location.none,
		 make_expr (Rexpr_await (Immediate, c)) Location.none)
	  end

      | Rexpr_halt -> Rexpr_halt (* ???? *)

      | Rexpr_emit e ->
	  let e' = tr ctrl e in
	  Rexpr_emit e'

      | Rexpr_emit_val (e1, e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_emit_val (e1', e2')

      | Rexpr_loop e ->
	  let e' = tr ctrl e in
	  Rexpr_loop e'

      | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  let e' = tr ctrl e in
	  Rexpr_fordopar (ident, e1', e2', direction_flag, e')

      | Rexpr_par (e1, e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_par (e1',e2')

      | Rexpr_merge (e1, e2) ->
	  let e1' = tr ctrl e1 in
	  let e2' = tr ctrl e2 in
	  Rexpr_merge (e1',e2')

      | Rexpr_signal (id_tyexpr_opt, None, e) ->
	  let e' = tr f e in
	  Rexpr_signal (id_tyexpr_opt, None, e')
      | Rexpr_signal (id_tyexpr_opt, Some(e1,e2), e) ->
	  let e1' = tr f e1 in
	  let e2' = tr f e2 in
	  let e' = tr f e in
	  Rexpr_signal (id_tyexpr_opt, Some(e1,e2), e')

      | Rexpr_run e ->
	  let e' = tr ctrl e in
	  begin match ctrl with
	  | None -> Rexpr_run e'
	  | Some c ->
	      Rexpr_run
		(make_expr
		   (Rexpr_apply
		      (e', [ make_expr (c) Location.none ]))
		   Location.none)
	  end

      | Rexpr_until (config, e, None) ->
	  let config' = tr_config config in
	  let e' = tr ctrl e in
	  begin match ctrl with
	  | None ->
	      Rexpr_until (config', e', None)
	  | Some c ->
	      let config'' =
		make_conf
		  (Rconf_and (c, config'))
		  Location.none
	      in
	      Rexpr_until (config'', e', None)
	  end
      | Rexpr_until (config, e, Some(p,e1)) ->
	  let config' = tr_config config in
	  let e' = tr ctrl e in
	  let e1' = tr ctrl e1 in
	  begin match ctrl with
	  | None ->
	      Rexpr_until (config', e', Some(p,e1'))
	  | Some c ->
	      let config'' =
		make_conf
		  (Rconf_and (c, config'))
		  Location.none
	      in
	      let e1'' =
		make_expr
		  (Rexpr_seq
		     (make_expr (Rexpr_await (Immediate, c)) Location.none),
		      e1')
	      in
	      Rexpr_until (config'', e', Some(p,e1''))
	  end

(*
      | Rexpr_when (config, e) ->
	  let config' = config_map f config in
	  let e' = tr f e in
	  f (make_expr (Rexpr_when (config',e')) loc)

      | Rexpr_control (config, e) ->
	  let config' = config_map f config in
	  let e' = tr f e in
	  f (make_expr (Rexpr_control (config', e')) loc)

      | Rexpr_get (e,patt,e1) ->
	  let e' = tr f e in
	  let e1' = tr f e1 in
	  f (make_expr (Rexpr_get (e',patt,e1')) loc)

      | Rexpr_present (config, e1, e2) ->
	  let config' = config_map f config in
	  let e1' = tr f e1 in
	  let e2' = tr f e2 in
	  f (make_expr (Rexpr_present (config', e1', e2')) loc)

      | Rexpr_await (immediate_flag, config) ->
	  let config' = config_map f config in
	  f (make_expr (Rexpr_await (immediate_flag, config')) loc)

      | Rexpr_await_val (immediate, kind, e, patt, e1) ->
	  let e' = tr f e in
	  let e1' = tr f e1 in
	  f (make_expr (Rexpr_await_val (immediate, kind, e', patt, e1')) loc)
*)
    in
    make_expr rexpr expr.expr_loc
  in
  tr
