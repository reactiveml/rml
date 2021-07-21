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

(* file: lco2caml.ml *)
(* created: 2004-06-04  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The translation of Lco to Caml *)

open Lco_ast
open Caml_ast
open Caml_misc
open Global
open Global_ident
open Asttypes
open Misc


(* Translation of type expressions *)
let rec translate_te typ =
  let ctyp =
    match typ.cote_desc with
    | Cotype_var x -> Ctype_var x
    | Cotype_arrow (t1, t2) ->
	Ctype_arrow (translate_te t1, translate_te t2)
    | Cotype_product typ_list ->
	Ctype_product (List.map translate_te typ_list)
    | Cotype_constr (cstr, te_list) ->
	Ctype_constr (cstr, List.map translate_te te_list)
    | Cotype_process t ->
	let proc_type = make_rml_type "process" [translate_te t] in
	proc_type.cte_desc
  in
  make_te ctyp typ.cote_loc

let pattern_of_signal (s,t) =
  let ps =
    make_patt (Cpatt_var (Cvarpatt_local s)) Location.none
  in
  match t with
  | None -> ps
  | Some t ->
      make_patt (Cpatt_constraint(ps, translate_te t)) Location.none

let pattern_of_signal_global (s,t) =
  let ps =
    make_patt (Cpatt_var (Cvarpatt_global s)) Location.none
  in
  match t with
  | None -> ps
  | Some t ->
      make_patt (Cpatt_constraint(ps, translate_te t)) Location.none


(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Cotype_abstract -> Ctype_abstract
  | Cotype_rebind t -> Ctype_rebind (translate_te t)
  | Cotype_variant constr_te_list ->
      let l =
	List.map
	  (fun (c, typ_opt) ->
	    let typ_opt =
	      match typ_opt with
	      | None -> None
	      | Some typ -> Some (translate_te typ)
	    in
	    (c, typ_opt))
	  constr_te_list
      in
      Ctype_variant l
  | Cotype_record l ->
      let l =
	List.map
	  (fun (lab, flag, typ) ->
	    (lab, flag, translate_te typ))
	  l
      in
      Ctype_record l

(* Translation of a pattern *)
let rec translate_pattern p =
  let cpatt =
    match p.copatt_desc with
    | Copatt_any -> Cpatt_any
    | Copatt_var x ->
	begin
	  match x with
	  | Covarpatt_global gl -> Cpatt_var (Cvarpatt_global gl)
	  | Covarpatt_local id -> Cpatt_var (Cvarpatt_local id)
	end
    | Copatt_alias (patt, x) ->
	let vp =
	  match x with
	  | Covarpatt_global gl -> Cvarpatt_global gl
	  | Covarpatt_local id ->  Cvarpatt_local id
	in
	Cpatt_alias (translate_pattern patt, vp)
    | Copatt_constant im -> Cpatt_constant im
    | Copatt_tuple l ->
	Cpatt_tuple (List.map translate_pattern l)
    | Copatt_construct (constr, patt_opt) ->
	Cpatt_construct (constr, opt_map translate_pattern patt_opt)
    | Copatt_or (p1, p2) ->
	Cpatt_or (translate_pattern p1, translate_pattern p2)
    | Copatt_record l ->
	Cpatt_record (List.map (fun (l,p) -> (l,translate_pattern p)) l)
    | Copatt_array l ->
	Cpatt_array (List.map translate_pattern l)
    | Copatt_constraint (patt, typ) ->
	Cpatt_constraint (translate_pattern patt, translate_te typ)
  in
  make_patt cpatt p.copatt_loc

(* Translation of ML expressions *)
let rec translate_ml e =
  let cexpr =
    match e.coexpr_desc with
    | Coexpr_local id -> Cexpr_local id

    | Coexpr_global gl -> Cexpr_global gl

    | Coexpr_constant im -> Cexpr_constant im

    | Coexpr_let (flag, patt_expr_list, expr) ->
	Cexpr_let (flag,
		   List.map
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     patt_expr_list,
		   translate_ml expr)

    | Coexpr_function  patt_expr_list ->
	Cexpr_function
          (List.map
	     (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
	     patt_expr_list)

    | Coexpr_apply (expr, expr_list) ->
	Cexpr_apply (translate_ml expr,
		     List.map translate_ml expr_list)

    | Coexpr_tuple expr_list ->
	Cexpr_tuple (List.map translate_ml expr_list)

    | Coexpr_construct (c, expr_opt) ->
	Cexpr_construct (c, opt_map translate_ml expr_opt)

    | Coexpr_array l ->
	Cexpr_array (List.map translate_ml l)

    | Coexpr_record l ->
	Cexpr_record (List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Coexpr_record_access (expr, label) ->
	Cexpr_record_access (translate_ml expr, label)

    | Coexpr_record_with (expr, l) ->
	Cexpr_record_with (translate_ml expr,
                           List.map (fun (lab,e) -> lab, translate_ml e) l)


    | Coexpr_record_update (e1, label, e2) ->
	Cexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Coexpr_constraint (expr, typ) ->
	Cexpr_constraint (translate_ml expr, translate_te typ)

    | Coexpr_trywith (expr, l) ->
	Cexpr_trywith
          (translate_ml expr,
	   List.map
	     (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
	     l)

    | Coexpr_assert expr -> Cexpr_assert (translate_ml expr)

    | Coexpr_ifthenelse (e1, e2, e3) ->
	Cexpr_ifthenelse (translate_ml e1,
			  translate_ml e2,
			  translate_ml e3)

    | Coexpr_match (expr, l) ->
	Cexpr_match
          (translate_ml expr,
	   List.map
	     (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
	     l)

    | Coexpr_while(e1, e2) ->
	Cexpr_while (translate_ml e1, translate_ml e2)

    | Coexpr_for (id, e1, e2, flag, e3) ->
	Cexpr_for (id,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_ml e3)

    | Coexpr_seq (e1, e2) ->
	Cexpr_seq (translate_ml e1, translate_ml e2)

    | Coexpr_process (p) ->
	Cexpr_constraint
	  (make_expr
	     (Cexpr_function [make_patt_unit(), None, translate_proc p])
	     e.coexpr_loc,
	   make_rml_type "process" [make_te Ctype_any Location.none])


    | Coexpr_pre(flag, s) ->
	let kind =
	  match flag with
	  | Status -> "status"
	  | Value -> "value"
	in
	Cexpr_apply
	  (make_instruction ("rml_pre_"^kind),
	   [translate_ml s])

    | Coexpr_last (s) ->
	Cexpr_apply
	  (make_instruction "rml_last",
	   [translate_ml s;])

    | Coexpr_default (s) ->
	Cexpr_apply
	  (make_instruction "rml_default",
	   [translate_ml s;])

    | Coexpr_emit (s) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit",
	   [translate_ml s;])

    | Coexpr_emit_val (s, e) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit_val",
	   [translate_ml s;
	    translate_ml e;])

    | Coexpr_signal (s, None, e) ->
	Cexpr_let (Nonrecursive,
		   [pattern_of_signal s,
		    make_expr
		      (Cexpr_apply
			 (make_instruction "rml_global_signal",
			  [make_expr
			     (Cexpr_constant Const_unit)
			     Location.none]))
		      Location.none],
		   translate_ml e)

    | Coexpr_signal (s, Some(Default,e1,e2), e) ->
	Cexpr_let (Nonrecursive,
		   [pattern_of_signal s,
		    make_expr
		      (Cexpr_apply
			 (make_instruction "rml_global_signal_combine",
			  [translate_ml e1;
			   translate_ml e2;]))
		      Location.none],
		   translate_ml e)

    | Coexpr_signal (s, Some(Memory,e1,e2), e) ->
	Cexpr_let (Nonrecursive,
		   [pattern_of_signal s,
		    make_expr
		      (Cexpr_apply
			 (make_instruction "rml_global_signal_memory_combine",
			  [translate_ml e1;
			   translate_ml e2;]))
		      Location.none],
		   translate_ml e)

    | Coexpr_exec p ->
        let hook = make_rml_exec_hook () in
        Cexpr_apply
          (make_module_value !Misc.rml_machine_module "rml_exec",
           [hook;
            translate_ml p])

  in
  make_expr cexpr e.coexpr_loc

(* Embedding of ML expressions in a process *)
and embed_ml e =
  make_expr
    (Cexpr_function [make_patt_unit(), None, translate_ml e])
    e.coexpr_loc

(* Translation of process *)
and translate_proc e =
  let cexpr =
    match e.coproc_desc with
    | Coproc_nothing -> (make_instruction "rml_nothing").cexpr_desc

    | Coproc_pause K_not_boi ->
	(make_instruction "rml_pause").cexpr_desc

    | Coproc_pause K_boi ->
	(make_instruction "rml_pause_kboi").cexpr_desc

    | Coproc_halt K_not_boi ->
	(make_instruction "rml_halt").cexpr_desc

    | Coproc_halt K_boi ->
	(make_instruction "rml_halt_kboi").cexpr_desc

    | Coproc_compute (expr) ->
	Cexpr_apply
	  (make_instruction "rml_compute",
	   [embed_ml expr;])

    | Coproc_emit (s) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_emit'",
	     [translate_ml s;])
	else
	  Cexpr_apply
	    (make_instruction "rml_emit",
	     [embed_ml s;])

    | Coproc_emit_val (s, e) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_emit_val'",
	     [translate_ml s;
	      embed_ml e;])
	else
	  Cexpr_apply
	    (make_instruction "rml_emit_val",
	     [embed_ml s;
	      embed_ml e;])

    | Coproc_loop (None, k) ->
	Cexpr_apply
	  (make_instruction "rml_loop",
	   [translate_proc k])

    | Coproc_loop (Some e, k) ->
	Cexpr_apply
	  (make_instruction "rml_loop_n",
	   [embed_ml e; translate_proc k])

    | Coproc_while (e1, k) ->
	Cexpr_apply
	  (make_instruction "rml_while",
	   [embed_ml e1;
	    translate_proc k])

    | Coproc_for (i, e1, e2, flag, k) ->
	Cexpr_apply
	  (make_instruction "rml_for",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr
	      (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	    make_expr
	      (Cexpr_function [make_patt
				 (Cpatt_var (Cvarpatt_local i))
				 Location.none,
                               None,
			       translate_proc k])
	      Location.none;])

    | Coproc_fordopar (i, e1, e2, flag, k) ->
	Cexpr_apply
	  (make_instruction "rml_fordopar",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr
	      (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	    make_expr
	      (Cexpr_function [make_patt
				 (Cpatt_var (Cvarpatt_local i))
				 Location.none,
                               None,
			       translate_proc k])
	      Location.none;])

    | Coproc_seq (k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_seq",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_par [k1; k2] ->
	Cexpr_apply
	  (make_instruction "rml_par",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_par k_list ->
	let c_list = List.map (fun k -> translate_proc k) k_list in
	Cexpr_apply
	  (make_instruction "rml_par_n",
	   [ make_list c_list ])

    | Coproc_merge (k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_merge",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_signal (s, None, k) ->
	Cexpr_apply
	  (make_instruction "rml_signal",
	   [make_expr
	      (Cexpr_function [pattern_of_signal s, None, translate_proc k])
	      Location.none])

    | Coproc_signal (s, Some(Default,e1,e2), k) ->
	Cexpr_apply
	  (make_instruction "rml_signal_combine",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr
	      (Cexpr_function [pattern_of_signal s, None, translate_proc k])
	      Location.none])

    | Coproc_signal (s, Some(Memory,e1,e2), k) ->
	Cexpr_apply
	  (make_instruction "rml_signal_memory_combine",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr
	      (Cexpr_function [pattern_of_signal s, None, translate_proc k])
	      Location.none])

    | Coproc_def ((patt, expr), k) ->
	if Lco_misc.is_value expr then
	  Cexpr_let
	    (Nonrecursive,
	     [translate_pattern patt, translate_ml expr],
	     translate_proc k)
	else
	  Cexpr_apply
	    (make_instruction "rml_def",
	     [embed_ml expr;
	      make_expr
		(Cexpr_function
                   [translate_pattern patt, None, translate_proc k])
		Location.none])

    | Coproc_def_dyn ((patt, k1), k2) ->
	Cexpr_apply
	  (make_instruction "rml_def_dyn",
	   [translate_proc k1;
	    make_expr
	      (Cexpr_function [translate_pattern patt, None, translate_proc k2])
	      Location.none])

    | Coproc_def_and_dyn (patt_proc_list, k) ->
	let caml_patt_proc_list =
	  List.map
	    (fun (patt,proc) ->
	      translate_pattern patt,
	      translate_proc proc)
	    patt_proc_list
	in
	let patt_array, proc_array =
	  let patt, proc = List.split caml_patt_proc_list in
	  make_patt
	    (Cpatt_array patt)
	    Location.none,
	  make_expr
	    (Cexpr_array proc)
	    Location.none
	in
	Cexpr_apply
	  (make_instruction "rml_def_and_dyn",
	   [proc_array;
	    make_expr
	      (Cexpr_function
		 [(patt_array, None, translate_proc k);
		  (make_patt_any(), None, make_raise_RML())])
	      Location.none])

    | Coproc_run (expr) ->
	Cexpr_apply
	  (make_instruction "rml_run",
	   [embed_ml expr;])

    | Coproc_until (k,
                    [{coconf_desc = Coconf_present (s, None)}, None, None]) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_until'",
	     [translate_ml s;
	      translate_proc k])
	else
	  Cexpr_apply
	    (make_instruction "rml_until",
	     [embed_ml s;
	      translate_proc k])

    | Coproc_until (k, [conf, when_opt, kh_opt]) ->
        let event_kind, cevt, cpatt =
          match conf.coconf_desc with
          | Coconf_present (s, patt_opt) ->
              let event_kind, cevt =
                if Lco_misc.is_value s then ("'", translate_ml s)
                else ("", embed_ml s)
              in
              let cpatt =
                match patt_opt with
                | None -> make_patt_any()
                | Some patt -> translate_pattern patt
              in
              event_kind, cevt, cpatt
          | Coconf_and (_, _) | Coconf_or (_, _) ->
              let cevt, cpatt = translate_conf conf in
              "_conf", cevt, cpatt
        in
        let is_partial_match = Caml_misc.partial_match cpatt in
        let match_kind, matching =
          match is_partial_match, when_opt with
          | (true, _) | (_, Some _) ->
              let match_kind = "_match" in
              let matching =
                make_expr
	          (Cexpr_function
		     [cpatt, opt_map translate_ml when_opt, make_true();
		      make_patt_any(), None, make_false();])
	          Location.none
              in
              match_kind, [matching]
          | false, None -> "", []
        in
        let handler_kind, handler =
          match kh_opt, when_opt with
          | Some kh, _ ->
	      let handler =
                make_expr
	          (Cexpr_function
		     ((cpatt, None, translate_proc kh) ::
                      if is_partial_match then
                        [(make_patt_any(), None, make_raise_RML())]
                      else []))
	          Location.none
              in
              "_handler", [handler]
          | None, Some _ ->
	      let handler =
                make_expr
	          (Cexpr_function
		     ((cpatt, None, make_instruction "rml_nothing") ::
                      if is_partial_match then
                        [(make_patt_any(), None, make_raise_RML())]
                      else []))
	          Location.none
              in
              "_handler", [handler]
          | None, None -> "", []
        in
        Cexpr_apply
	  (make_instruction ("rml_until"^handler_kind^match_kind^event_kind),
           [cevt] @ matching @ [translate_proc k] @ handler)

    | Coproc_until (k, conf_when_opt_kh_opt_list) ->
        let add_case case_list (patt, when_opt, expr) =
          let case_list =
            List.map
              (fun (patt, when_opt, expr) ->
                (make_patt
                   (Cpatt_tuple [ make_patt_some patt; make_patt_any(); ])
                   Location.none,
                 when_opt,
                 expr))
              case_list
          in
          let case =
            make_patt
              (Cpatt_tuple [ make_patt_any(); make_patt_some patt; ])
              Location.none,
            when_opt,
            expr
          in
          case_list @ [ case ]
        in
        let cconf_when_opt_kh_opt_list =
          List.map
            (fun (conf, when_opt, kh_opt) ->
              let cevt, cpatt = translate_conf conf in
              let cwhen_opt = opt_map translate_ml when_opt in
              let handler =
                match kh_opt with
                | Some kh -> translate_proc kh
                | None -> make_instruction "rml_nothing"
              in
              (cevt, cpatt, cwhen_opt, handler))
            conf_when_opt_kh_opt_list
        in
        let cconf, matching_cases, handler_cases =
          match cconf_when_opt_kh_opt_list with
          | [] -> assert false
          | (cevt, cpatt, cwhen_opt, handler) :: l ->
              List.fold_left
                (fun (cconf, matching_cases, handler_cases)
                    (cevt, cpatt, cwhen_opt, handler) ->
                  let cconf =
                    make_expr
                      (Cexpr_apply
                         (make_instruction "cfg_or_option", [cconf; cevt]))
                      Location.none
                  in
                  let matching_cases =
                    add_case matching_cases (cpatt, cwhen_opt, make_true())
                  in
                  let handler_cases =
                    add_case handler_cases (cpatt, None, handler)
                  in
                  (cconf, matching_cases, handler_cases))
                (cevt,
                 [cpatt, cwhen_opt, make_true()],
                 [cpatt, None, handler])
                l
        in
        Cexpr_apply
          (make_instruction "rml_until_handler_match_conf",
           [cconf;
            make_expr
              (Cexpr_function
                 (matching_cases @
                  [(make_patt_any(), None, make_false())]))
              Location.none;
            translate_proc k;
            make_expr
              (Cexpr_function
                 (handler_cases @
                  [(make_patt_any(), None, make_raise_RML())]))
              Location.none])

    | Coproc_when ({coconf_desc = Coconf_present (s, None)}, k) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_when'",
	     [translate_ml s;
	      translate_proc k])
	else
	  Cexpr_apply
	    (make_instruction "rml_when",
	     [embed_ml s;
	      translate_proc k])

    | Coproc_when (conf, k) ->
        let cconf, _ = translate_conf conf in
	Cexpr_apply
	  (make_instruction "rml_when_conf",
	   [cconf;
	    translate_proc k])

    | Coproc_control ({coconf_desc = Coconf_present (s, None)}, None, k) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_control'",
	     [translate_ml s;
	      translate_proc k])
	else
	  Cexpr_apply
	    (make_instruction "rml_control",
	     [embed_ml s;
	      translate_proc k])
    | Coproc_control ({coconf_desc = Coconf_present(s, patt_opt)}, Some e, k) ->
        let cpatt =
          match patt_opt with
          | None -> make_patt_any()
          | Some patt -> translate_pattern patt
        in
	let matching =
	  if Caml_misc.partial_match cpatt then
	    make_expr
	      (Cexpr_function
		 [cpatt, None, translate_ml e;
		  make_patt_any(), None, make_false();])
	      Location.none
	  else
	    make_expr
	      (Cexpr_function [cpatt, None, translate_ml e])
	      Location.none
	in
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_control_match'",
	     [translate_ml s;
	      matching;
	      translate_proc k])
	else
	  Cexpr_apply
	    (make_instruction "rml_control_match",
	     [embed_ml s;
	      matching;
	      translate_proc k])

    | Coproc_control (conf, when_opt, k) ->
        let cconf, cpatt = translate_conf conf in
        begin match Caml_misc.partial_match cpatt, when_opt with
        | false, None ->
	    Cexpr_apply
	      (make_instruction "rml_control_conf",
	       [cconf;
	        translate_proc k])
        | (true, _) | (_, Some _) ->
            Cexpr_apply
              (make_instruction "rml_control_match_conf",
               [cconf;
                make_expr
                  (Cexpr_function
                     [cpatt, opt_map translate_ml when_opt, make_true();
                      make_patt_any(), None, make_false();])
                  Location.none;
                translate_proc k]
              )
        end

    | Coproc_get (s, patt, k) ->
	Cexpr_apply
	  (make_instruction "rml_get",
	   [embed_ml s;
	    make_expr
	      (Cexpr_function [translate_pattern patt, None, translate_proc k])
	      Location.none])

    | Coproc_present ({coconf_desc = Coconf_present (s, None)}, k1, k2) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_present'",
	     [translate_ml s;
	      translate_proc k1;
	      translate_proc k2])
	else
	  Cexpr_apply
	    (make_instruction "rml_present",
	     [embed_ml s;
	      translate_proc k1;
	      translate_proc k2])
    | Coproc_present (conf, k1, k2) ->
        let cconf, _ = translate_conf conf in
	Cexpr_apply
	  (make_instruction "rml_present_conf",
	   [cconf;
	    translate_proc k1;
	    translate_proc k2])

    | Coproc_ifthenelse (expr, k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_if",
	   [embed_ml expr;
	    translate_proc k1;
	    translate_proc k2])

    | Coproc_match (expr, patt_proc_list) ->
	Cexpr_apply
	  (make_instruction "rml_match",
	   [embed_ml expr;
	    make_expr
	      (Cexpr_function
		 (List.map
		    (fun (patt,when_opt,k) ->
                      (translate_pattern patt,
                       opt_map translate_ml when_opt,
                       translate_proc k))
		    patt_proc_list))
	      Location.none])

    | Coproc_await (Nonimmediate, {coconf_desc = Coconf_present (s, None)}) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_await'",
	     [translate_ml s])
	else
	  Cexpr_apply
	    (make_instruction "rml_await",
	     [embed_ml s])
    | Coproc_await (Nonimmediate, conf) ->
        let cconf, _ = translate_conf conf in
	Cexpr_apply
          (make_instruction "rml_await_conf",
           [cconf])
    | Coproc_await (Immediate, {coconf_desc = Coconf_present (s, None)}) ->
	if Lco_misc.is_value s then
	  Cexpr_apply
	    (make_instruction "rml_await_immediate'",
	     [translate_ml s])
	else
	  Cexpr_apply
	    (make_instruction "rml_await_immediate",
	     [embed_ml s])
    | Coproc_await (Immediate, conf) ->
        let cconf, _ = translate_conf conf in
	Cexpr_apply
	  (make_instruction "rml_await_immediate_conf",
	   [cconf])

    | Coproc_await_val (flag1, flag2, conf, when_opt, k) ->
        let im =
          match flag1 with
          | Immediate -> "_immediate"
          | Nonimmediate -> ""
        in
        let kind =
          match flag2 with
          | One -> "_one"
          | All -> "_all"
        in
        let event_kind, cevt, cpatt =
          match conf.coconf_desc with
          | Coconf_present (s, patt_opt) ->
              let event_kind, cevt =
                if Lco_misc.is_value s then ("'", translate_ml s)
                else ("", embed_ml s)
              in
              let cpatt =
                match patt_opt with
                | None -> make_patt_any()
                | Some patt -> translate_pattern patt
              in
              event_kind, cevt, cpatt
          | Coconf_and (_, _) | Coconf_or (_, _) ->
              let cevt, cpatt = translate_conf conf in
              "_conf", cevt, cpatt
        in
        begin match Caml_misc.partial_match cpatt, when_opt with
        | false, None ->
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^event_kind),
               [cevt;
                make_expr
                  (Cexpr_function [cpatt, None, translate_proc k])
                  Location.none])
        | (true as partial_match, _) | (partial_match, Some _) ->
            let matching =
              make_expr
                (Cexpr_function
                   [cpatt, opt_map translate_ml when_opt, make_true();
                    make_patt_any(), None, make_false();])
                Location.none
            in
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^"_match"^event_kind),
               [cevt;
                matching;
                make_expr
                  (Cexpr_function
                     ((cpatt, None, translate_proc k)::
                      if partial_match then
                        [(make_patt_any(), None, make_raise_RML())]
                      else
                        []))
                  Location.none])
        end

  in
  make_expr cexpr e.coproc_loc

and translate_conf c =
  let cexpr, cpatt =
    match c.coconf_desc with
    | Coconf_present (s, patt_opt) ->
        let cexpr =
	  if Lco_misc.is_value s then
	    Cexpr_apply
	      (make_instruction "cfg_present'",
	       [translate_ml s])
	  else
	    Cexpr_apply
	      (make_instruction "cfg_present",
	       [embed_ml s])
        in
        let cpatt =
          match patt_opt with
          | None -> make_patt Cpatt_any c.coconf_loc
          | Some p -> translate_pattern p
        in
        cexpr, cpatt
    | Coconf_and(c1,c2) ->
        let cexpr1, cpatt1 = translate_conf c1 in
        let cexpr2, cpatt2 = translate_conf c2 in
	(Cexpr_apply (make_instruction "cfg_and", [cexpr1; cexpr2]),
         make_patt (Cpatt_tuple [cpatt1; cpatt2]) c.coconf_loc)

    | Coconf_or(c1,c2) ->
        let cexpr1, cpatt1 = translate_conf c1 in
        let cexpr2, cpatt2 = translate_conf c2 in
        let cpatt1 =
          make_patt
            (Cpatt_tuple [ make_patt_some cpatt1; make_patt_any(); ])
            Location.none
        in
        let cpatt2 =
          make_patt
            (Cpatt_tuple [ make_patt_any(); make_patt_some cpatt2; ])
            Location.none
        in
        (Cexpr_apply (make_instruction "cfg_or_option", [cexpr1; cexpr2]),
         make_patt (Cpatt_or (cpatt1, cpatt2)) c.coconf_loc)
  in
  make_expr cexpr c.coconf_loc, cpatt


let translate_impl_item info_chan item =
  let citem =
    match item.coimpl_desc with
    | Coimpl_expr e -> Cimpl_expr (translate_ml e)
    | Coimpl_let (flag, l) ->
	Cimpl_let (flag,
		   List.map
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     l)
    | Coimpl_signal l ->
	Cimpl_let (Nonrecursive,
		   List.map
		     (function
		       | ((s,ty_opt), None) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal",
				 [make_expr
				    (Cexpr_constant Const_unit)
				    Location.none]))
			     Location.none
		       | ((s,ty_opt), Some(Default,e1,e2)) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal_combine",
				 [translate_ml e1;
				  translate_ml e2;]))
			     Location.none
		       | ((s,ty_opt), Some(Memory,e1,e2)) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal_memory_combine",
				 [translate_ml e1;
				  translate_ml e2;]))
			     Location.none)
		     l)
    | Coimpl_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Cimpl_type l
    | Coimpl_exn (name, typ) ->
	Cimpl_exn (name, opt_map translate_te typ)
    | Coimpl_exn_rebind (name, gl_name) ->
	Cimpl_exn_rebind(name, gl_name)
    | Coimpl_open s ->
	Cimpl_open s
  in
  make_impl citem item.coimpl_loc

let translate_intf_item info_chan item =
  let citem =
    match item.cointf_desc with
    | Cointf_val (gl, typ) -> Cintf_val (gl, translate_te typ)

    | Cointf_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Cintf_type l

    | Cointf_exn (name, typ) ->
	Cintf_exn (name, opt_map translate_te typ)

    | Cointf_open m -> Cintf_open m

  in
  make_intf citem item.cointf_loc
