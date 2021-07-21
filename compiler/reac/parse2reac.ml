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

(* file: parse2reac.ml *)
(* created: 2004-04-26  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The translation of Parse to Reac *)

open Misc
open Asttypes
open Global
open Parse_ast
open Binding_errors
open Reac_ast
open Reac_misc
open Parse_ident
open Def_types
open Types

module Env =
  Symbol_table.Make
    (struct
      type t = string
      let compare = compare
      let name x = x
    end)



(* Translation of type expressions *)
let rec translate_te typ =
  let rtyp =
    match typ.pte_desc with
    | Ptype_var x -> Rtype_var x

    | Ptype_arrow (t1, t2) ->
	Rtype_arrow (translate_te t1, translate_te t2)

    | Ptype_tuple typ_list ->
	Rtype_product (List.map translate_te typ_list)

    | Ptype_constr (cstr, te_list) ->
	let gcstr = try (Modules.pfind_type_desc cstr.pident_id) with
	| Modules.Desc_not_found ->
	    unbound_type_err cstr.pident_id typ.pte_loc
	in
	Rtype_constr (gcstr, List.map translate_te te_list)

    | Ptype_process (t,k) -> Rtype_process ((translate_te t),k)

  in
  make_te rtyp typ.pte_loc

(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Ptype_abstract -> Rtype_abstract

  | Ptype_rebind typ -> Rtype_rebind (translate_te typ)

  | Ptype_variant constr_te_list ->
      let l =
	List.map
	  (fun (c, typ) ->
	    let id = Ident.create Ident.gen_constr c.psimple_id Ident.Constr in
	    let g = Modules.defined_global id (no_info()) in
	    let _ = Modules.add_constr g in
	    let typ =
	      match typ with
	      | None -> None
	      | Some typ -> Some (translate_te typ)
	    in
	    (g, typ))
	  constr_te_list
      in
      Rtype_variant l

  | Ptype_record l ->
      let l =
	List.map
	  (fun (lab, flag, typ) ->
	    let id = Ident.create Ident.gen_label lab.psimple_id Ident.Label in
	    let g = Modules.defined_global id (no_info()) in
	    let _ = Modules.add_label g in
	    (g, flag, translate_te typ))
	  l
      in
      Rtype_record l

(* Translation of a pattern :
   The function returns the list of new variables and
   the translation of the pattern.
   If a varible is bind twice an error is raised
   or_vars is the list of variables to bind
*)
let translate_pattern, translate_pattern_list, translate_pattern_record =
  let rec translate_pattern or_vars is_global p =
    let vars, rpatt =
      match p.ppatt_desc with
      | Ppatt_any -> [], Rpatt_any

      | Ppatt_var x ->
	  begin try
	    let vp = List.assoc x.psimple_id or_vars in
	    [(x.psimple_id, vp)], Rpatt_var vp
	  with
	  | Not_found ->
	      let id =
		Ident.create Ident.gen_var x.psimple_id Ident.Val_RML
	      in
	      if is_global
	      then
		let gl = Modules.defined_global id (no_info()) in
		let vp = Varpatt_global gl in
		[(x.psimple_id, vp)], Rpatt_var vp
	      else
		let vp = Varpatt_local id in
		[(x.psimple_id, vp)], Rpatt_var vp
	  end

      | Ppatt_alias (patt,x) ->
	  let vars, rpatt = translate_pattern or_vars is_global patt in
	  if List.mem_assoc x.psimple_id vars
	  then multiply_bound_variable_err x.psimple_id p.ppatt_loc
	  else
	    begin try
	      let vp = List.assoc x.psimple_id or_vars in
	      (x.psimple_id, vp) :: vars, Rpatt_alias (rpatt, vp)
	    with
	    | Not_found ->
		let id =
		  Ident.create Ident.gen_var x.psimple_id Ident.Val_RML
		in
		if is_global
		then
		  let gl = Modules.defined_global id (no_info()) in
		  let vp = Varpatt_global gl in
		  (x.psimple_id, vp) :: vars, Rpatt_alias (rpatt, vp)
		else
		  let vp = Varpatt_local id in
		  (x.psimple_id, vp) :: vars, Rpatt_alias (rpatt, vp)
	    end

      | Ppatt_constant im -> [], Rpatt_constant im

      | Ppatt_tuple patt_list ->
	  let vars, rpatt_list =
	    translate_pattern_list or_vars is_global patt_list
	  in
	  vars, Rpatt_tuple rpatt_list

      | Ppatt_construct (constr, None) ->
	  let gconstr = try Modules.pfind_constr_desc constr.pident_id with
	  | Modules.Desc_not_found ->
	      unbound_constr_err constr.pident_id constr.pident_loc
	  in
	  [], Rpatt_construct (gconstr, None)

      | Ppatt_construct (constr, Some patt) ->
	  let gconstr = try Modules.pfind_constr_desc constr.pident_id with
	  | Modules.Desc_not_found ->
	      unbound_constr_err constr.pident_id constr.pident_loc
	  in
	  let vars, rpatt = translate_pattern or_vars is_global patt in
	  vars, Rpatt_construct (gconstr, Some rpatt)

      | Ppatt_or (patt1, patt2) ->
	  let vars1, rpatt1 = translate_pattern or_vars is_global patt1 in
	  let vars2, rpatt2 = translate_pattern vars1 is_global patt2 in
	  if List.for_all (fun (x,_) -> List.mem_assoc x vars1) vars2 &&
	    List.for_all (fun (x,_) -> List.mem_assoc x vars2) vars1
	  then
	    vars1, Rpatt_or (rpatt1, rpatt2)
	  else
	    orpat_vars p.ppatt_loc

      | Ppatt_record l ->
	  let vars, l = translate_pattern_record or_vars is_global l in
	  vars, Rpatt_record(l)

      | Ppatt_array patt_list ->
	  let vars, rpatt_list =
	    translate_pattern_list or_vars is_global patt_list
	  in
	  vars, Rpatt_array rpatt_list

      | Ppatt_constraint (patt,typ) ->
	  let vars, rpatt = translate_pattern or_vars is_global patt in
	  let rtyp = translate_te typ in
	  vars, Rpatt_constraint (rpatt, rtyp)

    in
    vars, make_patt rpatt p.ppatt_loc


(* Translation of a list of patterns *)
  and translate_pattern_list or_vars is_global =
    let rec translate_pattern_list vars patt_list rpatt_list =
      match patt_list with
      | [] -> vars, rpatt_list

      | patt :: patt_list ->
	  let new_vars, rpatt = translate_pattern or_vars is_global patt in
	  let vars =
	    List.fold_left
	      (fun acc ((x,_) as e)  ->
		if List.mem_assoc x vars
		then multiply_bound_variable_err x patt.ppatt_loc
		else e::acc)
	      vars new_vars
	  in
	  translate_pattern_list vars patt_list (rpatt::rpatt_list)
    in
    fun patt_list ->
      let vars, rpatt_list = translate_pattern_list [] patt_list [] in
      (vars, List.rev rpatt_list)

(* Translation of a list of (label, pattern) *)
  and translate_pattern_record or_vars is_global =
    let rec translate_pattern_record vars lab_patt_list rlab_rpatt_list =
      match lab_patt_list with
      | [] -> vars, rlab_rpatt_list

      | (lab,patt) :: lab_patt_list ->
	  let glab = try Modules.pfind_label_desc lab.pident_id with
	  | Modules.Desc_not_found ->
	      unbound_label_err lab.pident_id lab.pident_loc
	  in
	  let new_vars, rpatt = translate_pattern or_vars is_global patt in
	  let vars =
	    List.fold_left
	      (fun acc ((x,_) as e)  ->
		if List.mem_assoc x vars
		then multiply_bound_variable_err x patt.ppatt_loc
		else e::acc)
	      vars new_vars
	  in
	  translate_pattern_record
	    vars
	    lab_patt_list
	    ((glab,rpatt) :: rlab_rpatt_list)
    in
    fun lab_patt_list ->
      let vars, rlab_rpatt_list =
	translate_pattern_record [] lab_patt_list []
      in
      (vars, List.rev rlab_rpatt_list)
  in
  (fun ?(or_vars=[]) is_global p -> translate_pattern or_vars is_global p),
  translate_pattern_list [],
  translate_pattern_record []

(* Translation of identifier *)
let translate_ident env x =
  match x.pident_id with
  | Pdot (mod_name,s) ->
      Rexpr_global (Modules.pfind_value_desc x.pident_id)

  | Pident s ->
      try
	let id = Env.find s env in
	Rexpr_local id
      with
      | Not_found ->
	  Rexpr_global (Modules.pfind_value_desc x.pident_id)

(* Translation of expressions *)
let rec translate env e =
  let rexpr =
    match e.pexpr_desc with
    | Pexpr_ident x ->
	begin
	  try translate_ident env x with
	  | Modules.Desc_not_found ->
	      unbound_variable_err x.pident_id e.pexpr_loc
	end

    | Pexpr_constant im -> Rexpr_constant im

    | Pexpr_let (_, [patt, {pexpr_desc= Pexpr_get s;}], expr) ->
	let vars, rpatt = translate_pattern false patt in
	let new_env = add_varpatt env vars in
	Rexpr_get(translate env s,
		  rpatt, translate new_env expr)

    | Pexpr_let (rec_flag, patt_expr_list, expr) ->
	let env, rpatt_rexpr_list =
	  translate_let false env rec_flag patt_expr_list
	in
	Rexpr_let (rec_flag, rpatt_rexpr_list, translate env expr)

    | Pexpr_function patt_expr_list ->
	Rexpr_function (translate_match env patt_expr_list)

    | Pexpr_apply (expr, expr_list) ->
	let rexpr = translate env expr in
	let rexpr_list = List.map (translate env) expr_list in
	Rexpr_apply (rexpr, rexpr_list)

    | Pexpr_tuple expr_list ->
	let rexpr_list = List.map (translate env) expr_list in
	Rexpr_tuple rexpr_list

    | Pexpr_construct (constr, expr_opt) ->
	let gconstr = try Modules.pfind_constr_desc constr.pident_id with
	| Modules.Desc_not_found ->
	    unbound_constr_err constr.pident_id constr.pident_loc
	in
	Rexpr_construct (gconstr, opt_map (translate env) expr_opt)

    | Pexpr_array expr_list ->
	let rexpr_list = List.map (translate env) expr_list in
	Rexpr_array rexpr_list

    | Pexpr_record lab_expr_list ->
	Rexpr_record (translate_record env lab_expr_list)

    | Pexpr_record_access (expr, lab) ->
	let glab = try Modules.pfind_label_desc lab.pident_id with
	| Modules.Desc_not_found ->
	    unbound_label_err lab.pident_id lab.pident_loc
	in
	Rexpr_record_access (translate env expr, glab)

    | Pexpr_record_with (expr, lab_expr_list) ->
        Rexpr_record_with (translate env expr, translate_record env lab_expr_list)

    | Pexpr_record_update (e1, lab, e2) ->
	let glab = try Modules.pfind_label_desc lab.pident_id with
	| Modules.Desc_not_found ->
	    unbound_label_err lab.pident_id lab.pident_loc
	in
	Rexpr_record_update (translate env e1,
			     glab,
			     translate env e2)

    | Pexpr_constraint (expr,typ) ->
	Rexpr_constraint (translate env expr, translate_te typ)

    | Pexpr_trywith (expr, patt_expr_list) ->
	Rexpr_trywith (translate env expr,
		       translate_match env patt_expr_list)

    | Pexpr_assert expr -> Rexpr_assert (translate env expr)

    | Pexpr_ifthenelse (e1, e2, None) ->
	let tr_e1 = translate env e1 in
	let tr_e2 = translate env e2 in
	Rexpr_ifthenelse (tr_e1,
			  tr_e2,
			  make_expr (Rexpr_constant(Const_unit))
			    Location.none)

    | Pexpr_ifthenelse (e1, e2, Some e3) ->
	let tr_e1 = translate env e1 in
	let tr_e2 = translate env e2 in
	let tr_e3 = translate env e3 in
	Rexpr_ifthenelse (tr_e1,
			  tr_e2,
			  tr_e3)

    | Pexpr_match (expr, patt_expr_list) ->
	Rexpr_match(translate env expr,
		    translate_match env patt_expr_list)

    | Pexpr_while (e1, e2) ->
	let tr_e1 = translate env e1 in
	let tr_e2 = translate env e2 in
	Rexpr_while(tr_e1, tr_e2)

    | Pexpr_for (i,e1,e2,flag,e3) ->
	let id = Ident.create Ident.gen_var i.psimple_id Ident.Val_ML in
	let env = Env.add i.psimple_id id env in
	let tr_e1 = translate env e1 in
	let tr_e2 = translate env e2 in
	let tr_e3 = translate env e3 in
	Rexpr_for (id,
		   tr_e1,
		   tr_e2,
		   flag,
		   tr_e3)

    | Pexpr_seq (e1, e2) ->
	let tr_e1 = translate env e1 in
	let tr_e2 = translate env e2 in
	Rexpr_seq [tr_e1;
		   tr_e2]

    | Pexpr_process (expr) ->
	Rexpr_process (translate env expr)

    | Pexpr_pre (flag, expr) ->
	Rexpr_pre (flag, translate env expr)

    | Pexpr_last expr ->
	Rexpr_last (translate env expr)

    | Pexpr_default expr ->
	Rexpr_default (translate env expr)

    | Pexpr_emit s ->
	Rexpr_emit (translate env s, None)

    | Pexpr_emit_val (s,expr) ->
	Rexpr_emit (translate env s,
		    Some (translate env expr))

    | Pexpr_signal (sig_typ_list, None, expr) ->
	(translate_signal env sig_typ_list None expr).expr_desc
    | Pexpr_signal (sig_typ_list, Some(k,e1,e2), expr) ->
	let comb = Some(k, translate env e1, translate env e2) in
	(translate_signal env sig_typ_list comb expr).expr_desc

    | Pexpr_fordopar (i, e1, e2, flag, e3) ->
	let id = Ident.create Ident.gen_var i.psimple_id Ident.Val_RML in
	let env = Env.add i.psimple_id id env in
	Rexpr_fordopar(id,
		       translate env e1,
		       translate env e2,
		       flag,
		       translate env e3)

    | Pexpr_nothing -> Rexpr_nothing

    | Pexpr_pause -> Rexpr_pause K_not_boi

    | Pexpr_halt -> Rexpr_halt K_not_boi

    | Pexpr_loop expr ->
	Rexpr_loop (None, translate env expr)

    | Pexpr_par (e1, e2) ->
	Rexpr_par [translate env e1;
		   translate env e2]

    | Pexpr_merge (e1, e2) ->
	Rexpr_merge(translate env e1,
		    translate env e2)

    | Pexpr_run (expr) ->
	Rexpr_run (translate env expr)

    | Pexpr_until (expr, conf_when_opt_expr_opt_list) ->
        let rexpr = translate env expr in
        let rconf_when_opt_expr_opt_list =
          List.map
            (fun (conf, when_opt, expr_opt) ->
              let vars, rconf = translate_conf env conf in
              let new_env = add_varpatt env vars in
              (rconf,
               opt_map (translate new_env) when_opt,
               opt_map (translate new_env) expr_opt))
            conf_when_opt_expr_opt_list
        in
        Rexpr_until (rexpr, rconf_when_opt_expr_opt_list)

    | Pexpr_when (conf, expr) ->
        let rexpr = translate env expr in
        let vars, rconf = translate_conf env conf in
        if vars <> [] then event_config_err conf.pconf_loc;
        Rexpr_when (rconf, rexpr)

    | Pexpr_control (conf, expr_opt, expr) ->
        let rexpr = translate env expr in
        let vars, rconf = translate_conf env conf in
        let new_env = add_varpatt env vars in
        Rexpr_control (rconf,
                       opt_map (translate new_env) expr_opt,
                       rexpr)

    | Pexpr_present (conf, e1, e2) ->
        let vars, rconf = translate_conf env conf in
        if vars <> [] then event_config_err conf.pconf_loc;
        Rexpr_present(rconf,
		      translate env e1,
		      translate env e2)

    | Pexpr_await (flag, conf) ->
        let vars, rconf = translate_conf env conf in
        if vars <> [] then event_config_err conf.pconf_loc;
        Rexpr_await (flag, rconf)

    | Pexpr_await_val (flag, k, conf, when_opt, expr) ->
        let vars, rconf = translate_conf env conf in
        let new_env = add_varpatt env vars in
        Rexpr_await_val (flag,
                         k,
                         rconf,
                         opt_map (translate new_env) when_opt,
                         translate new_env expr)

    | Pexpr_get _ ->
	raise (Internal (e.pexpr_loc,
			 "Parse2reac.translate: expr"))
  in
  make_expr rexpr e.pexpr_loc

(* Translation of event configurations *)
and translate_conf =
  let rec translate_conf or_vars env c =
    let vars, rconf =
      match c.pconf_desc with
      | Pconf_present (e, None) ->
          [], Rconf_present (translate env e, None)

      | Pconf_present (e, Some patt) ->
          let vars, rpatt = translate_pattern ~or_vars:or_vars false patt in
          vars, Rconf_present (translate env e, Some rpatt)

      | Pconf_and (conf1, conf2) ->
          let vars1, rconf1 = translate_conf or_vars env conf1 in
          let vars2, rconf2 = translate_conf or_vars env conf2 in
	  let vars =
	    List.fold_left
	      (fun acc ((x,_) as e)  ->
		if List.mem_assoc x vars1
		then multiply_bound_variable_err x c.pconf_loc
		else e::acc)
	      vars1 vars2
	  in
	  vars, Rconf_and (rconf1, rconf2)

      | Pconf_or (conf1, conf2) ->
          let vars1, rconf1 = translate_conf or_vars env conf1 in
          let vars2, rconf2 = translate_conf vars1 env conf2 in
          if List.for_all (fun (x,_) -> List.mem_assoc x vars1) vars2 &&
            List.for_all (fun (x,_) -> List.mem_assoc x vars2) vars1
          then
            vars1, Rconf_or (rconf1, rconf2)
          else
            orconfig_vars c.pconf_loc
    in
    vars, make_conf rconf c.pconf_loc
  in
  (fun env c -> translate_conf [] env c)

(* Translation of let definitions in an ML context *)
and translate_let is_global env rec_flag patt_expr_list =
  let patt_list, expr_list = List.split patt_expr_list in
  let new_vars, rpatt_list = translate_pattern_list is_global patt_list in
  let tr_env =
    if rec_flag = Recursive
    then add_varpatt env new_vars
    else env
  in
  let rpatt_rexpr_list =
    List.map2
      (fun rpatt expr -> rpatt,	translate tr_env expr)
      rpatt_list
      expr_list
  in
  let new_env =
    if rec_flag <> Recursive
    then add_varpatt env new_vars
    else tr_env
  in
  new_env, rpatt_rexpr_list

(* Translation of a pair of pattern and expression *)
and translate_patt_expr env (patt,expr)=
    let vars, rpatt = translate_pattern false patt in
    let env = add_varpatt env vars in
    let rexpr = translate env expr in
    (rpatt, rexpr)

(* Translation of a triple of pattern, expression option and expression *)
and translate_patt_expr_opt_expr env (patt,expr_opt,expr)=
    let vars, rpatt = translate_pattern false patt in
    let env = add_varpatt env vars in
    let rexpr_opt = opt_map (translate env) expr_opt in
    let rexpr = translate env expr in
    (rpatt, rexpr_opt, rexpr)

(* Translation of match in an ML context *)
and translate_match =
  fun env patt_expr_list ->
    List.map (translate_patt_expr_opt_expr env) patt_expr_list

(* Translation of record *)
and translate_record env lab_expr_list =
  List.map
    (fun (lab,expr) ->
      let glab = try Modules.pfind_label_desc lab.pident_id with
      | Modules.Desc_not_found ->
	  unbound_label_err lab.pident_id lab.pident_loc
      in
      (glab, translate env expr))
    lab_expr_list

(* Translation of signal declatation *)
and translate_signal env sig_typ_list comb expr =
  match sig_typ_list with
  | [] -> translate env expr
  | (s,typ) :: sig_typ_list ->
      let (id, rtyp) =
	Ident.create Ident.gen_var s.psimple_id Ident.Sig,
	opt_map translate_te typ
      in
      let env = Env.add s.psimple_id id env in
      make_expr
	(Rexpr_signal
	   ((id, rtyp),
	    comb,
	    translate_signal env sig_typ_list comb expr))
	Location.none

(* Add a varpatt in the environment *)
and add_varpatt env vars =
  List.fold_left
    (fun env (x,vp) ->
      match vp with
      | Varpatt_local id -> Env.add x id env
      | Varpatt_global gl -> Modules.add_value gl; env) env vars


(* Translation of type declatations *)
let translate_type_declaration l =
  let l_rename =
    List.map
      (fun (name, param, typ) ->
	let id = Ident.create Ident.gen_type name.psimple_id Ident.Type in
	let gl = Modules.defined_global id (no_info()) in
	let info = { type_constr = { gi = gl.gi;
				     info =
				     Some {constr_abbr = Constr_notabbrev}};
		     type_kind = Type_abstract;
		     type_arity = List.length param; }
	in
	let _ =
	  gl.info <- Some info;
	  Modules.add_type gl
	in
	(gl, param, typ))
      l
  in
  List.map
    (fun (gl, param, typ) -> (gl, param, translate_type_decl typ))
    l_rename

(* Translation of implementation item *)
let translate_impl_item info_fmt item =
  let ritem =
    match item.pimpl_desc with
    | Pimpl_expr expr -> Rimpl_expr (translate Env.empty expr)

    | Pimpl_let (flag, patt_expr_list) ->
	let env, rpatt_rexpr_list =
	  translate_let true Env.empty flag patt_expr_list
	in
	Rimpl_let (flag, rpatt_rexpr_list)

    | Pimpl_signal (sig_typ_list, comb_opt) ->
	Rimpl_signal
	  (List.map
	     (fun (s,ty_opt) ->
	       let id = Ident.create Ident.gen_var s.psimple_id Ident.Sig in
	       let gl = Modules.defined_global id (no_info()) in
	       let _ = Modules.add_value gl in
	       let rty_opt = opt_map translate_te ty_opt in
	       let rcomb_opt =
		 opt_map
		   (fun (k,e1,e2) ->
		     (k, translate Env.empty e1, translate Env.empty e2))
		   comb_opt
	       in
	       (gl,rty_opt), rcomb_opt)
	     sig_typ_list)

    | Pimpl_type l ->
	let l_translate = translate_type_declaration l in
	Rimpl_type l_translate

    | Pimpl_exn (name, typ) ->
	let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id (no_info()) in
	let _ = Modules.add_constr gl in
	Rimpl_exn (gl, opt_map translate_te typ)

    | Pimpl_exn_rebind (name, gl_name) ->
	let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id (no_info()) in
	let _ = Modules.add_constr gl in
	let gtype = try Modules.pfind_constr_desc gl_name.pident_id with
	| Modules.Desc_not_found ->
	    unbound_type_err gl_name.pident_id gl_name.pident_loc
	in
	Rimpl_exn_rebind(gl, gtype)

    | Pimpl_open s ->
	Modules.open_module s;
	Rimpl_open s

    | Pimpl_lucky _ ->
	raise (Internal (item.pimpl_loc,
			 "Parse2reac.translate_impl_item: must be translated"))

  in
  make_impl ritem item.pimpl_loc

(* Translation of interfacr item *)
let translate_intf_item info_fmt item =
  let ritem =
    match item.pintf_desc with
    | Pintf_val (s, t) ->
	let id = Ident.create Ident.gen_var s.psimple_id Ident.Val_ML in
	let gl = Modules.defined_global id (no_info()) in
	let _ = Modules.add_value gl in
	Rintf_val (gl, translate_te t)

    | Pintf_type l ->
	let l_translate = translate_type_declaration l in
	Rintf_type l_translate

    | Pintf_exn (name, typ) ->
	let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id (no_info()) in
	let _ = Modules.add_constr gl in
	Rintf_exn (gl, opt_map translate_te typ)

    | Pintf_open s ->
	Modules.open_module s;
	Rintf_open s

  in
  make_intf ritem item.pintf_loc
