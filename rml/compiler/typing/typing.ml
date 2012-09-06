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

(* file: typing.ml *)

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* created: 2004-05-13  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The type synthesizer *)

open Def_types
open Types
open Typing_errors
open Initialization
open Asttypes
open Global
open Global_ident
open Reac_ast
open Misc
open Annot

let unify_expr expr expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    Unify -> expr_wrong_type_err expr actual_ty expected_ty

let unify_patt pat expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> patt_wrong_type_err pat actual_ty expected_ty

let unify_event evt expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> event_wrong_type_err evt actual_ty expected_ty

let unify_emit loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> emit_wrong_type_err loc actual_ty expected_ty

let unify_run loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> run_wrong_type_err loc actual_ty expected_ty

let unify_var loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> var_wrong_type_err loc actual_ty expected_ty

(* special cases of unification *)
let filter_event ty =
  let ty = type_repr ty in
  let ty1 = new_var() in
  let ty2 = new_var() in
  unify ty (constr_notabbrev event_ident [ty1;ty2]);
  ty1, ty2

let filter_multi_event ty =
  let ty = type_repr ty in
  let ty1 = new_var() in
  unify ty (constr_notabbrev event_ident [ty1;
					  constr_notabbrev list_ident [ty1]]);
  ty1

let is_unit_process desc =
  let sch = desc.value_typ in
  let ty = instance sch in
  let unit_process = process type_unit { proc_static = None } in
  try
    unify unit_process ty;
    true
  with Unify -> false

(* Typing environment *)
module Env = Symbol_table.Make (Ident)


(* checks that every type is defined *)
(* and used with the correct arity *)
let check_type_constr_defined loc gl arity =
  let name = gl.gi in
  let ty_desc = Global.info gl in
  let arity' = ty_desc.type_arity in
  if arity' <> arity
  then type_constr_arity_err name arity' arity loc;
  ty_desc.type_constr

(* find the type of the constructor C *)
let get_type_of_constructor c loc =
  constr_instance (Global.info c)

(* find the type of a label *)
let get_type_of_label label loc =
  label_instance (Global.info label)

(* tests if an expression is expansive *)
let rec is_nonexpansive expr =
  match expr.expr_desc with
  | Rexpr_local _ -> true
  | Rexpr_global _ -> true
  | Rexpr_constant _ -> true
  | Rexpr_tuple l -> List.for_all is_nonexpansive l
  | Rexpr_construct (_, None) -> true
  | Rexpr_construct(_, Some e) -> is_nonexpansive e
  | Rexpr_let(rec_flag, bindings, body) ->
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) bindings &&
      is_nonexpansive body
  | Rexpr_function _ -> true
(*
  | Rexpr_trywith(body, pat_expr_list) ->
      is_nonexpansive body &&
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Rexpr_seq(e1, e2) -> is_nonexpansive e2
*)
  | Rexpr_ifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive ifnot
  | Rexpr_constraint(e, ty) -> is_nonexpansive e
  | Rexpr_array [] -> true
  | Rexpr_record lbl_expr_list ->
      List.for_all (fun (lbl, expr) ->
        (Global.info lbl).lbl_mut == Immutable && is_nonexpansive expr)
        lbl_expr_list
  | Rexpr_record_access(e, lbl) -> is_nonexpansive e
  | Rexpr_when_match(cond, act) -> is_nonexpansive act
  | Rexpr_process _ -> true
  | Rexpr_pre (_, e) -> is_nonexpansive e
  | Rexpr_last e -> is_nonexpansive e
  | Rexpr_default e -> is_nonexpansive e
  | Rexpr_nothing -> true
  | Rexpr_pause _ -> true
  | Rexpr_halt _ -> true
  | Rexpr_emit (e, None) -> is_nonexpansive e
  | Rexpr_emit (e1, Some e2) -> is_nonexpansive e1 && is_nonexpansive e2
  | Rexpr_present (e,e1,e2) ->
      is_nonexpansive_conf e && is_nonexpansive e1 && is_nonexpansive e2
  | Rexpr_await (_, e) ->
      is_nonexpansive_conf e
  | Rexpr_await_val (_, _, s, _, e) ->
      is_nonexpansive s && is_nonexpansive e
  | Rexpr_until (c, e, None) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Rexpr_until (c, e, Some (_, e')) ->
      is_nonexpansive_conf c && is_nonexpansive e && is_nonexpansive e'
  | Rexpr_when (c, e) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Rexpr_control (c, None, e) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Rexpr_control (c, Some (_,  e'), e) ->
      is_nonexpansive_conf c && is_nonexpansive e' && is_nonexpansive e
  | Rexpr_par e_list ->
      List.for_all is_nonexpansive e_list
  | Rexpr_merge (e1, e2) ->
      is_nonexpansive e1 && is_nonexpansive e2
  | _ -> false

and is_nonexpansive_conf c =
  match c.conf_desc with
  | Rconf_present e ->
      is_nonexpansive e
  | Rconf_and (c1,c2) ->
      is_nonexpansive_conf c1 && is_nonexpansive_conf c2
  | Rconf_or (c1,c2) ->
      is_nonexpansive_conf c1 && is_nonexpansive_conf c2

(* Typing functions *)

(* Typing of constants *)
let type_of_immediate i =
  match i with
  | Const_unit -> type_unit
  | Const_bool _ -> type_bool
  | Const_int(i) -> type_int
  | Const_float(i) -> type_float
  | Const_char(c) -> type_char
  | Const_string(c) -> type_string

(* Typing of type expressions *)
let type_of_type_expression typ_vars typexp =
  let rec type_of typexp =
    match typexp.te_desc with
    | Rtype_var s ->
	begin try
	  List.assoc s typ_vars
	with
	  Not_found -> unbound_typ_err s typexp.te_loc
	end

    | Rtype_arrow (t1, t2) ->
	arrow (type_of t1) (type_of t2)

    | Rtype_product (l) ->
	product (List.map type_of l)

    | Rtype_constr (s, ty_list) ->
	let name =
	  check_type_constr_defined typexp.te_loc s (List.length ty_list)
	in
	constr name (List.map type_of ty_list)

    | Rtype_process (ty,k) ->
	process (type_of ty) { proc_static = Some(Proc_def (ref k)); }
  in
  type_of typexp

(* Free variables of a type *)
let free_of_type ty =
  let rec vars v ty =
    match ty.te_desc with
      Rtype_var(x) -> if List.mem x v then v else x::v
    | Rtype_arrow(t1,t2) -> vars (vars v t1) t2
    | Rtype_product(t) ->
	List.fold_left vars v t
    | Rtype_constr(_,t) ->
	List.fold_left vars v t
    | Rtype_process (t, _) -> vars v t
  in vars [] ty

(* translating a declared type expression into an internal type *)
let full_type_of_type_expression typ =
  let lv = free_of_type typ in
  let typ_vars = List.map (fun v -> (v,new_generic_var ())) lv in
  let typ = type_of_type_expression typ_vars typ in
  { ts_binders = List.map snd typ_vars; ts_desc = typ }

(* Typing of patterns *)
let rec type_of_pattern global_env local_env patt ty =
  patt.patt_type <- ty;
  Stypes.record (Ti_patt patt);
  match patt.patt_desc with
  | Rpatt_any ->
      (global_env, local_env)

  | Rpatt_var (Varpatt_global gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.info <- Some { value_typ = forall [] ty };
      (gl::global_env, local_env)
  | Rpatt_var (Varpatt_local x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      global_env, (x,ty)::local_env

  | Rpatt_alias (p,Varpatt_global gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.info <- Some { value_typ = forall [] ty };
      type_of_pattern (gl::global_env) local_env p ty
  | Rpatt_alias (p,Varpatt_local x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      type_of_pattern global_env ((x,ty)::local_env) p ty

  | Rpatt_constant (i) ->
      unify_patt patt ty (type_of_immediate i);
      global_env, local_env

  | Rpatt_tuple (l) ->
      let ty_list = List.map (fun _ -> new_var ()) l in
      unify_patt patt ty (product ty_list);
      type_of_pattern_list global_env local_env l ty_list

  | Rpatt_construct (c, None) ->
      begin
	let { cstr_arg = ty_arg_opt;
	      cstr_res = actual_ty } = get_type_of_constructor c patt.patt_loc
	in
	unify_patt patt ty actual_ty;
	match ty_arg_opt with
	| None -> global_env, local_env
	| Some _ -> constr_arity_err c.gi patt.patt_loc
      end
  | Rpatt_construct (c, Some arg_patt) ->
      begin
	let { cstr_arg = ty_arg_opt;
	      cstr_res = ty_res; } = get_type_of_constructor c patt.patt_loc
	in
	unify_patt patt ty ty_res;
	match ty_arg_opt with
	| None -> constr_arity_err_2 c.gi patt.patt_loc
	| Some ty_arg ->
	    type_of_pattern global_env local_env arg_patt ty_arg
      end

  | Rpatt_or (p1,p2) ->
      let global_env1, local_env1 =
	type_of_pattern global_env local_env p1 ty
      in
      let global_env2, local_env2 =
	type_of_pattern global_env local_env p2 ty
      in
      List.iter
	(fun gl1 ->
	  let gl2 =
	    try
	      List.find (fun gl -> (gl1.gi.id = gl.gi.id)) global_env2
	    with
	    | Not_found -> orpat_vars p2.patt_loc (Ident.name gl1.gi.id)
	  in
	  unify_var p2.patt_loc
	    (Global.info gl1).value_typ.ts_desc
	    (Global.info gl2).value_typ.ts_desc)
	global_env1;
      List.iter
	(fun (x1,ty1) ->
	  let (x2,ty2) =
	    try
	      List.find (fun (x,ty) -> (x1 = x)) local_env2
	    with
	    | Not_found -> orpat_vars p2.patt_loc (Ident.name x1)
	  in
	  unify_var p2.patt_loc ty1 ty2)
	local_env1;
      (* A faire: Verifier si les 2 env sont egaux *)
      global_env2, local_env2

  | Rpatt_record (label_patt_list) ->
      let rec type_of_record global_env local_env label_list label_pat_list =
	match label_pat_list with
	  [] -> global_env, local_env
	| (label,label_pat) :: label_pat_list ->
	    let { lbl_arg = ty_arg;
		  lbl_res = ty_res } = get_type_of_label label patt.patt_loc
	    in
	    (* check that the label appears only once *)
	    if List.mem label label_list
	    then non_linear_record_err label.gi patt.patt_loc;
	    unify_patt patt ty ty_arg;
	    let global_env, local_env =
	      type_of_pattern global_env local_env label_pat ty_res
	    in
	    type_of_record
	      global_env local_env (label :: label_list) label_pat_list
      in
      type_of_record global_env local_env [] label_patt_list

  | Rpatt_array (l) ->
      let ty_var = new_var () in
      unify_patt patt ty (constr_notabbrev array_ident [ty_var]);
      List.fold_left
	(fun (gl_env,lc_env) p -> type_of_pattern gl_env lc_env p ty_var)
	(global_env,local_env) l

  | Rpatt_constraint (p,t) ->
      let new_ty = instance (full_type_of_type_expression t) in
      unify_patt p ty new_ty;
      type_of_pattern global_env local_env p new_ty

and type_of_pattern_list global_env local_env patt_list ty_list =
  match patt_list, ty_list with
  | [], [] -> global_env, local_env
  | p::patt_list, t::ty_list ->
      let global_env, local_env = type_of_pattern global_env local_env p t in
      type_of_pattern_list global_env local_env patt_list ty_list
  | _ -> raise (Internal (Location.none, "type_of_pattern_list"))

(* Typing of expressions *)
let rec type_of_expression env expr =
  let t =
    match expr.expr_desc with
    | Rexpr_constant (i) -> type_of_immediate i

    | Rexpr_local (n) ->
	let typ_sch = Env.find n env in
	instance typ_sch

    | Rexpr_global (n) ->
	instance (Global.info n).value_typ

    | Rexpr_let (flag, patt_expr_list, e) ->
	let gl_env, new_env = type_let (flag = Recursive) env patt_expr_list in
	type_of_expression new_env e

    | Rexpr_function (matching)  ->
	let ty_arg = new_var() in
	let ty_res = new_var() in
	let ty = arrow ty_arg ty_res in
	List.iter
	  (fun (p,e) ->
	    let gl_env, loc_env = type_of_pattern [] [] p ty_arg in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    type_expect new_env e ty_res)
	  matching;
	ty

    | Rexpr_apply (fct, args) ->
	let ty_fct = type_of_expression env fct in
	let rec type_args ty_res = function
	  | [] -> ty_res
	  | arg :: args ->
	      let t1, t2 =
		try
		  filter_arrow ty_res
		with Unify ->
		  application_of_non_function_err fct ty_fct
	      in
	      type_expect env arg t1;
	      type_args t2 args
	in
	type_args ty_fct args
    | Rexpr_tuple (l) ->
	product (List.map (type_of_expression env) l)

    | Rexpr_construct(c,None) ->
	begin
	  let { cstr_arg = ty_arg_opt;
		cstr_res = ty } = get_type_of_constructor c expr.expr_loc
	  in
	  match ty_arg_opt with
	  | None -> ty
	  | Some ty_arg -> constr_arity_err c.gi expr.expr_loc
	end
    | Rexpr_construct (c, Some arg) ->
	begin
	  let { cstr_arg = ty_arg_opt;
		cstr_res = ty_res; } = get_type_of_constructor c expr.expr_loc
	  in
	  match ty_arg_opt with
	  | None -> constr_arity_err_2 c.gi expr.expr_loc
	  | Some ty_arg ->
	      type_expect env arg ty_arg;
	      ty_res
	end

    | Rexpr_array (l) ->
	let ty_var = new_var () in
	List.iter (fun e -> type_expect env e ty_var) l;
	constr_notabbrev array_ident [ty_var]

    | Rexpr_record (l) ->
	let ty = new_var() in
	let rec typing_record label_list label_expr_list =
	  match label_expr_list with
	    [] -> ()
	  | (label,label_expr) :: label_expr_list ->
	      let { lbl_arg = ty_arg;
		    lbl_res = ty_res } = get_type_of_label label expr.expr_loc
	      in
	      (* check that the label appears only once *)
	      if List.mem label label_list
	      then non_linear_record_err label.gi expr.expr_loc;
	      type_expect env label_expr ty_res;
	      unify_expr expr ty ty_arg;
	      typing_record (label :: label_list) label_expr_list
	in
	typing_record [] l;
	ty

    | Rexpr_record_access (e, label) ->
	let { lbl_arg = ty_arg; lbl_res = ty_res } =
	  get_type_of_label label expr.expr_loc
	in
	type_expect env e ty_arg;
	ty_res

    | Rexpr_record_update (e1, label, e2) ->
	let { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
	  get_type_of_label label expr.expr_loc
	in
	if mut = Immutable then label_not_mutable_err expr label.gi;
	type_expect env e1 ty_arg;
	type_expect env e2 ty_res;
	type_unit

    | Rexpr_constraint(e,t) ->
	let expected_ty = instance (full_type_of_type_expression t) in
	type_expect env e expected_ty;
	expected_ty

    | Rexpr_trywith (body,matching) ->
	let ty = type_of_expression env body in
	List.iter
	  (fun (p,e) ->
	    let gl_env, loc_env = type_of_pattern [] [] p type_exn in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    type_expect new_env e ty)
	  matching;
	ty

    | Rexpr_assert e ->
	type_expect env e type_bool;
	new_var()

    | Rexpr_ifthenelse (cond,e1,e2) ->
	type_expect env cond type_bool;
	let ty = type_of_expression env e1 in
	type_expect env e2 ty;
	ty

    | Rexpr_match (body,matching) ->
	let ty_body = type_of_expression env body in
	let ty_res = new_var() in
	List.iter
	  (fun (p,e) ->
	    let gl_env, loc_env = type_of_pattern [] [] p ty_body in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    type_expect new_env e ty_res)
	  matching;
	ty_res

    | Rexpr_when_match (e1,e2) ->
	type_expect env e1 type_bool;
	type_of_expression env e2

    | Rexpr_while (e1,e2) ->
	type_expect env e1 type_bool;
	type_statement env e2;
	type_unit

    | Rexpr_for(i,e1,e2,flag,e3) ->
	type_expect env e1 type_int;
	type_expect env e2 type_int;
	type_statement (Env.add i (forall [] type_int) env) e3;
	type_unit

    | Rexpr_seq e_list ->
	let rec f l =
	  match l with
	  | [] -> assert false
	  | [e] -> type_of_expression env e
	  | e::l ->
	      type_statement env e;
	      f l
	in f e_list

    | Rexpr_process(e) ->
	let ty = type_of_expression env e in
        process ty { proc_static = Some(Proc_def (ref Def_static.Dontknow)); }

    | Rexpr_pre (Status, s) ->
	let ty_s = type_of_expression env s in
	let _, _ty =
	  try
	    filter_event ty_s
	  with Unify ->
	  non_event_err s
	in
	type_bool
    | Rexpr_pre (Value, s) ->
	let ty_s = type_of_expression env s in
	let _, ty =
	  try
	    filter_event ty_s
	  with Unify ->
	  non_event_err s
	in
	ty

    | Rexpr_last s ->
	let ty_s = type_of_expression env s in
	let _, ty =
	  try
	    filter_event ty_s
	  with Unify ->
	  non_event_err s
	in
	ty

    | Rexpr_default s ->
	let ty_s = type_of_expression env s in
	let _, ty =
	  try
	    filter_event ty_s
	  with Unify ->
	  non_event_err s
	in
	ty

    | Rexpr_emit (s, None) ->
	let ty_s = type_of_expression env s in
	let ty, _ =
	  try
	    filter_event ty_s
	  with Unify ->
	    non_event_err s
	in
	unify_emit expr.expr_loc type_unit ty;
	type_unit

    | Rexpr_emit (s, Some e) ->
	let ty_s = type_of_expression env s in
	let ty, _ =
	  try
	    filter_event ty_s
	  with Unify ->
	    non_event_err s
	in
	let ty_e = type_of_expression env e in
	unify_emit e.expr_loc ty ty_e;
	type_unit

    | Rexpr_signal ((s,te_opt), combine_opt, e) ->
	let ty_emit = new_var() in
	let ty_get = new_var() in
	let ty_s = constr_notabbrev event_ident [ty_emit; ty_get] in
	opt_iter
	  (fun te ->
	    unify_event s (instance (full_type_of_type_expression te)) ty_s)
	  te_opt;
	begin
	  match combine_opt with
	  | None ->
	      unify_event s
		(constr_notabbrev event_ident
		   [ty_emit; (constr_notabbrev list_ident [ty_emit])])
		ty_s
	  | Some (default,comb) ->
	      type_expect env default ty_get;
	      type_expect env comb (arrow ty_emit (arrow ty_get ty_get))
	end;
	type_of_expression (Env.add s (forall [] ty_s) env) e

    | Rexpr_nothing -> type_unit

    | Rexpr_pause _ -> type_unit

    | Rexpr_halt _ -> new_var()

    | Rexpr_loop (None, p) ->
	type_statement env p;
	type_unit

    | Rexpr_loop (Some n, p) ->
	type_expect env n type_int;
	type_statement env p;
	type_unit

    | Rexpr_fordopar(i,e1,e2,flag,p) ->
	type_expect env e1 type_int;
	type_expect env e2 type_int;
	type_statement (Env.add i (forall [] type_int) env) p;
	type_unit

    | Rexpr_par p_list ->
	List.iter (fun p -> ignore (type_statement env p)) p_list;
	type_unit

    | Rexpr_merge (p1,p2) ->
	type_statement env p1;
	type_statement env p2;
	type_unit

    | Rexpr_run (e) ->
	let ty_e = type_of_expression env e in
	let ty = new_var() in
	unify_run e.expr_loc
	  (process ty { proc_static = None; }) ty_e;
	ty

    | Rexpr_until (s,p,patt_proc_opt) ->
	begin match patt_proc_opt with
	| None ->
	    type_of_event_config env s;
	    type_expect env p type_unit;
	    type_unit
	| Some _ ->
	    begin match s.conf_desc with
	    | Rconf_present s ->
		let ty_s = type_of_expression env s in
		let ty_emit, ty_get =
		  try
		    filter_event ty_s
		  with Unify ->
		    non_event_err s
		in
		let ty_body = type_of_expression env p in
		opt_iter
		  (fun (patt,proc) ->
		    let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
		    assert (gl_env = []);
		    let new_env =
		      List.fold_left
			(fun env (x, ty) -> Env.add x (forall [] ty) env)
			env loc_env
		    in
		    type_expect new_env proc ty_body)
		  patt_proc_opt;
		ty_body
	    | _ ->
		non_event_err2 s
	    end
	end


    | Rexpr_when (s,p) ->
	type_of_event_config env s;
	type_of_expression env p

    | Rexpr_control (s, None, p) ->
	type_of_event_config env s;
	type_of_expression env p
    | Rexpr_control (s, (Some (patt, e)), p) ->
	begin match s.conf_desc with
	| Rconf_present s ->
	    let ty_s = type_of_expression env s in
	    let ty_emit, ty_get =
	      try
		filter_event ty_s
	      with Unify ->
		non_event_err s
	    in
	    let ty_body = type_of_expression env p in
	    let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    type_expect new_env e type_bool;
	    ty_body
	| _ ->
	    non_event_err2 s
	end

    | Rexpr_get (s,patt,p) ->
	let ty_s = type_of_expression env s in
	let _, ty_get =
	  try
	    filter_event ty_s
	  with Unify ->
	    non_event_err s
	in
	let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
	type_of_expression new_env p

    | Rexpr_present (s,p1,p2) ->
	type_of_event_config env s;
	let ty = type_of_expression env p1 in
	type_expect env p2 ty;
	ty

    | Rexpr_await (_,s) ->
	type_of_event_config env s;
	type_unit

    | Rexpr_await_val (_,All,s,patt,p) ->
	let ty_s = type_of_expression env s in
	let _, ty_get =
	  try
	    filter_event ty_s
	  with Unify ->
	    non_event_err s
	in
	let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
	type_of_expression new_env p
    | Rexpr_await_val (_,One,s,patt,p) ->
	let ty_s = type_of_expression env s in
	let ty_emit, ty_get =
	  try
	    filter_event ty_s
	  with Unify ->
	    non_event_err s
	in
	unify_expr s
	  (constr_notabbrev event_ident
	     [ty_emit; (constr_notabbrev list_ident [ty_emit])])
	  ty_s;
	let gl_env, loc_env = type_of_pattern [] [] patt ty_emit in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
	type_of_expression new_env p

  in
  expr.expr_type <- t;
  Stypes.record (Ti_expr expr);
  t


(* Typing of event configurations *)
and type_of_event_config env conf =
  match conf.conf_desc with
  | Rconf_present s ->
      let ty = type_of_expression env s in
      let _ =
	try
	  filter_event ty
	with Unify ->
	  non_event_err s
      in
      ()

  | Rconf_and (c1,c2) ->
      type_of_event_config env c1;
      type_of_event_config env c2

  | Rconf_or (c1,c2) ->
      type_of_event_config env c1;
      type_of_event_config env c2


(* Typing of let declatations *)
and type_let is_rec env patt_expr_list =
  push_type_level();
  let ty_list = List.map (fun _ -> new_var()) patt_expr_list in
  let global_env, local_env =
    type_of_pattern_list [] [] (List.map fst patt_expr_list) ty_list
  in
  let add_env =
    List.fold_left
      (fun env (x, ty) -> Env.add x (forall [] ty) env)
      Env.empty local_env
  in
  let let_env =
    if is_rec
    then Env.append add_env env
    else env
  in
  List.iter2
    (fun (patt,expr) ty -> type_expect let_env expr ty)
    patt_expr_list
    ty_list;
  pop_type_level();
  List.iter2
    (fun (_,expr) ty -> if not (is_nonexpansive expr) then non_gen ty)
    patt_expr_list
    ty_list;
  let _ =
    List.iter
      (fun gl ->
	gl.info <- Some { value_typ = gen (Global.info gl).value_typ.ts_desc })
      global_env
  in
  let gen_env = Env.map (fun ty -> gen ty.ts_desc) add_env in
  global_env, Env.append gen_env env


(* Typing of an expression with an expected type *)
and type_expect env expr expected_ty =
  let actual_ty = type_of_expression env expr in
  unify_expr expr expected_ty actual_ty

(* Typing of statements (expressions whose values are ignored) *)
and type_statement env expr =
  let ty = type_of_expression env expr in
  match (type_repr ty).type_desc with
  | Type_arrow(_,_) -> partial_apply_warning expr.expr_loc
  | Type_var -> ()
  | Type_constr (c, _) ->
      begin match type_unit.type_desc with
      | Type_constr (c_unit, _) ->
	  if not (same_type_constr c c_unit) then
	    not_unit_type_warning expr
      | _ -> assert false
      end
  | _ ->
      not_unit_type_warning expr


(* Checks multiple occurrences *)
let check_no_repeated_constructor loc l =
  let rec checkrec cont l =
    match l with
      [] -> ()
    | ({ gi = name }, _) :: l ->
	if List.mem name.id.Ident.id cont
	then repeated_constructor_definition_err name.id.Ident.name loc
	else checkrec (name.id.Ident.id :: cont) l
  in
  checkrec [] l

let check_no_repeated_label loc l =
  let rec checkrec cont l =
    match l with
      [] -> ()
    | ({ gi = name },_ , _) :: l ->
	if List.mem name.id.Ident.id cont
	then repeated_label_definition_err name.id.Ident.name loc
	else checkrec (name.id.Ident.id :: cont) l
  in
  checkrec [] l

(* Typing of type declatations *)
let type_of_type_declaration loc (type_gl, typ_params, type_decl) =
  let typ_vars = List.map (fun v -> (v,new_generic_var ())) typ_params in
  let final_typ =
    constr_notabbrev type_gl.gi (List.map snd typ_vars)
  in
  let type_desc, abbr =
    match type_decl with
    | Rtype_abstract -> Type_abstract, Constr_notabbrev

    | Rtype_variant constr_decl_list ->
	check_no_repeated_constructor loc constr_decl_list;
	let cstr_list =
	  List.rev_map
	    (fun (gl_cstr,te_opt) ->
	      let ty_arg_opt =
		opt_map (type_of_type_expression typ_vars) te_opt
	      in
	      gl_cstr.info <- Some { cstr_arg = ty_arg_opt;
				     cstr_res = final_typ; };
	      gl_cstr)
	    constr_decl_list
	in
	Type_variant cstr_list, Constr_notabbrev

    | Rtype_record label_decl_list ->
	check_no_repeated_label loc label_decl_list;
	let lbl_list =
	  List.rev_map
	    (fun (gl_lbl, mut, te) ->
	      let ty_res = type_of_type_expression typ_vars te in
	      gl_lbl.info <- Some { lbl_res = ty_res;
				    lbl_arg = final_typ;
				    lbl_mut = mut; };
	      gl_lbl)
	    label_decl_list
	in
	Type_record lbl_list, Constr_notabbrev

    | Rtype_rebind (te) ->
	let ty_te = type_of_type_expression typ_vars te in
	Type_rebind (ty_te),
	Constr_abbrev (List.map snd typ_vars, ty_te)

  in
  type_gl.info <-
    Some { type_constr = {gi = type_gl.gi;
			  info = Some {constr_abbr = abbr}};
	   type_kind = type_desc;
	   type_arity = List.length typ_vars };
  type_gl


(* Check that an implementation without interface does not export values
   with non-generalizable types. *)
let check_nongen_values impl_item_list =
  List.iter
    (fun impl_item ->
      match impl_item.impl_desc with
      | Rimpl_let (_, patt_expr_list) ->
	  List.iter (fun (patt,expr) ->
	    if free_type_vars notgeneric expr.expr_type != []
	    then
              cannot_generalize_err expr)
	    patt_expr_list
      | _ -> ())
    impl_item_list

(* Typing of implementation items *)
let type_impl_item info_chan item =
  match item.impl_desc with
  | Rimpl_expr (e) ->
      ignore (type_of_expression Env.empty e)

  | Rimpl_let (flag, patt_expr_list) ->
      let global_env, local_env =
	type_let (flag = Recursive) Env.empty patt_expr_list
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_value_type_declaration info_chan global_env

  | Rimpl_signal (l) ->
      List.iter
	(fun ((s,te_opt), combine_opt) ->
	  let ty_emit = new_var() in
	  let ty_get = new_var() in
	  let ty_s = constr_notabbrev event_ident [ty_emit; ty_get] in
	  opt_iter
	    (fun te ->
	      unify_event s.gi.id
		(instance (full_type_of_type_expression te)) ty_s)
	    te_opt;
	  begin
	    match combine_opt with
	    | None ->
		unify_event s.gi.id
		  (constr_notabbrev list_ident [ty_emit])
		  ty_get
	    | Some (default,comb) ->
		type_expect Env.empty default ty_get;
		type_expect Env.empty comb
		  (arrow ty_emit (arrow ty_get ty_get))
	  end;
	  s.info <- Some { value_typ = forall [] ty_s };
	  (* verbose mode *)
	  if !print_type
	  then Types_printer.output_value_type_declaration info_chan [s])
	l
  | Rimpl_type (l) ->
      let global_env =
	List.map (type_of_type_declaration item.impl_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_type_declaration info_chan global_env

  | Rimpl_exn (gl_cstr, te_opt) ->
      gl_cstr.info <-
	Some {cstr_arg = opt_map (type_of_type_expression []) te_opt;
	      cstr_res = type_exn; };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr

  | Rimpl_exn_rebind (gl_cstr1, gl_cstr2) ->
      gl_cstr1.info <- Some (Global.info gl_cstr2);
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr1

  | Rimpl_open _ -> ()

(* Typing of interface items *)
let type_intf_item info_chan item =
  match item.intf_desc with
  | Rintf_val (gl, te) ->
      gl.info <-
	Some { value_typ = gen (full_type_of_type_expression te).ts_desc };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_value_type_declaration info_chan [gl]

  | Rintf_type l ->
      let global_env =
	List.map (type_of_type_declaration item.intf_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_type_declaration info_chan global_env

  | Rintf_exn (gl_cstr, te_opt) ->
      gl_cstr.info <-
	Some {cstr_arg = opt_map (type_of_type_expression []) te_opt;
	      cstr_res = type_exn; };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr

  | Rintf_open _ -> ()
