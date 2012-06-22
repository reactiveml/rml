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

let unify_expr ?(regions = false) expr expected_ty actual_ty =
  try
    unify ~regions expected_ty actual_ty
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
    unify ~regions:true expected_ty actual_ty
  with Unify -> emit_wrong_type_err loc actual_ty expected_ty

let unify_emit_usage loc expected_ty actual_ty =
  try
    unify ~regions:true expected_ty actual_ty
  with Unify -> emit_wrong_usage_err loc actual_ty expected_ty

let unify_get_usage loc expected_ty actual_ty =
  try
    unify ~regions:true expected_ty actual_ty
  with Unify -> get_wrong_usage_err loc actual_ty expected_ty

let unify_run loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> run_wrong_type_err loc actual_ty expected_ty
    | Usages.Forbidden_usage (loc1, loc2) -> usage_wrong_type_err loc1 loc2

let unify_var loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with Unify -> var_wrong_type_err loc actual_ty expected_ty
    | Usages.Forbidden_usage (loc1, loc2) -> usage_wrong_type_err loc1 loc2

(* special cases of unification *)
let filter_event ty =
  let ty = type_repr ty in
  let ty1 = new_var() in
  let ty2 = new_var() in
  let ty3 = new_var() in
  let ty4 = new_var() in
  unify ty (constr_notabbrev event_ident [ty1;ty2;ty3;ty4]);
  (* emit, get, emit usage, get usage *)
  ty1,     ty2, ty3,        ty4

let filter_event_or_err ty s =
  try
    filter_event ty
  with Unify ->
    non_event_err s
    | Usages.Forbidden_usage (loc1, loc2) -> usage_wrong_type_err loc1 loc2

let filter_usage ty s =
  let _, _, u_emit, u_get = filter_event_or_err ty s in
  Usages.mk_su
    s.expr_loc
    (Usages_misc.usage_of_type u_emit)
    (Usages_misc.usage_of_type u_get)

let unify_usage loc ty_emit ty_get u_new =
  let new_u_emit, new_u_get = Usages.km_s u_new in
  let new_ty_emit = Usages_misc.type_of_usage new_u_emit
  and new_ty_get = Usages_misc.type_of_usage new_u_get in
  unify_emit_usage loc ty_emit new_ty_emit;
  unify_get_usage loc ty_get new_ty_get

let accumulate_usage ty loc u_new =
  try
    let u_result = Usages.add_s ty.type_usage u_new in
    ty.type_usage <- u_result
  with Usages.Forbidden_usage (loc1, loc2) ->
    usage_wrong_type_err loc1 loc2

let is_unit_process desc =
  let sch = desc.value_typ in
  let ty = instance sch in
  let unit_process = process type_unit { proc_static = None } in
  try
    unify unit_process ty;
    true
  with Unify | Usages.Forbidden_usage _ -> false

(* Typing environment *)
module Env = Symbol_table.Make (Ident)

let print_env table =
  if not (Env.is_empty table) then begin
    Env.iter
      (fun id ty_sch -> Printf.printf "%s(id=%d){eff=%s}:"
        (Ident.name id)
        id.Ident.id
        (Usages_misc.string_of_signal_usage ty_sch.ts_desc.type_usage);
        Types_printer.print_scheme ty_sch
      )
      table;
    Printf.printf "\n%!"
  end

let pu ty =
  Usages_misc.string_of_signal_usage ty.type_usage

(* Usages environment *)
module Effects = Usages_misc.Table

let gleff = Hashtbl.create 1023

let apply_eff effs ty loc =
  try
    let _, _, u_emit, u_get = filter_event ty in
    let eff = Usages_misc.mk_t loc u_emit u_get in
    Effects.apply eff effs
  with _ ->
    effs

(* let dprint msg f x = *)
  (* Printf.printf "BEGIN %s:\n%!" msg; *)
  (* f x; *)
  (* Printf.printf "END;\n%!" *)

let register_effects patt_vars effects =
  List.iter (fun (patterns, effects) ->
    List.iter (function
      | Varpatt_local x ->
          let id = x.Ident.id in
          (* let name = x.Ident.name in *)
          Hashtbl.add gleff id effects
      | Varpatt_global x ->
          let id = x.gi.Global_ident.id.Ident.id in
          (* let name = x.gi.Global_ident.id.Ident.name in *)
          Hashtbl.add gleff id effects
      )
      patterns
    )
    (List.combine patt_vars effects)

let get_effects id loc =
  try
    Hashtbl.find gleff id
  with _ ->
    Effects.singleton id loc (new_var ()) (new_var ())

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
  | Rexpr_emit (_, e, None) -> is_nonexpansive e
  | Rexpr_emit (_, e1, Some e2) -> is_nonexpansive e1 && is_nonexpansive e2
  | Rexpr_present (e,e1,e2) ->
      is_nonexpansive_conf e && is_nonexpansive e1 && is_nonexpansive e2
  | Rexpr_await (_, _, e) ->
      is_nonexpansive_conf e
  | Rexpr_await_val (_, _, _, s, _, e) ->
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

let rec vars_of_patt = function
  | [] -> []
  | (p,e)::patt_expr_list ->
      (Reac_misc.vars_of_patt p) :: vars_of_patt patt_expr_list

let ids_of_patt patts =
  List.fold_left
    (fun acc -> function
    | Varpatt_local x -> x.Ident.id :: acc
    | Varpatt_global x -> x.gi.Global_ident.id.Ident.id :: acc
    )
    []
    patts

(* Typing of expressions *)
let rec type_of_expression env expr =
  let t =
    match expr.expr_desc with
    | Rexpr_constant (i) -> type_of_immediate i, Effects.empty

    | Rexpr_local (n) ->
	let typ_sch = Env.find n env in
        let ty = instance typ_sch in
	ty, Effects.singleton n.Ident.id expr.expr_loc (new_var ()) (new_var ())

    | Rexpr_global (n) ->
        let g_ty = (Global.info n).value_typ in
        let ty = instance g_ty in
        let idx = n.gi.Global_ident.id.Ident.id in
        let effects = get_effects idx expr.expr_loc in
	ty, effects

    | Rexpr_let (flag, patt_expr_list, e) ->
	let gl_env, new_env, effects_1 = type_let (flag = Recursive) env patt_expr_list in
        let patt_vars = vars_of_patt patt_expr_list in
        register_effects patt_vars effects_1;
        let ty, effects_2 = type_of_expression new_env e in
        ty, Effects.merge (Effects.flatten effects_1) effects_2

    | Rexpr_function (matching)  ->
	let ty_arg = new_var() in
	let ty_res = new_var() in
	let ty = arrow ty_arg ty_res in
        let local_vars = ids_of_patt (List.flatten (vars_of_patt matching)) in
        let effects =
	  List.map
	    (fun (p,e) ->
	      let gl_env, loc_env = type_of_pattern [] [] p ty_arg in
	      assert (gl_env = []);
	      let new_env =
	        List.fold_left
		  (fun env (x, ty) -> Env.add x (forall [] ty) env)
		  env loc_env
	      in
              snd (type_expect ~regions:true new_env e ty_res)
            )
	    matching
        in
        ty, Effects.filter (Effects.flatten effects) local_vars

    | Rexpr_apply (fct, args) ->
	let ty_fct, effects_f = type_of_expression env fct in
        let effects_f = Effects.update_loc effects_f fct.expr_loc in
	let rec type_args u ty_res = function
	  | [] -> ty_res, u
	  | arg :: args ->
	      let t1, t2 =
		try
		  filter_arrow ~regions:true ty_res
		with Unify ->
		  application_of_non_function_err fct ty_fct
                | Usages.Forbidden_usage (loc1, loc2) -> usage_wrong_type_err loc1 loc2
	      in
              let ty_arg, u2 = type_expect ~regions:true env arg t1 in
              let effects = Effects.merge (apply_eff u2 ty_arg arg.expr_loc) u in
	      type_args effects t2 args
	in
	type_args effects_f ty_fct args

    | Rexpr_tuple (l) ->
        let ty_s, effects = List.fold_right
          (fun l (ty_s,u_s) ->
            let ty, u = type_of_expression env l in
            ty::ty_s, Effects.merge u u_s
          )
          l
          ([], Effects.empty)
        in
	product ty_s, effects

    | Rexpr_construct(c,None) ->
	begin
	  let { cstr_arg = ty_arg_opt;
		cstr_res = ty } = get_type_of_constructor c expr.expr_loc
	  in
	  match ty_arg_opt with
	  | None -> ty, Effects.empty
	  | Some ty_arg -> constr_arity_err c.gi expr.expr_loc, Effects.empty
	end
    | Rexpr_construct (c, Some arg) ->
	begin
	  let { cstr_arg = ty_arg_opt;
		cstr_res = ty_res; } = get_type_of_constructor c expr.expr_loc
	  in
	  match ty_arg_opt with
	  | None -> constr_arity_err_2 c.gi expr.expr_loc, Effects.empty
	  | Some ty_arg ->
              let _, effects = type_expect env arg ty_arg in
              ty_res, effects
	end

    | Rexpr_array (l) ->
	let ty_var = new_var () in
	let effects = List.fold_left (fun a e ->
          let _, u = type_expect env e ty_var in
          Effects.merge u a)
          Effects.empty
          l
        in
	constr_notabbrev array_ident [ty_var], effects

    | Rexpr_record (l) ->
	let ty = new_var() in
	let rec typing_record effects label_list label_expr_list =
	  match label_expr_list with
	    [] -> effects
	  | (label,label_expr) :: label_expr_list ->
	      let { lbl_arg = ty_arg;
		    lbl_res = ty_res } = get_type_of_label label expr.expr_loc
	      in
	      (* check that the label appears only once *)
	      if List.mem label label_list
	      then non_linear_record_err label.gi expr.expr_loc;
              let _, u2 = type_expect env label_expr ty_res in
              let effects = Effects.merge u2 effects in
	      unify_expr expr ty ty_arg;
	      typing_record effects (label :: label_list) label_expr_list
	in
        let effects = typing_record Effects.empty [] l in
        ty, effects

    | Rexpr_record_access (e, label) ->
	let { lbl_arg = ty_arg; lbl_res = ty_res } =
	  get_type_of_label label expr.expr_loc
	in
        let _, effects = type_expect env e ty_arg in
        ty_res, effects

    | Rexpr_record_update (e1, label, e2) ->
	let { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
	  get_type_of_label label expr.expr_loc
	in
	if mut = Immutable then label_not_mutable_err expr label.gi;
	let _, u1 = type_expect env e1 ty_arg in
	let _, u2 = type_expect env e2 ty_res in
	type_unit, Effects.merge u1 u2

    | Rexpr_constraint(e,t) ->
	let expected_ty = instance (full_type_of_type_expression t) in
        let _, effects = type_expect env e expected_ty in
	expected_ty, effects

    | Rexpr_trywith (body,matching) ->
	let ty, effects_e = type_of_expression env body in
        let effects =
	List.fold_left
	  (fun a (p,e) ->
	    let gl_env, loc_env = type_of_pattern [] [] p type_exn in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    let _, u = type_expect new_env e ty in
            Effects.merge u a)
          Effects.empty
	  matching
        in
	ty, Effects.merge effects effects_e

    | Rexpr_assert e ->
        let _, effects = type_expect env e type_bool in
	new_var(), effects

    | Rexpr_ifthenelse (cond,e1,e2) ->
        let _, u1 = type_expect env cond type_bool in
	let ty, u2 = type_of_expression env e1 in
	let _, u3 = type_expect env e2 ty in
	ty, Effects.flatten [u1; u2; u3]

    | Rexpr_match (body,matching) ->
	let ty_body, u1 = type_of_expression env body in
	let ty_res = new_var() in
        let u2 =
	  List.map
	    (fun (p,e) ->
	      let gl_env, loc_env = type_of_pattern [] [] p ty_body in
	      assert (gl_env = []);
	      let new_env =
	        List.fold_left
		  (fun env (x, ty) -> Env.add x (forall [] ty) env)
		  env loc_env
	      in
	      snd (type_expect new_env e ty_res))
	    matching
        in
	ty_res, Effects.flatten u2

    | Rexpr_when_match (e1,e2) ->
        let _, u1 = type_expect env e1 type_bool in
        let ty, u2 = type_of_expression env e2 in
        ty, Effects.merge u1 u2

    | Rexpr_while (e1,e2) ->
        let _, u1 = type_expect env e1 type_bool in
	let u2 = type_statement env e2 in
	type_unit, Effects.merge u1 u2

    | Rexpr_for(i,e1,e2,flag,e3) ->
        let _, u1 = type_expect env e1 type_int in
        let _, u2 = type_expect env e2 type_int in
	let u3 = type_statement (Env.add i (forall [] type_int) env) e3 in
	type_unit, Effects.flatten [u1; u2; u3]

    | Rexpr_seq e_list ->
	let rec f u l =
	  match l with
	  | [] -> assert false
	  | [e] -> let ty, u2 = type_of_expression env e in
                   ty, Effects.merge u2 u
	  | e::l -> f (type_statement env e) l
	in f Effects.empty e_list

    | Rexpr_process(e) ->
	let ty, u = type_of_expression env e in
        (process ty { proc_static = Some(Proc_def (ref Def_static.Dontknow)); }), u

    | Rexpr_pre (Status, s) ->
	let ty_s, u_s = type_of_expression env s in
	let _, _ty, _, _ = filter_event_or_err ty_s s in
	type_bool, u_s

    | Rexpr_pre (Value, s) ->
	let ty_s, u_s = type_of_expression env s in
	let _, ty, _, _ = filter_event_or_err ty_s s in
	ty, u_s

    | Rexpr_last s ->
	let ty_s, u_s = type_of_expression env s in
	let _, ty, _, _ = filter_event_or_err ty_s s in
	ty, u_s

    | Rexpr_default s ->
	let ty_s, u_s = type_of_expression env s in
	let _, ty, _, _ = filter_event_or_err ty_s s in
	ty, u_s

    | Rexpr_emit (affine, s, None) ->
	let ty_s, u_s = type_of_expression env s in
	let ty, _, u_emit, u_get = filter_event_or_err ty_s s in
        let new_usage = Usages.send_u s.expr_loc affine in
        accumulate_usage ty_s expr.expr_loc new_usage;
        unify_usage expr.expr_loc u_emit u_get ty_s.type_usage;
	unify_emit expr.expr_loc type_unit ty;
        let effects = Effects.apply new_usage u_s in
	type_unit, effects

    | Rexpr_emit (affine, s, Some e) ->
	let ty_s, u_s = type_of_expression env s in
	let ty, _, u_emit, u_get = filter_event_or_err ty_s s in
	let ty_e, u_e = type_of_expression env e in
        let new_usage = Usages.send_u s.expr_loc affine in
        accumulate_usage ty_s expr.expr_loc new_usage;
        unify_usage expr.expr_loc u_emit u_get ty_s.type_usage;
	unify_emit e.expr_loc ty ty_e;
        let r_s = ty_s.type_index in
        let effects = Effects.flatten [u_s; u_e; Effects.singleton r_s s.expr_loc u_emit u_get] in
	type_unit, effects

    | Rexpr_signal ((s,te_opt), combine_opt, e) ->
	let ty_emit = new_var() in
	let ty_get = new_var() in
        let u_emit = new_var () in
        let u_get = new_var () in
	let ty_s = constr_notabbrev event_ident [ty_emit; ty_get; u_emit; u_get] in
	opt_iter
	  (fun te ->
	    unify_event s (instance (full_type_of_type_expression te)) ty_s)
	  te_opt;
        let gather_has_effects, gather_loc =
	  begin
	    match combine_opt with
	      | None ->
                unify_event s
                  (constr_notabbrev list_ident [ty_emit])
                  ty_get;
                false, Location.none
	      | Some (default,comb) ->
                let _, _ = type_expect env default ty_get in
                let _, effects = type_expect env comb (arrow ty_emit (arrow ty_get ty_get)) in
                not (Effects.is_empty effects), comb.expr_loc
	  end
        in
        gather_wrong_effects_err gather_has_effects gather_loc;
        let ty, effects_e = type_of_expression (Env.add s (forall [] ty_s) env) e in
        ty, effects_e

    | Rexpr_nothing -> type_unit, Effects.empty

    | Rexpr_pause _ -> type_unit, Effects.empty

    | Rexpr_halt _ -> new_var(), Effects.empty

    | Rexpr_loop (None, p) ->
        let effects = type_statement env p in
        type_unit, effects

    | Rexpr_loop (Some n, p) ->
        let _, u1 = type_expect env n type_int in
        let u2 = type_statement env p in
	type_unit, Effects.merge u1 u2

    | Rexpr_fordopar(i,e1,e2,flag,p) ->
        let _, u1 = type_expect env e1 type_int in
        let _, u2 = type_expect env e2 type_int in
	let u3 = type_statement (Env.add i (forall [] type_int) env) p in
	type_unit, Effects.flatten [u1; u2; u3]

    | Rexpr_par p_list ->
        let effects_l = List.map (fun p -> type_statement env p) p_list in
	type_unit, Effects.flatten effects_l

    | Rexpr_merge (p1,p2) ->
        let u1 = type_statement env p1 in
        let u2 = type_statement env p2 in
	type_unit, Effects.merge u1 u2

    | Rexpr_run (e) ->
	let ty_e, u_e = type_of_expression env e in
	let ty = new_var() in
	unify_run e.expr_loc
	  ty_e (process ty { proc_static = None; });
	ty, u_e

    | Rexpr_until (s,p,patt_proc_opt) ->
	begin match patt_proc_opt with
	| None ->
            let u1 = type_of_event_config env s in
            let _, u2 = type_expect env p type_unit in
	    type_unit, Effects.merge u1 u2
	| Some _ ->
	    begin match s.conf_desc with
	    | Rconf_present s ->
		let ty_s, u_s = type_of_expression env s in
		let ty_emit, ty_get, u_emit, u_get = filter_event_or_err ty_s s in
		let ty_body, u_b = type_of_expression env p in
                let effects = opt_map_default Effects.empty
		  (fun (patt,proc) ->
		    let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
		    assert (gl_env = []);
		    let new_env =
		      List.fold_left
			(fun env (x, ty) -> Env.add x (forall [] ty) env)
			env loc_env
		    in
		    snd (type_expect new_env proc ty_body))
		  patt_proc_opt in
		ty_body, effects
	    | _ ->
		non_event_err2 s
	    end
	end


    | Rexpr_when (s,p) ->
        let u1 = type_of_event_config env s in
	let ty, u2 = type_of_expression env p in
        ty, Effects.merge u1 u2

    | Rexpr_control (s, None, p) ->
	let u1 = type_of_event_config env s in
	let ty, u2 = type_of_expression env p in
        ty, Effects.merge u1 u2

    | Rexpr_control (s, (Some (patt, e)), p) ->
	begin match s.conf_desc with
	| Rconf_present s ->
	    let ty_s, u_s = type_of_expression env s in
	    let ty_emit, ty_get, u_emit, u_get = filter_event_or_err ty_s s in
	    let ty_body, u_b = type_of_expression env p in
	    let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	    assert (gl_env = []);
	    let new_env =
	      List.fold_left
		(fun env (x, ty) -> Env.add x (forall [] ty) env)
		env loc_env
	    in
	    let _, u_e = type_expect new_env e type_bool in
	    ty_body, Effects.flatten [u_s; u_b; u_e]
	| _ ->
	    non_event_err2 s
	end

    | Rexpr_get (s,patt,p) ->
	let ty_s, u_s = type_of_expression env s in
	let _, ty_get, u_emit, u_get = filter_event_or_err ty_s s in
	let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
        let ty, u_p = type_of_expression new_env p in
        ty, Effects.merge u_s u_p

    | Rexpr_present (s,p1,p2) ->
        let u1 = type_of_event_config env s in
	let ty, u2 = type_of_expression env p1 in
	let _, u3 = type_expect env p2 ty in
	ty, Effects.flatten [u1; u2; u3]

    | Rexpr_await (affine,_,s) ->
        let effects = type_of_event_config env s in
        type_unit, effects

    | Rexpr_await_val (affine,_,All,s,patt,p) ->
	let ty_s, u_s = type_of_expression env s in
	let _, ty_get, u_emit, u_get = filter_event_or_err ty_s s in
	let gl_env, loc_env = type_of_pattern [] [] patt ty_get in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
        let new_usage = Usages.await_u expr.expr_loc affine in
        accumulate_usage ty_s expr.expr_loc new_usage;
        unify_usage expr.expr_loc u_emit u_get ty_s.type_usage;
        let ty, u_p = type_of_expression new_env p in
        let r_s = ty_s.type_index in
        ty, Effects.flatten [u_s; u_p; Effects.singleton r_s s.expr_loc u_emit u_get]

    | Rexpr_await_val (_,_,One,s,patt,p) ->
	let ty_s, u_s = type_of_expression env s in
	let ty_emit, ty_get, u_emit, u_get = filter_event_or_err ty_s s in
        unify_expr s
          (constr_notabbrev list_ident [ty_emit])
          ty_get;
	let gl_env, loc_env = type_of_pattern [] [] patt ty_emit in
	assert (gl_env = []);
	let new_env =
	  List.fold_left
	    (fun env (x, ty) -> Env.add x (forall [] ty) env)
	    env loc_env
	in
        let affine = true (* Always the case here since it is an "await one" *) in
        let new_usage = Usages.await_u expr.expr_loc affine in
        accumulate_usage ty_s expr.expr_loc new_usage;
        unify_usage expr.expr_loc u_emit u_get ty_s.type_usage;
        let ty, u_p = type_of_expression new_env p in
        let r_s = ty_s.type_index in
        ty, Effects.flatten [u_s; u_p; Effects.singleton r_s s.expr_loc u_emit u_get]

  in
  expr.expr_type <- fst t;
  Stypes.record (Ti_expr expr);
  t


(* Typing of event configurations *)
and type_of_event_config env conf =
  match conf.conf_desc with
  | Rconf_present s ->
      let ty, u = type_of_expression env s in
      let _ = filter_event_or_err ty s in
      u

  | Rconf_and (c1,c2) ->
      Effects.flatten [
        type_of_event_config env c1;
        type_of_event_config env c2
      ]

  | Rconf_or (c1,c2) ->
      Effects.flatten [
        type_of_event_config env c1;
        type_of_event_config env c2
      ]

and events_from_event_config conf =
  match conf.conf_desc with
    | Rconf_present s -> [s]
    | Rconf_and (c1, c2) | Rconf_or (c1, c2) ->
      let c1 = events_from_event_config c1 in
      let c2 = events_from_event_config c2 in
      c1 @ c2

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
  let effects =
    List.map2
      (fun (patt,expr) ty -> snd (type_expect let_env expr ty))
      patt_expr_list
      ty_list
  in
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
  global_env, Env.append gen_env env, effects


(* Typing of an expression with an expected type *)
and type_expect ?(regions = false) env expr expected_ty =
  let actual_ty, effects = type_of_expression env expr in
  let () = unify_expr ~regions expr expected_ty actual_ty in
  actual_ty, effects

(* Typing of statements (expressions whose values are ignored) *)
and type_statement env expr =
  let ty, u = type_of_expression env expr in
  begin
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
  end;
  u


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
  try
  match item.impl_desc with
  | Rimpl_expr (e) ->
      let _, _ = type_of_expression Env.empty e in
      ()

  | Rimpl_let (flag, patt_expr_list) ->
      let global_env, local_env, effects =
	type_let (flag = Recursive) Env.empty patt_expr_list
      in
      let patt_vars = vars_of_patt patt_expr_list in
      register_effects patt_vars effects;
      (* verbose mode *)
      if !print_type
      then Types_printer.output_value_type_declaration info_chan global_env

  | Rimpl_signal (l) ->
      List.iter
	(fun ((s,te_opt), combine_opt) ->
	  let ty_emit = new_var() in
	  let ty_get = new_var() in
          let u_emit = new_var() in
          let u_get = new_var() in
	  let ty_s = constr_notabbrev event_ident [ty_emit; ty_get; u_emit; u_get] in
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
                let _, _ = type_expect Env.empty default ty_get in
                let _, _ = type_expect Env.empty comb (arrow ty_emit (arrow ty_get ty_get)) in
                ()
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
  with Usages.Forbidden_usage (loc1, loc2) ->
    Typing_errors.usage_wrong_type_err loc1 loc2

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
