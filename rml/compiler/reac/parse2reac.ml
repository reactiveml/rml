(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : parse2reac.ml                                              *)
(*  Date de creation : 26/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* The translation of Parse to Reac *)

open Misc
open Asttypes
open Global
open Parse_ast
open Binding_errors
open Reac_ast
open Parse_ident
open Def_types
open Types
open Def_static

module Env = 
  Symbol_table.Make 
    (struct
      type t = string
      let compare = compare
      let name x = x
    end)

let gen_var = new Ident.name_generator
let gen_constr = new Ident.name_generator
let gen_type = new Ident.name_generator
let gen_label = new Ident.name_generator

let make_expr e loc =
  { expr_desc = e;
    expr_loc = loc; 
    expr_type = no_type_expression; }

let make_proc e loc =
  { proc_desc = e;
    proc_loc = loc; }

let make_patt p loc =
  { patt_desc = p;
    patt_loc = loc; 
    patt_type = no_type_expression; }

let make_te t loc =
  { te_desc = t;
    te_loc = loc; }

let make_impl it loc =
  { impl_desc = it;
    impl_loc = loc; }

let make_intf it loc =
  { intf_desc = it;
    intf_loc = loc; }

(* Compute the list of variables introduce in a pattern *)
let rec vars_of_rpatt p =
  match p.patt_desc with
  | Rpatt_any -> []

  | Rpatt_var x -> [x]

  | Rpatt_alias (patt,x) -> x :: (vars_of_rpatt patt)

  | Rpatt_constant _ -> []

  | Rpatt_tuple patt_list ->
      List.fold_left (fun vars patt -> (vars_of_rpatt patt)@vars) [] patt_list 

  | Rpatt_construct (_, None) -> []

  | Rpatt_construct (_, Some patt) -> vars_of_rpatt patt

  | Rpatt_or (patt1, patt2) -> (vars_of_rpatt patt1) @ (vars_of_rpatt patt2)

  | Rpatt_record label_patt_list ->
      List.fold_left 
	(fun vars (_,patt) -> (vars_of_rpatt patt)@vars)
	[] label_patt_list 

  | Rpatt_array patt_list ->
      List.fold_left (fun vars patt -> (vars_of_rpatt patt)@vars) [] patt_list 

  | Rpatt_constraint (patt, _) -> vars_of_rpatt patt

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

    | Ptype_process -> Rtype_process

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
	    let id = Ident.create gen_constr c.psimple_id Ident.Constr in
	    let g = Modules.defined_global id no_info in
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
	    let id = Ident.create gen_label lab.psimple_id Ident.Label in
	    let g = Modules.defined_global id no_info in
	    let _ = Modules.add_label g in
	    (g, flag, translate_te typ))
	  l
      in
      Rtype_record l

(* Translation of a pattern :
   The function returns the list of new variables and 
   the translation of the pattern.
   If a varible is bind twice an error is raised
*)
let rec translate_pattern is_global ctx p =
  let vars, rpatt =
    match p.ppatt_desc with
    | Ppatt_any -> [], Rpatt_any

    | Ppatt_var x -> 
	let id = 
	  Ident.create gen_var x.psimple_id 
	    (if ctx = Process then Ident.Val_RML else Ident.Val_ML) 
	in
	if is_global 
	then 
	  let gl = Modules.defined_global id no_info in
	  let vp = Varpatt_global gl in
	  [(x.psimple_id, vp)], Rpatt_var vp
	else
	  let vp = Varpatt_local id in
	  [(x.psimple_id, vp)], Rpatt_var vp

    | Ppatt_alias (patt,x) ->
	let vars, rpatt = translate_pattern is_global ctx patt in
	if List.mem_assoc x.psimple_id vars
	then multiply_bound_variable_err x.psimple_id p.ppatt_loc
	else
	  let id = 
	    Ident.create gen_var x.psimple_id 
	      (if ctx = Process then Ident.Val_RML else Ident.Val_ML) 
	  in
	  if is_global 
	  then 
	    let gl = Modules.defined_global id no_info in
	    let vp = Varpatt_global gl in
	    (x.psimple_id, vp) :: vars, Rpatt_alias (rpatt, vp)
	  else
	    let vp = Varpatt_local id in
	    (x.psimple_id, vp) :: vars, Rpatt_alias (rpatt, vp)
    | Ppatt_constant im -> [], Rpatt_constant im

    | Ppatt_tuple patt_list ->
	let vars, rpatt_list = 
	  translate_pattern_list is_global ctx patt_list 
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
	let vars, rpatt = translate_pattern is_global ctx patt in
	vars, Rpatt_construct (gconstr, Some rpatt) 

    | Ppatt_or (patt1, patt2) ->
	let vars1, rpatt1 = translate_pattern is_global ctx patt1 in
	let vars2, rpatt2 = translate_pattern is_global ctx patt2 in
	let vars =
	  List.fold_left 
	    (fun vars ((x,_) as e)  -> 
	      if List.mem_assoc x vars2 
	      then multiply_bound_variable_err x p.ppatt_loc
	      else e::vars) 
	    vars2 vars1
	in
	vars, Rpatt_or (rpatt1, rpatt2)

    | Ppatt_record l ->
	let vars, l = translate_pattern_record is_global ctx l in
	vars, Rpatt_record(l)

    | Ppatt_array patt_list ->
	let vars, rpatt_list = 
	  translate_pattern_list is_global ctx patt_list 
	in
	vars, Rpatt_array rpatt_list

    | Ppatt_constraint (patt,typ) ->
	let vars, rpatt = translate_pattern is_global ctx patt in
	let rtyp = translate_te typ in
	vars, Rpatt_constraint (rpatt, rtyp)

  in
  vars, make_patt rpatt p.ppatt_loc

(* Translation of a list of patterns *)
and translate_pattern_list is_global ctx =
  let rec translate_pattern_list vars patt_list rpatt_list =
    match patt_list with
    | [] -> vars, rpatt_list

    | patt :: patt_list ->
	let new_vars, rpatt = translate_pattern is_global ctx patt in
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
and translate_pattern_record is_global ctx =
  let rec translate_pattern_record vars lab_patt_list rlab_rpatt_list =
    match lab_patt_list with
    | [] -> vars, rlab_rpatt_list
	  
    | (lab,patt) :: lab_patt_list ->
	let glab = try Modules.pfind_label_desc lab.pident_id with
	| Modules.Desc_not_found -> 
	    unbound_label_err lab.pident_id lab.pident_loc
	in
	let new_vars, rpatt = translate_pattern is_global ctx patt in
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
    let vars, rlab_rpatt_list = translate_pattern_record [] lab_patt_list [] in
    (vars, List.rev rlab_rpatt_list)

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

(* Translation of expressions in an ML context *)
let rec translate_ml env e =
  let rexpr =
    match e.pexpr_desc with
    | Pexpr_ident x -> 
	begin
	  try translate_ident env x with
	  | Modules.Desc_not_found -> 
	      unbound_variable_err x.pident_id e.pexpr_loc
	end

    | Pexpr_constant im -> Rexpr_constant im

    | Pexpr_let (rec_flag, patt_expr_list, expr) ->
	let env, rpatt_rexpr_list = 
	  translate_ml_let false env rec_flag patt_expr_list
	in
	Rexpr_let (rec_flag, rpatt_rexpr_list, translate_ml env expr)

    | Pexpr_function patt_expr_list ->
	Rexpr_function (translate_ml_match env patt_expr_list)

    | Pexpr_apply (expr, expr_list) ->
	let rexpr = translate_ml env expr in
	let rexpr_list = List.map (translate_ml env) expr_list in
	Rexpr_apply (rexpr, rexpr_list)

    | Pexpr_tuple expr_list -> 
	let rexpr_list = List.map (translate_ml env) expr_list in
	Rexpr_tuple rexpr_list

    | Pexpr_construct (constr, expr_opt) ->
	let gconstr = try Modules.pfind_constr_desc constr.pident_id with
	| Modules.Desc_not_found -> 
	    unbound_constr_err constr.pident_id constr.pident_loc
	in 
	Rexpr_construct (gconstr, opt_map (translate_ml env) expr_opt)

    | Pexpr_array expr_list -> 
	let rexpr_list = List.map (translate_ml env) expr_list in
	Rexpr_array rexpr_list

    | Pexpr_record lab_expr_list ->
	Rexpr_record (translate_ml_record env lab_expr_list)

    | Pexpr_record_access (expr, lab) ->
	let glab = try Modules.pfind_label_desc lab.pident_id with
	| Modules.Desc_not_found -> 
	    unbound_label_err lab.pident_id lab.pident_loc
	in
	Rexpr_record_access (translate_ml env expr, glab)

    | Pexpr_record_update (e1, lab, e2) ->
	let glab = try Modules.pfind_label_desc lab.pident_id with
	| Modules.Desc_not_found -> 
	    unbound_label_err lab.pident_id lab.pident_loc
	in
	Rexpr_record_update (translate_ml env e1, 
			     glab,
			     translate_ml env e2)

    | Pexpr_constraint (expr,typ) ->
	Rexpr_constraint (translate_ml env expr, translate_te typ)

    | Pexpr_trywith (expr, patt_expr_list) ->
	Rexpr_trywith (translate_ml env expr,
		       translate_ml_match env patt_expr_list)

    | Pexpr_assert expr -> Rexpr_assert (translate_ml env expr)

    | Pexpr_ifthenelse (e1, e2, None) ->
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	Rexpr_ifthenelse (tr_e1,
			  tr_e2,
			  make_expr (Rexpr_constant(Const_unit)) 
			    Location.none)

    | Pexpr_ifthenelse (e1, e2, Some e3) ->
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	let tr_e3 = translate_ml env e3 in
	Rexpr_ifthenelse (tr_e1,
			  tr_e2,
			  tr_e3)

    | Pexpr_match (expr, patt_expr_list) ->
	Rexpr_match(translate_ml env expr,
		    translate_ml_match env patt_expr_list)

    | Pexpr_when_match (e1, e2) ->
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	Rexpr_when(tr_e1, tr_e2)

    | Pexpr_while (e1, e2) ->
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	Rexpr_while(tr_e1, tr_e2)

    | Pexpr_for (i,e1,e2,flag,e3) ->
	let id = Ident.create gen_var i.psimple_id Ident.Val_ML in
	let env = Env.add i.psimple_id id env in
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	let tr_e3 = translate_ml env e3 in
	Rexpr_for (id,
		   tr_e1,
		   tr_e2,
		   flag,
		   tr_e3)

    | Pexpr_seq (e1, e2) ->
	let tr_e1 = translate_ml env e1 in
	let tr_e2 = translate_ml env e2 in
	Rexpr_seq(tr_e1,
		  tr_e2)

    | Pexpr_process (expr) ->
	Rexpr_process (translate_proc env expr)

    | Pexpr_pre (flag, expr) ->
	Rexpr_pre (flag, translate_ml env expr)

    | Pexpr_emit s -> 
	Rexpr_emit (translate_ml env s)

    | Pexpr_emit_val (s,expr) ->
	Rexpr_emit_val (translate_ml env s,
			translate_ml env expr)

    | _ -> 
	raise (Internal (e.pexpr_loc,"Parse2reac.translate_ml: non ML expr"))

  in
  make_expr rexpr e.pexpr_loc

(* Translation of expressions in a PROCESS context *)
and translate_proc env e =
  let rproc =
    match e.pexpr_static with
    | Def_static.Static ->
	Rproc_compute(translate_ml env e)

    | Def_static.Dynamic ->
	begin
	  match e.pexpr_desc with
	  | Pexpr_let (_, [patt,({pexpr_desc= Pexpr_get s;} as v)], expr) ->
	      let vars, rpatt = translate_pattern false Process patt in
	      let new_env = add_varpatt env vars in
	      Rproc_get(translate_ml env s,
			rpatt, translate_proc new_env expr)

	  | Pexpr_let (rec_flag, patt_expr_list, expr) ->
	      let env, rpatt_rexpr = 
		translate_proc_let env rec_flag patt_expr_list 
	      in
	      Rproc_def(rpatt_rexpr, translate_proc env expr)

	  | Pexpr_ifthenelse (e1, e2, None) ->
	      Rproc_ifthenelse (translate_ml env e1,
				translate_proc env e2,
				make_proc Rproc_nothing Location.none) 

	  | Pexpr_ifthenelse (e1, e2, Some e3) ->
	      Rproc_ifthenelse (translate_ml env e1,
				translate_proc env e2,
				translate_proc env e3)

	  | Pexpr_match (e1, patt_expr_list) ->
	      Rproc_match(translate_ml env e1,
			  translate_proc_match env patt_expr_list)

	  | Pexpr_while (e1, e2) ->
	      Rproc_while (translate_ml env e1,
			   translate_proc env e2)

	  | Pexpr_for (i, e1, e2, flag, e3) ->
	      let id = Ident.create gen_var i.psimple_id Ident.Val_RML in
	      let env = Env.add i.psimple_id id env in
	      Rproc_for(id,
			translate_ml env e1,
			translate_ml env e2,
			flag,
			translate_proc env e3)

	  | Pexpr_fordopar (i, e1, e2, flag, e3) ->
	      let id = Ident.create gen_var i.psimple_id Ident.Val_RML in
	      let env = Env.add i.psimple_id id env in
	      Rproc_fordopar(id,
			     translate_ml env e1,
			     translate_ml env e2,
			     flag,
			     translate_proc env e3)

	  | Pexpr_seq (e1, e2) ->
	      Rproc_seq(translate_proc env e1,
			translate_proc env e2)

	  | Pexpr_nothing -> Rproc_nothing

	  | Pexpr_pause -> Rproc_pause

	  | Pexpr_emit s -> 
	      Rproc_emit (translate_ml env s)

	  | Pexpr_emit_val (s,expr) ->
	      Rproc_emit_val (translate_ml env s,
			      translate_ml env expr)

	  | Pexpr_loop expr ->
	      Rproc_loop (translate_proc env expr)

	  | Pexpr_par (e1, e2) ->
	      Rproc_par(translate_proc env e1,
			translate_proc env e2)

	  | Pexpr_merge (e1, e2) ->
	      Rproc_merge(translate_proc env e1,
			  translate_proc env e2)

	  | Pexpr_signal (sig_typ_list, None, expr) ->
	      (translate_proc_signal env sig_typ_list None expr).proc_desc
	  | Pexpr_signal (sig_typ_list, Some(e1,e2), expr) ->
	      let comb = Some(translate_ml env e1, translate_ml env e2) in
	      (translate_proc_signal env sig_typ_list comb expr).proc_desc

	  | Pexpr_run (expr) ->
	      Rproc_run (translate_ml env expr)

	  | Pexpr_until (s, expr, patt_expr_opt) ->
	      Rproc_until (translate_ml env s,
			   translate_proc env expr,
			   opt_map 
			     (translate_proc_patt_expr env) 
			     patt_expr_opt)

	  | Pexpr_when (s, expr) ->
	      Rproc_when (translate_ml env s,
			  translate_proc env expr)

	  | Pexpr_control (s, expr) ->
	      Rproc_control (translate_ml env s,
			     translate_proc env expr)

	  | Pexpr_present (s, e1, e2) ->
	      Rproc_present(translate_ml env s,
			    translate_proc env e1,
			    translate_proc env e2)

	  | Pexpr_await (flag, s) ->
	      Rproc_await (flag, 
			   translate_ml env s)

	  | Pexpr_await_val (flag, k, s, patt, expr) ->
	      let vars, rpatt = translate_pattern false Process patt in
	      let new_env = add_varpatt env vars in
	      Rproc_await_val (flag,
			       k,
			       translate_ml env s,
			       rpatt, 
			       translate_proc new_env expr)

	  | _ -> raise (Internal (e.pexpr_loc,
				  "Parse2reac.translate_proc: non proc expr"))
	end

  in
  make_proc rproc e.pexpr_loc


(* Translation of let definitions in an ML context *)
and translate_ml_let is_global env rec_flag patt_expr_list =
  let patt_list, expr_list = List.split patt_expr_list in
  let new_vars, rpatt_list = translate_pattern_list is_global ML patt_list in
  let tr_env = 
    if rec_flag = Recursive 
    then add_varpatt env new_vars 
    else env 
  in
  let rpatt_rexpr_list =
    List.map2 
      (fun rpatt expr -> rpatt,	translate_ml tr_env expr)
      rpatt_list
      expr_list
  in
  let new_env =
    if rec_flag <> Recursive 
    then add_varpatt env new_vars 
    else tr_env 
  in
  new_env, rpatt_rexpr_list

(* Translation of let definitions in a PROCESS context *)
(* x, C y = e1 and z = e2                              *)
(* is translated in                                    *)
(* x,y,z = let x, C y = e1 and z = e2 in x,y,z         *)
and translate_proc_let env rec_flag patt_expr_list =
  let local_env, rpatt_rexpr_list = 
    translate_ml_let false env rec_flag patt_expr_list 
  in
  let vars = 
    List.fold_left 
      (fun vars (rp,_) -> (vars_of_rpatt rp) @ vars) 
      [] rpatt_rexpr_list
  in
  let local_vars =
    List.fold_right
      (fun x acc -> 
	match x with
	| Varpatt_local id -> id::acc
	| Varpatt_global _ -> acc)
      vars []
  in
  let rexpr_ident_of_var x = 
    match x with 
    | Varpatt_local id -> make_expr (Rexpr_local id) Location.none 
    | Varpatt_global gl -> (* should not occur *)
	make_expr (Rexpr_global gl) Location.none 
  in
  let rexpr_of_vars =
    make_expr 
      (Rexpr_tuple (List.map rexpr_ident_of_var vars)) 
      Location.none
  in
  let body = 
    make_expr 
      (Rexpr_let(rec_flag, rpatt_rexpr_list, rexpr_of_vars)) Location.none 
  in
  let new_env, new_vars = 
    List.fold_left 
      (fun (env,vars) x -> 
	let id = Ident.create gen_var (Ident.name x) Ident.Val_RML in
	let env = Env.add (Ident.name id) id env in
	(env, id::vars))
      (env,[])
      local_vars
  in
  let rpatt_var_of_var x = 
    make_patt (Rpatt_var (Varpatt_local x)) Location.none 
  in
  let rpatt_of_new_vars =
    make_patt 
      (Rpatt_tuple (List.rev_map rpatt_var_of_var new_vars)) Location.none
  in
  new_env, (rpatt_of_new_vars, body)

(* Translation of match in an ML context *)
and translate_ml_match =
  let tr_patt_expr env (patt,expr)=
    let vars, rpatt = translate_pattern false ML patt in
    let env = add_varpatt env vars in
    let rexpr = translate_ml env expr in
    (rpatt, rexpr)
  in 
  fun env patt_expr_list ->
    List.map (tr_patt_expr env) patt_expr_list

(* Translation of match in a PROCESS context *)
and translate_proc_patt_expr env (patt,expr)=
  let vars, rpatt = translate_pattern false Process patt in
  let env = add_varpatt env vars in
  let rproc = translate_proc env expr in
  (rpatt, rproc)

and translate_proc_match =
  fun env patt_expr_list ->
    List.map (translate_proc_patt_expr env) patt_expr_list

(* Translation of record *)
and translate_ml_record env lab_expr_list =
  List.map 
    (fun (lab,expr) ->
      let glab = try Modules.pfind_label_desc lab.pident_id with
      | Modules.Desc_not_found -> 
	  unbound_label_err lab.pident_id lab.pident_loc
      in
      (glab, translate_ml env expr))
    lab_expr_list

(* Translation of signal *)
and translate_proc_signal env sig_typ_list comb expr =
  match sig_typ_list with
  | [] -> translate_proc env expr
  | (s,typ) :: sig_typ_list ->
      let (id, rtyp) = 
	Ident.create gen_var s.psimple_id Ident.Sig, 
	opt_map translate_te typ 
      in
      let env = Env.add s.psimple_id id env in
      make_proc
	(Rproc_signal 
	   ((id, rtyp), 
	    comb, 
	    translate_proc_signal env sig_typ_list comb expr))
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
	let id = Ident.create gen_type name.psimple_id Ident.Type in
	let gl = Modules.defined_global id no_info in
	let info = { type_constr = { gi = gl.gi; 
				     info = 
				     {constr_abbr = Constr_notabbrev}};
		     type_kind = Type_abstract;
		     type_arity = List.length param; }
	in
	let _ = 
	  gl.info <- info;
	  Modules.add_type gl 
	in
	(gl, param, typ))
      l
  in 
  List.map 
    (fun (gl, param, typ) -> (gl, param, translate_type_decl typ))
    l_rename

(* Translation of implementation item *)
let translate_impl_item info_chan item =
  let ritem =
    match item.pimpl_desc with 
    | Pimpl_expr expr -> Rimpl_expr (translate_ml Env.empty expr)

    | Pimpl_let (flag, patt_expr_list) ->
	let env, rpatt_rexpr_list = 
	  translate_ml_let true Env.empty flag patt_expr_list
	in
	Rimpl_let (flag, rpatt_rexpr_list)
	  
(* !!!!!!!!!!!!!!!!!!!!! A FAIRE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

    | Pimpl_signal (sig_typ_list, comb_opt) ->
	Rimpl_signal
	  (List.map
	     (fun (s,ty_opt) ->
	       let id = Ident.create gen_var s.psimple_id Ident.Sig in
	       let gl = Modules.defined_global id no_info in
	       let _ = Modules.add_value gl in
	       let rty_opt = opt_map translate_te ty_opt in
	       let rcomb_opt =
		 opt_map 
		   (fun (e1,e2) -> 
		     (translate_ml Env.empty e1, translate_ml Env.empty e2))
		   comb_opt
	       in
	       (gl,rty_opt), rcomb_opt)
	     sig_typ_list)

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

    | Pimpl_type l ->
	let l_translate = translate_type_declaration l in
	Rimpl_type l_translate

    | Pimpl_exn (name, typ) ->
	let id = Ident.create gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id no_info in
	let _ = Modules.add_constr gl in
	Rimpl_exn (gl, opt_map translate_te typ)

    | Pimpl_exn_rebind (name, gl_name) ->
	let id = Ident.create gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id no_info in
	let _ = Modules.add_constr gl in
	let gtype = try Modules.pfind_constr_desc gl_name.pident_id with
	| Modules.Desc_not_found -> 
	    unbound_type_err gl_name.pident_id gl_name.pident_loc
	in
	Rimpl_exn_rebind(gl, gtype)

    | Pimpl_open s -> 
	Modules.open_module s;
	Rimpl_open s

  in
  make_impl ritem item.pimpl_loc

(* Translation of interfacr item *)
let translate_intf_item info_chan item =
  let ritem = 
    match item.pintf_desc with
    | Pintf_val (s, t) -> 
	let id = Ident.create gen_var s.psimple_id Ident.Val_ML in
	let gl = Modules.defined_global id no_info in
	let _ = Modules.add_value gl in
	Rintf_val (gl, translate_te t)

    | Pintf_type l ->
	let l_translate = translate_type_declaration l in
	Rintf_type l_translate

    | Pintf_exn (name, typ) ->
	let id = Ident.create gen_constr name.psimple_id Ident.Exn in
	let gl = Modules.defined_global id no_info in
	let _ = Modules.add_constr gl in
	Rintf_exn (gl, opt_map translate_te typ)

    | Pintf_open s -> 
	Modules.open_module s;
	Rintf_open s

  in
  make_intf ritem item.pintf_loc
