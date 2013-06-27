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

open Compiler_options
open Clocks
open Clocks_utils
open Clocks_utils_norow
open Reactivity
open Clocking_errors
open Initialization
open Asttypes
open Global
open Global_ident
open Reac
open Misc
open Annot
open Global_mapfold
open Reac_mapfold

let add_effect eff =
  if not !Compiler_options.no_clock_effects then
    current_effect := eff_sum !current_effect eff
let add_effect_ck ck =
  if not !Compiler_options.no_clock_effects then
    add_effect (eff_depend (carrier_row_one ck))

let set_current_react r =
  if not !Compiler_options.no_reactivity then (
   (* Printf.eprintf "Set react: %a\n" Clocks_printer.output_react r;*)
    current_react := r
  )

let add_to_list x l =
  if List.mem x l then l else x::l

let arrow ck1 ck2 eff = arrow ck1 ck2 (eff_row_one eff)
let process ck act_ck eff r = process ck act_ck (eff_row_one eff) r
let depend c = depend (carrier_row_one c)
let eff_depend c = eff_depend (carrier_row_one c)
let react_carrier c = react_carrier (carrier_row_one c)

let rec filter_depend ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_depend c -> get_car c
    | _ ->
        let c = new_carrier_var generic_prefix_name in
        unify ck (depend c);
        c

let rec filter_arrow ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_arrow(ck1, ck2, eff) -> ck1, ck2, get_eff eff
    | Clock_constr({ck_info = Some { constr_abbr = Constr_abbrev (params, body) }}, args) ->
        filter_arrow (expand_abbrev params body args)
    | _ ->
        let ck1 = new_clock_var () in
        let ck2 = new_clock_var () in
        let eff = new_effect_var () in
        unify ck (arrow ck1 ck2 eff);
        ck1, ck2, eff

let filter_event ?(force_activation_ck=false) ck =
  let ck = clock_repr ck in
  let ck1 = new_clock_var() in
  let ck2 = new_clock_var() in
  let sck =
    if force_activation_ck then
      !activation_carrier
    else
      new_carrier_var generic_prefix_name
  in
  unify ck (constr_notabbrev event_ident [Kclock ck1; Kclock ck2; Kcarrier sck]);
  add_effect_ck sck;
  ck1, ck2, sck

let filter_multi_event ck =
  let ck = clock_repr ck in
  let ck1 = new_clock_var() in
  let sck = new_carrier_var generic_prefix_name in
  unify ck (constr_notabbrev event_ident
               [Kclock ck1; Kclock (constr_notabbrev list_ident [Kclock ck1]); Kcarrier sck]);
  add_effect_ck sck;
  ck1, sck

let unify_expr expr expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    | Unify -> expr_wrong_clock_err expr actual_ty expected_ty
    | Escape (s, _) -> expr_wrong_clock_escape_err expr s actual_ty

let unify_patt pat expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> patt_wrong_clock_err pat actual_ty expected_ty

let unify_event evt expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> event_wrong_clock_err evt actual_ty expected_ty

let unify_emit loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    | Unify -> emit_wrong_clock_err loc actual_ty expected_ty
    | Escape (s, _) -> emit_wrong_clock_escape_err loc s actual_ty

let unify_update loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    | Unify -> update_wrong_clock_err loc actual_ty expected_ty
    | Escape (s, _) -> update_wrong_clock_escape_err loc s actual_ty


let unify_run loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    | Unify -> run_wrong_clock_err loc actual_ty expected_ty
    | Escape (s, _) -> run_wrong_clock_escape_err loc s actual_ty

let unify_var loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> var_wrong_clock_err loc actual_ty expected_ty

(* special cases of unification *)

(* Typing environment *)
module Env = Symbol_table.Make (Ident)

(* checks that every type is defined *)
(* and used with the correct arity *)
let check_clock_constr_defined loc gl arity =
  let name = gl.gi in
  let ty_desc = Global.ck_info gl in
  let arity' = ty_desc.clock_arity in
  if arity' <> arity
  then clock_constr_arity_err name arity arity' loc;
  ty_desc.clock_constr

(* find the type of the constructor C *)
let get_clock_of_constructor c loc =
  constr_instance (Global.ck_info c)

(* find the type of a label *)
let get_clock_of_label label loc =
  label_instance (Global.ck_info label)

(* tests if an expression is expansive *)
let rec is_nonexpansive expr =
  match expr.e_desc with
  | Elocal _ -> true
  | Eglobal _ -> true
  | Econstant _ -> true
  | Ebase | Etopck -> true
  | Etuple l -> List.for_all is_nonexpansive l
  | Econstruct (_, None) -> true
  | Econstruct(_, Some e) -> is_nonexpansive e
  | Elet(rec_flag, bindings, body) ->
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) bindings &&
      is_nonexpansive body
  | Efunction _ -> true
  | Eifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive ifnot
  | Econstraint(e, ty) -> is_nonexpansive e
  | Earray [] -> true
  | Erecord lbl_expr_list ->
      List.for_all (fun (lbl, expr) ->
        (Global.ck_info lbl).lbl_mut == Immutable && is_nonexpansive expr)
        lbl_expr_list
  | Erecord_access(e, lbl) -> is_nonexpansive e
  | Ewhen_match(cond, act) -> is_nonexpansive act
  | Eprocess _ -> true
  | Epre (_, e) -> is_nonexpansive e
  | Elast e -> is_nonexpansive e
  | Edefault e -> is_nonexpansive e
  | Enothing -> true
  | Epause (_,_, _) -> true
  | Ehalt _ -> true
  | Eemit (e, None) -> is_nonexpansive e
  | Eemit (e1, Some e2) -> is_nonexpansive e1 && is_nonexpansive e2
  | Epresent (e,e1,e2) ->
      is_nonexpansive_conf e && is_nonexpansive e1 && is_nonexpansive e2
  | Eawait (_, e) ->
      is_nonexpansive_conf e
  | Eawait_val (_, _, s, _, e) ->
      is_nonexpansive s && is_nonexpansive e
  | Euntil (c, e, None, _) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Euntil (c, e, Some (_, e'), _) ->
      is_nonexpansive_conf c && is_nonexpansive e && is_nonexpansive e'
  | Ewhen (c, e) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Econtrol (c, None, e) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Econtrol (c, Some (_,  e'), e) ->
      is_nonexpansive_conf c && is_nonexpansive e' && is_nonexpansive e
  | Epar e_list ->
      List.for_all is_nonexpansive e_list
  | Emerge (e1, e2) ->
      is_nonexpansive e1 && is_nonexpansive e2
  | _ -> false

and is_nonexpansive_conf c =
  match c.conf_desc with
  | Cpresent e ->
      is_nonexpansive e
  | Cand (c1,c2) ->
      is_nonexpansive_conf c1 && is_nonexpansive_conf c2
  | Cor (c1,c2) ->
      is_nonexpansive_conf c1 && is_nonexpansive_conf c2

(* Typing functions *)

let new_generic_var_param pe = match pe with
  | Kclock { te_desc = Tvar v } -> v, Kclock (new_generic_clock_var ())
  | Kcarrier { ce_desc = Cvar v } -> v, Kcarrier (new_generic_carrier_var v)
  | Kcarrier_row { cer_desc = Crow_var v } -> v, Kcarrier_row (new_generic_carrier_row_var ())
  | Keffect { ee_desc = Effvar v } -> v, Keffect (new_generic_effect_var ())
  | Keffect_row { eer_desc = Effrow_var v } -> v, Keffect_row (new_generic_effect_row_var ())
  | Kreact _ -> assert false
  | _ -> assert false

let new_var_param pe = match pe with
  | Kclock { te_desc = Tvar v } -> v, Kclock (new_clock_var ())
  | Kcarrier { ce_desc = Cvar v } -> v, Kcarrier (new_carrier_var v)
  | Kcarrier_row { cer_desc = Crow_var v } -> v, Kcarrier_row (new_carrier_row_var ())
  | Keffect { ee_desc = Effvar v } -> v, Keffect (new_effect_var ())
  | Keffect_row { eer_desc = Effrow_var v } -> v, Keffect_row (new_effect_row_var ())
  | Kreact _ -> assert false
  | _ -> assert false

let new_generic_var k = match k with
  | Kclock s -> s, Kclock (new_generic_clock_var ())
  | Kcarrier s -> s, Kcarrier (new_generic_carrier_var s)
  | Kcarrier_row s -> s, Kcarrier_row (new_generic_carrier_row_var ())
  | Keffect s -> s, Keffect (new_generic_effect_var ())
  | Keffect_row s -> s, Keffect_row (new_generic_effect_row_var ())
  | Kreact s -> s, Kreact (new_generic_react_var ())


let assoc_of_params pl =
  let assoc_of_param p = match p with
    | Kclock { te_desc = Tvar v }
    | Kcarrier { ce_desc = Cvar v }
    | Kcarrier_row { cer_desc = Crow_var v }
    | Keffect { ee_desc = Effvar v }
    | Keffect_row { eer_desc = Effrow_var v } -> v, p
    | _ -> assert false
  in
  List.map assoc_of_param pl

(* Typing of constants *)
let clock_of_immediate i =
  match i with
  | Const_unit -> clock_unit
  | Const_bool _ -> clock_bool
  | Const_int(i) -> clock_int
  | Const_float(i) -> clock_float
  | Const_char(c) -> clock_char
  | Const_string(c) -> clock_string

(* Typing of type expressions *)
let clock_of_type_expression ty_vars env typexp =
  let rec clock_of_te ty_vars typexp =
    match typexp.te_desc with
    | Tvar s ->
        begin try
          expect_clock (List.assoc s ty_vars)
        with
          Not_found | Invalid_argument _ -> unbound_clock_err s typexp.te_loc
        end

    | Tforall(params, te) ->
        let params = List.map new_generic_var_param params in
        let ty_vars = params@ty_vars in
        let params = snd (List.split params) in
        ty_forall params (clock_of_te ty_vars te)

    | Tsome (params, te) ->
        let ty_vars = (List.map new_var_param params)@ty_vars in
        clock_of_te ty_vars te

    | Tarrow (t1, t2, ee) ->
        arrow (clock_of_te ty_vars t1) (clock_of_te ty_vars t2)
          (get_eff (effect_row_of_eer ty_vars ee))

    | Tproduct (l) ->
        product (List.map (clock_of_te ty_vars) l)

    | Tconstr (s, p_list) ->
        let name = check_clock_constr_defined typexp.te_loc s (list_arity p_list) in
        constr name (List.map (param_of_pe ty_vars) p_list)

    | Tprocess (te,_, ce, ee) ->
        process (clock_of_te ty_vars te) (carrier_of_ce ty_vars ce)
          (get_eff (effect_row_of_eer ty_vars ee)) (*TODO*) no_react

    | Tdepend cer ->
        depend (get_car (carrier_row_of_cer ty_vars cer))

  and carrier_of_ce ty_vars ce = match ce.ce_desc with
    | Cvar s ->
        (try
            expect_carrier (List.assoc s ty_vars)
          with
              Not_found | Invalid_argument _  -> unbound_carrier_err s ce.ce_loc)
    | Cident n ->
        let ck_sch = Env.find n env in
        let ck = ensure_monotype (clock_of_sch ck_sch) in
        (try
            filter_depend ck
          with
            | Unify -> non_clock_err ce.ce_loc)
    | Ctopck -> topck_carrier

  and carrier_row_of_cer ty_vars cr = match cr.cer_desc with
    | Crow_one ce ->
      let c = carrier_of_ce ty_vars ce in
      carrier_row_one c
    | _ -> assert false

  and effect_of_ee ty_vars ee = match ee.ee_desc with
    | Effempty -> no_effect
    | Effvar s ->
        (try
            expect_effect (List.assoc s ty_vars)
          with
              Not_found | Invalid_argument _ -> unbound_effect_err s ee.ee_loc)
    | Effsum (ee1, ee2) ->
        eff_sum (effect_of_ee ty_vars ee1) (effect_of_ee ty_vars ee2)
    | Effdepend c ->
        eff_depend (get_car (carrier_row_of_cer ty_vars c))
    | Effone _ -> assert false

  and effect_row_of_eer ty_vars eer = match eer.eer_desc with
    | Effrow_one ee ->
      let eff = effect_of_ee ty_vars ee in
      eff_row_one eff
    | _ -> assert false

  and param_of_pe ty_vars pe = match pe with
    | Kclock te -> Kclock (clock_of_te ty_vars te)
    | Kcarrier ce -> Kcarrier (carrier_of_ce ty_vars ce)
    | Kcarrier_row cer -> Kcarrier_row (carrier_row_of_cer ty_vars cer)
    | Keffect ee -> Keffect (effect_of_ee ty_vars ee)
    | Keffect_row eer -> Keffect_row (effect_row_of_eer ty_vars eer)
    | Kreact _ -> assert false
  in
  clock_of_te ty_vars typexp

(* Free variables of a type *)
let free_of_typeexp te =
  let type_expression_desc funs vars ted = match ted with
    | Tvar s -> ted, (Kclock s)::vars
    | _ -> raise Fallback
  in
  let carrier_expression_desc funs vars ted = match ted with
    | Cvar s -> ted, (Kcarrier s)::vars
    | _ -> raise Fallback
  in
  let carrier_row_expression_desc funs vars ted = match ted with
    | Crow_var s -> ted, (Kcarrier_row s)::vars
    | _ -> raise Fallback
  in
  let effect_expression_desc funs vars ted = match ted with
    | Effvar s -> ted, (Keffect s)::vars
    | _ -> raise Fallback
  in
  let effect_row_expression_desc funs vars ted = match ted with
    | Effrow_var s -> ted, (Keffect_row s)::vars
    | _ -> raise Fallback
  in
  let funs = { Reac_mapfold.defaults with
    type_expression_desc = type_expression_desc;
    carrier_expression_desc = carrier_expression_desc;
    carrier_row_expression_desc = carrier_row_expression_desc;
    effect_expression_desc = effect_expression_desc;
    effect_row_expression_desc = effect_row_expression_desc;
  } in
  snd (Reac_mapfold.type_expression_it funs [] te)

(* translating a declared type expression into an internal type *)
let full_clock_of_type_expression env typ =
  let ty_vars = free_of_typeexp typ in
  let ty_vars = List.map new_generic_var ty_vars in
  let typ = clock_of_type_expression ty_vars env typ in
  let vars = snd (List.split ty_vars) in
  { cs_vars = vars;
    cs_desc = typ }

let simple_clock_of_type_expression env typ =
  let ty_vars = free_of_typeexp typ in
  let ty_vars = List.map new_generic_var ty_vars in
  let typ = clock_of_type_expression ty_vars env typ in
  typ

(* Typing of patterns *)
let rec clock_of_pattern global_env local_env env patt ty =
  patt.patt_clock <- ty;
  Stypes.record (Ti_patt patt);
  match patt.patt_desc with
  | Pany ->
      (global_env, local_env)

  | Pvar (Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ck_info <- Some { value_ck = forall [] ty };
      (gl::global_env, local_env)
  | Pvar (Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      global_env, (x,ty)::local_env

  | Palias (p,Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ck_info <- Some { value_ck = forall [] ty };
      clock_of_pattern (gl::global_env) local_env env p ty
  | Palias (p,Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      clock_of_pattern global_env ((x,ty)::local_env) env p ty

  | Pconstant (i) ->
      unify_patt patt ty (clock_of_immediate i);
      global_env, local_env

  | Ptuple (l) ->
      let ty_list = List.map (fun _ -> new_clock_var ()) l in
      unify_patt patt ty (product ty_list);
      clock_of_pattern_list global_env local_env env l ty_list

  | Pconstruct (c, None) ->
      begin
        let { cstr_arg = ty_arg_opt;
              cstr_res = actual_ty } = get_clock_of_constructor c patt.patt_loc
        in
        unify_patt patt ty actual_ty;
        match ty_arg_opt with
        | None -> global_env, local_env
        | Some _ -> constr_arity_err c.gi patt.patt_loc
      end
  | Pconstruct (c, Some arg_patt) ->
      begin
        let { cstr_arg = ty_arg_opt;
              cstr_res = ty_res; } = get_clock_of_constructor c patt.patt_loc
        in
        unify_patt patt ty ty_res;
        match ty_arg_opt with
        | None -> constr_arity_err_2 c.gi patt.patt_loc
        | Some ty_arg ->
            clock_of_pattern global_env local_env env arg_patt ty_arg
      end

  | Por (p1,p2) ->
      let global_env1, local_env1 =
        clock_of_pattern global_env local_env env p1 ty
      in
      let global_env2, local_env2 =
        clock_of_pattern global_env local_env env p2 ty
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
            (Global.ck_info gl1).value_ck.cs_desc
            (Global.ck_info gl2).value_ck.cs_desc)
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

  | Precord (label_patt_list) ->
      let rec clock_of_record global_env local_env label_list label_pat_list =
        match label_pat_list with
          [] -> global_env, local_env
        | (label,label_pat) :: label_pat_list ->
            let { lbl_arg = ty_arg;
                  lbl_res = ty_res } = get_clock_of_label label patt.patt_loc
            in
            (* check that the label appears only once *)
            if List.mem label label_list
            then non_linear_record_err label.gi patt.patt_loc;
            unify_patt patt ty ty_arg;
            let global_env, local_env =
              clock_of_pattern global_env local_env env label_pat ty_res
            in
            clock_of_record
              global_env local_env (label :: label_list) label_pat_list
      in
      clock_of_record global_env local_env [] label_patt_list

  | Parray (l) ->
      let ty_var = new_clock_var () in
      unify_patt patt ty (constr_notabbrev array_ident [Kclock ty_var]);
      List.fold_left
        (fun (gl_env,lc_env) p -> clock_of_pattern gl_env lc_env env p ty_var)
        (global_env,local_env) l

  | Pconstraint (p,t) ->
      let new_ty = instance (full_clock_of_type_expression env t) in
      unify_patt p ty new_ty;
      clock_of_pattern global_env local_env env p new_ty

and clock_of_pattern_list global_env local_env env patt_list ty_list =
  match patt_list, ty_list with
  | [], [] -> global_env, local_env
  | p::patt_list, t::ty_list ->
      let global_env, local_env = clock_of_pattern global_env local_env env p t in
      clock_of_pattern_list global_env local_env env patt_list ty_list
  | _ -> raise (Internal (Location.none, "clock_of_pattern_list"))

(* Typing of expressions *)
let rec schema_of_expression env expr =
  let t =
    match expr.e_desc with
    | Econstant (i) -> clock_of_immediate i

    | Elocal (n) ->
        let typ_sch = Env.find n env in
        (*instance*) clock_of_sch typ_sch

    | Eglobal (n) ->
        (*instance*) clock_of_sch (Global.ck_info n).value_ck

    | Elet (flag, patt_expr_list, e) ->
        let gl_env, new_env = type_let (flag = Recursive) env patt_expr_list in
        let old_react = !current_react in
        let ck, r = clock_react_of_expression new_env e in
        set_current_react (react_seq old_react r);
        ck

    | Efunction (matching)  ->
        let ty_arg = new_clock_var() in
        let ty_res = new_clock_var() in
        let eff = new_effect_var () in
        let ty = arrow ty_arg ty_res eff in
        let old_current_effect = !current_effect in
        current_effect := no_effect;
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] env p ty_arg in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] ty) env)
                env loc_env
            in
            type_expect new_env e ty_res)
          matching;
        (* take the current effect and put it in the arrow *)
        let computed_eff =
          if !Compiler_options.no_clock_effects then
            new_effect_var ()
          else
            simplify_effect !current_effect
        in
        effect_unify eff computed_eff;
        current_effect := old_current_effect;
        ty

    | Eapply (fct, args) ->
        let ty_fct = clock_of_expression env fct in
        let rec type_args ty_res = function
          | [] -> ty_res
          | arg :: args ->
              let t1, t2, eff =
                try
                  filter_arrow ty_res
                with Unify ->
                  application_of_non_function_err fct ty_fct
              in
              (match t1.desc with
                | Clock_forall _ -> schema_expect env arg t1
                | _ ->  type_expect env arg t1);
              add_effect eff;
              type_args t2 args
        in
        type_args ty_fct args

    | Etuple (l) ->
        product (List.map (clock_of_expression env) l)

    | Econstruct(c,None) ->
        begin
          let { cstr_arg = ty_arg_opt;
                cstr_res = ty } = get_clock_of_constructor c expr.e_loc
          in
          match ty_arg_opt with
          | None -> ty
          | Some ty_arg -> constr_arity_err c.gi expr.e_loc
        end
    | Econstruct (c, Some arg) ->
        begin
          let { cstr_arg = ty_arg_opt;
                cstr_res = ty_res; } = get_clock_of_constructor c expr.e_loc
          in
          match ty_arg_opt with
          | None -> constr_arity_err_2 c.gi expr.e_loc
          | Some ty_arg ->
              type_expect env arg ty_arg;
              ty_res
        end

    | Earray (l) ->
        let ty_var = new_clock_var () in
        List.iter (fun e -> type_expect env e ty_var) l;
        constr_notabbrev array_ident [Kclock ty_var]

    | Erecord (l) ->
        let ty = new_clock_var() in
        let rec typing_record label_list label_expr_list =
          match label_expr_list with
            [] -> ()
          | (label,label_expr) :: label_expr_list ->
              let { lbl_arg = ty_arg;
                    lbl_res = ty_res } = get_clock_of_label label expr.e_loc
              in
              (* check that the label appears only once *)
              if List.mem label label_list
              then non_linear_record_err label.gi expr.e_loc;
              schema_expect env label_expr ty_res;
              unify_expr expr ty ty_arg;
              typing_record (label :: label_list) label_expr_list
        in
        typing_record [] l;
        ty

    | Erecord_access (e, label) ->
        let { lbl_arg = ty_arg; lbl_res = ty_res } =
          get_clock_of_label label expr.e_loc
        in
        type_expect env e ty_arg;
        ty_res

    | Erecord_with (e, l) ->
        let ty = new_clock_var() in
        let rec typing_record label_list label_expr_list =
          match label_expr_list with
            [] -> ()
          | (label,label_expr) :: label_expr_list ->
              let { lbl_arg = ty_arg;
                    lbl_res = ty_res } = get_clock_of_label label expr.e_loc
              in
              (* check that the label appears only once *)
              if List.mem label label_list
              then non_linear_record_err label.gi expr.e_loc;
              schema_expect env label_expr ty_res;
              unify_expr expr ty ty_arg;
              typing_record (label :: label_list) label_expr_list
        in
        typing_record [] l;
        type_expect env e ty;
        ty

    | Erecord_update (e1, label, e2) ->
        let { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
          get_clock_of_label label expr.e_loc
        in
        type_expect env e1 ty_arg;
        type_expect env e2 ty_res;
        clock_unit

    | Econstraint(e,t) ->
        let expected_ty = instance (full_clock_of_type_expression env t) in
        type_expect env e expected_ty;
        expected_ty

    | Etrywith (body,matching) ->
        let ty = clock_of_expression env body in
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] env p clock_exn in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] ty) env)
                env loc_env
            in
            type_expect new_env e ty)
          matching;
        ty

    | Eassert e ->
        type_expect env e clock_bool;
        new_clock_var()

    | Eifthenelse (cond,e1,e2) ->
        type_expect env cond clock_bool;
        let ty, r2 = clock_react_of_expression env e2 in
        let r1 = type_react_expect env e1 ty in
        set_current_react (react_or r1 r2);
        ty

    | Ematch (body,matching) ->
        let ty_body = clock_of_expression env body in
        let ty_res = new_clock_var() in
        let local_react = ref None in
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] env p ty_body in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] ty) env)
                env loc_env
            in
            let r = type_react_expect new_env e ty_res in
            match !local_react with
              | None -> local_react := Some r
              | Some local -> local_react := Some (react_or r local))
          matching;
        (match !local_react with
          | None -> ()
          | Some local -> set_current_react local);
        ty_res

    | Ewhen_match (e1,e2) ->
        type_expect env e1 clock_bool;
        clock_of_expression env e2

    | Ewhile (e1,e2) ->
        type_expect env e1 clock_bool;
        type_statement env e2;
        clock_unit

    | Efor(i,e1,e2,flag,e3) ->
        type_expect env e1 clock_int;
        type_expect env e2 clock_int;
        type_statement (Env.add i (forall [] clock_int) env) e3;
        clock_unit

    | Eseq e_list ->
        let local_react = ref no_react in
        let rec f l =
          match l with
          | [] -> assert false
          | [e] ->
              let ck, r = clock_react_of_expression env e in
              local_react := react_seq !local_react r;
              ck
          | e::l ->
              let r = type_statement_react env e in
              local_react := react_seq !local_react r;
              f l
        in
        let ck = f e_list in
        set_current_react !local_react;
        ck

    | Eprocess(e) ->
        let old_activation_carrier = !activation_carrier in
        let old_current_effect = !current_effect in
        activation_carrier := new_carrier_var generic_activation_name;
        current_effect := no_effect;
        push_type_level ();
        let ck, r = clock_react_of_expression env e in
        pop_type_level ();
        let r = remove_local_from_react r in
        let r =
          if !Compiler_options.no_reactivity then
            new_react_var ()
          else
            react_or (new_react_var ()) r
        in
        let eff =
          if !Compiler_options.no_clock_effects then
            new_effect_var ()
          else
            simplify_effect !current_effect
        in
        let res_ck = process ck !activation_carrier eff r in
        activation_carrier := old_activation_carrier;
        current_effect := old_current_effect;
        set_current_react no_react;
        res_ck

    | Epre (Status, s) ->
        let ty_s = clock_of_expression env s in
        let _, _, _ =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        clock_bool
    | Epre (Value, s) ->
        let ty_s = clock_of_expression env s in
        let _, ty, _ =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Elast s ->
        let ty_s = clock_of_expression env s in
        let _, ty, _ =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Edefault s ->
        let ty_s = clock_of_expression env s in
        let _, ty, _ =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Eemit (s, None) ->
        let ty_s = clock_of_expression env s in
        let ty, _, sck =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        unify_emit expr.e_loc clock_unit ty;
        clock_unit

    | Eemit (s, Some e) ->
        let ty_s = clock_of_expression env s in
        let ty, _, sck =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let ty_e = clock_of_expression env e in
        unify_emit e.e_loc ty ty_e;
        clock_unit

    | Esignal (_, (s,te_opt), ce, _, combine_opt, _, e) ->
        let ty_emit = new_clock_var() in
        let ty_get = new_clock_var() in
        let ty_ck = type_clock_expr env ce in
        let ty_s = constr_notabbrev event_ident
          [Kclock ty_emit; Kclock ty_get; Kcarrier ty_ck] in
        opt_iter
          (fun te ->
            unify_event s (instance (full_clock_of_type_expression env te)) ty_s)
          te_opt;
        begin
          match combine_opt with
          | None ->
              unify_event s
                (constr_notabbrev event_ident
                   [Kclock ty_emit;
                    Kclock (constr_notabbrev list_ident [Kclock ty_emit]);
                    Kcarrier ty_ck])
                ty_s
          | Some (default,comb) ->
              type_expect env default ty_get;
              type_expect env comb (arrow ty_emit (arrow ty_get ty_get no_effect) no_effect)
        end;
        clock_of_expression (Env.add s (forall [] ty_s) env) e

    | Enothing -> clock_unit

    | Epause (_, k, ce) ->
        let c = type_clock_expr env ce in
        add_effect (eff_depend c);
        let rc =
          match k with
            | Strong -> c
            | Weak -> topck_carrier
        in
        set_current_react (react_carrier rc);
        clock_unit

    | Ehalt _ ->
        set_current_react (react_loop (react_carrier !activation_carrier));
        new_clock_var()

    | Eloop (None, p) ->
        let r = type_statement_react env p in
        set_current_react (react_loop r);
        expr.e_react <- !current_react;
        clock_unit

    | Eloop (Some n, p) ->
        type_expect env n clock_int;
        let r = type_statement_react env p in
        set_current_react (react_loop r);
        expr.e_react <- !current_react;
        clock_unit

    | Efordopar(i,e1,e2,flag,p) ->
        type_expect env e1 clock_int;
        type_expect env e2 clock_int;
        type_statement (Env.add i (forall [] clock_int) env) p;
        clock_unit

    | Epar p_list ->
        let rl = List.map (fun p -> type_statement_react env p) p_list in
        set_current_react (make_generic (React_par rl));
        clock_unit

    | Emerge (p1,p2) ->
        type_statement env p1;
        type_statement env p2;
        clock_unit

    | Erun (e1) ->
        let ty_e = clock_of_expression env e1 in
        let ty = new_clock_var() in
        let eff = new_effect_var () in
        let r = new_react_var () in
        unify_run e1.e_loc ty_e (process ty !activation_carrier eff r);
        add_effect eff;
        set_current_react (react_run r);
        expr.e_react <- !current_react;
        ty

    | Euntil (s,p,patt_proc_opt, k) ->
        begin match patt_proc_opt with
        | None ->
            ignore (type_of_event_config env s);
            type_expect env p clock_unit;
            clock_unit
        | Some _ ->
            let cont_react = ref no_react in
            begin match s.conf_desc with
            | Cpresent s ->
                let ty_s = clock_of_expression env s in
                let ty_emit, ty_get, ty_ck =
                  try
                    filter_event ty_s
                  with Unify ->
                    non_event_err s
                in
                let ty_body, r_body = clock_react_of_expression env p in
                opt_iter
                  (fun (patt,proc) ->
                    let gl_env, loc_env = clock_of_pattern [] [] env patt ty_get in
                    assert (gl_env = []);
                    let new_env =
                      List.fold_left
                        (fun env (x, ty) -> Env.add x (forall [] ty) env)
                        env loc_env
                    in
                    let r = type_react_expect new_env proc ty_body in
                    cont_react := r)
                  patt_proc_opt;
                let rc =
                  match k with
                    | Strong -> ty_ck
                    | Weak -> topck_carrier
                in
                set_current_react (react_or r_body (react_seq (react_carrier rc) !cont_react));
                ty_body
            | _ ->
                non_event_err2 s
            end
        end


    | Ewhen (s,p) ->
        (try
            ignore (type_of_event_config ~force_activation_ck:true env s)
          with
            | Escape (sk, _) -> immediate_dep_escape_err expr.e_loc sk);
        clock_of_expression env p

    | Econtrol (s, None, p) ->
        ignore (type_of_event_config env s);
        clock_of_expression env p

    | Econtrol (s, (Some (patt, e)), p) ->
        begin match s.conf_desc with
        | Cpresent s ->
            let ty_s = clock_of_expression env s in
            let ty_emit, ty_get, ty_ck =
              try
                filter_event ty_s
              with Unify ->
                non_event_err s
            in
            let ty_body = clock_of_expression env p in
            let gl_env, loc_env = clock_of_pattern [] [] env patt ty_get in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] ty) env)
                env loc_env
            in
            type_expect new_env e clock_bool;
            ty_body
        | _ ->
            non_event_err2 s
        end

    | Eget (s,patt,p) ->
        let ty_s = clock_of_expression env s in
        let _, ty_get, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let gl_env, loc_env = clock_of_pattern [] [] env patt ty_get in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] ty) env)
            env loc_env
        in
        clock_of_expression new_env p

    | Epresent (s,p1,p2) ->
        (try
           ignore (type_of_event_config ~force_activation_ck:true env s)
         with
           | Escape (sk, _) ->immediate_dep_escape_err expr.e_loc sk);
        let ty, r1 = clock_react_of_expression env p1 in
        let r2 = type_react_expect env p2 ty in
        (* r = r1 + (ck; r2) *)
        let r2 = react_seq (react_carrier !activation_carrier) r2 in
        set_current_react (react_or r1 r2);
        ty

    | Eawait (imm,s) ->
        let sck::_ =
          try
            type_of_event_config ~force_activation_ck:(imm = Immediate) env s
          with
            | Escape (sk, _) when imm = Immediate -> immediate_dep_escape_err expr.e_loc sk
        in
        if imm <> Immediate then set_current_react (react_carrier sck);
        clock_unit

    | Eawait_val (_,All,s,patt,p) ->
        let ty_s = clock_of_expression env s in
        let _, ty_get, sck =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let gl_env, loc_env = clock_of_pattern [] [] env patt ty_get in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] ty) env)
            env loc_env
        in
        let ck, r = clock_react_of_expression new_env p in
        set_current_react (react_seq (react_carrier sck) r);
        ck
    | Eawait_val (imm,One,s,patt,p) ->
        let ty_s = clock_of_expression env s in
        let ty_emit, ty_get, ty_ck =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        unify_expr s
          (constr_notabbrev event_ident
             [Kclock ty_emit;
              Kclock (constr_notabbrev list_ident [Kclock ty_emit]);
              Kcarrier ty_ck])
          ty_s;
        let gl_env, loc_env = clock_of_pattern [] [] env patt ty_emit in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] ty) env)
            env loc_env
        in
        let ck, r = clock_react_of_expression new_env p in
        if imm <> Immediate then
          set_current_react (react_seq (react_carrier ty_ck) r)
        else
          set_current_react r;
        ck

    | Enewclock (id, sch, period, e) ->
      let sch_type = arrow clock_int
        (product [clock_int; clock_int]) no_effect in
      Misc.opt_iter (fun sch -> type_expect env sch sch_type) sch;
      Misc.opt_iter (fun p -> type_expect env p clock_int) period;
      push_type_level ();
      (* create a fresh skolem *)
      let c = carrier_skolem id.Ident.name Clocks_utils.names#name in
      let new_ck = depend c in
      let env = Env.add id (forall [] new_ck) env in
      (* change activation clock *)
      let old_activation_carrier = !activation_carrier in
      activation_carrier := c;
      (*type the body*)
      let ck, r = clock_react_of_expression env e in
      (* reset activation clock *)
      activation_carrier := old_activation_carrier;
      pop_type_level ();
      (* remove clock from current effect *)
      current_effect := remove_ck_from_effect c !current_effect;
      (* r = r[c <- 0] *)
      let r =
        match period with
          | None -> remove_ck_from_react c r (* r [ck <- 0] *)
          | Some _ -> cond_subst_react c (react_carrier !activation_carrier) r
      in
      set_current_react r;
      expr.e_react <- r;
      ck

    | Etopck -> depend topck_carrier
    | Ebase -> depend !activation_carrier
  in
  expr.e_clock <- t;
  Stypes.record (Ti_expr expr);
  Reactivity.check_exp expr;
  t

and clock_of_expression env e =
  ensure_monotype (schema_of_expression env e)


(* Typing of event configurations *)
and type_of_event_config ?(force_activation_ck=false) env conf =
  match conf.conf_desc with
  | Cpresent s ->
      let ty = clock_of_expression env s in
      let _, _, sck =
        try
          filter_event ~force_activation_ck:force_activation_ck ty
        with Unify ->
          if force_activation_ck then
            event_bad_clock_err s ty !activation_carrier
          else
            non_event_err s
      in
      [sck]

  (* TODO: renvoyer la liste des horloges *)
  | Cand (c1,c2) ->
      let l1 = type_of_event_config env c1 in
      let l2 = type_of_event_config env c2 in
      unify_carrier_lists l1 l2;
      l1@l2

  | Cor (c1,c2) ->
      let l1 = type_of_event_config env c1 in
      let l2 = type_of_event_config env c2 in
      l1@l2

and type_clock_expr env ce =
  let ty_ce = clock_of_expression env ce in
  try
    filter_depend ty_ce
  with
    | Unify -> non_clock_err ce.e_loc


(* Typing of let declatations *)
and type_let is_rec env patt_expr_list =
  push_type_level();
  let ty_list = List.map (fun _ -> new_clock_var()) patt_expr_list in
  let global_env, local_env =
    clock_of_pattern_list [] [] env (List.map fst patt_expr_list) ty_list
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
  let local_react = ref no_react in
  List.iter2
    (fun (patt,expr) ty -> let r = type_react_expect let_env expr ty in
                           local_react := react_par r !local_react)
    patt_expr_list
    ty_list;
  set_current_react !local_react;
  pop_type_level();
  List.iter2
    (fun (_,expr) ty -> if not (is_nonexpansive expr) then non_gen ty)
    patt_expr_list
    ty_list;
  List.iter simplify_clock ty_list;
  let _ =
    List.iter
      (fun gl ->
        gl.ck_info <- Some { value_ck = gen (Global.ck_info gl).value_ck.cs_desc })
      global_env
  in
  let gen_env = Env.map (fun ty -> gen ty.cs_desc) add_env in
  global_env, Env.append gen_env env

and clock_react_of_expression env expr =
  let old_current_react = !current_react in
  set_current_react no_react;
  let ck = clock_of_expression env expr in
  let r = !current_react in
  set_current_react old_current_react;
  ck, r

(* Typing of an expression with an expected type *)
and type_expect env expr expected_ty =
  let actual_ty = clock_of_expression env expr in
  unify_expr expr expected_ty actual_ty

and type_react_expect env expr expected_ty =
  let actual_ty, r = clock_react_of_expression env expr in
  unify_expr expr expected_ty actual_ty;
  r

and schema_expect env expr expected_ty =
  let actual_ty = schema_of_expression env expr in
  unify_expr expr expected_ty actual_ty

(* Typing of statements (expressions whose values are ignored) *)
and type_statement env expr =
  ignore (clock_of_expression env expr)
  (*match (clock_repr ty).desc with
  | Type_arrow(_,_) -> partial_apply_warning expr.e_loc
  | Type_var -> ()
  | Type_constr (c, _) ->
      begin match type_unit.type_desc with
      | Type_constr (c_unit, _) ->
          if not (same_type_constr c c_unit) then
            not_unit_type_warning expr
      | _ -> assert false
      end
  | _ ->
      not_dot_clock_warning expr*)

and type_statement_react env expr =
  let _, r = clock_react_of_expression env expr in
  r

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
let clock_of_type_declaration loc (type_gl, typ_params, type_decl) =
  let typ_vars = List.map new_generic_var typ_params in
  let vars = List.map snd typ_vars in
  let final_typ = constr_notabbrev type_gl.gi vars in
  let type_desc, abbr =
    match type_decl with
    | Tabstract -> Clock_abstract, Constr_notabbrev

    | Tvariant constr_decl_list ->
        check_no_repeated_constructor loc constr_decl_list;
        let cstr_list =
          List.rev_map
            (fun (gl_cstr,te_opt) ->
              let ty_arg_opt =
                opt_map (clock_of_type_expression typ_vars Env.empty) te_opt
              in
              gl_cstr.ck_info <- Some { cstr_arg = ty_arg_opt;
                                        cstr_res = final_typ; };
              Clocks_utils.add_type_description gl_cstr)
            constr_decl_list
        in
        Clock_variant cstr_list, Constr_notabbrev

    | Trecord label_decl_list ->
        check_no_repeated_label loc label_decl_list;
        let lbl_list =
          List.rev_map
            (fun (gl_lbl, mut, te) ->
              let ty_res = clock_of_type_expression typ_vars Env.empty te in
              gl_lbl.ck_info <- Some { lbl_res = ty_res;
                                    lbl_arg = final_typ;
                                    lbl_mut = mut; };
              Clocks_utils.add_type_description gl_lbl)
            label_decl_list
        in
        Clock_record lbl_list, Constr_notabbrev

    | Trebind (te) ->
        let ty_te = clock_of_type_expression typ_vars Env.empty te in
        Clock_rebind (ty_te),
        Constr_abbrev (List.map snd typ_vars, ty_te)

  in
  let clock_arity = list_arity vars in
  let info = Global.ck_info type_gl in
  (* Update the existing abbreviation *)
  let clock_constr = info.clock_constr in
  clock_constr.ck_info <- Some { constr_abbr = abbr };
  (* Update the type description *)
  type_gl.ck_info <-
    Some { clock_constr = clock_constr;
           clock_kind = type_desc;
           clock_arity = clock_arity;
           clock_def_arity = info.clock_def_arity; };
  type_gl


(* Check that an implementation without interface does not export values
   with non-generalizable types.*)
let check_nongen_values impl_item =
  let check_expr expr =
    let vars = kind_sum_split (free_clock_vars notgeneric expr.e_clock) in
    if vars.k_clock != [] or
      (vars.k_react != [] && not !Compiler_options.no_reactivity) then
      cannot_generalize_err expr;
    (*  Non-generalizable carriers and effects are set to topck *)
    List.iter (fun ck -> carrier_unify ck topck_carrier) vars.k_carrier;
    List.iter
      (fun eff -> effect_unify eff (eff_depend topck_carrier))
      vars.k_effect
    (* TODO: remove row variables of reacts but only after typing the whole file
         Il faut compiler toutes les impl d'un coup au lieu d'une par une *)
  in
  match impl_item.impl_desc with
    | Ilet (_, patt_expr_list) ->
        List.iter (fun (_,expr) -> check_expr expr) patt_expr_list
    | _ -> ()

(* Typing of implementation items *)
let impl info_chan item =
  (match item.impl_desc with
  | Iexpr (e) ->
      ignore (clock_of_expression Env.empty e)

  | Ilet (flag, patt_expr_list) ->
      let global_env, local_env =
        type_let (flag = Recursive) Env.empty patt_expr_list
      in
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_value_declaration info_chan global_env

  | Isignal (l) ->
      List.iter
        (fun (_, (s,te_opt), combine_opt) ->
          let ty_emit = new_clock_var() in
          let ty_get = new_clock_var() in
          let ty_ck = topck_carrier in
          let ty_s = constr_notabbrev event_ident
            [Kclock ty_emit; Kclock ty_get; Kcarrier ty_ck] in
          opt_iter
            (fun te ->
              unify_event s.gi.id
                (instance (full_clock_of_type_expression Env.empty te)) ty_s)
            te_opt;
          begin
            match combine_opt with
            | None ->
                unify_event s.gi.id
                  (constr_notabbrev list_ident [Kclock ty_emit])
                  ty_get
            | Some (default,comb) ->
                type_expect Env.empty default ty_get;
                type_expect Env.empty comb
                  (arrow ty_emit (arrow ty_get ty_get no_effect) no_effect)
          end;
          s.ck_info <- Some { value_ck = forall [] ty_s };
          (* verbose mode *)
          if !print_type
          then Clocks_printer.output_value_declaration info_chan [s])
        l
  | Itype (l) ->
      let global_env =
        List.map (clock_of_type_declaration item.impl_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_type_declaration info_chan global_env

  | Iexn (gl_cstr, te_opt) ->
      gl_cstr.ck_info <-
        Some {cstr_arg = opt_map (clock_of_type_expression [] Env.empty) te_opt;
              cstr_res = clock_exn; };
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_exception_declaration info_chan gl_cstr

  | Iexn_rebind (gl_cstr1, gl_cstr2) ->
      gl_cstr1.ck_info <- Some (Global.ck_info gl_cstr2);
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_exception_declaration info_chan gl_cstr1

  | Iopen _ -> ()
  );
  item

(* Typing of interface items *)
let intf info_chan item =
  (match item.intf_desc with
  | Dval (gl, te) ->
      gl.ck_info <-
        Some { value_ck = gen (full_clock_of_type_expression Env.empty te).cs_desc };
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_value_declaration info_chan [gl]

  | Dtype l ->
      let global_env =
        List.map (clock_of_type_declaration item.intf_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_type_declaration info_chan global_env

  | Dexn (gl_cstr, te_opt) ->
      gl_cstr.ck_info <-
        Some {cstr_arg = opt_map (clock_of_type_expression [] Env.empty) te_opt;
              cstr_res = clock_exn; };
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_exception_declaration info_chan gl_cstr

  | Dopen _ -> ()
  );
  item
