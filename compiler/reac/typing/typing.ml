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
open Types
open Types_utils
open Typing_errors
open Initialization
open Asttypes
open Global
open Global_ident
open Reac
open Misc
open Annot

let unify_expr expr expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with
    | Unify -> expr_wrong_type_err expr actual_ty expected_ty
    | Unify_detailed (expected_sub_ty, actual_sub_ty) ->
        expr_wrong_type_detailed_err expr actual_ty expected_ty actual_sub_ty expected_sub_ty

let unify_patt pat expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> patt_wrong_type_err pat actual_ty expected_ty

let unify_event evt expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> event_wrong_type_err evt actual_ty expected_ty

let unify_emit loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> emit_wrong_type_err loc actual_ty expected_ty

let unify_run loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> run_wrong_type_err loc actual_ty expected_ty

let unify_var loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> var_wrong_type_err loc actual_ty expected_ty

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
  let ty_desc = Global.ty_info gl in
  let arity' = ty_desc.type_arity in
  if arity' <> arity
  then type_constr_arity_err name arity' arity loc;
  ty_desc.type_constr

(* find the type of the constructor C *)
let get_type_of_constructor c loc =
  constr_instance (Global.ty_info c)

(* find the type of a label *)
let get_type_of_label label loc =
  label_instance (Global.ty_info label)

(* tests if an expression is expansive *)
let rec is_nonexpansive expr =
  match expr.e_desc with
  | Elocal _ -> true
  | Eglobal _ -> true
  | Econstant _ -> true
  | Etuple l -> List.for_all is_nonexpansive l
  | Econstruct (_, None) -> true
  | Econstruct(_, Some e) -> is_nonexpansive e
  | Elet(rec_flag, bindings, body) ->
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) bindings &&
      is_nonexpansive body
  | Efunction _ -> true
(*
  | Etrywith(body, pat_expr_list) ->
      is_nonexpansive body &&
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Eseq(e1, e2) -> is_nonexpansive e2
*)
  | Eifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive ifnot
  | Econstraint(e, ty) -> is_nonexpansive e
  | Earray [] -> true
  | Erecord lbl_expr_list ->
      List.for_all (fun (lbl, expr) ->
        (Global.ty_info lbl).lbl_mut == Immutable && is_nonexpansive expr)
        lbl_expr_list
  | Erecord_access(e, lbl) -> is_nonexpansive e
  | Ewhen_match(cond, act) -> is_nonexpansive act
  | Eprocess _ -> true
  | Epre (_, e) -> is_nonexpansive e
  | Elast e -> is_nonexpansive e
  | Edefault e -> is_nonexpansive e
  | Enothing -> true
  | Epause (_, _) -> true
  | Ehalt _ -> true
  | Eemit (e, None) -> is_nonexpansive e
  | Eemit (e1, Some e2) -> is_nonexpansive e1 && is_nonexpansive e2
  | Epresent (e,e1,e2) ->
      is_nonexpansive_conf e && is_nonexpansive e1 && is_nonexpansive e2
  | Eawait (_, e) ->
      is_nonexpansive_conf e
  | Eawait_val (_, _, s, _, e) ->
      is_nonexpansive s && is_nonexpansive e
  | Euntil (c, e, None) ->
      is_nonexpansive_conf c && is_nonexpansive e
  | Euntil (c, e, Some (_, e')) ->
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
    | Tvar (s, Ttype_var) ->
        begin try
          List.assoc s typ_vars
        with
          Not_found -> unbound_typ_err s typexp.te_loc
        end
    | Tvar (s, Tcarrier_var) -> assert false

    | Tarrow (t1, t2) ->
        arrow (type_of t1) (type_of t2)

    | Tproduct (l) ->
        product (List.map type_of l)

    | Tconstr (s, ty_list) ->
        let name =
          check_type_constr_defined typexp.te_loc s (List.length ty_list)
        in
        constr name (List.map type_of ty_list)

    | Tprocess (ty,k) ->
        process (type_of ty) { proc_static = Some(Proc_def (ref k)); }
  in
  type_of typexp

(* Free variables of a type *)
let free_of_type ty =
  let rec vars v ty =
    match ty.te_desc with
      Tvar(x, Ttype_var) -> if List.mem x v then v else x::v
    | Tvar(x, Tcarrier_var) -> v
    | Tarrow(t1,t2) -> vars (vars v t1) t2
    | Tproduct(t) ->
        List.fold_left vars v t
    | Tconstr(_,t) ->
        List.fold_left vars v t
    | Tprocess (t, _) -> vars v t
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
  | Pany ->
      (global_env, local_env)

  | Pvar (Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ty_info <- Some { value_typ = forall [] ty };
      (gl::global_env, local_env)
  | Pvar (Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      global_env, (x,ty)::local_env

  | Palias (p,Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ty_info <- Some { value_typ = forall [] ty };
      type_of_pattern (gl::global_env) local_env p ty
  | Palias (p,Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      type_of_pattern global_env ((x,ty)::local_env) p ty

  | Pconstant (i) ->
      unify_patt patt ty (type_of_immediate i);
      global_env, local_env

  | Ptuple (l) ->
      let ty_list = List.map (fun _ -> new_var ()) l in
      unify_patt patt ty (product ty_list);
      type_of_pattern_list global_env local_env l ty_list

  | Pconstruct (c, None) ->
      begin
        let { cstr_arg = ty_arg_opt;
              cstr_res = actual_ty } = get_type_of_constructor c patt.patt_loc
        in
        unify_patt patt ty actual_ty;
        match ty_arg_opt with
        | None -> global_env, local_env
        | Some _ -> constr_arity_err c.gi patt.patt_loc
      end
  | Pconstruct (c, Some arg_patt) ->
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

  | Por (p1,p2) ->
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
            (Global.ty_info gl1).value_typ.ts_desc
            (Global.ty_info gl2).value_typ.ts_desc)
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

  | Parray (l) ->
      let ty_var = new_var () in
      unify_patt patt ty (constr_notabbrev array_ident [ty_var]);
      List.fold_left
        (fun (gl_env,lc_env) p -> type_of_pattern gl_env lc_env p ty_var)
        (global_env,local_env) l

  | Pconstraint (p,t) ->
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
    match expr.e_desc with
    | Econstant (i) -> type_of_immediate i

    | Elocal (n) ->
        let typ_sch = Env.find n env in
        instance typ_sch

    | Eglobal (n) ->
        instance (Global.ty_info n).value_typ

    | Elet (flag, patt_expr_list, e) ->
        let gl_env, new_env = type_let (flag = Recursive) env patt_expr_list in
        type_of_expression new_env e

    | Efunction (matching)  ->
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

    | Eapply (fct, args) ->
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
    | Etuple (l) ->
        product (List.map (type_of_expression env) l)

    | Econstruct(c,None) ->
        begin
          let { cstr_arg = ty_arg_opt;
                cstr_res = ty } = get_type_of_constructor c expr.e_loc
          in
          match ty_arg_opt with
          | None -> ty
          | Some ty_arg -> constr_arity_err c.gi expr.e_loc
        end
    | Econstruct (c, Some arg) ->
        begin
          let { cstr_arg = ty_arg_opt;
                cstr_res = ty_res; } = get_type_of_constructor c expr.e_loc
          in
          match ty_arg_opt with
          | None -> constr_arity_err_2 c.gi expr.e_loc
          | Some ty_arg ->
              type_expect env arg ty_arg;
              ty_res
        end

    | Earray (l) ->
        let ty_var = new_var () in
        List.iter (fun e -> type_expect env e ty_var) l;
        constr_notabbrev array_ident [ty_var]

    | Erecord (l) ->
        let ty = new_var() in
        let rec typing_record label_list label_expr_list =
          match label_expr_list with
            [] -> ()
          | (label,label_expr) :: label_expr_list ->
              let { lbl_arg = ty_arg;
                    lbl_res = ty_res } = get_type_of_label label expr.e_loc
              in
              (* check that the label appears only once *)
              if List.mem label label_list
              then non_linear_record_err label.gi expr.e_loc;
              type_expect env label_expr ty_res;
              unify_expr expr ty ty_arg;
              typing_record (label :: label_list) label_expr_list
        in
        typing_record [] l;
        ty

    | Erecord_access (e, label) ->
        let { lbl_arg = ty_arg; lbl_res = ty_res } =
          get_type_of_label label expr.e_loc
        in
        type_expect env e ty_arg;
        ty_res

    | Erecord_update (e1, label, e2) ->
        let { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
          get_type_of_label label expr.e_loc
        in
        if mut = Immutable then label_not_mutable_err expr label.gi;
        type_expect env e1 ty_arg;
        type_expect env e2 ty_res;
        type_unit

    | Econstraint(e,t) ->
        let expected_ty = instance (full_type_of_type_expression t) in
        type_expect env e expected_ty;
        expected_ty

    | Etrywith (body,matching) ->
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

    | Eassert e ->
        type_expect env e type_bool;
        new_var()

    | Eifthenelse (cond,e1,e2) ->
        type_expect env cond type_bool;
        let ty = type_of_expression env e1 in
        type_expect env e2 ty;
        ty

    | Ematch (body,matching) ->
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

    | Ewhen_match (e1,e2) ->
        type_expect env e1 type_bool;
        type_of_expression env e2

    | Ewhile (e1,e2) ->
        type_expect env e1 type_bool;
        type_statement env e2;
        type_unit

    | Efor(i,e1,e2,flag,e3) ->
        type_expect env e1 type_int;
        type_expect env e2 type_int;
        type_statement (Env.add i (forall [] type_int) env) e3;
        type_unit

    | Eseq e_list ->
        let rec f l =
          match l with
          | [] -> assert false
          | [e] -> type_of_expression env e
          | e::l ->
              type_statement env e;
              f l
        in f e_list

    | Eprocess(e) ->
        let ty = type_of_expression env e in
        process ty { proc_static = Some(Proc_def (ref Static.Dontknow)); }

    | Epre (Status, s) ->
        let ty_s = type_of_expression env s in
        let _, _ty =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        type_bool
    | Epre (Value, s) ->
        let ty_s = type_of_expression env s in
        let _, ty =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Elast s ->
        let ty_s = type_of_expression env s in
        let _, ty =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Edefault s ->
        let ty_s = type_of_expression env s in
        let _, ty =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        ty

    | Eemit (s, None) ->
        let ty_s = type_of_expression env s in
        let ty, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        unify_emit expr.e_loc type_unit ty;
        type_unit

    | Eemit (s, Some e) ->
        let ty_s = type_of_expression env s in
        let ty, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let ty_e = type_of_expression env e in
        unify_emit e.e_loc ty ty_e;
        type_unit

    | Esignal ((s,te_opt), _, _, combine_opt, e) ->
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

    | Enothing -> type_unit

    | Epause (_, ck) ->
      (match ck with
        | CkExpr e -> type_expect env e type_clock
        | _ -> ());
      type_unit

    | Ehalt _ -> new_var()

    | Eloop (None, p) ->
        type_statement env p;
        type_unit

    | Eloop (Some n, p) ->
        type_expect env n type_int;
        type_statement env p;
        type_unit

    | Efordopar(i,e1,e2,flag,p) ->
        type_expect env e1 type_int;
        type_expect env e2 type_int;
        type_statement (Env.add i (forall [] type_int) env) p;
        type_unit

    | Epar p_list ->
        List.iter (fun p -> ignore (type_statement env p)) p_list;
        type_unit

    | Emerge (p1,p2) ->
        type_statement env p1;
        type_statement env p2;
        type_unit

    | Erun (e) ->
        let ty_e = type_of_expression env e in
        let ty = new_var() in
        unify_run e.e_loc
          ty_e (process ty { proc_static = None; });
        ty

    | Euntil (s,p,patt_proc_opt) ->
        begin match patt_proc_opt with
        | None ->
            type_of_event_config env s;
            type_expect env p type_unit;
            type_unit
        | Some _ ->
            begin match s.conf_desc with
            | Cpresent s ->
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


    | Ewhen (s,p) ->
        type_of_event_config env s;
        type_of_expression env p

    | Econtrol (s, None, p) ->
        type_of_event_config env s;
        type_of_expression env p
    | Econtrol (s, (Some (patt, e)), p) ->
        begin match s.conf_desc with
        | Cpresent s ->
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

    | Eget (s,patt,p) ->
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

    | Epresent (s,p1,p2) ->
        type_of_event_config env s;
        let ty = type_of_expression env p1 in
        type_expect env p2 ty;
        ty

    | Eawait (_,s) ->
        type_of_event_config env s;
        type_unit

    | Eawait_val (_,All,s,patt,p) ->
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
    | Eawait_val (_,One,s,patt,p) ->
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

    | Enewclock (id, sch, e) ->
      let env = Env.add id (forall [] type_clock) env in
      let sch_type = arrow type_int (product [type_int; type_int]) in
      Misc.opt_iter (fun sch -> type_expect env sch sch_type) sch;
      type_of_expression env e

    | Epauseclock ck ->
      type_expect env ck type_clock;
      type_unit

    | Etopck -> type_clock

  in
  expr.e_type <- t;
  Stypes.record (Ti_expr expr);
  t


(* Typing of event configurations *)
and type_of_event_config env conf =
  match conf.conf_desc with
  | Cpresent s ->
      let ty = type_of_expression env s in
      let _ =
        try
          filter_event ty
        with Unify ->
          non_event_err s
      in
      ()

  | Cand (c1,c2) ->
      type_of_event_config env c1;
      type_of_event_config env c2

  | Cor (c1,c2) ->
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
        gl.ty_info <- Some { value_typ = gen (Global.ty_info gl).value_typ.ts_desc })
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
  let typ_vars = List.map (fun (v, _) -> (v,new_generic_var ())) typ_params in
  let final_typ =
    constr_notabbrev type_gl.gi (List.map snd typ_vars)
  in
  let type_desc, abbr =
    match type_decl with
    | Tabstract -> Type_abstract, Constr_notabbrev

    | Tvariant constr_decl_list ->
        check_no_repeated_constructor loc constr_decl_list;
        let cstr_list =
          List.rev_map
            (fun (gl_cstr,te_opt) ->
              let ty_arg_opt =
                opt_map (type_of_type_expression typ_vars) te_opt
              in
              gl_cstr.ty_info <- Some { cstr_arg = ty_arg_opt;
                                     cstr_res = final_typ; };
              Types_utils.add_clock_description gl_cstr)
            constr_decl_list
        in
        Type_variant cstr_list, Constr_notabbrev

    | Trecord label_decl_list ->
        check_no_repeated_label loc label_decl_list;
        let lbl_list =
          List.rev_map
            (fun (gl_lbl, mut, te) ->
              let ty_res = type_of_type_expression typ_vars te in
              gl_lbl.ty_info <- Some { lbl_res = ty_res;
                                    lbl_arg = final_typ;
                                    lbl_mut = mut; };
              Types_utils.add_clock_description gl_lbl)
            label_decl_list
        in
        Type_record lbl_list, Constr_notabbrev

    | Trebind (te) ->
        let ty_te = type_of_type_expression typ_vars te in
        Type_rebind (ty_te),
        Constr_abbrev (List.map snd typ_vars, ty_te)

  in
  type_gl.ty_info <-
    Some { type_constr = {gi = type_gl.gi;
                          ty_info = Some {constr_abbr = abbr};
                          ck_info = None };
           type_kind = type_desc;
           type_arity = List.length typ_vars };
  type_gl


(* Check that an implementation without interface does not export values
   with non-generalizable types. *)
let check_nongen_values impl_item_list =
  List.iter
    (fun impl_item ->
      match impl_item.impl_desc with
      | Ilet (_, patt_expr_list) ->
          List.iter (fun (patt,expr) ->
            if free_type_vars notgeneric expr.e_type != []
            then
              cannot_generalize_err expr)
            patt_expr_list
      | _ -> ())
    impl_item_list

(* Typing of implementation items *)
let impl info_chan item =
  (match item.impl_desc with
  | Iexpr (e) ->
      ignore (type_of_expression Env.empty e)

  | Ilet (flag, patt_expr_list) ->
      let global_env, local_env =
        type_let (flag = Recursive) Env.empty patt_expr_list
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_value_type_declaration info_chan global_env

  | Isignal (l) ->
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
          s.ty_info <- Some { value_typ = forall [] ty_s };
          (* verbose mode *)
          if !print_type
          then Types_printer.output_value_type_declaration info_chan [s])
        l
  | Itype (l) ->
      let global_env =
        List.map (type_of_type_declaration item.impl_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_type_declaration info_chan global_env

  | Iexn (gl_cstr, te_opt) ->
      gl_cstr.ty_info <-
        Some {cstr_arg = opt_map (type_of_type_expression []) te_opt;
              cstr_res = type_exn; };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr

  | Iexn_rebind (gl_cstr1, gl_cstr2) ->
      gl_cstr1.ty_info <- Some (Global.ty_info gl_cstr2);
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr1

  | Iopen _ -> ()
  );
  item

(* Typing of interface items *)
let intf info_chan item =
  (match item.intf_desc with
  | Dval (gl, te) ->
      gl.ty_info <-
        Some { value_typ = gen (full_type_of_type_expression te).ts_desc };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_value_type_declaration info_chan [gl]

  | Dtype l ->
      let global_env =
        List.map (type_of_type_declaration item.intf_loc) l
      in
      (* verbose mode *)
      if !print_type
      then Types_printer.output_type_declaration info_chan global_env

  | Dexn (gl_cstr, te_opt) ->
      gl_cstr.ty_info <-
        Some {cstr_arg = opt_map (type_of_type_expression []) te_opt;
              cstr_res = type_exn; };
      (* verbose mode *)
      if !print_type
      then Types_printer.output_exception_declaration info_chan gl_cstr

  | Dopen _ -> ()
  );
  item
