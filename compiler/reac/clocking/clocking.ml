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
open Clocking_errors
open Initialization
open Asttypes
open Global
open Global_ident
open Reac
open Misc
open Annot


let filter_event ck =
  let ck = clock_repr ck in
  let ck1 = new_clock_var() in
  let ck2 = new_clock_var() in
  let sck = new_carrier_clock_var () in
  unify ck (constr_notabbrev event_ident [ck1; ck2; sck]);
  ck1, ck2, sck

let filter_multi_event ck =
  let ck = clock_repr ck in
  let ck1 = new_clock_var() in
  let sck = new_carrier_clock_var () in
  unify ck (constr_notabbrev event_ident [ck1;
                                          constr_notabbrev list_ident [ck1]; sck]);
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

let unify_run loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> run_wrong_clock_err loc actual_ty expected_ty

let unify_var loc expected_ty actual_ty =
  try
    unify expected_ty actual_ty
  with _ -> var_wrong_clock_err loc actual_ty expected_ty

(* special cases of unification *)

(* Typing environment *)
module Env = Symbol_table.Make (Ident)

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

(* Typing of type expressions *)
let clock_of_type_expression ty_vars typexp =
  let rec clock_of typexp =
    match typexp.te_desc with
    | Tvar (s, _) ->
        begin try
          List.assoc s ty_vars
        with
          Not_found -> unbound_clock_err s typexp.te_loc
        end

    | Tarrow (t1, t2) ->
        arrow (clock_of t1) (clock_of t2)

    | Tproduct (l) ->
        product (List.map clock_of l)

    | Tconstr (s, ty_list) ->
        let ck_desc = Global.ck_info s in
        constr ck_desc.clock_constr (List.map clock_of ty_list)

    | Tprocess (te,_) ->
        process (clock_of te)
  in
  clock_of typexp

(* Free variables of a type *)
let free_of_type ty =
  let rec vars (ty_vars, car_vars) ty =
    match ty.te_desc with
      | Tvar(x, Ttype_var) ->
          if List.mem x ty_vars then
            ty_vars, car_vars
          else
            x::ty_vars, car_vars
      | Tvar(x, Tcarrier_var) ->
          if List.mem x car_vars then
            ty_vars, car_vars
          else
            ty_vars, x::car_vars
      | Tarrow(t1,t2) -> vars (vars (ty_vars, car_vars) t1) t2
      | Tproduct(t) ->
          List.fold_left vars (ty_vars, car_vars) t
      | Tconstr(_,t) ->
          List.fold_left vars (ty_vars, car_vars) t
      | Tprocess (t, _) -> vars (ty_vars, car_vars) t
  in vars ([], []) ty

(* translating a declared type expression into an internal type *)
let full_clock_of_type_expression typ =
  let ty_vars, car_vars = free_of_type typ in
  let ty_vars = List.map (fun v -> v, new_generic_clock_var ()) ty_vars in
  let car_vars = List.map (fun v -> v, make_generic_carrier v) car_vars in
  let car_ty_vars = List.map (fun (v, c) -> v, make_clock (Clock_depend c)) car_vars in
  let typ = clock_of_type_expression (ty_vars @ car_ty_vars) typ in
  { cs_clock_vars = List.map snd ty_vars;
    cs_carrier_vars = List.map snd car_vars;
    cs_desc = typ }

(* Typing of patterns *)
let rec clock_of_pattern global_env local_env patt ty =
  patt.patt_clock <- ty;
  Stypes.record (Ti_patt patt);
  match patt.patt_desc with
  | Pany ->
      (global_env, local_env)

  | Pvar (Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ck_info <- Some { value_ck = forall [] [] ty };
      (gl::global_env, local_env)
  | Pvar (Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      global_env, (x,ty)::local_env

  | Palias (p,Vglobal gl) ->
      if List.exists (fun g -> g.gi.id = gl.gi.id) global_env
      then non_linear_pattern_err patt (Ident.name gl.gi.id);
      gl.ck_info <- Some { value_ck = forall [] [] ty };
      clock_of_pattern (gl::global_env) local_env p ty
  | Palias (p,Vlocal x) ->
      if List.mem_assoc x local_env
      then non_linear_pattern_err patt (Ident.name x);
      clock_of_pattern global_env ((x,ty)::local_env) p ty

  | Pconstant (i) ->
      unify_patt patt ty Clocks_utils.static;
      global_env, local_env

  | Ptuple (l) ->
      let ty_list = List.map (fun _ -> new_clock_var ()) l in
      unify_patt patt ty (product ty_list);
      clock_of_pattern_list global_env local_env l ty_list

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
            clock_of_pattern global_env local_env arg_patt ty_arg
      end

  | Por (p1,p2) ->
      let global_env1, local_env1 =
        clock_of_pattern global_env local_env p1 ty
      in
      let global_env2, local_env2 =
        clock_of_pattern global_env local_env p2 ty
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
              clock_of_pattern global_env local_env label_pat ty_res
            in
            clock_of_record
              global_env local_env (label :: label_list) label_pat_list
      in
      clock_of_record global_env local_env [] label_patt_list

  | Parray (l) ->
      let ty_var = new_clock_var () in
      unify_patt patt ty (constr_notabbrev array_ident [ty_var]);
      List.fold_left
        (fun (gl_env,lc_env) p -> clock_of_pattern gl_env lc_env p ty_var)
        (global_env,local_env) l

  | Pconstraint (p,t) ->
      let new_ty = instance (full_clock_of_type_expression t) in
      unify_patt p ty new_ty;
      clock_of_pattern global_env local_env p new_ty

and clock_of_pattern_list global_env local_env patt_list ty_list =
  match patt_list, ty_list with
  | [], [] -> global_env, local_env
  | p::patt_list, t::ty_list ->
      let global_env, local_env = clock_of_pattern global_env local_env p t in
      clock_of_pattern_list global_env local_env patt_list ty_list
  | _ -> raise (Internal (Location.none, "clock_of_pattern_list"))

(* Typing of expressions *)
let rec clock_of_expression env expr =
  let t =
    match expr.e_desc with
    | Econstant (i) -> Clocks_utils.static

    | Elocal (n) ->
        let typ_sch = Env.find n env in
        instance typ_sch

    | Eglobal (n) ->
        instance (Global.ck_info n).value_ck

    | Elet (flag, patt_expr_list, e) ->
        let gl_env, new_env = type_let (flag = Recursive) env patt_expr_list in
        clock_of_expression new_env e

    | Efunction (matching)  ->
        let ty_arg = new_clock_var() in
        let ty_res = new_clock_var() in
        let ty = arrow ty_arg ty_res in
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] p ty_arg in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
                env loc_env
            in
            type_expect new_env e ty_res)
          matching;
        ty

    | Eapply (fct, args) ->
        let ty_fct = clock_of_expression env fct in
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
        constr_notabbrev array_ident [ty_var]

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
              type_expect env label_expr ty_res;
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

    | Erecord_update (e1, label, e2) ->
        let { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
          get_clock_of_label label expr.e_loc
        in
        type_expect env e1 ty_arg;
        type_expect env e2 ty_res;
        Clocks_utils.static

    | Econstraint(e,t) ->
        let expected_ty = instance (full_clock_of_type_expression t) in
        type_expect env e expected_ty;
        expected_ty

    | Etrywith (body,matching) ->
        let ty = clock_of_expression env body in
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] p Clocks_utils.static in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
                env loc_env
            in
            type_expect new_env e ty)
          matching;
        ty

    | Eassert e ->
        type_expect env e Clocks_utils.static;
        new_clock_var()

    | Eifthenelse (cond,e1,e2) ->
        type_expect env cond Clocks_utils.static;
        let ty = clock_of_expression env e1 in
        type_expect env e2 ty;
        ty

    | Ematch (body,matching) ->
        let ty_body = clock_of_expression env body in
        let ty_res = new_clock_var() in
        List.iter
          (fun (p,e) ->
            let gl_env, loc_env = clock_of_pattern [] [] p ty_body in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
                env loc_env
            in
            type_expect new_env e ty_res)
          matching;
        ty_res

    | Ewhen_match (e1,e2) ->
        type_expect env e1 Clocks_utils.static;
        clock_of_expression env e2

    | Ewhile (e1,e2) ->
        type_expect env e1 Clocks_utils.static;
        type_statement env e2;
        Clocks_utils.static

    | Efor(i,e1,e2,flag,e3) ->
        type_expect env e1 Clocks_utils.static;
        type_expect env e2 Clocks_utils.static;
        type_statement (Env.add i (forall [] [] Clocks_utils.static) env) e3;
        Clocks_utils.static

    | Eseq e_list ->
        let rec f l =
          match l with
          | [] -> assert false
          | [e] -> clock_of_expression env e
          | e::l ->
              type_statement env e;
              f l
        in f e_list

    | Eprocess(e) ->
        let ty = clock_of_expression env e in
        process ty

    | Epre (Status, s) ->
        let ty_s = clock_of_expression env s in
        let _, _, _ =
          try
            filter_event ty_s
          with Unify ->
          non_event_err s
        in
        Clocks_utils.static
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
        let ty, _, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        unify_emit expr.e_loc Clocks_utils.static ty;
        Clocks_utils.static

    | Eemit (s, Some e) ->
        let ty_s = clock_of_expression env s in
        let ty, _, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let ty_e = clock_of_expression env e in
        unify_emit e.e_loc ty ty_e;
        Clocks_utils.static

    | Esignal ((s,te_opt), ce, _, combine_opt, e) ->
        let ty_emit = new_clock_var() in
        let ty_get = new_clock_var() in
        let ty_ck = type_clock_expr env ce in
        let ty_s = constr_notabbrev event_ident [ty_emit; ty_get; ty_ck] in
        opt_iter
          (fun te ->
            unify_event s (instance (full_clock_of_type_expression te)) ty_s)
          te_opt;
        begin
          match combine_opt with
          | None ->
              unify_event s
                (constr_notabbrev event_ident
                   [ty_emit; (constr_notabbrev list_ident [ty_emit]); ty_ck])
                ty_s
          | Some (default,comb) ->
              type_expect env default ty_get;
              type_expect env comb (arrow ty_emit (arrow ty_get ty_get))
        end;
        clock_of_expression (Env.add s (forall [] [] ty_s) env) e

    | Enothing -> Clocks_utils.static

    | Epause (_, ce) ->
        (match ce with
          | CkExpr ce ->
              let ck_ce = clock_of_expression env ce in
              (try
                  ignore (filter_depend ck_ce)
                with
                  | Unify -> non_clock_err ce)
          | _ -> ());
        Clocks_utils.static

    | Ehalt _ -> new_clock_var()

    | Eloop (None, p) ->
        type_statement env p;
        Clocks_utils.static

    | Eloop (Some n, p) ->
        type_expect env n Clocks_utils.static;
        type_statement env p;
        Clocks_utils.static

    | Efordopar(i,e1,e2,flag,p) ->
        type_expect env e1 Clocks_utils.static;
        type_expect env e2 Clocks_utils.static;
        type_statement (Env.add i (forall [] [] Clocks_utils.static) env) p;
        Clocks_utils.static

    | Epar p_list ->
        List.iter (fun p -> ignore (type_statement env p)) p_list;
        Clocks_utils.static

    | Emerge (p1,p2) ->
        type_statement env p1;
        type_statement env p2;
        Clocks_utils.static

    | Erun (e) ->
        let ty_e = clock_of_expression env e in
        let ty = new_clock_var() in
        unify_run e.e_loc
          ty_e (process ty);
        ty

    | Euntil (s,p,patt_proc_opt) ->
        begin match patt_proc_opt with
        | None ->
            type_of_event_config env s;
            type_expect env p Clocks_utils.static;
            Clocks_utils.static
        | Some _ ->
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
                opt_iter
                  (fun (patt,proc) ->
                    let gl_env, loc_env = clock_of_pattern [] [] patt ty_get in
                    assert (gl_env = []);
                    let new_env =
                      List.fold_left
                        (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
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
        clock_of_expression env p

    | Econtrol (s, None, p) ->
        type_of_event_config env s;
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
            let gl_env, loc_env = clock_of_pattern [] [] patt ty_get in
            assert (gl_env = []);
            let new_env =
              List.fold_left
                (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
                env loc_env
            in
            type_expect new_env e Clocks_utils.static;
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
        let gl_env, loc_env = clock_of_pattern [] [] patt ty_get in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
            env loc_env
        in
        clock_of_expression new_env p

    | Epresent (s,p1,p2) ->
        type_of_event_config env s;
        let ty = clock_of_expression env p1 in
        type_expect env p2 ty;
        ty

    | Eawait (_,s) ->
        type_of_event_config env s;
        Clocks_utils.static

    | Eawait_val (_,All,s,patt,p) ->
        let ty_s = clock_of_expression env s in
        let _, ty_get, _ =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        let gl_env, loc_env = clock_of_pattern [] [] patt ty_get in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
            env loc_env
        in
        clock_of_expression new_env p
    | Eawait_val (_,One,s,patt,p) ->
        let ty_s = clock_of_expression env s in
        let ty_emit, ty_get, ty_ck =
          try
            filter_event ty_s
          with Unify ->
            non_event_err s
        in
        unify_expr s
          (constr_notabbrev event_ident
             [ty_emit; (constr_notabbrev list_ident [ty_emit]); ty_ck])
          ty_s;
        let gl_env, loc_env = clock_of_pattern [] [] patt ty_emit in
        assert (gl_env = []);
        let new_env =
          List.fold_left
            (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
            env loc_env
        in
        clock_of_expression new_env p

    | Enewclock (id, sch, e) ->
      let sch_type = arrow Clocks_utils.static (product [Clocks_utils.static; Clocks_utils.static]) in
      Misc.opt_iter (fun sch -> type_expect env sch sch_type) sch;
      push_type_level ();
      let new_ck = depend (carrier_skolem id.Ident.name Clocks_utils.names#name) in
      let env = Env.add id (forall [] [] new_ck) env in
      pop_type_level ();
      clock_of_expression env e

    | Epauseclock ce ->
        let ty_ce = clock_of_expression env ce in
        let _ =
          try
            filter_depend ty_ce
          with
            | Unify -> non_clock_err ce
        in
        Clocks_utils.static

    | Etopck -> clock_topck

  in
  expr.e_clock <- t;
  Stypes.record (Ti_expr expr);
  t


(* Typing of event configurations *)
and type_of_event_config env conf =
  match conf.conf_desc with
  | Cpresent s ->
      let ty = clock_of_expression env s in
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

and type_clock_expr env ce =
  match ce with
    | CkExpr e ->
        let ty_ce = clock_of_expression env e in
        let _ =
          (try
              filter_depend ty_ce
            with
              | Unify -> non_clock_err e)
        in
        ty_ce
    | CkTop -> clock_topck
    | CkLocal -> assert false


(* Typing of let declatations *)
and type_let is_rec env patt_expr_list =
  push_type_level();
  let ty_list = List.map (fun _ -> new_clock_var()) patt_expr_list in
  let global_env, local_env =
    clock_of_pattern_list [] [] (List.map fst patt_expr_list) ty_list
  in
  let add_env =
    List.fold_left
      (fun env (x, ty) -> Env.add x (forall [] [] ty) env)
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
        gl.ck_info <- Some { value_ck = gen (Global.ck_info gl).value_ck.cs_desc })
      global_env
  in
  let gen_env = Env.map (fun ty -> gen ty.cs_desc) add_env in
  global_env, Env.append gen_env env


(* Typing of an expression with an expected type *)
and type_expect env expr expected_ty =
  let actual_ty = clock_of_expression env expr in
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
  let new_type_var (v, k) =
    if k = Ttype_var then
      v, new_generic_clock_var ()
    else
      v, new_generic_carrier_clock_var ()
  in
  let typ_vars = List.map new_type_var typ_params in
  let final_typ =
    constr_notabbrev type_gl.gi (List.map snd typ_vars)
  in
  let type_desc, abbr =
    match type_decl with
    | Tabstract -> Clock_abstract, Constr_notabbrev

    | Tvariant constr_decl_list ->
        check_no_repeated_constructor loc constr_decl_list;
        let cstr_list =
          List.rev_map
            (fun (gl_cstr,te_opt) ->
              let ty_arg_opt =
                opt_map (clock_of_type_expression typ_vars) te_opt
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
              let ty_res = clock_of_type_expression typ_vars te in
              gl_lbl.ck_info <- Some { lbl_res = ty_res;
                                    lbl_arg = final_typ;
                                    lbl_mut = mut; };
              Clocks_utils.add_type_description gl_lbl)
            label_decl_list
        in
        Clock_record lbl_list, Constr_notabbrev

    | Trebind (te) ->
        let ty_te = clock_of_type_expression typ_vars te in
        Clock_rebind (ty_te),
        Constr_abbrev (List.map snd typ_vars, ty_te)

  in
  type_gl.ck_info <-
    Some { clock_constr = {gi = type_gl.gi;
                           ty_info = None;
                           ck_info = Some {constr_abbr = abbr}};
           clock_kind = type_desc;
           clock_arity = List.length typ_vars };
  type_gl


(* Check that an implementation without interface does not export values
   with non-generalizable types. *)
let check_nongen_values impl_item_list =
  List.iter
    (fun impl_item ->
      match impl_item.impl_desc with
      | Ilet (_, patt_expr_list) ->
          List.iter (fun (patt,expr) ->
            if free_clock_vars notgeneric expr.e_clock != []
            then
              cannot_generalize_err expr)
            patt_expr_list
      | _ -> ())
    impl_item_list

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
        (fun ((s,te_opt), combine_opt) ->
          let ty_emit = new_clock_var() in
          let ty_get = new_clock_var() in
          let ty_ck = clock_topck in
          let ty_s = constr_notabbrev event_ident [ty_emit; ty_get; ty_ck] in
          opt_iter
            (fun te ->
              unify_event s.gi.id
                (instance (full_clock_of_type_expression te)) ty_s)
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
          s.ck_info <- Some { value_ck = forall [] [] ty_s };
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
        Some {cstr_arg = opt_map (clock_of_type_expression []) te_opt;
              cstr_res = Clocks_utils.static; };
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
        Some { value_ck = gen (full_clock_of_type_expression te).cs_desc };
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
        Some {cstr_arg = opt_map (clock_of_type_expression []) te_opt;
              cstr_res = Clocks_utils.static; };
      (* verbose mode *)
      if !print_type
      then Clocks_printer.output_exception_declaration info_chan gl_cstr

  | Dopen _ -> ()
  );
  item
