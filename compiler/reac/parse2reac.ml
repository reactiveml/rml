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
open Reac
open Reac_utils
open Parse_ident
open Clocks
open Clocks_utils
open Types
open Types_utils

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
    | Ptype_var x -> Tvar x
    | Ptype_forall (params, te) ->
        Tforall (List.map translate_pe params, translate_te te)

    | Ptype_arrow (t1, t2, ee) ->
        Tarrow (translate_te t1, translate_te t2, translate_ee ee)

    | Ptype_tuple typ_list ->
        Tproduct (List.map translate_te typ_list)

    | Ptype_constr (cstr, pe_list) ->
        let gcstr = Modules.pfind_type_desc cstr.pident_id in
        Tconstr (gcstr, List.map translate_pe pe_list)

    | Ptype_process (t,k,act,ee) -> Tprocess ((translate_te t),k, translate_ce act, translate_ee ee)

    | Ptype_depend ce -> Tdepend (translate_ce ce)
  in
  make_te rtyp typ.pte_loc

and translate_ce pce =
  let ce =
    match pce.pce_desc with
      | Pcar_var s -> Cvar s
      | Pcar_topck -> Ctopck
      | Pcar_fresh -> assert false
  in
  make_ce ce pce.pce_loc

and translate_ee pee =
  let ee =
    match pee.pee_desc with
      | Peff_empty -> Effempty
      | Peff_var s -> Effvar s
      | Peff_sum (ee1, ee2) -> Effsum (translate_ee ee1, translate_ee ee2)
      | Peff_depend ce -> Effdepend (translate_ce ce)
      | Peff_fresh -> assert false
  in
  make_ee ee pee.pee_loc

and translate_pe ppe = match ppe with
  | Pptype pte -> Ptype (translate_te pte)
  | Ppcarrier pce -> Pcarrier (translate_ce pce)
  | Ppeffect pee -> Peffect (translate_ee pee)


(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Ptype_abstract -> Tabstract

  | Ptype_rebind typ -> Trebind (translate_te typ)

  | Ptype_variant constr_te_list ->
      let l =
        List.map
          (fun (c, typ) ->
            let id = Ident.create Ident.gen_constr c.psimple_id Ident.Constr in
            let g = Modules.defined_global id (no_info()) (no_info()) in
            let _ = Modules.add_constr g in
            let typ =
              match typ with
              | None -> None
              | Some typ -> Some (translate_te typ)
            in
            (g, typ))
          constr_te_list
      in
      Tvariant l

  | Ptype_record l ->
      let l =
        List.map
          (fun (lab, flag, typ) ->
            let id = Ident.create Ident.gen_label lab.psimple_id Ident.Label in
            let g = Modules.defined_global id (no_info()) (no_info()) in
            let _ = Modules.add_label g in
            (g, flag, translate_te typ))
          l
      in
      Trecord l

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
      | Ppatt_any -> [], Pany

      | Ppatt_var x ->
          begin try
            let vp = List.assoc x.psimple_id or_vars in
            [(x.psimple_id, vp)], Pvar vp
          with
          | Not_found ->
              let id =
                Ident.create Ident.gen_var x.psimple_id Ident.Val_RML
              in
              if is_global
              then
                let gl = Modules.defined_global id (no_info()) (no_info()) in
                let vp = Vglobal gl in
                [(x.psimple_id, vp)], Pvar vp
              else
                let vp = Vlocal id in
                [(x.psimple_id, vp)], Pvar vp
          end

      | Ppatt_alias (patt,x) ->
          let vars, rpatt = translate_pattern or_vars is_global patt in
          if List.mem_assoc x.psimple_id vars
          then multiply_bound_variable_err x.psimple_id p.ppatt_loc
          else
            begin try
              let vp = List.assoc x.psimple_id or_vars in
              (x.psimple_id, vp) :: vars, Palias (rpatt, vp)
            with
            | Not_found ->
                let id =
                  Ident.create Ident.gen_var x.psimple_id Ident.Val_RML
                in
                if is_global
                then
                  let gl = Modules.defined_global id (no_info()) (no_info()) in
                  let vp = Vglobal gl in
                  (x.psimple_id, vp) :: vars, Palias (rpatt, vp)
                else
                  let vp = Vlocal id in
                  (x.psimple_id, vp) :: vars, Palias (rpatt, vp)
            end

      | Ppatt_constant im -> [], Pconstant im

      | Ppatt_tuple patt_list ->
          let vars, rpatt_list =
            translate_pattern_list or_vars is_global patt_list
          in
          vars, Ptuple rpatt_list

      | Ppatt_construct (constr, None) ->
          let gconstr = try Modules.pfind_constr_desc constr.pident_id with
          | Modules.Desc_not_found ->
              unbound_constr_err constr.pident_id constr.pident_loc
          in
          [], Pconstruct (gconstr, None)

      | Ppatt_construct (constr, Some patt) ->
          let gconstr = try Modules.pfind_constr_desc constr.pident_id with
          | Modules.Desc_not_found ->
              unbound_constr_err constr.pident_id constr.pident_loc
          in
          let vars, rpatt = translate_pattern or_vars is_global patt in
          vars, Pconstruct (gconstr, Some rpatt)

      | Ppatt_or (patt1, patt2) ->
          let vars1, rpatt1 = translate_pattern or_vars is_global patt1 in
          let vars2, rpatt2 = translate_pattern vars1 is_global patt2 in
          if List.for_all (fun (x,_) -> List.mem_assoc x vars1) vars2 &
            List.for_all (fun (x,_) -> List.mem_assoc x vars2) vars1
          then
            vars1, Por (rpatt1, rpatt2)
          else
            orpat_vars p.ppatt_loc

      | Ppatt_record l ->
          let vars, l = translate_pattern_record or_vars is_global l in
          vars, Precord(l)

      | Ppatt_array patt_list ->
          let vars, rpatt_list =
            translate_pattern_list or_vars is_global patt_list
          in
          vars, Parray rpatt_list

      | Ppatt_constraint (patt,typ) ->
          let vars, rpatt = translate_pattern or_vars is_global patt in
          let rtyp = translate_te typ in
          vars, Pconstraint (rpatt, rtyp)

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
  translate_pattern [], translate_pattern_list [], translate_pattern_record []

(* Translation of identifier *)
let translate_ident env x =
  match x.pident_id with
  | Pdot (mod_name,s) ->
      Eglobal (Modules.pfind_value_desc x.pident_id)

  | Pident s ->
      try
        let id = Env.find s env in
        Elocal id
      with
      | Not_found ->
          Eglobal (Modules.pfind_value_desc x.pident_id)

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

    | Pexpr_constant im -> Econstant im

    | Pexpr_let (_, [patt, {pexpr_desc= Pexpr_get s;}], expr) ->
        let vars, rpatt = translate_pattern false patt in
        let new_env = add_varpatt env vars in
        Eget(translate env s,
                  rpatt, translate new_env expr)

    | Pexpr_let (rec_flag, patt_expr_list, expr) ->
        let env, rpatt_rexpr_list =
          translate_let false env rec_flag patt_expr_list
        in
        Elet (rec_flag, rpatt_rexpr_list, translate env expr)

    | Pexpr_function patt_expr_list ->
        Efunction (translate_match env patt_expr_list)

    | Pexpr_apply (expr, expr_list) ->
        let rexpr = translate env expr in
        let rexpr_list = List.map (translate env) expr_list in
        Eapply (rexpr, rexpr_list)

    | Pexpr_tuple expr_list ->
        let rexpr_list = List.map (translate env) expr_list in
        Etuple rexpr_list

    | Pexpr_construct (constr, expr_opt) ->
        let gconstr = try Modules.pfind_constr_desc constr.pident_id with
        | Modules.Desc_not_found ->
            unbound_constr_err constr.pident_id constr.pident_loc
        in
        Econstruct (gconstr, opt_map (translate env) expr_opt)

    | Pexpr_array expr_list ->
        let rexpr_list = List.map (translate env) expr_list in
        Earray rexpr_list

    | Pexpr_record lab_expr_list ->
        Erecord (translate_record env lab_expr_list)

    | Pexpr_record_access (expr, lab) ->
        let glab = try Modules.pfind_label_desc lab.pident_id with
        | Modules.Desc_not_found ->
            unbound_label_err lab.pident_id lab.pident_loc
        in
        Erecord_access (translate env expr, glab)

    | Pexpr_record_update (e1, lab, e2) ->
        let glab = try Modules.pfind_label_desc lab.pident_id with
        | Modules.Desc_not_found ->
            unbound_label_err lab.pident_id lab.pident_loc
        in
        Erecord_update (translate env e1,
                             glab,
                             translate env e2)

    | Pexpr_constraint (expr,typ) ->
        Econstraint (translate env expr, translate_te typ)

    | Pexpr_trywith (expr, patt_expr_list) ->
        Etrywith (translate env expr,
                       translate_match env patt_expr_list)

    | Pexpr_assert expr -> Eassert (translate env expr)

    | Pexpr_ifthenelse (e1, e2, None) ->
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        Eifthenelse (tr_e1,
                          tr_e2,
                          make_expr (Econstant(Const_unit))
                            Location.none)

    | Pexpr_ifthenelse (e1, e2, Some e3) ->
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        let tr_e3 = translate env e3 in
        Eifthenelse (tr_e1,
                          tr_e2,
                          tr_e3)

    | Pexpr_match (expr, patt_expr_list) ->
        Ematch(translate env expr,
                    translate_match env patt_expr_list)

    | Pexpr_when_match (e1, e2) ->
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        Ewhen_match (tr_e1, tr_e2)

    | Pexpr_while (e1, e2) ->
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        Ewhile(tr_e1, tr_e2)

    | Pexpr_for (i,e1,e2,flag,e3) ->
        let id = Ident.create Ident.gen_var i.psimple_id Ident.Val_ML in
        let env = Env.add i.psimple_id id env in
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        let tr_e3 = translate env e3 in
        Efor (id,
                   tr_e1,
                   tr_e2,
                   flag,
                   tr_e3)

    | Pexpr_seq (e1, e2) ->
        let tr_e1 = translate env e1 in
        let tr_e2 = translate env e2 in
        Eseq [tr_e1;
                   tr_e2]

    | Pexpr_process (expr) ->
        Eprocess (translate env expr)

    | Pexpr_pre (flag, expr) ->
        Epre (flag, translate env expr)

    | Pexpr_last expr ->
        Elast (translate env expr)

    | Pexpr_default expr ->
        Edefault (translate env expr)

    | Pexpr_emit s ->
        Eemit (translate env s, None)

    | Pexpr_emit_val (s,expr) ->
        Eemit (translate env s,
                    Some (translate env expr))

    | Pexpr_signal (sig_typ_list, (ck, r), comb, expr) ->
      let ck = Misc.clock_map (translate env) ck in
      let r = Misc.clock_map (translate env) r in
      let comb = match comb with
        | None -> None
        | Some (e1, e2) -> Some (translate env e1, translate env e2)
      in
        (translate_signal env sig_typ_list ck r comb expr).e_desc

    | Pexpr_fordopar (i, e1, e2, flag, e3) ->
        let id = Ident.create Ident.gen_var i.psimple_id Ident.Val_RML in
        let env = Env.add i.psimple_id id env in
        Efordopar(id,
                       translate env e1,
                       translate env e2,
                       flag,
                       translate env e3)

    | Pexpr_nothing -> Enothing

    | Pexpr_pause ck ->
      let tr_ck = match ck with
        | CkTop -> CkTop
        | CkLocal -> CkLocal
        | CkExpr e -> CkExpr (translate env e) in
      Epause (K_not_boi, tr_ck)

    | Pexpr_halt -> Ehalt K_not_boi

    | Pexpr_loop expr ->
        Eloop (None, translate env expr)

    | Pexpr_par (e1, e2) ->
        Epar [translate env e1;
                   translate env e2]

    | Pexpr_merge (e1, e2) ->
        Emerge(translate env e1,
                    translate env e2)

    | Pexpr_run (expr) ->
        Erun (translate env expr)

    | Pexpr_until (s, expr, patt_expr_opt) ->
        Euntil (translate_conf env s,
                     translate env expr,
                     opt_map
                       (translate_patt_expr env)
                       patt_expr_opt)

    | Pexpr_when (s, expr) ->
        Ewhen (translate_conf env s,
                    translate env expr)

    | Pexpr_control (s, patt_expr_opt, expr) ->
        Econtrol (translate_conf env s,
                       opt_map
                         (translate_patt_expr env)
                         patt_expr_opt,
                       translate env expr)

    | Pexpr_present (s, e1, e2) ->
        Epresent(translate_conf env s,
                      translate env e1,
                      translate env e2)

    | Pexpr_await (flag, s) ->
        Eawait (flag,
                     translate_conf env s)

    | Pexpr_await_val (flag, k, s, patt, expr) ->
        let vars, rpatt = translate_pattern false patt in
        let new_env = add_varpatt env vars in
        Eawait_val (flag,
                         k,
                         translate env s,
                         rpatt,
                         translate new_env expr)

    | Pexpr_newclock (x, sch, e) ->
      let id = Ident.create Ident.gen_var x.psimple_id Ident.Val_ML in
      let env = Env.add x.psimple_id id env in
      let sch = Misc.opt_map (translate env) sch in
      Enewclock (id, sch, translate env e)

    | Pexpr_pauseclock e ->
      Epauseclock (translate env e)

    | Pexpr_topck -> Etopck

    | Pexpr_get _ ->
        raise (Internal (e.pexpr_loc,
                         "Parse2reac.translate: expr"))
    | Pconf_or _ | Pconf_and _ | Pconf_present _ ->
        event_config_err e.pexpr_loc
  in
  make_expr rexpr e.pexpr_loc

(* Translation of event configurations *)
and translate_conf env c =
  let rconf =
    match c.pexpr_desc with
    | Pconf_present e -> Cpresent (translate env e)

    | Pconf_and (conf1, conf2) ->
        Cand (translate_conf env conf1, translate_conf env conf2)

    | Pconf_or (conf1, conf2) ->
        Cor (translate_conf env conf1, translate_conf env conf2)

    | _ -> Cpresent (translate env c)
  in
  make_conf rconf c.pexpr_loc

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
      (fun rpatt expr -> rpatt,         translate tr_env expr)
      rpatt_list
      expr_list
  in
  let new_env =
    if rec_flag <> Recursive
    then add_varpatt env new_vars
    else tr_env
  in
  new_env, rpatt_rexpr_list

(* Translation of a pair of pattern and expression*)
and translate_patt_expr env (patt,expr)=
    let vars, rpatt = translate_pattern false patt in
    let env = add_varpatt env vars in
    let rexpr = translate env expr in
    (rpatt, rexpr)

(* Translation of match in an ML context *)
and translate_match =
  fun env patt_expr_list ->
    List.map (translate_patt_expr env) patt_expr_list

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


and translate_signal env sig_typ_list ck r comb expr =
  match sig_typ_list with
  | [] -> translate env expr
  | (s,typ) :: sig_typ_list ->
      let (id, rtyp) =
        Ident.create Ident.gen_var s.psimple_id Ident.Sig,
        opt_map translate_te typ
      in
      let env = Env.add s.psimple_id id env in
      make_expr
        (Esignal
           ((id, rtyp),
            ck, r, comb,
            translate_signal env sig_typ_list ck r comb expr))
        Location.none

(* Add a varpatt in the environment *)
and add_varpatt env vars =
  List.fold_left
    (fun env (x,vp) ->
      match vp with
      | Vlocal id -> Env.add x id env
      | Vglobal gl -> Modules.add_value gl; env) env vars


(* Translation of type declatations *)
let translate_type_declaration l =
  let l_rename =
    List.map
      (fun (name, param, typ) ->
        let id = Ident.create Ident.gen_type name.psimple_id Ident.Type in
        let gl = Modules.defined_global id (no_info()) (no_info()) in
        let ty_info = { type_constr = { gi = gl.gi;
                                        ty_info = Some {constr_abbr = Constr_notabbrev};
                                        ck_info = None; };
                        type_kind = Type_abstract;
                        type_arity = List.length param; }
        in
        let ck_info = { clock_constr = { gi = gl.gi;
                                         ty_info = None;
                                         ck_info = Some { Clocks.constr_abbr = Clocks.Constr_notabbrev} };
                        clock_kind = Clock_abstract;
                        clock_arity = Clock_vars.arity_of_type_vars param;
                        clock_def_arity = Clock_vars.arity_of_type_vars param }
        in
        let _ =
          gl.ty_info <- Some ty_info;
          gl.ck_info <- Some ck_info;
          Modules.add_type gl
        in
        (gl, param, typ))
      l
  in
  let l_rename = Clock_vars.add_missing_vars l_rename in
  List.map
    (fun (gl, param, typ) -> (gl, param, translate_type_decl typ))
    l_rename

(* Translation of implementation item *)
let translate_impl_item info_chan item =
  let ritem =
    match item.pimpl_desc with
    | Pimpl_expr expr -> Iexpr (translate Env.empty expr)

    | Pimpl_let (flag, patt_expr_list) ->
        let env, rpatt_rexpr_list =
          translate_let true Env.empty flag patt_expr_list
        in
        Ilet (flag, rpatt_rexpr_list)

    | Pimpl_signal (sig_typ_list, comb_opt) ->
        Isignal
          (List.map
             (fun (s,ty_opt) ->
               let id = Ident.create Ident.gen_var s.psimple_id Ident.Sig in
               let gl = Modules.defined_global id (no_info()) (no_info()) in
               let _ = Modules.add_value gl in
               let rty_opt = opt_map translate_te ty_opt in
               let rcomb_opt =
                 opt_map
                   (fun (e1,e2) ->
                     (translate Env.empty e1, translate Env.empty e2))
                   comb_opt
               in
               (gl,rty_opt), rcomb_opt)
             sig_typ_list)

    | Pimpl_type l ->
        let l_translate = translate_type_declaration l in
        Itype l_translate

    | Pimpl_exn (name, typ) ->
        let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
        let gl = Modules.defined_global id (no_info()) (no_info()) in
        let _ = Modules.add_constr gl in
        Iexn (gl, opt_map translate_te typ)

    | Pimpl_exn_rebind (name, gl_name) ->
        let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
        let gl = Modules.defined_global id (no_info()) (no_info()) in
        let _ = Modules.add_constr gl in
        let gtype = try Modules.pfind_constr_desc gl_name.pident_id with
        | Modules.Desc_not_found ->
            unbound_type_err gl_name.pident_id gl_name.pident_loc
        in
        Iexn_rebind(gl, gtype)

    | Pimpl_open s ->
        Modules.open_module s;
        Iopen s

    | Pimpl_lucky _ ->
        raise (Internal (item.pimpl_loc,
                         "Parse2reac.translate_impl_item: must be translated"))

  in
  make_impl ritem item.pimpl_loc

(* Translation of interfacr item *)
let translate_intf_item info_chan item =
  let ritem =
    match item.pintf_desc with
    | Pintf_val (s, t) ->
        let id = Ident.create Ident.gen_var s.psimple_id Ident.Val_ML in
        let gl = Modules.defined_global id (no_info()) (no_info()) in
        let _ = Modules.add_value gl in
        Dval (gl, translate_te t)

    | Pintf_type l ->
        let l_translate = translate_type_declaration l in
        Dtype l_translate

    | Pintf_exn (name, typ) ->
        let id = Ident.create Ident.gen_constr name.psimple_id Ident.Exn in
        let gl = Modules.defined_global id (no_info()) (no_info()) in
        let _ = Modules.add_constr gl in
        Dexn (gl, opt_map translate_te typ)

    | Pintf_open s ->
        Modules.open_module s;
        Dopen s

  in
  make_intf ritem item.pintf_loc
