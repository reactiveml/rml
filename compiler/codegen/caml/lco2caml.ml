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

let unit_value = make_expr (Cexpr_constant Const_unit) Location.none

(* Translation of type expressions *)
let rec translate_te typ =
  let ctyp =
    match typ.cote_desc with
    | Cotype_var (_, Tcarrier_var) -> assert false
    | Cotype_var (x, Ttype_var) -> Ctype_var x
    | Cotype_arrow (t1, t2) ->
        Ctype_arrow (translate_te t1, translate_te t2)
    | Cotype_product typ_list ->
        Ctype_product (List.map translate_te typ_list)
    | Cotype_constr (cstr, te_list) ->
        let is_type_var te = match te.cote_desc with
          | Cotype_var (_, Ttype_var) -> true
          | _ -> false
        in
        let te_list = List.filter is_type_var te_list in
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
        Cexpr_function (List.map
                          (fun (p,e) -> (translate_pattern p, translate_ml e))
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

    | Coexpr_record_update (e1, label, e2) ->
        Cexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Coexpr_constraint (expr, typ) ->
        Cexpr_constraint (translate_ml expr, translate_te typ)

    | Coexpr_trywith (expr, l) ->
        Cexpr_trywith (translate_ml expr,
                       List.map
                         (fun (p,e) -> translate_pattern p, translate_ml e)
                         l)

    | Coexpr_assert expr -> Cexpr_assert (translate_ml expr)

    | Coexpr_ifthenelse (e1, e2, e3) ->
        Cexpr_ifthenelse (translate_ml e1,
                          translate_ml e2,
                          translate_ml e3)

    | Coexpr_match (expr, l) ->
        Cexpr_match (translate_ml expr,
                     List.map
                       (fun (p,e) -> translate_pattern p, translate_ml e)
                       l)

    | Coexpr_when_match (e1, e2) ->
        Cexpr_when_match (translate_ml e1, translate_ml e2)

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
             (Cexpr_function [make_patt_unit(), translate_proc p])
             e.coexpr_loc,
           make_rml_type "process" [make_te Ctype_any Location.none])


    | Coexpr_pre(flag, s) ->
        let kind =
          match flag with
          | Status -> "status"
          | Value -> "value"
        in
        Cexpr_apply
          (make_instruction ("rml_pre_"^kind), [translate_ml s])

    | Coexpr_last (s) ->
        Cexpr_apply
          (make_instruction "rml_last",
           [translate_ml s])

    | Coexpr_default (s) ->
        Cexpr_apply
          (make_instruction "rml_default", [translate_ml s])

    | Coexpr_emit (s) ->
        Cexpr_apply
          (make_instruction "rml_expr_emit", [translate_ml s])

    | Coexpr_emit_val (s, e) ->
        Cexpr_apply
          (make_instruction "rml_expr_emit_val",
           [translate_ml s;
            translate_ml e])

    | Coexpr_signal (s, ck, r, None, e) ->
        Cexpr_let (Nonrecursive,
                   [pattern_of_signal s,
                    make_expr
                      (Cexpr_apply
                         (make_instruction "rml_global_signal",
                          [translate_clock_expr ck; translate_clock_expr r]))
                      Location.none],
                   translate_ml e)

    | Coexpr_signal (s, ck, r, Some(e1,e2), e) ->
        Cexpr_let (Nonrecursive,
                   [pattern_of_signal s,
                    make_expr
                      (Cexpr_apply
                         (make_instruction "rml_global_signal_combine",
                          [translate_clock_expr ck;
                           translate_clock_expr r;
                           translate_ml e1;
                           translate_ml e2;]))
                      Location.none],
                   translate_ml e)

    | Coexpr_topck ->
        (make_instruction "rml_top_clock").cexpr_desc
  in
  make_expr cexpr e.coexpr_loc

(* Embedding of ML expressions in a process *)
and embed_ml e =
  make_expr (Cexpr_fun ([make_patt_unit ()], translate_ml e)) e.coexpr_loc

(* Translation of process *)
and translate_proc e =
  let cexpr =
    match e.coproc_desc with
    | Coproc_nothing -> (make_instruction "rml_nothing").cexpr_desc

    | Coproc_pause (K_not_boi, CkLocal) ->
      (make_instruction "rml_pause").cexpr_desc

    | Coproc_pause (K_not_boi, CkTop) ->
        Cexpr_apply (make_instruction "rml_pause_at'", [make_instruction "rml_top_clock"])

    | Coproc_pause (K_not_boi, CkExpr e) ->
      if Lco_misc.is_value e then
        Cexpr_apply (make_instruction "rml_pause_at'", [translate_ml e])
      else
        Cexpr_apply (make_instruction "rml_pause_at", [embed_ml e])

    | Coproc_pause (K_boi, _) ->
      (make_instruction "rml_pause_kboi").cexpr_desc

    | Coproc_halt K_not_boi ->
        (make_instruction "rml_halt").cexpr_desc

    | Coproc_halt K_boi ->
        (make_instruction "rml_halt_kboi").cexpr_desc

    | Coproc_compute (expr) ->
        Cexpr_apply
          (make_instruction "rml_compute", [embed_ml expr])

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

    | Coproc_signal (s, ck, r, None, k) ->
        Cexpr_apply
          (make_instruction "rml_signal",
           [translate_clock_expr ck;
            translate_clock_expr r;
            make_expr
              (Cexpr_function [pattern_of_signal s, translate_proc k])
              Location.none])

    | Coproc_signal (s, ck, r, Some(e1,e2), k) ->
        Cexpr_apply
          (make_instruction "rml_signal_combine",
           [translate_clock_expr ck;
            translate_clock_expr r;
            embed_ml e1;
            embed_ml e2;
            make_expr
              (Cexpr_function [pattern_of_signal s, translate_proc k])
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
                (Cexpr_function [translate_pattern patt, translate_proc k])
                Location.none])

    | Coproc_def_dyn ((patt, k1), k2) ->
        Cexpr_apply
          (make_instruction "rml_def_dyn",
           [translate_proc k1;
            make_expr
              (Cexpr_function [translate_pattern patt, translate_proc k2])
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
                 [(patt_array, translate_proc k);
                  (make_patt (Cpatt_any) Location.none, make_raise_RML())])
              Location.none])

    | Coproc_run (expr) ->
        Cexpr_apply
          (make_instruction "rml_run",
           [embed_ml expr;])

    | Coproc_until ({coconf_desc = Coconf_present s}, k, None) ->
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
    | Coproc_until (s, k, None) ->
        Cexpr_apply
          (make_instruction "rml_until_conf",
           [translate_conf s;
            translate_proc k])

(************)
    | Coproc_until ({coconf_desc = Coconf_present s}, k,
                    Some(patt, {coproc_desc = Coproc_when_match(e1,kh)})) ->
        let cpatt = translate_pattern patt in
        Cexpr_apply
          (make_instruction
             (if Lco_misc.is_value s then "rml_until_handler_match'"
             else "rml_until_handler_match"),
           if Caml_misc.partial_match cpatt then
             [if Lco_misc.is_value s then translate_ml s else embed_ml s;
              make_expr
                (Cexpr_function
                   [cpatt, translate_ml e1;
                    make_patt Cpatt_any Location.none,
                    make_expr
                      (Cexpr_constant (Const_bool false)) Location.none ])
              Location.none;
              translate_proc k;
              make_expr
                (Cexpr_function
                   [(cpatt, translate_proc kh);
                    (make_patt Cpatt_any Location.none, make_raise_RML())])
                Location.none;]
           else
             [if Lco_misc.is_value s then translate_ml s else embed_ml s;
              make_expr
                (Cexpr_function [cpatt, translate_ml e1])
                Location.none;
              translate_proc k;
              make_expr
                (Cexpr_function [(cpatt, translate_proc kh);]) Location.none;])
    | Coproc_until
        ({coconf_desc = Coconf_present s}, k,
         Some(patt, ({coproc_desc = Coproc_compute
                       { coexpr_desc = Coexpr_when_match(e1,e2); }} as kh))) ->
        let cpatt = translate_pattern patt in
        Cexpr_apply
          (make_instruction
             (if Lco_misc.is_value s then "rml_until_handler_match'"
             else "rml_until_handler_match"),
           if Caml_misc.partial_match cpatt then
           [if Lco_misc.is_value s then translate_ml s else embed_ml s;
            make_expr
              (Cexpr_function
                 [cpatt, translate_ml e1;
                  make_patt Cpatt_any Location.none,
                  make_expr
                    (Cexpr_constant (Const_bool false)) Location.none;])
              Location.none;
            translate_proc k;
            make_expr
              (Cexpr_function
                 [(cpatt,
                   translate_proc { kh with coproc_desc = Coproc_compute (e2)});
                  (make_patt Cpatt_any Location.none, make_raise_RML())])
              Location.none;]
         else
           [if Lco_misc.is_value s then translate_ml s else embed_ml s;
            make_expr (Cexpr_function [cpatt, translate_ml e1;]) Location.none;
            translate_proc k;
            make_expr
              (Cexpr_function
                 [(cpatt,
                   translate_proc { kh with coproc_desc = Coproc_compute (e2)});
                  ])
              Location.none;])
(************)
    | Coproc_until ({coconf_desc = Coconf_present s}, k, Some(patt, kh)) ->
        let cpatt = translate_pattern patt in
        if Caml_misc.partial_match cpatt then
          Cexpr_apply
            (make_instruction
               (if Lco_misc.is_value s then "rml_until_handler_match'"
               else "rml_until_handler_match"),
             [if Lco_misc.is_value s then translate_ml s else embed_ml s;
              make_expr
                (Cexpr_function
                   [cpatt,
                    make_expr
                      (Cexpr_constant (Const_bool true)) Location.none;
                    make_patt Cpatt_any Location.none,
                    make_expr
                      (Cexpr_constant (Const_bool false)) Location.none;])
                Location.none;
              translate_proc k;
              make_expr
                (Cexpr_function
                   [(cpatt, translate_proc kh);
                    (make_patt Cpatt_any Location.none, make_raise_RML())])
                Location.none;])
        else
          Cexpr_apply
            (make_instruction
               (if Lco_misc.is_value s then "rml_until_handler'"
               else "rml_until_handler"),
             [if Lco_misc.is_value s then translate_ml s else embed_ml s;
              translate_proc k;
              make_expr
                (Cexpr_function [cpatt, translate_proc kh])
                Location.none;])

    | Coproc_until _ ->
        raise
          (Internal (e.coproc_loc, "Lco2caml.translate_proc: Coproc_until"))

    | Coproc_when ({coconf_desc = Coconf_present s}, k) ->
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

    | Coproc_when (s, k) ->
        Cexpr_apply
          (make_instruction "rml_when_conf",
           [translate_conf s;
            translate_proc k])

    | Coproc_control ({coconf_desc = Coconf_present s}, None, k) ->
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
    | Coproc_control ({coconf_desc = Coconf_present s}, Some(p,e), k) ->
        let cpatt = translate_pattern p in
        let matching =
          if Caml_misc.partial_match cpatt then
            make_expr
              (Cexpr_function
                 [cpatt, translate_ml e;
                  make_patt Cpatt_any Location.none,
                  make_expr
                    (Cexpr_constant (Const_bool false)) Location.none;])
              Location.none
          else
            make_expr
              (Cexpr_function [cpatt, translate_ml e])
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

    | Coproc_control (s, None, k) ->
        Cexpr_apply
          (make_instruction "rml_control_conf",
           [translate_conf s;
            translate_proc k])

    | Coproc_control (s, Some _, k) ->
        not_yet_implemented "Lco2caml.translate_proc Coproc_control"

    | Coproc_get (s, patt, k) ->
        Cexpr_apply
          (make_instruction "rml_get",
           [embed_ml s;
            make_expr
              (Cexpr_function [translate_pattern patt, translate_proc k])
              Location.none])

    | Coproc_present ({coconf_desc = Coconf_present s}, k1, k2) ->
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
    | Coproc_present (s, k1, k2) ->
        Cexpr_apply
          (make_instruction "rml_present_conf",
           [translate_conf s;
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
                    (fun (patt,k) -> translate_pattern patt, translate_proc k)
                    patt_proc_list))
              Location.none])

    | Coproc_when_match (expr,proc) ->
        Cexpr_when_match (translate_ml expr, translate_proc proc)

    | Coproc_await (Nonimmediate, {coconf_desc = Coconf_present s}) ->
        if Lco_misc.is_value s then
          Cexpr_apply
            (make_instruction "rml_await'",
             [translate_ml s])
        else
          Cexpr_apply
            (make_instruction "rml_await",
             [embed_ml s])
    | Coproc_await (Nonimmediate, s) ->
        Cexpr_apply
          (make_instruction "rml_await_conf",
           [translate_conf s])
    | Coproc_await (Immediate, {coconf_desc = Coconf_present s}) ->
        if Lco_misc.is_value s then
          Cexpr_apply
            (make_instruction "rml_await_immediate'",
             [translate_ml s])
        else
          Cexpr_apply
            (make_instruction "rml_await_immediate",
             [embed_ml s])
    | Coproc_await (Immediate, s) ->
        Cexpr_apply
          (make_instruction "rml_await_immediate_conf",
           [translate_conf s])

    | Coproc_await_val (flag1, flag2, s, patt, k) ->
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
        let cpatt = translate_pattern patt in
        begin match Caml_misc.partial_match cpatt, k.coproc_desc with
        | partial_match, Coproc_when_match (e1, k) ->
            if flag2 = One then not_yet_implemented "await_one_match";
            let matching =
              make_expr
                (Cexpr_function
                   [cpatt,
                    make_expr
                      (Cexpr_when_match(translate_ml e1,
                                        make_expr
                                          (Cexpr_constant (Const_bool true))
                                          Location.none))
                      Location.none;
                    make_patt Cpatt_any Location.none,
                    make_expr
                      (Cexpr_constant (Const_bool false)) Location.none;])
                Location.none
            in
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^"_match"),
               [embed_ml s;
                matching;
                make_expr
                  (Cexpr_function
                     ((cpatt, translate_proc k)::
                      if partial_match then
                        [(make_patt Cpatt_any Location.none, make_raise_RML())]
                      else
                        []))
                  Location.none])
        | partial_match,
            Coproc_compute { coexpr_desc = Coexpr_when_match (e1, e2); }
          ->
            if flag2 = One then not_yet_implemented "await_one_match";
            let matching =
              make_expr
                (Cexpr_function
                   [cpatt,
                    make_expr
                      (Cexpr_when_match(translate_ml e1,
                                        make_expr
                                          (Cexpr_constant (Const_bool true))
                                          Location.none))
                      Location.none;
                    make_patt Cpatt_any Location.none,
                    make_expr
                      (Cexpr_constant (Const_bool false)) Location.none;])
                Location.none
            in
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^"_match"),
               [embed_ml s;
                matching;
                make_expr
                  (Cexpr_function
                     ((cpatt,
                       translate_proc { coproc_desc = Coproc_compute e2;
                                        coproc_loc = e2.coexpr_loc })::
                      if partial_match then
                        [(make_patt Cpatt_any Location.none, make_raise_RML())]
                      else
                        []))
                  Location.none])
        | true, _ ->
            if flag2 = One then not_yet_implemented "await_one_match";
            let matching =
              make_expr
                (Cexpr_function
                   [cpatt,
                    make_expr
                      (Cexpr_constant (Const_bool true)) Location.none;
                    make_patt Cpatt_any Location.none,
                    make_expr
                      (Cexpr_constant (Const_bool false)) Location.none;])
                Location.none
            in
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^"_match"),
               [embed_ml s;
                matching;
                make_expr
                  (Cexpr_function
                     [(cpatt, translate_proc k);
                      (make_patt Cpatt_any Location.none, make_raise_RML())])
                  Location.none])
        | false, _ ->
            if Lco_misc.is_value s then
              Cexpr_apply
                (make_instruction ("rml_await"^im^kind^"'"),
                 [translate_ml s;
                  make_expr
                    (Cexpr_function [cpatt, translate_proc k])
                    Location.none])
            else
              Cexpr_apply
                (make_instruction ("rml_await"^im^kind),
                 [embed_ml s;
                  make_expr
                    (Cexpr_function [cpatt, translate_proc k])
                    Location.none])
        end
(* XXXXXX
        if Caml_misc.partial_match cpatt then
          begin
            if flag2 = One then not_yet_implemented "await_one_match";
            Cexpr_apply
              (make_instruction ("rml_await"^im^kind^"_match"),
               [embed_ml s;
                make_expr
                  (Cexpr_function
                     [cpatt,
                      make_expr
                        (Cexpr_constant (Const_bool true)) Location.none;
                      make_patt Cpatt_any Location.none,
                      make_expr
                        (Cexpr_constant (Const_bool false)) Location.none;])
                  Location.none;
                make_expr
                  (Cexpr_function
                     [(cpatt, translate_proc k);
                      (make_patt Cpatt_any Location.none, make_raise_RML())])
                  Location.none])
          end
        else
          Cexpr_apply
            (make_instruction ("rml_await"^im^kind),
             [embed_ml s;
              make_expr
                (Cexpr_function [cpatt, translate_proc k])
                Location.none])
*)

    | Coproc_newclock (id, sch, p) ->
        let e =
          make_expr
            (Cexpr_function [(make_patt_var_local id, translate_proc p)])
            Location.none
        in
        let sch = match sch with
          | None -> make_constr "Pervasives" "None" None
          | Some e ->
              let e = translate_ml e in
              make_constr "Pervasives" "Some" (Some e)
        in
          Cexpr_apply (make_instruction "rml_newclock", [sch; e])

    | Coproc_pauseclock e ->
      if Lco_misc.is_value e then
        Cexpr_apply (make_instruction "rml_pauseclock'", [translate_ml e])
      else
        Cexpr_apply (make_instruction "rml_pauseclock", [embed_ml e])


  in
  make_expr cexpr e.coproc_loc

and translate_conf c =
  let cexpr =
    match c.coconf_desc with
    | Coconf_present (s) ->
        if Lco_misc.is_value s then
          Cexpr_apply
            (make_instruction "cfg_present'",
             [translate_ml s])
        else
          Cexpr_apply
            (make_instruction "cfg_present",
             [embed_ml s])

    | Coconf_and(c1,c2) ->
        Cexpr_apply
          (make_instruction "cfg_and",
           [translate_conf c1;
            translate_conf c2;])

    | Coconf_or(c1,c2) ->
        Cexpr_apply
          (make_instruction "cfg_or",
           [translate_conf c1;
            translate_conf c2;])
  in
  make_expr cexpr c.coconf_loc

and translate_clock_expr e =
  match e with
    | CkLocal -> make_instruction "rml_local_clock"
    | CkTop -> make_instruction "rml_top_clock"
    | CkExpr e -> translate_ml e

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
                       | ((s,ty_opt), Some(e1,e2)) ->
                           pattern_of_signal_global (s, ty_opt),
                           make_expr
                             (Cexpr_apply
                                (make_instruction "rml_global_signal_combine",
                                 [translate_ml e1;
                                  translate_ml e2;]))
                             Location.none)
                     l)
    | Coimpl_type l ->
        let l =
          List.map
            (fun (name, param, typ) ->
              let param = List.filter (fun (v, k) -> k = Ttype_var) param in
              let param = fst (List.split param) in
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
              let param = List.filter (fun (v, k) -> k = Ttype_var) param in
              let param = fst (List.split param) in
              (name, param, translate_type_decl typ))
            l
        in
        Cintf_type l

    | Cointf_exn (name, typ) ->
        Cintf_exn (name, opt_map translate_te typ)

    | Cointf_open m -> Cintf_open m

  in
  make_intf citem item.cointf_loc
