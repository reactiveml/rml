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

(* file: depend.ml *)

(* Warning: *)
(* This file is based on the original version of depend.ml *)
(* from the Objective Caml 3.10 distribution, INRIA        *)

(* first modification: 2007-02-16 *)
(* modified by: Louis Mandel      *)


(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Location
open Parse_ident
open Parse_ast

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let rec addmodule bv lid =
  match lid with
    Pdot (s,_) ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names
  | Pident _ -> ()

let add bv id = addmodule bv id.pident_id

let add_opt add_fn bv = function
  | None -> ()
  | Some x -> add_fn bv x

let rec add_type bv ty =
  match ty.pte_desc with
  | Ptype_var _ -> ()
  | Ptype_arrow (t1, t2) -> add_type bv t1; add_type bv t2
  | Ptype_tuple tl -> List.iter (add_type bv) tl
  | Ptype_constr (id, tl) -> add bv id; List.iter (add_type bv) tl
  | Ptype_process (t, _) ->  add_type bv t

let add_type_declaration bv td =
  match td with
  | Ptype_abstract -> ()
  | Ptype_rebind te -> add_type bv te
  | Ptype_variant cstrs ->
      List.iter (fun (c, args) -> add_opt add_type bv args) cstrs
  | Ptype_record lbls ->
      List.iter (fun (l, mut, ty) -> add_type bv ty) lbls

let rec add_pattern bv pat =
  match pat.ppatt_desc with
    Ppatt_any -> ()
  | Ppatt_var _ -> ()
  | Ppatt_alias(p, _) -> add_pattern bv p
  | Ppatt_constant _ -> ()
  | Ppatt_tuple pl -> List.iter (add_pattern bv) pl
  | Ppatt_construct(c, op) -> add bv c; add_opt add_pattern bv op
  | Ppatt_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppatt_record pl ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppatt_array pl -> List.iter (add_pattern bv) pl
  | Ppatt_constraint(p, ty) -> add_pattern bv p; add_type bv ty

let rec add_expr bv exp =
  match exp.pexpr_desc with
    Pexpr_ident l -> add bv l
  | Pexpr_constant _ -> ()
  | Pexpr_let(_, pel, e) -> add_pat_expr_list bv pel; add_expr bv e
  | Pexpr_function pel -> add_pat_when_opt_expr_list bv pel
  | Pexpr_apply(e, el) ->
      add_expr bv e; List.iter (fun e -> add_expr bv e) el
  | Pexpr_tuple el -> List.iter (add_expr bv) el
  | Pexpr_construct(c, opte) -> add bv c; add_opt add_expr bv opte
  | Pexpr_array el -> List.iter (add_expr bv) el
  | Pexpr_record lblel ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel
  | Pexpr_record_access(e, fld) -> add_expr bv e; add bv fld
  | Pexpr_record_with (e, lblel) ->
      add_expr bv e;
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel
  | Pexpr_record_update(e1, fld, e2) ->
      add_expr bv e1; add bv fld; add_expr bv e2
  | Pexpr_constraint(e1, ty) ->
      add_expr bv e1;
      add_type bv ty
  | Pexpr_trywith(e, pel) -> add_expr bv e; add_pat_when_opt_expr_list bv pel
  | Pexpr_assert (e) -> add_expr bv e
  | Pexpr_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexpr_match(e, pel) -> add_expr bv e; add_pat_when_opt_expr_list bv pel
  | Pexpr_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexpr_for(_, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexpr_fordopar(_, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexpr_seq(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexpr_nothing -> ()
  | Pexpr_pause -> ()
  | Pexpr_halt -> ()
  | Pexpr_emit(e1) -> add_expr bv e1
  | Pexpr_emit_val(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexpr_loop(e1) -> add_expr bv e1
  | Pexpr_par(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexpr_merge(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexpr_signal(ioel, koee, e) ->
      List.iter (fun (i, oe) -> add_opt add_type bv oe) ioel;
      Misc.opt_iter (fun (_, e1, e2) -> add_expr bv e1; add_expr bv e2) koee;
      add_expr bv e
  | Pexpr_process(e1) -> add_expr bv e1
  | Pexpr_run(e1) -> add_expr bv e1
  | Pexpr_until(e1, cfg_when_opt_oe_list) ->
      add_expr bv e1;
      List.iter
        (fun (cfg, when_opt, oe) ->
          add_config bv cfg;
          Misc.opt_iter (add_expr bv) when_opt;
          Misc.opt_iter (fun e -> add_expr bv e) oe)
        cfg_when_opt_oe_list
  | Pexpr_when(cfg, e1) -> add_config bv cfg; add_expr bv e1
  | Pexpr_control(cfg, oe, e1) ->
      add_config bv cfg;
      Misc.opt_iter (fun e -> add_expr bv e) oe;
      add_expr bv e1
  | Pexpr_get(e1) -> add_expr bv e1
  | Pexpr_present(cfg, e1, e2) ->
      add_config bv cfg; add_expr bv e1; add_expr bv e2
  | Pexpr_await(_, cfg) -> add_config bv cfg
  | Pexpr_await_val(_, _, cfg, when_opt, e1) ->
      add_config bv cfg;
      add_config bv cfg;
      Misc.opt_iter (add_expr bv) when_opt;
      add_expr bv e1
  | Pexpr_pre(_, e1) -> add_expr bv e1
  | Pexpr_last(e1) -> add_expr bv e1
  | Pexpr_default(e1) -> add_expr bv e1

and add_config bv conf =
  match conf.pconf_desc with
  | Pconf_present(e1, op) ->
      add_expr bv e1; Misc.opt_iter (fun p -> add_pattern bv p) op
  | Pconf_and(e1, e2) -> add_config bv e1; add_config bv e2
  | Pconf_or(e1, e2) -> add_config bv e1; add_config bv e2

and add_pat_expr_list bv pel =
  List.iter (fun (p, e) -> add_pattern bv p; add_expr bv e) pel

and add_pat_when_opt_expr_list bv pel =
  List.iter
    (fun (p, when_opt, e) ->
      add_pattern bv p;
      Misc.opt_iter (add_expr bv) when_opt;
      add_expr bv e)
    pel

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.pintf_desc with
    Pintf_val(id, vd) ->
      add_type bv vd; bv
  | Pintf_type dcls ->
      List.iter (fun (_, _, td) -> add_type_declaration bv td) dcls; bv
  | Pintf_exn(id, oty) ->
      add_opt add_type bv oty; bv
  | Pintf_open s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names;
      bv

and add_structure bv item_list =
  List.fold_left add_struct_item bv item_list

and add_struct_item bv item =
  match item.pimpl_desc with
    Pimpl_expr e ->
      add_expr bv e; bv
  | Pimpl_let(_, pel) ->
      add_pat_expr_list bv pel; bv
  | Pimpl_signal(ioel, koee) ->
      List.iter (fun (i, oe) -> add_opt add_type bv oe) ioel;
      Misc.opt_iter (fun (_,e1, e2) -> add_expr bv e1; add_expr bv e2) koee;
      bv
  | Pimpl_type dcls ->
      List.iter (fun (_, _, td) -> add_type_declaration bv td) dcls; bv
  | Pimpl_exn(id, oty) ->
      add_opt add_type bv oty; bv
  | Pimpl_exn_rebind(id, l) ->
      add bv l; bv
  | Pimpl_open s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names;
      bv

  | Pimpl_lucky (_, itl1, itl2, _) ->
      List.iter (fun (_, t) -> add_type bv t) itl1;
      List.iter (fun (_, t) -> add_type bv t) itl2;
      bv

and add_use_file bv top_phrs =
  ignore (List.fold_left add_struct_item bv top_phrs)

