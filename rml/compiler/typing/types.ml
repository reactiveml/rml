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

(* file: types.ml *)

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* created: 2004-05-13  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Basic operations over types *)

open Misc
open Def_types
open Global

exception Unify

let reset_type_var () =
  Def_types.current_level := 0; ()
and push_type_level () =
  incr Def_types.current_level; ()
and pop_type_level () =
  decr Def_types.current_level; ()
;;


(* To take the canonical representative of a type.
   We do path compression there. *)

let rec type_repr ty =
  match ty.type_desc with
  | Type_link t ->
      let t = type_repr t in
      ty.type_desc <- Type_link t;
      t
  | _ ->
      ty


(* To generalize a type *)

(* generalisation and non generalisation of a type. *)
(* the level of generalised type variables *)
(* is set to [generic] when the flag [is_gen] is true *)
(* and set to [!binding_level] when the flag is false *)
(* returns [generic] when a sub-term can be generalised *)

let list_of_typ_vars = ref []

let rec gen_ty is_gen ty =
  let ty = type_repr ty in
  begin match ty.type_desc with
    Type_var ->
      if ty.type_level > !Def_types.current_level
      then if is_gen
      then (ty.type_level <- generic;
	    list_of_typ_vars := ty :: !list_of_typ_vars)
      else ty.type_level <- !Def_types.current_level
  | Type_arrow(ty1, ty2) ->
      let level1 = gen_ty is_gen ty1 in
      let level2 = gen_ty is_gen ty2 in
      ty.type_level <- min level1 level2
  | Type_product(ty_list) ->
      ty.type_level <-
	List.fold_left (fun level ty -> min level (gen_ty is_gen ty))
	  notgeneric ty_list
  | Type_constr(name, ty_list) ->
      ty.type_level <-
	List.fold_left
	  (fun level ty -> min level (gen_ty is_gen ty))
	  notgeneric ty_list
  | Type_link(link) ->
      ty.type_level <- gen_ty is_gen link
  | Type_process(ty_body, _) ->
      ty.type_level <- min generic (gen_ty is_gen ty_body)
  end;
  ty.type_level

(* main generalisation function *)
let gen ty =
  list_of_typ_vars := [];
  let _ = gen_ty true ty in
  { ts_binders = !list_of_typ_vars;
    ts_desc = ty }
let non_gen ty = ignore (gen_ty false ty)

(* To compute the free type variables in a type *)
let free_type_vars level ty =
  let fv = ref [] in
  let rec free_vars ty =
    let ty = type_repr ty in
    match ty.type_desc with
      Type_var ->
        if ty.type_level >= level then fv := ty :: !fv
    | Type_arrow(t1,t2) ->
	free_vars t1; free_vars t2
    | Type_product(ty_list) ->
	List.iter free_vars ty_list
    | Type_constr(c, ty_list) ->
	List.iter free_vars ty_list
    | Type_link(link) ->
	free_vars link
    | Type_process(ty, _) -> free_vars ty
  in
  free_vars ty;
  !fv

let s = ref []
let save v = s := v :: !s
let cleanup () =
  List.iter (fun ty -> ty.type_desc <- Type_var) !s;
  s := []

(* makes a copy of a type *)
let rec copy ty =
  let level = ty.type_level in
  let effects = ty.type_effects in
  let n = ty.type_neutral in
  match ty.type_desc with
  | Type_var ->
      if level = generic
      then
	let v = new_var () in
	ty.type_desc <- Type_link(v);
	save ty;
	v
      else ty
  | Type_link(link) ->
      if level = generic
      then link
      else copy link
  | Type_arrow(ty1, ty2) ->
      if level = generic
      then
	arrow ~n ~effects (copy ty1) (copy ty2)
      else ty
  | Type_product(ty_list) ->
      if level = generic
      then
	product ~n ~effects (List.map copy ty_list)
      else ty
  | Type_constr(name, ty_list) ->
      if level = generic
      then
	constr ~n ~effects name (List.map copy ty_list)
      else ty
  | Type_process(ty, k) ->
      if level = generic
      then
	process ~n ~effects (copy ty) k
      else
	ty

(* instanciation *)
let instance { ts_desc = ty } =
  let ty_i = copy ty in
  cleanup ();
  ty_i


let constr_instance
    { cstr_arg = ty_opt; cstr_res = ty_res; } =
  let ty_opt = opt_map copy ty_opt in
  let ty_res = copy ty_res in
  cleanup ();
  { cstr_arg = ty_opt; cstr_res = ty_res; }

let label_instance { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut } =
  let ty_arg = copy ty_arg in
  let ty_res = copy ty_res in
  cleanup ();
  { lbl_arg = ty_arg; lbl_res = ty_res; lbl_mut = mut }


(* the occur check *)
let rec occur_check level index ty =
  let rec check ty =
    let ty = type_repr ty in
    match ty.type_desc with
      Type_var ->
	if ty == index
	then raise Unify
	else if ty.type_level > level then ty.type_level <- level
    | Type_arrow(ty1,ty2) -> check ty1; check ty2
    | Type_product(ty_list) -> List.iter check ty_list
    | Type_constr(name, ty_list) ->
 	List.iter check ty_list
    | Type_link(link) -> check link
    | Type_process(ty, _) -> check ty
  in check ty


(* type constructor equality *)
let same_type_constr c1 c2 = Global_ident.same c1.gi c2.gi


(* Expansion of an abbreviation *)
let bind_variable ty1 ty2 =
  match ty1.type_desc with
    Type_var -> ty1.type_desc <- Type_link ty2
  | _ -> fatal_error "bind_variable"

let expand_abbrev params body args =
  let params' = List.map copy params
  and body' = copy body in
  cleanup();
  List.iter2 bind_variable params' args;
  body'

let merge_effects effs =
  try
    Effects.flatten effs
  with Usages.Forbidden_signal_usage (loc1, loc2) ->
    Typing_errors.usage_wrong_type_err loc1 loc2

(* unification *)
let rec unify expected_ty actual_ty =
  if expected_ty == actual_ty then ()
  else
    let expected_ty = type_repr expected_ty in
    let actual_ty = type_repr actual_ty in
    if expected_ty == actual_ty then ()
    else if Initialization.is_usage expected_ty && Initialization.is_usage actual_ty then
      try
        Usages_misc.unify expected_ty actual_ty
      with Usages_misc.Unify _ ->
        raise Unify
    else
      let new_effects = merge_effects [expected_ty.type_effects; actual_ty.type_effects] in
      match expected_ty.type_desc, actual_ty.type_desc with
	Type_var, _ ->
	  occur_check expected_ty.type_level expected_ty actual_ty;
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  expected_ty.type_desc <- Type_link(actual_ty)
      | _, Type_var ->
	  occur_check actual_ty.type_level actual_ty expected_ty;
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  actual_ty.type_desc <- Type_link(expected_ty)
      | Type_product(l1), Type_product(l2) ->
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  begin try
	    List.iter2 unify l1 l2
	  with
	  | Invalid_argument _ -> raise Unify
	  end
      | Type_arrow(ty1, ty2), Type_arrow(ty3, ty4) ->
          let n = expected_ty.type_neutral || actual_ty.type_neutral in
          expected_ty.type_neutral <- n;
          actual_ty.type_neutral <- n;
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  unify ty1 ty3;
	  unify ty2 ty4
      |	Type_constr(c1, ty_l1),
	  Type_constr(c2, ty_l2) when same_type_constr c1 c2 ->
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  begin try
	    List.iter2 unify ty_l1 ty_l2
	  with
	  | Invalid_argument _ -> raise Unify
	  end;
      | Type_constr
	  ({ info = Some { constr_abbr=Constr_abbrev(params,body) } }, args),
	_ ->
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  unify (expand_abbrev params body args) actual_ty
      | _,
	Type_constr
	  ({ info = Some { constr_abbr=Constr_abbrev(params,body) } },args) ->
	    actual_ty.type_effects <- new_effects;
	    expected_ty.type_effects <- new_effects;
	    unify expected_ty (expand_abbrev params body args)
      | Type_process(ty1, pi1), Type_process(ty2, pi2) ->
	  begin match pi1.proc_static, pi2.proc_static with
	  | None, None -> ()
	  | Some ps, None -> pi2.proc_static <- Some (Proc_link ps)
	  | None, Some ps -> pi1.proc_static <- Some (Proc_link ps)
	  | Some ps1, Some ps2 ->
	      pi1.proc_static <- Some (Proc_unify (ps1, ps2))
	  end;
          actual_ty.type_effects <- new_effects;
          expected_ty.type_effects <- new_effects;
	  unify ty1 ty2
      | _ -> raise Unify


(* special cases of unification *)
let rec filter_arrow ty =
  let ty = type_repr ty in
  match ty.type_desc with
    Type_arrow(ty1, ty2) -> ty1, ty2
  | Type_constr({info=Some{constr_abbr=Constr_abbrev(params,body)}},args) ->
      filter_arrow (expand_abbrev params body args)
  | _ ->
      let ty1 = new_var () in
      let ty2 = new_var () in
      unify ty (arrow ty1 ty2);
      ty1, ty2

let rec filter_product arity ty =
  let ty = type_repr ty in
  match ty.type_desc with
    Type_product(l) ->
      if List.length l = arity then l else raise Unify
  | Type_constr({info=Some{constr_abbr=Constr_abbrev(params,body)}},args) ->
      filter_product arity (expand_abbrev params body args)
  | _ ->
      let ty_list = new_var_list arity in
      unify ty (product ty_list);
      ty_list

