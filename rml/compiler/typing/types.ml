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
open Reactivity_effects
open Global

exception Unify


(* generating fresh names *)
let names = new Ident.name_generator


(* The current nesting level of lets *)
let current_level = ref 0;;

let reset_type_var () =
  reactivity_current_level := 0;
  current_level := 0; ()
and push_type_level () =
  incr reactivity_current_level;
  incr current_level; ()
and pop_type_level () =
  decr reactivity_current_level;
  decr current_level; ()
;;

(* making types *)
let make_type ty =
  { type_desc = ty;
    type_level = generic;
    type_index = names#name; }

let product ty_list =
  make_type (Type_product(ty_list))

let constr ty_constr ty_list =
  make_type (Type_constr(ty_constr, ty_list))

let constr_notabbrev name ty_list =
  make_type (Type_constr({ gi = name;
			   info = Some {constr_abbr = Constr_notabbrev}; },
			 ty_list))

let arrow ty1 ty2 =
  make_type (Type_arrow(ty1, ty2))

let rec arrow_list ty_l ty_res =
  match ty_l with
    [] -> ty_res
  | [ty] -> arrow ty ty_res
  | ty :: ty_l -> arrow ty (arrow_list ty_l ty_res)

let process ty k =
  make_type (Type_process (ty, k))


let no_type_expression =
  { type_desc = Type_product[];
    type_level = generic;
    type_index = -1; }

(* To get fresh type variables *)

let new_var () =
  { type_desc = Type_var;
    type_level = !current_level;
    type_index = names#name }

let new_generic_var () =
  { type_desc = Type_var;
    type_level = generic;
    type_index = names#name }

let rec new_var_list n =
  match n with
    0 -> []
  | n -> (new_var ()) :: new_var_list (n - 1)

let forall l lr typ =
  { ts_binders = l;
    ts_rbinders = lr;
    ts_desc = typ; }


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



(* To compute the free type variables in a type *)
let free_type_vars level ty =
  let fv = ref [] in
  let frv = ref [] in
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
    | Type_process(ty, proc_info) ->
        free_vars ty;
        frv := List.rev_append (free_react_vars level proc_info.proc_react) !frv
  in
  free_vars ty;
  (!fv, List.rev !frv)

let s = ref []
let save v = s := v :: !s
let cleanup () =
  cleanup_react();
  List.iter (fun ty -> ty.type_desc <- Type_var) !s;
  s := []

(* makes a copy of a type *)
let rec copy ty =
  let level = ty.type_level in
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
	arrow (copy ty1) (copy ty2)
      else ty
  | Type_product(ty_list) ->
      if level = generic
      then
	product (List.map copy ty_list)
      else ty
  | Type_constr(name, ty_list) ->
      if level = generic
      then
	constr name (List.map copy ty_list)
      else ty
  | Type_process(ty, info) ->
      if level = generic
      then
	process (copy ty) (copy_proc_info info)
      else
	ty

and copy_proc_info info =
  { proc_react = copy_react info.proc_react; }


(* instanciation *)
let instance { ts_desc = ty } =
  let ty_i = copy ty in
  cleanup ();
  ty_i


let instance_and_vars { ts_binders = typ_vars;
                        ts_rbinders = react_vars;
                        ts_desc = ty } =
  let ty_i = copy ty in
  let typ_vars = List.map type_repr typ_vars in
  let react_vars = List.map react_effect_repr react_vars in
  cleanup ();
  typ_vars, react_vars, ty_i

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
    | Type_process(ty, info) ->
        check ty;
        ignore (occur_check_react level no_react info.proc_react)
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

(* unification *)
let rec unify expected_ty actual_ty =
  if expected_ty == actual_ty then ()
  else
    let expected_ty = type_repr expected_ty in
    let actual_ty = type_repr actual_ty in
    if expected_ty == actual_ty then ()
    else
      match expected_ty.type_desc, actual_ty.type_desc with
	Type_var, _ ->
	  occur_check expected_ty.type_level expected_ty actual_ty;
	  expected_ty.type_desc <- Type_link(actual_ty)
      | _, Type_var ->
	  occur_check actual_ty.type_level actual_ty expected_ty;
	  actual_ty.type_desc <- Type_link(expected_ty)
      | Type_product(l1), Type_product(l2) ->
	  begin try
	    List.iter2 unify l1 l2
	  with
	  | Invalid_argument _ -> raise Unify
	  end
      | Type_arrow(ty1, ty2), Type_arrow(ty3, ty4) ->
	  unify ty1 ty3;
	  unify ty2 ty4
      |	Type_constr(c1, ty_l1),
	  Type_constr(c2, ty_l2) when same_type_constr c1 c2 ->
	  begin try
	    List.iter2 unify ty_l1 ty_l2
	  with
	  | Invalid_argument _ -> raise Unify
	  end
      | Type_constr
	  ({ info = Some { constr_abbr=Constr_abbrev(params,body) } }, args),
	_ ->
	  unify (expand_abbrev params body args) actual_ty
      | _,
	Type_constr
	  ({ info = Some { constr_abbr=Constr_abbrev(params,body) } },args) ->
	    unify expected_ty (expand_abbrev params body args)
      | Type_process(ty1, pi1), Type_process(ty2, pi2) ->
          begin try
            unify_react_effect pi1.proc_react pi2.proc_react
          with React_Unify -> raise Unify (* ne devrait pas arriver *)
          end;
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

