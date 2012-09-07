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

(* file: def_types.ml *)

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* first modification: 2004-04-23 *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The abstract syntax for the types *)

open Asttypes
open Global

(* types *)
type type_scheme =
    { ts_binders: type_expression list;        (* generalised variables *)
      ts_desc: type_expression;                (* the type *)
    }

and type_expression =
    { mutable type_desc: type_expression_desc;
      mutable type_level: int;
      type_index: int;
      mutable type_neutral: bool;
      mutable type_effects: Effects.t;
    }

and type_expression_desc =
    Type_var
  | Type_arrow of type_expression * type_expression
  | Type_product of type_expression list
  | Type_constr of type_constr global * type_expression list
  | Type_link of type_expression
  | Type_process of type_expression * process_info

and process_info =
    { mutable proc_static: process_static option }

and process_static =
  | Proc_def of Def_static.instantaneous ref
  | Proc_link of process_static
  | Proc_unify of process_static * process_static

(* Type constructors *)
and type_constr =
    { mutable constr_abbr: type_abbrev }      (* Abbreviation or not *)

and type_abbrev =
  | Constr_notabbrev
  | Constr_abbrev of
      type_expression list * type_expression  (* Parameters and body *)

(* Varibable kind *)
and var_kind =
  | Kind_val
  | Kind_sig

(* Value descriptions *)

and value_type_description =
    { value_typ: type_scheme; }


(* Constructor descriptions *)

and constructor_type_description =
    { cstr_arg: type_expression option;
      cstr_res: type_expression; }


(* Record label descriptions *)
(* type t_arg = {e.label: t_res;...} *)
and label_type_description =
    { lbl_res: type_expression;          (* Result type *)
      lbl_arg: type_expression;          (* Argument type *)
      lbl_mut: mutable_flag;             (* Mutable or not *)
    }

(* Type definitions *)

and type_description =
    { type_constr: type_constr global;
      type_kind: type_kind;
      type_arity: int; }

and type_kind =
    Type_abstract
  | Type_rebind of type_expression
  | Type_variant of constructor_type_description global list
  | Type_record of label_type_description global list

type exception_declaration = type_expression list

let generic = (-1)
and notgeneric = 0

let type_of_global g = (info g).value_typ
let type_of_constr_arg g = (info g).cstr_arg
let type_of_label_res g = (info g).lbl_res

(* The current nesting level of lets *)
let current_level = ref 0;;

(* generating fresh names *)
let names = new Ident.name_generator

(* making types *)
let make_type ?(effects = Effects.empty) ty =
  { type_desc = ty;
    type_level = generic;
    type_index = names#name;
    type_neutral = false;
    type_effects = effects;
  }

let product ?(effects = Effects.empty) ty_list =
  make_type ~effects (Type_product(ty_list))

let constr ?(effects = Effects.empty) ty_constr ty_list =
  make_type ~effects (Type_constr(ty_constr, ty_list))

let constr_notabbrev name ty_list =
  make_type (Type_constr({ gi = name;
			   info = Some {constr_abbr = Constr_notabbrev}; },
			 ty_list))

let arrow ?(n=false) ?(effects = Effects.empty) ty1 ty2 =
  let ty = make_type (Type_arrow(ty1, ty2)) in
  ty.type_neutral <- n;
  ty.type_effects <- effects;
  ty

let process ?(effects = Effects.empty) ty k =
  make_type ~effects (Type_process (ty, k))

let rec arrow_list ty_l ty_res =
  match ty_l with
    [] -> ty_res
  | [ty] -> arrow ty ty_res
  | ty :: ty_l -> arrow ty (arrow_list ty_l ty_res)

let forall l typ =
  { ts_binders = l;
    ts_desc = typ; }

let no_type_expression =
  { type_desc = Type_product[];
    type_level = generic;
    type_index = -1;
    type_neutral = false;
    type_effects = Effects.empty;
  }

(* To get fresh type variables *)

let new_var () =
  { type_desc = Type_var;
    type_level = !current_level;
    type_index = names#name;
    type_neutral = false;
    type_effects = Effects.empty;
  }

let new_generic_var () =
  { type_desc = Type_var;
    type_level = generic;
    type_index = names#name;
    type_neutral = false;
    type_effects = Effects.empty;
  }

let rec new_var_list n =
  match n with
    0 -> []
  | n -> (new_var ()) :: new_var_list (n - 1)
