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
      ts_rbinders: reactivity_effect list;     (* generalised reactivity vars *)
      ts_desc: type_expression;                (* the type *)
    }

and type_expression =
    { mutable type_desc: type_expression_desc;
      mutable type_level: int;
      type_index: int; }
and type_expression_desc =
    Type_var
  | Type_arrow of type_expression * type_expression
  | Type_product of type_expression list
  | Type_constr of type_constr global * type_expression list
  | Type_link of type_expression
  | Type_process of type_expression * process_info

and process_info =
    { mutable proc_react: reactivity_effect; }

and process_static =
  | Proc_def of Def_static.instantaneous ref
  | Proc_link of process_static
  | Proc_unify of process_static * process_static

and reactivity_effect =
    { mutable react_desc: reactivity_effect_desc;
      mutable react_level: int;
      react_index: int; }

and reactivity_effect_desc =
  | React_var
  | React_pause
  | React_epsilon
  | React_seq of reactivity_effect list
  | React_par of reactivity_effect list
  | React_or of reactivity_effect list
  | React_raw of reactivity_effect * reactivity_effect (* k * var *)
  | React_rec of checked * reactivity_effect (* k *)
  | React_run of reactivity_effect
  | React_link of reactivity_effect

and checked = bool

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
