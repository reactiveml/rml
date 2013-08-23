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

(* file: caml_ast.ml *)
(* created: 2004-05-02  *)
(* author: Louis Mandel *)

(* $Id: caml_ast.ml,v 1.1.1.1 2005/01/23 17:55:36 mandel Exp $ *)

(* The abstract syntax for CAML *)

open Asttypes
open Def_types

type signal = Ident.t
type ident = Ident.t
type 'a global = 'a Global.global

(* Expressions *)

(* ML expressions *)
type expression =
  { cexpr_desc: expression_desc;
    cexpr_loc: Location.t; }
and expression_desc =
  | Cexpr_local of ident
  | Cexpr_global of value_type_description global
  | Cexpr_constant of immediate
  | Cexpr_let of rec_flag * (pattern * expression) list * expression
  | Cexpr_function of (pattern * expression option * expression) list
  | Cexpr_fun of pattern list * expression
  | Cexpr_apply of expression * expression list
  | Cexpr_tuple of expression list
  | Cexpr_construct of constructor_type_description global * expression option
  | Cexpr_array of expression list
  | Cexpr_record of (label_type_description global * expression) list
  | Cexpr_record_access of expression * label_type_description global
  | Cexpr_record_with of
      expression * (label_type_description global * expression) list
  | Cexpr_record_update of
      expression * label_type_description global * expression
  | Cexpr_constraint of expression * type_expression
  | Cexpr_trywith of
      expression * (pattern * expression option * expression) list
  | Cexpr_assert of expression
  | Cexpr_ifthenelse of expression * expression * expression
  | Cexpr_match of expression * (pattern * expression option * expression) list
  | Cexpr_while of expression * expression
  | Cexpr_for of
      ident * expression * expression * direction_flag * expression
  | Cexpr_seq of expression * expression

(* Patterns *)
and pattern =
    { cpatt_desc: pattern_desc;
      cpatt_loc: Location.t;}
and pattern_desc =
  | Cpatt_any
  | Cpatt_var of varpatt
  | Cpatt_alias of pattern * varpatt
  | Cpatt_constant of immediate
  | Cpatt_tuple of pattern list
  | Cpatt_construct of constructor_type_description global * pattern option
  | Cpatt_or of pattern * pattern
  | Cpatt_record of (label_type_description global * pattern) list
  | Cpatt_array of pattern list
  | Cpatt_constraint of pattern * type_expression

and varpatt =
  | Cvarpatt_local of ident
  | Cvarpatt_global of value_type_description global

(* Types *)
and type_expression =
    { cte_desc: type_expression_desc;
      cte_loc: Location.t}
and type_expression_desc =
    Ctype_var of string
  | Ctype_arrow of type_expression * type_expression
  | Ctype_product of type_expression list
  | Ctype_constr of type_description global * type_expression list
  | Ctype_any

and type_declaration =
  | Ctype_abstract
  | Ctype_rebind of type_expression
  | Ctype_variant of
      (constructor_type_description global * type_expression option) list
  | Ctype_record of
      (label_type_description global * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { cimpl_desc: impl_desc;
    cimpl_loc: Location.t;}
and impl_desc =
  | Cimpl_expr of expression
  | Cimpl_let of rec_flag * (pattern * expression) list
  | Cimpl_type of
      (type_description global * string list * type_declaration) list
  | Cimpl_exn of constructor_type_description global * type_expression option
  | Cimpl_exn_rebind of
      constructor_type_description global * constructor_type_description global
  | Cimpl_open of string

(* Signature *)
type intf_item =
    {cintf_desc: intf_desc;
     cintf_loc: Location.t;}
and intf_desc =
  | Cintf_val of value_type_description global * type_expression
  | Cintf_type of
      (type_description global * string list * type_declaration) list
  | Cintf_exn of
      constructor_type_description global * type_expression option
  | Cintf_open of string
