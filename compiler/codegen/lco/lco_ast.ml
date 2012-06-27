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

(* file: lco_ast.ml *)
(* created: 2004-06-04  *)
(* author: Louis Mandel *)

(* $Id: lco_ast.ml,v 1.2 2005/03/14 09:58:54 mandel Exp $ *)

(* The abstract syntax for the Lco language *)

open Asttypes
open Types
open Modules

type ident = Ident.t

(* Expressions *)

(* ML expressions *)
type expression =
  { coexpr_desc: expression_desc;
    coexpr_loc: Location.t; }
and expression_desc =
  | Coexpr_local of ident
  | Coexpr_global of value_description
  | Coexpr_constant of immediate
  | Coexpr_let of rec_flag * (pattern * expression) list * expression
  | Coexpr_function of (pattern * expression) list
  | Coexpr_apply of expression * expression list
  | Coexpr_tuple of expression list
  | Coexpr_construct of constructor_description * expression option
  | Coexpr_array of expression list
  | Coexpr_record of (label_description * expression) list
  | Coexpr_record_access of expression * label_description
  | Coexpr_record_update of
      expression * label_description * expression
  | Coexpr_constraint of expression * type_expression
  | Coexpr_trywith of expression * (pattern * expression) list
  | Coexpr_assert of expression
  | Coexpr_ifthenelse of expression * expression * expression
  | Coexpr_match of expression * (pattern * expression) list
  | Coexpr_when_match of expression * expression
  | Coexpr_while of expression * expression
  | Coexpr_for of
      ident * expression * expression * direction_flag * expression
  | Coexpr_seq of expression * expression
  | Coexpr_process of process
  | Coexpr_pre of pre_kind * expression
  | Coexpr_last of expression
  | Coexpr_default of expression
  | Coexpr_emit of expression
  | Coexpr_emit_val of expression * expression
  | Coexpr_signal of
      (ident * type_expression option) * expression Asttypes.clock_expr * expression Asttypes.clock_expr
    * (expression * expression) option * expression
  | Coexpr_topck
  | Coexpr_last_mem of expression
  | Coexpr_update of expression * expression
  | Coexpr_set_mem of expression * expression

(* Process expressions *)
and process =
  { coproc_desc: process_desc;
    coproc_loc: Location.t;}
and process_desc =
  | Coproc_nothing
  | Coproc_pause of continue_begin_of_instant * expression Asttypes.clock_expr
  | Coproc_halt of continue_begin_of_instant
  | Coproc_compute of expression
  | Coproc_emit of expression
  | Coproc_emit_val of expression * expression
  | Coproc_loop of expression option * process
  | Coproc_while of expression * process
  | Coproc_for of ident * expression * expression * direction_flag * process
  | Coproc_fordopar of
      ident * expression * expression * direction_flag * process
  | Coproc_seq of process * process
  | Coproc_par of process list
  | Coproc_merge of process * process
  | Coproc_signal of
      (ident * type_expression option) * expression Asttypes.clock_expr * expression Asttypes.clock_expr
    * (expression * expression) option * process
  | Coproc_def of (pattern * expression) * process
  | Coproc_def_dyn of (pattern * process) * process
  | Coproc_def_and_dyn of (pattern * process) list * process
  | Coproc_run of expression
  | Coproc_until of event_config * process * (pattern * process) option
  | Coproc_when of event_config * process
  | Coproc_control of event_config * (pattern * expression) option * process
  | Coproc_get of expression * pattern * process
  | Coproc_present of event_config * process * process
  | Coproc_ifthenelse of expression * process * process
  | Coproc_match of expression * (pattern * process) list
  | Coproc_when_match of expression * process
  | Coproc_await of immediate_flag * event_config
  | Coproc_await_val of
      immediate_flag * await_kind * expression * pattern * process
  | Coproc_newclock of ident * expression option * process
  | Coproc_pauseclock of expression
  | Coproc_memory of ident * expression Asttypes.clock_expr * expression * process
  | Coproc_await_new of expression * pattern * process
  | Coproc_update of expression * expression
  | Coproc_set_mem of expression * expression

(* event configuration *)
and event_config =
    { coconf_desc: event_config_desc;
      coconf_loc: Location.t; }
and event_config_desc =
  | Coconf_present of expression
  | Coconf_and of event_config * event_config
  | Coconf_or of event_config * event_config

(* Patterns *)
and pattern =
    { copatt_desc: pattern_desc;
      copatt_loc: Location.t; }
and pattern_desc =
  | Copatt_any
  | Copatt_var of varpatt
  | Copatt_alias of pattern * varpatt
  | Copatt_constant of immediate
  | Copatt_tuple of pattern list
  | Copatt_construct of constructor_description * pattern option
  | Copatt_or of pattern * pattern
  | Copatt_record of (label_description * pattern) list
  | Copatt_array of pattern list
  | Copatt_constraint of pattern * type_expression

and varpatt =
  | Covarpatt_local of ident
  | Covarpatt_global of value_description

(* Types *)
and type_expression =
    { cote_desc: type_expression_desc;
      cote_loc: Location.t}
and type_expression_desc =
    Cotype_var of string
  | Cotype_arrow of type_expression * type_expression * effect_expression
  | Cotype_product of type_expression list
  | Cotype_constr of type_description * param_expression list
  | Cotype_process of type_expression * carrier_expression * effect_expression
  | Cotype_depend of carrier_expression
  | Cotype_forall of param_expression list * type_expression
  | Cotype_some of param_expression list * type_expression

and carrier_expression =
   { coce_desc : carrier_expression_desc;
     coce_loc : Location.t }
and carrier_expression_desc =
    | Cocar_var of string
    | Cocar_topck

and effect_expression =
    { coee_desc : effect_expression_desc;
      coee_loc : Location.t }
and effect_expression_desc =
    | Coeff_empty
    | Coeff_var of string
    | Coeff_sum of effect_expression * effect_expression
    | Coeff_depend of carrier_expression

and param_expression =
    | Cop_type of type_expression
    | Cop_carrier of carrier_expression
    | Cop_effect of effect_expression


and type_declaration =
  | Cotype_abstract
  | Cotype_rebind of type_expression
  | Cotype_variant of
      (constructor_description * type_expression option) list
  | Cotype_record of
      (label_description * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { coimpl_desc: impl_desc;
    coimpl_loc: Location.t;}
and impl_desc =
  | Coimpl_expr of expression
  | Coimpl_let of rec_flag * (pattern * expression) list
  | Coimpl_signal of
      ((value_description * type_expression option)
	 * (expression * expression) option) list
  | Coimpl_type of
      (type_description * (string * type_var_kind) list * type_declaration) list
  | Coimpl_exn of
      constructor_description * type_expression option
  | Coimpl_exn_rebind of
      constructor_description * constructor_description
  | Coimpl_open of string

(* Signature *)
type intf_item =
    {cointf_desc: intf_desc;
     cointf_loc: Location.t;}
and intf_desc =
  | Cointf_val of value_description * type_expression
  | Cointf_type of
      (type_description * (string * type_var_kind) list * type_declaration) list
  | Cointf_exn of
      constructor_description * type_expression option
  | Cointf_open of string

