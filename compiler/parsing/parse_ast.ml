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

(* file: parse_ast.ml *)

(* Warning: *)
(* This file is based on the original version of syntax.ml *)
(* from the CamlLight 0.75 distribution, INRIA             *)

(* first modification: 2004-04-23  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The abstract syntax for the parsed language *)

open Asttypes

type ident =
    { pident_id: Parse_ident.t;
      pident_loc: Location.t; }
and simple_ident =
    { psimple_id: string;
      psimple_loc: Location.t; }

(* Expressions *)
type expression =
    { pexpr_desc: expression_desc;
      pexpr_loc: Location.t; }
and expression_desc =
  | Pexpr_ident of ident
  | Pexpr_constant of immediate
  | Pexpr_let of rec_flag * (pattern * expression) list * expression
  | Pexpr_function of (pattern * expression) list
(*  | Pexpr_fun of pattern list * expression *)
  | Pexpr_apply of expression * expression list
  | Pexpr_tuple of expression list
  | Pexpr_construct of ident * expression option
  | Pexpr_array of expression list
  | Pexpr_record of (ident * expression) list
  | Pexpr_record_with of expression * (ident * expression) list
  | Pexpr_record_access of expression * ident
  | Pexpr_record_update of expression * ident * expression
  | Pexpr_constraint of expression * type_expression
  | Pexpr_trywith of expression * (pattern * expression) list
  | Pexpr_assert of expression
  | Pexpr_ifthenelse of expression * expression * expression option
  | Pexpr_match of expression * (pattern * expression) list
  | Pexpr_when_match of expression * expression
  | Pexpr_while of expression * expression
  | Pexpr_for of
      simple_ident * expression * expression * direction_flag * expression
  | Pexpr_fordopar of
      simple_ident * expression * expression * direction_flag * expression
  | Pexpr_seq of expression * expression
  | Pexpr_nothing
  | Pexpr_pause of expression * pause_kind
  | Pexpr_halt
  | Pexpr_emit of expression
  | Pexpr_emit_val of expression * expression
  | Pexpr_loop of expression
  | Pexpr_par of expression * expression
  | Pexpr_merge of expression * expression
  | Pexpr_signal of
      signal_kind * (simple_ident * type_expression option) list *
        (expression (*ck*) * expression (*region*)) *
        (expression * expression) option (*default * gather *) *
        expression option (*reset*) * expression
  | Pexpr_process of expression
  | Pexpr_run of expression
  | Pexpr_until of
      event_config * expression * (pattern * expression) option
  (* signal        * body       * handler *)
  | Pexpr_when of event_config * expression
  | Pexpr_control of event_config * (pattern * expression) option * expression
  | Pexpr_get of expression
  | Pexpr_present of event_config * expression * expression
  | Pexpr_await of immediate_flag * event_config
  | Pexpr_await_val of
      immediate_flag * await_kind * expression * pattern * expression
  | Pexpr_pre of pre_kind * expression
  | Pexpr_last of expression
  | Pexpr_default of expression
  (*reparml related expreessions *)
  | Pexpr_newclock of simple_ident * expression option * expression option * expression
      (* ck, scheduling annotation, period, body *)
  | Pexpr_topck
  | Pexpr_base
(* event configuration *)
  | Pconf_present of expression
  | Pconf_and of event_config * event_config
  | Pconf_or of event_config * event_config

and event_config = expression

(* Patterns *)
and pattern =
    {ppatt_desc: pattern_desc;
     ppatt_loc: Location.t;}
and pattern_desc =
  | Ppatt_any
  | Ppatt_var of simple_ident
  | Ppatt_alias of pattern * simple_ident
  | Ppatt_constant of immediate
  | Ppatt_tuple of pattern list
  | Ppatt_construct of ident * pattern option
  | Ppatt_or of pattern * pattern
  | Ppatt_record of (ident * pattern) list
  | Ppatt_array of pattern list
  | Ppatt_constraint of pattern * type_expression

(* Types *)
and type_expression =
    {pte_desc: type_expression_desc;
     pte_loc: Location.t;}
and type_expression_desc =
  | Ptype_var of string
  | Ptype_forall of param_expression list * type_expression
  | Ptype_some of param_expression list * type_expression
  | Ptype_arrow of type_expression * type_expression * effect_row_expression
  | Ptype_tuple of type_expression list
  | Ptype_constr of ident * param_expression list
  | Ptype_process of type_expression * Static.instantaneous
                       * carrier_expression * effect_row_expression
  | Ptype_depend of carrier_row_expression

and carrier_expression =
   { pce_desc : carrier_expression_desc;
     pce_loc : Location.t }
and carrier_expression_desc =
    | Pcar_var of string
    | Pcar_ident of string
    | Pcar_fresh
    | Pcar_topck

and carrier_row_expression =
   { pcer_desc : carrier_row_expression_desc;
     pcer_loc : Location.t }
and carrier_row_expression_desc =
    | Pcar_row_var of string
    | Pcar_row_ident of string
    | Pcar_row_fresh
    | Pcar_row_empty
    | Pcar_row_one of carrier_expression
    | Pcar_row of carrier_row_expression * carrier_row_expression

and effect_expression =
    { pee_desc : effect_expression_desc;
      pee_loc : Location.t }
and effect_expression_desc =
    | Peff_empty
    | Peff_var of string
    | Peff_fresh
    | Peff_sum of effect_expression * effect_expression
    | Peff_depend of carrier_row_expression
    | Peff_one of effect_row_expression

and effect_row_expression =
    { peer_desc : effect_row_expression_desc;
      peer_loc : Location.t }
and effect_row_expression_desc =
    | Peff_row_var of string
    | Peff_row_fresh
    | Peff_row_empty
    | Peff_row_one of effect_expression
    | Peff_row of effect_row_expression * effect_row_expression

and param_expression = (type_expression, carrier_expression, carrier_row_expression,
                        effect_expression, effect_row_expression, unit) kind_sum

and type_declaration =
  | Ptype_abstract
  | Ptype_rebind of type_expression
  | Ptype_variant of (simple_ident * type_expression option) list
  | Ptype_record of (simple_ident * mutable_flag * type_expression) list

(* Structure *)
type implementation = impl_item list

and impl_item =
  { pimpl_desc: impl_desc;
    pimpl_loc: Location.t;}
and impl_desc =
  | Pimpl_expr of expression
  | Pimpl_let of rec_flag * (pattern * expression) list
  | Pimpl_signal of
      signal_kind * (simple_ident * type_expression option) list *
        (expression * expression) option
  | Pimpl_type of (simple_ident * type_var_kind list * type_declaration) list
  | Pimpl_exn of simple_ident * type_expression option
  | Pimpl_exn_rebind of simple_ident * ident
  | Pimpl_open of string
  | Pimpl_lucky of
      simple_ident *
	(simple_ident * type_expression) list * (* inputs: (id * ty) *)
	(simple_ident * type_expression) list * (* outputs: (id * ty) *)
	string list (* files *)

(* Signature *)
type interface = intf_item list

and intf_item =
    {pintf_desc: intf_desc;
     pintf_loc: Location.t;}
and intf_desc =
  | Pintf_val of simple_ident * type_expression
  | Pintf_type of (simple_ident * type_var_kind list * type_declaration) list
  | Pintf_exn of simple_ident * type_expression option
  | Pintf_open of string

