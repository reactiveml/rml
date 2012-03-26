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

(* file: reac_ast.ml *)

(* Warning: *)
(* This file is based on the original version of syntax.ml *)
(* from the CamlLight 0.75 distribution, INRIA             *)

(* first modification: 2004-04-23 *)
(* modified by: Louis Mandel *)

(* $Id$ *)

(* The abstract syntax for the reac language *)

open Asttypes
open Types
open Clocks
open Modules

type ident = Ident.t

(*type global_ident = Global_ident.qualified_ident*)

(* Expressions *)

(* ML expressions *)
type expression =
  { e_desc: expression_desc;
    e_loc: Location.t;
    mutable e_type: Types.type_expression;
    mutable e_clock: Clocks.clock;
    mutable e_static: Static.static;
    mutable e_reactivity: (varpatt * int) list; }

and expression_desc =
  | Elocal of ident
  | Eglobal of value_description
  | Econstant of immediate
  | Elet of rec_flag * (pattern * expression) list * expression
  | Efunction of (pattern * expression) list
  | Eapply of expression * expression list
  | Etuple of expression list
  | Econstruct of constructor_description * expression option
  | Earray of expression list
  | Erecord of (label_description * expression) list
  | Erecord_access of expression * label_description
  | Erecord_update of
      expression * label_description * expression
  | Econstraint of expression * type_expression
  | Etrywith of expression * (pattern * expression) list
  | Eassert of expression
  | Eifthenelse of expression * expression * expression
  | Ematch of expression * (pattern * expression) list
  | Ewhen_match of expression * expression
  | Ewhile of expression * expression
  | Efor of
      ident * expression * expression * direction_flag * expression
  | Eseq of expression list
  | Eprocess of expression
  | Epre of pre_kind * expression
  | Elast of expression
  | Edefault of expression
  | Enothing
  | Epause of continue_begin_of_instant * expression Asttypes.clock_expr
  | Ehalt of continue_begin_of_instant
  | Eemit of expression * expression option
  | Eloop of expression option * expression
  | Efordopar of
      ident * expression * expression * direction_flag * expression
  | Epar of expression list
  | Emerge of expression * expression
  | Esignal of
      (ident * type_expression option) * expression Asttypes.clock_expr * expression Asttypes.clock_expr
        * (expression * expression) option * expression
  | Erun of expression
  | Euntil of event_config * expression * (pattern * expression) option
  | Ewhen of event_config * expression
  | Econtrol of event_config * (pattern * expression) option * expression
  | Eget of expression * pattern * expression
  | Epresent of event_config * expression * expression
  | Eawait of immediate_flag * event_config
  | Eawait_val of
      immediate_flag * await_kind * expression * pattern * expression
(*reparml related expressions*)
  | Enewclock of ident * expression option * expression
  | Epauseclock of expression
  | Etopck

(* event configuration *)
and event_config =
    { conf_desc: event_config_desc;
      conf_loc: Location.t; }
and event_config_desc =
  | Cpresent of expression
  | Cand of event_config * event_config
  | Cor of event_config * event_config

(* Patterns *)
and pattern =
    { patt_desc: pattern_desc;
      patt_loc: Location.t;
      mutable patt_type: Types.type_expression;
      mutable patt_clock : Clocks.clock }
and pattern_desc =
  | Pany
  | Pvar of varpatt
  | Palias of pattern * varpatt
  | Pconstant of immediate
  | Ptuple of pattern list
  | Pconstruct of constructor_description * pattern option
  | Por of pattern * pattern
  | Precord of (label_description * pattern) list
  | Parray of pattern list
  | Pconstraint of pattern * type_expression

and varpatt =
  | Vlocal of ident
  | Vglobal of value_description

(* Types *)
and type_expression =
    { te_desc: type_expression_desc;
      te_loc: Location.t}
and type_expression_desc =
    Tvar of string
  | Tforall of param_expression list * type_expression
  | Tarrow of type_expression * type_expression * effect_expression
  | Tproduct of type_expression list
  | Tconstr of type_description * param_expression list
  | Tprocess of type_expression * Static.instantaneous * carrier_expression * effect_expression
      (* result type, static, activation clock *)
  | Tdepend of carrier_expression

and carrier_expression =
   { ce_desc : carrier_expression_desc;
     ce_loc : Location.t }
and carrier_expression_desc =
    | Cvar of string
    | Ctopck

and effect_expression =
    { ee_desc : effect_expression_desc;
      ee_loc : Location.t }
and effect_expression_desc =
    | Effempty
    | Effvar of string
    | Effsum of effect_expression * effect_expression
    | Effdepend of carrier_expression

and param_expression =
    | Ptype of type_expression
    | Pcarrier of carrier_expression
    | Peffect of effect_expression

and type_declaration =
  | Tabstract
  | Trebind of type_expression
  | Tvariant of
      (constructor_description * type_expression option) list
  | Trecord of
      (label_description * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { impl_desc: impl_desc;
    impl_loc: Location.t;}
and impl_desc =
  | Iexpr of expression
  | Ilet of rec_flag * (pattern * expression) list
  | Isignal of
      ((value_description * type_expression option)
         * (expression * expression) option) list
  | Itype of
      (type_description * (string * type_var_kind) list * type_declaration) list
  | Iexn of
      constructor_description * type_expression option
  | Iexn_rebind of
      constructor_description * constructor_description
  | Iopen of string

(* Signature *)
type intf_item =
    {intf_desc: intf_desc;
     intf_loc: Location.t;}
and intf_desc =
  | Dval of value_description  * type_expression
  | Dtype of
      (type_description * (string * type_var_kind) list * type_declaration) list
  | Dexn of
      constructor_description  * type_expression option
  | Dopen of string

