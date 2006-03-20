(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac_ast.ml                                                *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : taken from CamlLight                                      *)
(*************************************************************************)

(* $Id: reac_ast.ml,v 1.2 2005/03/14 09:58:54 mandel Exp $ *) 

(* The abstract syntax for the reac language *)

open Asttypes
open Def_types

type ident = Ident.t

(*type global_ident = Global_ident.qualified_ident*)
type 'a global = 'a Global.global

(* Expressions *)

(* ML expressions *)
type expression =
  { expr_desc: expression_desc;
    expr_loc: Location.t;
    mutable expr_type: Def_types.type_expression; 
    mutable expr_static: Def_static.static;}

and expression_desc =
  | Rexpr_local of ident
  | Rexpr_global of value_type_description global
  | Rexpr_constant of immediate
  | Rexpr_let of rec_flag * (pattern * expression) list * expression
  | Rexpr_function of (pattern * expression) list
  | Rexpr_apply of expression * expression list
  | Rexpr_tuple of expression list
  | Rexpr_construct of constructor_type_description global * expression option
  | Rexpr_array of expression list
  | Rexpr_record of (label_type_description global * expression) list
  | Rexpr_record_access of expression * label_type_description global
  | Rexpr_record_update of 
      expression * label_type_description global * expression
  | Rexpr_constraint of expression * type_expression
  | Rexpr_trywith of expression * (pattern * expression) list
  | Rexpr_assert of expression
  | Rexpr_ifthenelse of expression * expression * expression
  | Rexpr_match of expression * (pattern * expression) list
  | Rexpr_when_match of expression * expression
  | Rexpr_while of expression * expression
  | Rexpr_for of 
      ident * expression * expression * direction_flag * expression
  | Rexpr_seq of expression list
  | Rexpr_process of expression
  | Rexpr_pre of pre_kind * expression
  | Rexpr_nothing
  | Rexpr_pause
  | Rexpr_halt
  | Rexpr_emit of expression
  | Rexpr_emit_val of expression * expression
  | Rexpr_loop of expression
  | Rexpr_fordopar of 
      ident * expression * expression * direction_flag * expression
  | Rexpr_par of expression list
  | Rexpr_merge of expression * expression
  | Rexpr_signal of 
      (ident * type_expression option) 
	* (expression * expression) option * expression
  | Rexpr_run of expression 
  | Rexpr_until of event_config * expression * (pattern * expression) option
  | Rexpr_when of event_config * expression
  | Rexpr_control of event_config * expression
  | Rexpr_get of expression * pattern * expression
  | Rexpr_present of event_config * expression * expression
  | Rexpr_await of immediate_flag * event_config
  | Rexpr_await_val of 
      immediate_flag * await_kind * expression * pattern * expression

(* event configuration *)
and event_config =
    { conf_desc: event_config_desc;
      conf_loc: Location.t; }
and event_config_desc =
  | Rconf_present of expression
  | Rconf_and of event_config * event_config
  | Rconf_or of event_config * event_config

(* Patterns *)
and pattern =
    { patt_desc: pattern_desc;
      patt_loc: Location.t;
      mutable patt_type: Def_types.type_expression; }
and pattern_desc =
  | Rpatt_any
  | Rpatt_var of varpatt
  | Rpatt_alias of pattern * varpatt
  | Rpatt_constant of immediate
  | Rpatt_tuple of pattern list
  | Rpatt_construct of constructor_type_description global * pattern option
  | Rpatt_or of pattern * pattern
  | Rpatt_record of (label_type_description global * pattern) list
  | Rpatt_array of pattern list
  | Rpatt_constraint of pattern * type_expression

and varpatt =
  | Varpatt_local of ident
  | Varpatt_global of value_type_description global

(* Types *)
and type_expression =
    { te_desc: type_expression_desc;
      te_loc: Location.t}
and type_expression_desc =
    Rtype_var of string
  | Rtype_arrow of type_expression * type_expression 
  | Rtype_product of type_expression list                  
  | Rtype_constr of type_description global * type_expression list
  | Rtype_process of type_expression

and type_declaration =
  | Rtype_abstract
  | Rtype_rebind of type_expression
  | Rtype_variant of 
      (constructor_type_description global * type_expression option) list
  | Rtype_record of 
      (label_type_description global * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { impl_desc: impl_desc;
    impl_loc: Location.t;}
and impl_desc =
  | Rimpl_expr of expression
  | Rimpl_let of rec_flag * (pattern * expression) list 
  | Rimpl_signal of 
      ((value_type_description global * type_expression option) 
	 * (expression * expression) option) list
  | Rimpl_type of 
      (type_description global * string list * type_declaration) list
  | Rimpl_exn of 
      constructor_type_description global * type_expression option
  | Rimpl_exn_rebind of 
      constructor_type_description global * constructor_type_description global
  | Rimpl_open of string

(* Signature *)
type intf_item =
    {intf_desc: intf_desc;
     intf_loc: Location.t;}
and intf_desc =
  | Rintf_val of value_type_description global * type_expression
  | Rintf_type of 
      (type_description global * string list * type_declaration) list
  | Rintf_exn of 
      constructor_type_description global * type_expression option
  | Rintf_open of string

