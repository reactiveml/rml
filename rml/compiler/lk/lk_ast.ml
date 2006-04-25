(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lk_ast.ml                                                  *)
(*  Date de creation : 09/08/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *) 

(* The abstract syntax for the Lk language (cf. thesis) *)

open Asttypes
open Def_types

type ident = Ident.t

type 'a global = 'a Global.global

(* Expressions *)

(* ML expressions *)
type expression =
  { kexpr_desc: expression_desc;
    kexpr_loc: Location.t; }
and expression_desc =
  | Kexpr_local of ident
  | Kexpr_global of value_type_description global
  | Kexpr_constant of immediate
  | Kexpr_let of rec_flag * (pattern * expression) list * expression
  | Kexpr_function of (pattern * expression) list
  | Kexpr_apply of expression * expression list
  | Kexpr_tuple of expression list
  | Kexpr_construct of constructor_type_description global * expression option
  | Kexpr_array of expression list
  | Kexpr_record of (label_type_description global * expression) list
  | Kexpr_record_access of expression * label_type_description global
  | Kexpr_record_update of 
      expression * label_type_description global * expression
  | Kexpr_constraint of expression * type_expression
  | Kexpr_trywith of expression * (pattern * expression) list
  | Kexpr_assert of expression
  | Kexpr_ifthenelse of expression * expression * expression
  | Kexpr_match of expression * (pattern * expression) list
  | Kexpr_when_match of expression * expression
  | Kexpr_while of expression * expression
  | Kexpr_for of 
      ident * expression * expression * direction_flag * expression
  | Kexpr_seq of expression * expression
  | Kexpr_process of ident * ident * process
  | Kexpr_pre of pre_kind * expression
  | Kexpr_emit of expression
  | Kexpr_emit_val of expression * expression
  | Kexpr_signal of 
      (ident * type_expression option) 
	* (expression * expression) option * expression

(* Process expressions *)
and process =
  { kproc_desc: process_desc;
    kproc_loc: Location.t;}
and process_desc =
  | Kproc_pause of process * ident
  | Kproc_halt of process
  | Kproc_compute of expression * process
  | Kproc_emit of expression * process
  | Kproc_emit_val of expression * expression * process
  | Kproc_loop of ident * process
  | Kproc_loop_n of ident * expression * process * process
  | Kproc_while of expression * (ident * process) * process
  | Kproc_for of 
      ident * expression * expression * direction_flag * 
	(ident * process) * process
  | Kproc_fordopar of 
      ident * expression * expression * direction_flag * 
	(ident * process) * process
  | Kproc_split_def of ident * ident list * process list
  | Kproc_join_def of ident * ident * ident list * process
(*   | Kproc_split_par of ident * process list *)
  | Kproc_split_par of ident * pattern * process * process list
  | Kproc_join_par of ident * process
(*  | Kproc_merge of process * process *)
  | Kproc_signal of 
      (ident * type_expression option) 
	* (expression * expression) option * process
  | Kproc_def of rec_flag * (pattern * expression) list * process
  | Kproc_def_dyn of pattern * process
  | Kproc_def_and_dyn of pattern list * process
  | Kproc_run of expression  * process * ident
  | Kproc_start_until of 
      (* ident * *)
      event_config * (ident * process) * (pattern * process) 
  | Kproc_end_until of ident * process
  | Kproc_start_when of
      (* ident * *) event_config * (ident * process) 
(*  | Kproc_when of ident * expression * ident *)
  | Kproc_end_when of ident * process
  | Kproc_start_control of
      (* ident * *) event_config * (ident * process) 
  | Kproc_end_control of ident * process
  | Kproc_get of expression * pattern * process * ident
  | Kproc_present of ident * event_config * process * process
  | Kproc_ifthenelse of expression * process * process
  | Kproc_match of expression * (pattern * process) list
  | Kproc_when_match of expression * process
  | Kproc_await of immediate_flag * event_config * process * ident
  | Kproc_await_val of 
      immediate_flag * await_kind * expression * pattern * process * ident
  | Kproc_bind of
      pattern * process * process
  | Kproc_var of ident


(* event configuration *)
and event_config =
    { kconf_desc: event_config_desc;
      kconf_loc: Location.t; }
and event_config_desc =
  | Kconf_present of expression
  | Kconf_and of event_config * event_config
  | Kconf_or of event_config * event_config

(* Patterns *)
and pattern =
    { kpatt_desc: pattern_desc;
      kpatt_loc: Location.t; }
and pattern_desc =
  | Kpatt_any
  | Kpatt_var of varpatt
  | Kpatt_alias of pattern * varpatt
  | Kpatt_constant of immediate
  | Kpatt_tuple of pattern list
  | Kpatt_construct of constructor_type_description global * pattern option
  | Kpatt_or of pattern * pattern
  | Kpatt_record of (label_type_description global * pattern) list
  | Kpatt_array of pattern list
  | Kpatt_constraint of pattern * type_expression

and varpatt =
  | Kvarpatt_local of ident
  | Kvarpatt_global of value_type_description global

(* Types *)
and type_expression =
    { kte_desc: type_expression_desc;
      kte_loc: Location.t}
and type_expression_desc =
    Ktype_var of string
  | Ktype_arrow of type_expression * type_expression 
  | Ktype_product of type_expression list                  
  | Ktype_constr of type_description global * type_expression list
  | Ktype_process of type_expression

and type_declaration =
  | Ktype_abstract
  | Ktype_rebind of type_expression
  | Ktype_variant of 
      (constructor_type_description global * type_expression option) list
  | Ktype_record of 
      (label_type_description global * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { kimpl_desc: impl_desc;
    kimpl_loc: Location.t;}
and impl_desc =
  | Kimpl_expr of expression
  | Kimpl_let of rec_flag * (pattern * expression) list 
  | Kimpl_signal of 
      ((value_type_description global * type_expression option) 
	 * (expression * expression) option) list
  | Kimpl_type of 
      (type_description global * string list * type_declaration) list
  | Kimpl_exn of 
      constructor_type_description global * type_expression option
  | Kimpl_exn_rebind of 
      constructor_type_description global * constructor_type_description global
  | Kimpl_open of string

(* Signature *)
type intf_item =
    {kintf_desc: intf_desc;
     kintf_loc: Location.t;}
and intf_desc =
  | Kintf_val of value_type_description global * type_expression
  | Kintf_type of 
      (type_description global * string list * type_declaration) list
  | Kintf_exn of 
      constructor_type_description global * type_expression option
  | Kintf_open of string

