(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lk_ast.ml                                                  *)
(*  Date de creation : 30/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: lk_ast.ml,v 1.2 2005/03/14 09:58:54 mandel Exp $ *)

(* The abstract syntax for the continuation language *)

open Asttypes
open Def_types

type ident = Ident.t

(*type global_ident = Global_ident.qualified_ident*)
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
  | Kexpr_when of expression * expression
  | Kexpr_while of expression * expression
  | Kexpr_for of 
      ident * expression * expression * direction_flag * expression
  | Kexpr_seq of expression * expression
  | Kexpr_process of process
  | Kexpr_pre of pre_kind * expression
  | Kexpr_emit of expression 
  | Kexpr_emit_val of expression * expression 
  | Kexpr_signal of 
      (ident * type_expression option) * 
	(expression * expression) option * expression

(* Process expressions *)
and process =
  { kproc_desc: process_desc;
    kproc_loc: Location.t;}
and process_desc =
  | Kproc_var of ident
  | Kproc_abs of ident * process
  | Kproc_apply of process * process
  | Kproc_term
  | Kproc_pause of process
  | Kproc_compute of expression * process
  | Kproc_emit of expression * process
  | Kproc_emit_val of expression * expression * process
  | Kproc_loop of process
  | Kproc_while of expression * process * process
  | Kproc_for of 
      ident * expression * expression * direction_flag * process *process
  | Kproc_fordopar of 
      ident * expression * expression * direction_flag * process *process
  | Kproc_par of process * process * process
  | Kproc_merge of process * process * process
  | Kproc_signal of 
      (ident * type_expression option) * 
	(expression * expression) option * process
  | Kproc_def of (pattern * expression) * process
  | Kproc_run of expression * process
  | Kproc_until of expression * process * (pattern * process) option * process
  | Kproc_when of expression * process * process
  | Kproc_control of expression * process * process
  | Kproc_get of expression * pattern * process
  | Kproc_present of expression * process * process
  | Kproc_ifthenelse of expression * process * process
  | Kproc_match of expression * (pattern * process) list 
  | Kproc_await of immediate_flag * expression * process
  | Kproc_await_val of 
      immediate_flag * await_kind * expression * pattern * process

(* Patterns *)
and pattern =
    { kpatt_desc: pattern_desc;
      kpatt_loc: Location.t;}
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
  | Ktype_process 

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

