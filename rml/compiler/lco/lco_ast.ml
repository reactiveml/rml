(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lco_ast.ml                                                 *)
(*  Date de creation : 04/06/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *) 

(* The abstract syntax for the Lco language *)

open Asttypes
open Def_types

type ident = Ident.t

type 'a global = 'a Global.global

(* Expressions *)

(* ML expressions *)
type expression =
  { coexpr_desc: expression_desc;
    coexpr_loc: Location.t; }
and expression_desc =
  | Coexpr_local of ident
  | Coexpr_global of value_type_description global
  | Coexpr_constant of immediate
  | Coexpr_let of rec_flag * (pattern * expression) list * expression
  | Coexpr_function of (pattern * expression) list
  | Coexpr_apply of expression * expression list
  | Coexpr_tuple of expression list
  | Coexpr_construct of constructor_type_description global * expression option
  | Coexpr_array of expression list
  | Coexpr_record of (label_type_description global * expression) list
  | Coexpr_record_access of expression * label_type_description global
  | Coexpr_record_update of 
      expression * label_type_description global * expression
  | Coexpr_constraint of expression * type_expression
  | Coexpr_trywith of expression * (pattern * expression) list
  | Coexpr_assert of expression
  | Coexpr_ifthenelse of expression * expression * expression
  | Coexpr_match of expression * (pattern * expression) list
  | Coexpr_when of expression * expression
  | Coexpr_while of expression * expression
  | Coexpr_for of 
      ident * expression * expression * direction_flag * expression
  | Coexpr_seq of expression * expression
  | Coexpr_process of process
  | Coexpr_pre of pre_kind * expression
  | Coexpr_emit of expression
  | Coexpr_emit_val of expression * expression

(* Process expressions *)
and process =
  { coproc_desc: process_desc;
    coproc_loc: Location.t;}
and process_desc =
  | Coproc_nothing
  | Coproc_pause
  | Coproc_compute of expression
  | Coproc_emit of expression
  | Coproc_emit_val of expression * expression
  | Coproc_loop of process
  | Coproc_while of expression * process
  | Coproc_for of ident * expression * expression * direction_flag * process
  | Coproc_fordopar of 
      ident * expression * expression * direction_flag * process
  | Coproc_seq of process * process
  | Coproc_par of process * process
  | Coproc_merge of process * process
  | Coproc_signal of 
      (ident * type_expression option) 
	* (expression * expression) option * process
  | Coproc_def of (pattern * expression) * process
  | Coproc_run of expression 
  | Coproc_until of expression * process * (pattern * process) option
  | Coproc_when of expression * process
  | Coproc_control of expression * process
  | Coproc_get of expression * pattern * process
  | Coproc_present of expression * process * process
  | Coproc_ifthenelse of expression * process * process
  | Coproc_match of expression * (pattern * process) list
  | Coproc_await of immediate_flag * expression
  | Coproc_await_val of 
      immediate_flag * await_kind * expression * pattern * process

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
  | Copatt_construct of constructor_type_description global * pattern option
  | Copatt_or of pattern * pattern
  | Copatt_record of (label_type_description global * pattern) list
  | Copatt_array of pattern list
  | Copatt_constraint of pattern * type_expression

and varpatt =
  | Covarpatt_local of ident
  | Covarpatt_global of value_type_description global

(* Types *)
and type_expression =
    { cote_desc: type_expression_desc;
      cote_loc: Location.t}
and type_expression_desc =
    Cotype_var of string
  | Cotype_arrow of type_expression * type_expression 
  | Cotype_product of type_expression list                  
  | Cotype_constr of type_description global * type_expression list
  | Cotype_process 

and type_declaration =
  | Cotype_abstract
  | Cotype_rebind of type_expression
  | Cotype_variant of 
      (constructor_type_description global * type_expression option) list
  | Cotype_record of 
      (label_type_description global * mutable_flag * type_expression) list

(* Structure *)
type impl_item =
  { coimpl_desc: impl_desc;
    coimpl_loc: Location.t;}
and impl_desc =
  | Coimpl_expr of expression
  | Coimpl_let of rec_flag * (pattern * expression) list 
  | Coimpl_signal of 
      ((value_type_description global * type_expression option) 
	 * (expression * expression) option) list
  | Coimpl_type of 
      (type_description global * string list * type_declaration) list
  | Coimpl_exn of 
      constructor_type_description global * type_expression option
  | Coimpl_exn_rebind of 
      constructor_type_description global * constructor_type_description global
  | Coimpl_open of string

(* Signature *)
type intf_item =
    {cointf_desc: intf_desc;
     cointf_loc: Location.t;}
and intf_desc =
  | Cointf_val of value_type_description global * type_expression
  | Cointf_type of 
      (type_description global * string list * type_declaration) list
  | Cointf_exn of 
      constructor_type_description global * type_expression option
  | Cointf_open of string

