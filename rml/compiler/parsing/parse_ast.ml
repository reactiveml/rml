(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : parse_ast.ml                                               *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : inspired by CamlLight                                     *)
(*************************************************************************)

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
    {pexpr_desc: expression_desc;
     pexpr_loc: Location.t;
     mutable pexpr_static: Def_static.static;}
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
  | Pexpr_seq of expression * expression
  | Pexpr_nothing
  | Pexpr_pause
  | Pexpr_emit of expression
  | Pexpr_emit_val of expression * expression
  | Pexpr_loop of expression
  | Pexpr_par of expression * expression
  | Pexpr_merge of expression * expression
  | Pexpr_signal of 
      (simple_ident * type_expression option) list * 
	(expression * expression) option * expression
  | Pexpr_process of expression
  | Pexpr_run of expression 
  | Pexpr_until of expression * expression (* signal * body *)
  | Pexpr_when of expression * expression
  | Pexpr_control of expression * expression
  | Pexpr_get of expression 
  | Pexpr_present of expression * expression * expression
  | Pexpr_await of immediate_flag * expression
  | Pexpr_await_val of 
      immediate_flag * await_kind * expression * pattern * expression
  | Pexpr_pre of pre_kind * expression

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
  | Ptype_arrow of type_expression * type_expression
  | Ptype_tuple of type_expression list
  | Ptype_constr of ident * type_expression list
  | Ptype_process 

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
      (simple_ident * type_expression option) list * 
	(expression * expression) option
  | Pimpl_type of (simple_ident * string list * type_declaration) list
  | Pimpl_exn of simple_ident * type_expression option
  | Pimpl_exn_rebind of simple_ident * ident
  | Pimpl_open of string

(* Signature *)
type interface = intf_item list

and intf_item =
    {pintf_desc: intf_desc;
     pintf_loc: Location.t;}
and intf_desc =
  | Pintf_val of simple_ident * type_expression
  | Pintf_type of (simple_ident * string list * type_declaration) list
  | Pintf_exn of simple_ident * type_expression option
  | Pintf_open of string

