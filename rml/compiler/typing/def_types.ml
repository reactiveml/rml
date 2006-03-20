(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : def_types.ml                                               *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : inspired by Lucid Synchrone                               *)
(*************************************************************************)

(* $Id: def_types.ml,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

(* The abstract syntax for the types *)

open Asttypes
open Global

(* types *)
type type_scheme =
    { ts_binders: type_expression list;        (* generalised variables *)
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
  | Type_process of type_expression

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

let type_of_global g = g.info.value_typ
let type_of_constr_arg g = g.info.cstr_arg 
let type_of_label_res g = g.info.lbl_res
