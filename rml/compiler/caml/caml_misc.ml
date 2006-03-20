(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : caml_misc.ml                                               *)
(*  Date de creation : 03/08/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Functions on Caml AST *)

open Asttypes
open Caml_ast
open Global
open Global_ident
open Misc

(* Building functions *)

let make_expr e loc =
  { cexpr_desc = e;
    cexpr_loc = loc; }

let make_patt p loc =
  { cpatt_desc = p;
    cpatt_loc = loc; }

let make_te t loc =
  { cte_desc = t;
    cte_loc = loc; }

let make_impl it loc =
  { cimpl_desc = it;
    cimpl_loc = loc; }

let make_intf it loc =
  { cintf_desc = it;
    cintf_loc = loc; }

let make_instruction s =
  make_expr 
    (Cexpr_global 
       { gi = { qual = !interpreter_module; 
		id = Ident.create Ident.gen_var s Ident.Internal };
	 info = no_info; })
    Location.none

let make_rml_type s ty_list =
  make_te
    (Ctype_constr ({ gi = { qual = !interpreter_module; 
			    id = Ident.create Ident.gen_type s Ident.Type };
		     info = no_info; }, ty_list))
    Location.none


let make_patt_unit () =
  make_patt (Cpatt_constant Const_unit) Location.none

let make_expr_unit () =
  make_expr (Cexpr_constant Const_unit) Location.none

let make_patt_var_local id =
  make_patt (Cpatt_var (Cvarpatt_local id)) Location.none

let make_expr_var_local id =
  make_expr (Cexpr_local id) Location.none

let make_raise_RML () =
  make_expr 
    (Cexpr_apply 
       (make_expr
	  (Cexpr_global 
	     { gi = { qual = pervasives_module;
		      id = Ident.create Ident.gen_var "raise" Ident.Val_ML };
	       info = no_info; })
	  Location.none,
	[make_expr
	   (Cexpr_construct 
	      ({ gi = { qual = !interpreter_module;
			id = Ident.create Ident.gen_constr 
			  "RML" Ident.Internal };
		 info = no_info; },
	       None))
	   Location.none])) 
    Location.none

let make_list c_list =
  List.fold_right
    (fun e acc -> 
      make_expr
	(Cexpr_construct
	   (Initialization.cons_constr_desc, 
	    Some 
	      (make_expr
		 (Cexpr_tuple [e; acc])
		 Location.none)))
	Location.none)
    c_list
    (make_expr 
       (Cexpr_construct (Initialization.nil_constr_desc, None)) 
       Location.none)

(* Test if an expression is an ML value*)
let rec is_value e =
  match e.cexpr_desc with
  | Cexpr_local _  | Cexpr_global _ | Cexpr_constant _
  | Cexpr_function _ | Cexpr_fun _ -> 
      true

  | Cexpr_let(_, patt_expr_list, expr) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Cexpr_tuple expr_list ->
      List.for_all is_value expr_list

  | Cexpr_construct (_, None) -> true
  | Cexpr_construct (_, Some expr) -> is_value expr

  | Cexpr_constraint (expr, _) -> is_value expr
      
  | Cexpr_trywith (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Cexpr_assert expr -> is_value expr

  | Cexpr_ifthenelse (e1, e2, e3) ->
      (is_value e1) && (is_value e2) && (is_value e3) 

  | Cexpr_match (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | _ -> false

(* Test is a pattern is a partial matching *)
let rec partial_match patt = 
  match patt.cpatt_desc with
  | Cpatt_any -> false
  | Cpatt_var _ -> false
  | Cpatt_alias (p, _) -> partial_match p
  | Cpatt_constant Const_unit -> false
  | Cpatt_constant _ -> true
  | Cpatt_tuple patt_list -> List.exists partial_match patt_list
  | Cpatt_construct _ -> true
  | Cpatt_or (p1, p2) -> (partial_match p1) & (partial_match p2)
  | Cpatt_record _ -> true
  | Cpatt_array _ -> true
  | Cpatt_constraint (p, _) -> partial_match p

