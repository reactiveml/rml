(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lco_misc.ml                                                *)
(*  Date de creation : 03/08/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Functions on Lco AST *)

open Lco_ast

let rec is_value e =
  match e.coexpr_desc with
  | Coexpr_local _  | Coexpr_global _ | Coexpr_constant _
  | Coexpr_function _ -> 
      true

  | Coexpr_let(_, patt_expr_list, expr) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Coexpr_tuple expr_list ->
      List.for_all is_value expr_list

  | Coexpr_construct (_, None) -> true
  | Coexpr_construct (_, Some expr) -> is_value expr

  | Coexpr_constraint (expr, _) -> is_value expr
      
  | Coexpr_trywith (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Coexpr_assert expr -> is_value expr

  | Coexpr_ifthenelse (e1, e2, e3) ->
      (is_value e1) && (is_value e2) && (is_value e3) 

  | Coexpr_match (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | _ -> false 
