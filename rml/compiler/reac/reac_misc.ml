(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac_misc.ml                                               *)
(*  Date de creation : 05/05/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Functions on Reac AST *)

open Reac_ast
open Def_types
open Types

let make_expr e loc =
  { expr_desc = e;
    expr_loc = loc; 
    expr_type = no_type_expression; 
    expr_static = Def_static.Dynamic; }

let make_patt p loc =
  { patt_desc = p;
    patt_loc = loc; 
    patt_type = no_type_expression; }

let make_conf c loc =
  { conf_desc = c;
    conf_loc = loc; }

let make_te t loc =
  { te_desc = t;
    te_loc = loc; }

let make_impl it loc =
  { impl_desc = it;
    impl_loc = loc; }

let make_intf it loc =
  { intf_desc = it;
    intf_loc = loc; }


(* Compute the list of variables introduce in a pattern *)
let rec vars_of_patt p =
  match p.patt_desc with
  | Rpatt_any -> []

  | Rpatt_var x -> [x]

  | Rpatt_alias (patt,x) -> x :: (vars_of_patt patt)

  | Rpatt_constant _ -> []

  | Rpatt_tuple patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list 

  | Rpatt_construct (_, None) -> []

  | Rpatt_construct (_, Some patt) -> vars_of_patt patt

  | Rpatt_or (patt1, patt2) -> (vars_of_patt patt1) @ (vars_of_patt patt2)

  | Rpatt_record label_patt_list ->
      List.fold_left 
	(fun vars (_,patt) -> (vars_of_patt patt)@vars)
	[] label_patt_list 

  | Rpatt_array patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list 

  | Rpatt_constraint (patt, _) -> vars_of_patt patt


