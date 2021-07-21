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

(* file: reac_optimization.ml *)

open Asttypes
open Reac_ast
open Def_types
open Reac_misc


(* Translate binary seq and par to n-ary operators *)
let binary2nary e =
  let e' =
    match e.expr_desc with
    | Rexpr_seq e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_seq e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
      in Rexpr_seq (f [] e_list)

    | Rexpr_par e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_par e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
	in Rexpr_par (f [] e_list)
    | e -> e
  in { e with expr_desc = e' }


(* Translate reactive seq to combinatorial seq and set the statut of emit *)
let dynamic2static e =
  if snd e.expr_static = Def_static.Static then e
  else
    begin match e.expr_desc with
    | Rexpr_seq e_list ->
	let rec f left l =
	  match l with
	  | ({ expr_static = ctx1, Def_static.Static } as e1)
	    :: ({ expr_static = ctx2, Def_static.Static } as e2) :: l' ->
	      let e' =
		make_expr
		  (Rexpr_seq [e1;e2])
		  (Location.concat e1.expr_loc e2.expr_loc)
	      in
              assert (ctx1 = ctx2);
	      e'.expr_static <- ctx1, Def_static.Static;
	      f left (e'::l')

	  | ({ expr_desc = Rexpr_emit _ } as e1)
	    :: ({ expr_static = ctx2, Def_static.Dynamic _ } as e2) :: l' ->
              let ctx1 = fst e1.expr_static in
              let k1 = Def_static.Dynamic Def_static.Instantaneous in
	      e1.expr_static <- (ctx1, k1);
	      f (left@[e1]) (e2::l')


	  | [ { expr_static = ctx1, Def_static.Dynamic _ } as e1;
	      { expr_desc = Rexpr_emit _ } as e2 ] ->
              let ctx2 = fst e2.expr_static in
              let k2 = Def_static.Dynamic Def_static.Instantaneous in
	      e2.expr_static <- (ctx2, k2);
	      left@[e1; e2]

	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'

	  | [] -> left
	in
	begin match f [] e_list with
	| [] -> assert false
	| [e] -> e
	| e_list' ->
	    let e' = make_expr (Rexpr_seq e_list') e.expr_loc in
	    e'.expr_static <- e.expr_static;
	    e'
	end
    | Rexpr_when(_, ({ expr_desc = Rexpr_emit _ } as e2)) ->
        let ctx2 = fst e2.expr_static in
        let k2 = Def_static.Dynamic Def_static.Instantaneous in
	e2.expr_static <- (ctx2, k2);
	e
    | _ -> e
    end


(* Translate for to loop_n *)
let for2loop_n expr =
  begin match expr.expr_desc with
  | Rexpr_for(ident, e1, e2, direction_flag, e)
    when snd expr.expr_static <> Def_static.Static ->
      let fv = expr_free_vars e in
      if is_free (Varpatt_local ident) fv then
	begin
	  let n =
	    let minus =
	      make_expr
		(Rexpr_global
		   (Modules.pfind_value_desc
		      (Parse_ident.Pident ("-"))))
		Location.none
	    in
	    minus.expr_static <- (Def_static.Process, Def_static.Static);
	    minus.expr_type <-
	      Types.arrow
		Initialization.type_int
		(Types.arrow
		   Initialization.type_int
		   Initialization.type_int);
            let e' =
	      make_expr
		(Rexpr_apply
		   (minus,
		    begin match direction_flag with
		    | Upto -> [e2; e1]
		    | Downto -> [e1; e2]
		    end))
		Location.none
	    in
	    e'.expr_static <- (Def_static.Process, Def_static.Static);
	    e'.expr_type <- Initialization.type_int;
	    let plus =
	      make_expr
		(Rexpr_global
		   (Modules.pfind_value_desc
		      (Parse_ident.Pident ("+"))))
		Location.none
	    in
	    plus.expr_static <- (Def_static.Process, Def_static.Static);
	    plus.expr_type <-
	      Types.arrow
		Initialization.type_int
		(Types.arrow
		   Initialization.type_int
		   Initialization.type_int);
            let one =
	      make_expr
		(Rexpr_constant (Const_int 1))
		Location.none
	    in
	    one.expr_static <- (Def_static.Process, Def_static.Static);
	    one.expr_type <- Initialization.type_int;
	    let n =
	      make_expr
		(Rexpr_apply (plus, [e'; one]))
		Location.none
	    in
	    n.expr_static <- (Def_static.Process, Def_static.Static);
	    n.expr_type <- Initialization.type_int;
	    n
	  in
	  let loop_n =
	    make_expr
	      (Rexpr_loop(Some n, e))
	      expr.expr_loc
	  in
	  loop_n.expr_static <- expr.expr_static;
	  loop_n.expr_type <- Initialization.type_unit;
	  loop_n
	end
      else
	expr
  | _ -> expr
  end
