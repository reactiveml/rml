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

(* file: reac2reac.ml *)
(* created: 2005-09-07  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Source to source transformations *)

open Asttypes
open Reac
open Types
open Reac_utils

(*

(* Translate binary seq and par to n-ary operators *)
let binary2nary e =
  let e' =
    match e.expr_desc with
    | Eseq e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Eseq e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
      in Eseq (f [] e_list)

    | Epar e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Epar e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
	in Epar (f [] e_list)
    | e -> e
  in { e with expr_desc = e' }


(* Translate reactive seq to combinatorial seq and set the statut of emit *)
let dynamic2static e =
  if e.expr_static = Def_static.Static then e
  else
    begin match e.expr_desc with
    | Eseq e_list ->
	let rec f left l =
	  match l with
	  | ({ expr_static = Def_static.Static } as e1)
	    :: ({ expr_static = Def_static.Static } as e2) :: l' ->
	      let e' =
		make_expr
		  (Eseq [e1;e2])
		  (Location.concat e1.expr_loc e2.expr_loc)
	      in
	      e'.expr_static <- Def_static.Static;
	      f left (e'::l')

	  | ({ expr_desc = Eemit _ } as e1)
	    :: ({ expr_static = Def_static.Dynamic _ } as e2) :: l' ->
	      e1.expr_static <- Def_static.Dynamic Def_static.Instantaneous;
	      f (left@[e1]) (e2::l')


	  | [ { expr_static = Def_static.Dynamic _ } as e1;
	      { expr_desc = Eemit _ } as e2 ] ->
		e2.expr_static <- Def_static.Dynamic Def_static.Instantaneous;
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
	    let e' = make_expr (Eseq e_list') e.expr_loc in
	    e'.expr_static <- e.expr_static;
	    e'
	end
    | Ewhen(_, ({ expr_desc = Eemit _ } as e2)) ->
	e2.expr_static <- Def_static.Dynamic Def_static.Instantaneous;
	e
    | _ -> e
    end


(* Translate for to loop_n *)
let for2loop_n expr =
  begin match expr.expr_desc with
  | Efor(ident, e1, e2, direction_flag, e)
    when expr.expr_static <> Def_static.Static ->
      let fv = expr_free_vars e in
      if is_free (Vlocal ident) fv then
	begin
	  let n =
	    let minus =
	      make_expr
		(Eglobal
		   (Modules.pfind_value_desc
		      (Parse_ident.Pident ("-"))))
		Location.none
	    in
	    minus.expr_static <- Def_static.Static;
	    minus.expr_type <-
	      Types.arrow
		Initialization.type_int
		(Types.arrow
		   Initialization.type_int
		   Initialization.type_int);
            let e' =
	      make_expr
		(Eapply
		   (minus,
		    begin match direction_flag with
		    | Upto -> [e2; e1]
		    | Downto -> [e1; e2]
		    end))
		Location.none
	    in
	    e'.expr_static <- Def_static.Static;
	    e'.expr_type <- Initialization.type_int;
	    let plus =
	      make_expr
		(Eglobal
		   (Modules.pfind_value_desc
		      (Parse_ident.Pident ("+"))))
		Location.none
	    in
	    plus.expr_static <- Def_static.Static;
	    plus.expr_type <-
	      Types.arrow
		Initialization.type_int
		(Types.arrow
		   Initialization.type_int
		   Initialization.type_int);
            let one =
	      make_expr
		(Econstant (Const_int 1))
		Location.none
	    in
	    one.expr_static <- Def_static.Static;
	    one.expr_type <- Initialization.type_int;
	    let n =
	      make_expr
		(Eapply (plus, [e'; one]))
		Location.none
	    in
	    n.expr_static <- Def_static.Static;
	    n.expr_type <- Initialization.type_int;
	    n
	  in
	  let loop_n =
	    make_expr
	      (Eloop(Some n, e))
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


(* Print static information *)
let print_static e =
  Location.print_oc stderr e.expr_loc;
  prerr_string (Def_static.string_of_static e.expr_static);
  prerr_newline ();
  e


(* Check left branche of |> operator and annotate pause statement *)
let translate_merge =
  let merge_error exp =
    Printf.eprintf
      "%aThis expression is not allowed on the left of a |> operator.\n"
      Location.print_oc exp.expr_loc;
    raise Misc.Error
  in

  let annotate_pause expr =
    begin match expr.expr_desc with
    | Epause _ ->
	{ expr with expr_desc = Epause K_boi }
    | Ehalt _ ->
	{ expr with expr_desc = Ehalt K_boi }
    | Elocal _
    | Eglobal _
    | Econstant _
    | Elet _
    | Efunction _
    | Eapply _
    | Etuple _
    | Econstruct _
    | Earray _
    | Erecord _
    | Erecord_access _
    | Erecord_update _
    | Econstraint _
    | Etrywith _
    | Eassert _
    | Eifthenelse _
    | Ematch _
    | Ewhen_match _
    | Ewhile _
    | Efor _
    | Eseq _
    | Eprocess _
    | Epre _
    | Elast _
    | Edefault _
    | Enothing
    | Eemit _
    | Eloop _
    | Efordopar _
    | Epar _
    | Emerge _
    | Esignal _ ->
	expr
    | Erun _
    | Euntil _
    | Ewhen _
    | Econtrol _
    | Epresent _
    | Eawait _
    | Eawait_val _ ->
	merge_error expr
    | Eget _ ->
	merge_error expr
    end
  in
  fun expr ->
    begin match expr.expr_desc with
    | Emerge (e1, e2) ->
	{ expr with expr_desc = Emerge (expr_map annotate_pause e1, e2) }
    | _ -> expr
    end


*)
