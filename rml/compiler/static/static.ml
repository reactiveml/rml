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

(* file: static.ml *)
(* created: 2004-04-25  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Set the Static/Dynamique status in parse_ast *)

open Asttypes
open Reac_ast
open Def_static
open Static_errors

let id x = x

exception Unify_static of instantaneous * instantaneous

(* Subtyping relation:                                                  *)
(* Instantaneous and Noninstantaneous are more precise than Dontknow    *)

let unify_instantaneous expected_k actual_k =
  match expected_k, actual_k with
  | Instantaneous, Instantaneous -> Instantaneous
  | Noninstantaneous, Noninstantaneous -> Noninstantaneous
  | Dontknow, Dontknow -> Dontknow
  | _ ->
      (* XXX !!!! TODO !!!! XXX *)
      Dontknow
      (* raise (Unify_static (actual_k, expected_k)) *)

let unify expected_k actual_k =
  match expected_k, actual_k with
  | Static, Static -> Static
  | Static, Dynamic k ->
      Dynamic (unify_instantaneous Instantaneous k)
  | Dynamic k, Static ->
      Dynamic (unify_instantaneous k Instantaneous)
  | Dynamic k1, Dynamic k2 ->
      Dynamic (unify_instantaneous k1 k2)


(* The maximum of the two types.                                        *)
(* The order over static types is:                                      *)
(* Static < Dynamic _ and Instantaneous < Dontknow < Noninstantaneous   *)

let max_instantaneous k1 k2 =
  match k1, k2 with
(*   | Instantaneous, Instantaneous -> Instantaneous *)
  | Instantaneous, k | k, Instantaneous -> k
  | Dontknow, Dontknow -> Dontknow
  | _ -> Noninstantaneous

let max typ1 typ2 =
  match typ1, typ2 with
  | Static, Static -> Static
  | Static, Dynamic k
  | Dynamic k, Static -> Dynamic k
  | Dynamic k1, Dynamic k2 ->
      Dynamic (max_instantaneous k1 k2)


(* Compress type path *)
let rec get_type ty =
  match ty.Def_types.type_desc with
  | Def_types.Type_link (ty') -> get_type ty'
  | _ -> ty

(* Extract the instantaneous status of a process *)
let rec get_process_status =
  let rec aux x =
    match x with
    | Def_types.Proc_def k -> !k
    | Def_types.Proc_link y -> aux y
    | Def_types.Proc_unify (x1, x2) ->
	let k1 = aux x1 in
	let k2 = aux x2 in
	unify_instantaneous k1 k2
  in
  fun ty ->
    match (get_type ty).Def_types.type_desc with
    | Def_types.Type_process (_, pi) ->
	begin match pi.Def_types.proc_static with
	| None -> Dynamic Dontknow
	| Some k -> Dynamic (aux k)
	end
    | _ -> assert false


let static_expr_list static_expr combine filter ctx l =
  match l with
  | [] -> Static
  | [x] -> static_expr ctx (filter x)
  | x::l ->
      let ty = static_expr ctx (filter x) in
      List.fold_left
	(fun typ x -> combine (* max *) typ (static_expr ctx (filter x)))
	ty l


let rec static_expr ctx e =
  let t =
    match e.expr_desc with
    | Rexpr_local x -> Static

    | Rexpr_global x -> Static

    | Rexpr_constant im -> Static

    | Rexpr_let (Recursive, patt_expr_list, e1) ->
	if static_expr_list static_expr max snd ML patt_expr_list = Static
	then static_expr ctx e1
	else expr_wrong_static_err e
    | Rexpr_let (Nonrecursive, patt_expr_list, e1) ->
	let typ1 = static_expr_list static_expr max snd ctx patt_expr_list in
	let typ2 = static_expr ctx e1 in
	max typ1 typ2

    | Rexpr_function patt_expr_list ->
	if static_expr_list static_expr max snd ML patt_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_apply (e1, expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr max id ML expr_list in
	if max typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_tuple expr_list ->
	if static_expr_list static_expr max id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_construct (_, None) -> Static
    | Rexpr_construct (_, Some e1) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_array expr_list ->
	if static_expr_list static_expr max id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record ide_expr_list ->
	if static_expr_list static_expr max snd ML ide_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record_access (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record_update (e1, _, e2) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if max typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_constraint (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_trywith (e1, patt_expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr max snd ML patt_expr_list in
	if max typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_assert e1 ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_ifthenelse (e1, e2, e3) ->
	if static_expr ML e1 = Static
	then
	  let typ2 = static_expr ctx e2 in
	  let typ3 = static_expr ctx e3 in
	  begin match typ2, typ3 with
	  | Static, Static -> Static
	  | Static, Dynamic Instantaneous
	  | Dynamic Instantaneous, Static -> Dynamic Instantaneous
	  | Static, Dynamic _
	  | Dynamic _, Static -> Dynamic Dontknow
	  | Dynamic Instantaneous, Dynamic Instantaneous ->
	      Dynamic Instantaneous
	  | Dynamic Noninstantaneous, Dynamic Noninstantaneous ->
	      Dynamic Noninstantaneous
	  | Dynamic _, Dynamic _ -> Dynamic Dontknow
	  end
	else expr_wrong_static_err e

    | Rexpr_match (e1, patt_expr_list) ->
	let typ1 = static_expr ML e1 in
	if typ1 <> Static then expr_wrong_static_err e1;
	let typ2 =
	  let combine typ1 typ2 =
	    begin match typ1, typ2 with
	    | Static, Static -> Static
	    | Static, Dynamic Instantaneous
	    | Dynamic Instantaneous, Static -> Dynamic Instantaneous
	    | Static, Dynamic _
	    | Dynamic _, Static -> Dynamic Dontknow
	    | Dynamic Instantaneous, Dynamic Instantaneous ->
		Dynamic Instantaneous
	    | Dynamic Noninstantaneous, Dynamic Noninstantaneous ->
		Dynamic Noninstantaneous
	    | Dynamic _, Dynamic _ -> Dynamic Dontknow
	    end
	  in
	  static_expr_list static_expr combine snd ctx patt_expr_list
	in
	typ2

    | Rexpr_when_match (e1,e2) ->
	let _typ1 = static_expr ML e1 in
	let typ2 = static_expr ctx e2 in
	begin match ctx with
	| ML -> typ2
	| Process ->
	    begin match typ2 with
	    | Static -> Dynamic Instantaneous
	    | Dynamic _ -> typ2
	    end
	end

    | Rexpr_while (e1,e2) ->
	let _typ1 = static_expr ML e1 in
	let typ2 = static_expr ctx e2 in
	typ2

    | Rexpr_for (_, e1, e2, dir, e3) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if max typ1 typ2 = Static
	then
	  begin match static_expr ctx e3 with
	  | Dynamic Noninstantaneous ->
	      begin match e1.expr_desc, e2.expr_desc with
	      | Rexpr_constant (Const_int n1),
		Rexpr_constant (Const_int n2) ->
		  let cmp =
		    begin match dir with
		    | Upto -> (<=)
		    | Downto -> (>=)
		    end
		  in
		  if cmp n1 n2 then Dynamic Noninstantaneous
		  else Dynamic Dontknow
	      | _ ->
		  Dynamic Dontknow
	      end
	  | ty -> ty
	  end
	else expr_wrong_static_err e

    | Rexpr_fordopar (_, e1, e2, dir, e3) ->
	if ctx = Process
	then
	  let typ1 = static_expr ML e1 in
	  let typ2 = static_expr ML e2 in
	  if max typ1 typ2 = Static
	  then
	    begin match static_expr Process e3 with
	    | Static -> Dynamic Instantaneous
	    | Dynamic Noninstantaneous ->
		begin match e1.expr_desc, e2.expr_desc with
		| Rexpr_constant (Const_int n1),
		  Rexpr_constant (Const_int n2) ->
		    let cmp =
		      begin match dir with
		      | Upto -> (<=)
		      | Downto -> (>=)
		      end
		    in
		    if cmp n1 n2 then Dynamic Noninstantaneous
		    else Dynamic Dontknow
		| _ ->
		    Dynamic Dontknow
		end
	    | ty -> ty
	    end
	  else expr_wrong_static_err e
	else expr_wrong_static_err e

    | Rexpr_seq e_list ->
	static_expr_list static_expr max id ctx e_list

    | Rexpr_nothing ->
	if ctx = Process
	then Dynamic Instantaneous
	else expr_wrong_static_err e

    | Rexpr_pause _ ->
	if ctx = Process
	then Dynamic Noninstantaneous
	else expr_wrong_static_err e

    | Rexpr_halt _ ->
	if ctx = Process
	then Dynamic Noninstantaneous
	else expr_wrong_static_err e

    | Rexpr_emit (s, None) ->
	if static_expr ML s = Static
	then Static
	else expr_wrong_static_err s

    | Rexpr_emit (s, Some e1) ->
	if static_expr ML s = Static
	then
	  if static_expr ML e1 = Static
	  then Static
	  else expr_wrong_static_err e1
	else expr_wrong_static_err s

    | Rexpr_loop (None, e1) ->
	if ctx = Process
	then
(*
	  begin match static_expr Process e1 with
	  | Static -> Dynamic Instantaneous
	  | ty -> ty
	  end
*)
	  let _ty = static_expr Process e1 in
	  Dynamic Noninstantaneous
	else
	  expr_wrong_static_err e

    | Rexpr_loop (Some n, e1) ->
	if static_expr ML n = Static
	then
	  if ctx = Process
	  then
	    begin match static_expr Process e1 with
	    | Static -> Dynamic Instantaneous
	    | ty -> ty
	    end
	  else
	    expr_wrong_static_err e
	else expr_wrong_static_err n

    | Rexpr_par e_list ->
	if ctx = Process
	then
	  let ty = static_expr_list static_expr max id ctx e_list in
	  begin match ty with
	  | Static -> Dynamic Instantaneous
	  | _ -> ty
	  end
	else
	  expr_wrong_static_err e

    | Rexpr_merge (e1,e2) ->
	if ctx = Process
	then
	  let typ1 = static_expr ctx e1 in
	  let typ2 = static_expr ctx e2 in
	  begin match max typ1 typ2 with
	  | Static -> Dynamic Instantaneous
	  | ty -> ty
	  end
	else
	  expr_wrong_static_err e

    | Rexpr_signal (_, None, p) ->
	static_expr ctx p

    | Rexpr_signal (_, Some(e1,e2), p) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	let typ3 = static_expr ctx p in
	if max typ1 typ2 = Static
	then typ3
	else expr_wrong_static_err e

    | Rexpr_process (p) ->
	let typ = static_expr Process p in
	let k =
	  match typ with
	  | Static -> Instantaneous
	  | Dynamic(k) -> k
	in
	begin match (get_type e.expr_type).Def_types.type_desc with
	| Def_types.Type_process
	    (t, { Def_types.proc_static = Some (Def_types.Proc_def r) }) ->
	    r := k
	| Def_types.Type_process
	    (t, { Def_types.proc_static = Some
		    (Def_types.Proc_unify
		       (Def_types.Proc_def r1,
			Def_types.Proc_def r2)) }) ->
	    begin try
	      r1 := unify_instantaneous !r2 k
	    with
	    | Unify_static (k1, k2) -> unify_err e k1 k2
	    end
	| _ ->
	    (* XXX !!! TODO !!! XXX *)
	    (* raise (Misc.Internal (e.expr_loc, "Static.static_expr")) *)
	    ()
	end;
	Static

    | Rexpr_run (e1) ->
	if static_expr ML e1 = Static
	then
	  try
	    (* XXX !!! TODO !!! XXX *)
            Dynamic Dontknow
(*	    get_process_status e1.expr_type *)
	  with
	  | Unify_static (k1, k2) -> unify_err e1 k2 k1
        (* Dynamic Dontknow *)
	else expr_wrong_static_err e

    | Rexpr_async (s, e1) ->
	if static_expr ML e1 = Static
	then
	  try
	    (* XXX !!! TODO !!! XXX *)
            Dynamic Dontknow
(*	    get_process_status e1.expr_type *)
	  with
	  | Unify_static (k1, k2) -> unify_err e1 k2 k1
        (* Dynamic Dontknow *)
	else expr_wrong_static_err e

    | Rexpr_until (s, p, p_e_opt) ->
	if ctx = Process
	then
	  (static_conf s;
	   let typ1 = static_expr Process p in
	   let _typ2_opt =
	     Misc.opt_map (fun (_,e) -> static_expr Process e) p_e_opt
	   in
	   begin match typ1 with
	   | Static -> Dynamic Instantaneous
	   | _ -> typ1
	   end)
	else
	  expr_wrong_static_err e

    | Rexpr_when (s, p) ->
	if ctx = Process
	then
	  (static_conf s;
	   let typ1 = static_expr Process p in
	   max (Dynamic Dontknow) typ1)
	else
	  expr_wrong_static_err e

    | Rexpr_control (s, p_e_opt, p) ->
	if ctx = Process
	then
	  (static_conf s;
	   let typ1 = static_expr Process p in
	   Misc.opt_iter
	     (fun (_,e) ->
	       if static_expr ML e <> Static
	       then expr_wrong_static_err e)
	     p_e_opt;
	   begin match typ1 with
	   | Static -> Dynamic Instantaneous
	   | _ -> typ1
	   end)
	else
	  expr_wrong_static_err e

    | Rexpr_present (s, p1, p2) ->
	if ctx = Process
	then
	  (static_conf s;
	   let typ1 = static_expr ctx p1 in
	   let _typ2 = static_expr ctx p2 in
	   max (Dynamic Dontknow) typ1)
 	else
	  expr_wrong_static_err e

    | Rexpr_await (Immediate, s) ->
	if ctx = Process
	then
	  (static_conf s;
	   Dynamic Dontknow)
	else expr_wrong_static_err e
    | Rexpr_await (Nonimmediate, s) ->
	if ctx = Process
	then
	  (static_conf s;
	   Dynamic Noninstantaneous)
	else expr_wrong_static_err e

    | Rexpr_await_val (Immediate, One, s, _, p) ->
	if ctx = Process
	then
	  if static_expr ML s = Static
	  then
	    let typ = static_expr Process p in
	    max (Dynamic Dontknow) typ
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e
    | Rexpr_await_val (_, _, s, _, p) ->
	if ctx = Process
	then
	  if static_expr ML s = Static
	  then
	    let _typ1 = static_expr Process p in
	    Dynamic Noninstantaneous
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e

    | Rexpr_pre (_, s) ->
	if static_expr ML s = Static
	then Static
 	else expr_wrong_static_err s

    | Rexpr_last s ->
	if static_expr ML s = Static
	then Static
 	else expr_wrong_static_err s

    | Rexpr_default s ->
	if static_expr ML s = Static
	then Static
 	else expr_wrong_static_err s

    | Rexpr_get (s, _, p) ->
 	if ctx = Process
	then
	  if static_expr ML s = Static
	  then
	    let _typ = static_expr ctx p in
	    Dynamic Noninstantaneous
	  else expr_wrong_static_err s
 	else
	  expr_wrong_static_err p
  in
  e.expr_static <- t;
  t

and static_conf conf =
  let t =
    match conf.conf_desc with
    | Rconf_present e ->
	if static_expr ML e = Static
	then ()
	else expr_wrong_static_err e

    | Rconf_and (c1, c2) ->
	static_conf c1;
	static_conf c2

    | Rconf_or (c1, c2) ->
	static_conf c1;
	static_conf c2
  in
  t


let static info_chan impl =
  let typ =
    match impl.impl_desc with
    | Rimpl_expr e -> static_expr ML e
    | Rimpl_let (_, l) ->
	static_expr_list static_expr max snd ML l
    | Rimpl_signal (s_list) ->
	List.iter
	  (fun (_, combine) ->
	    match combine with
	    | Some(e1,e2) ->
		if (static_expr ML e1) <> Static
		then expr_wrong_static_err e1
		else
		  if (static_expr ML e2) <> Static
		  then expr_wrong_static_err e2
		  else ()
	    | None -> ())
	  s_list;
	Static
    | _ -> Static
  in
  if typ <> Static then impl_wrong_static_err impl


