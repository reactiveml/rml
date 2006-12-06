(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : static.ml                                                  *)
(*  Date de creation : 25/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: static.ml,v 1.2 2005/03/14 09:58:54 mandel Exp $ *)

(* Set the Static/Dynamique status in parse_ast *)

open Asttypes
open Reac_ast
open Def_static
open Static_errors
open Annot

let id x = x


(* The unification of two static types is the maximum of the two types. *)
(* The order over static types is:                                      *)
(* Static < Dynamic _ and Instantaneous < Dontknow < Noninstantaneous   *)

let unify_instantaneous k1 k2 =
  match k1, k2 with
  | Instantaneous, Instantaneous -> Instantaneous
  | Instantaneous, k | k, Instantaneous -> k
  | Dontknow, Dontknow -> Dontknow
  | _ -> Noninstantaneous

let unify typ1 typ2 = 
  match typ1, typ2 with
  | Static, Static -> Static
  | Static, Dynamic k
  | Dynamic k, Static -> Dynamic k
  | Dynamic k1, Dynamic k2 -> 
      Dynamic (unify_instantaneous k1 k2)


let static_expr_list static_expr filter ctx l =
  match l with
  | [] -> assert false
  | [x] -> static_expr ctx (filter x)
  | x::l ->
      let ty = static_expr ctx (filter x) in
      List.fold_left
	(fun typ x -> unify typ (static_expr ctx (filter x)))
	ty l


let rec static_expr ctx e =
  let t = 
    match e.expr_desc with
    | Rexpr_local x -> Static

    | Rexpr_global x -> Static

    | Rexpr_constant im -> Static

    | Rexpr_let (Recursive, patt_expr_list, e1) ->
	if static_expr_list static_expr snd ML patt_expr_list = Static
	then static_expr ctx e1 
	else expr_wrong_static_err e
    | Rexpr_let (Nonrecursive, patt_expr_list, e1) ->
	let typ1 = static_expr_list static_expr snd ctx patt_expr_list in
	let typ2 = static_expr ctx e1 in
	unify typ1 typ2

    | Rexpr_function patt_expr_list ->
	if static_expr_list static_expr snd ML patt_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_apply (e1, expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr id ML expr_list in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_tuple expr_list ->
	if static_expr_list static_expr id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_construct (_, None) -> Static
    | Rexpr_construct (_, Some e1) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_array expr_list ->
	if static_expr_list static_expr id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record ide_expr_list ->
	if static_expr_list static_expr snd ML ide_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record_access (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_record_update (e1, _, e2) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_constraint (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Rexpr_trywith (e1, patt_expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr snd ML patt_expr_list in
	if unify typ1 typ2 = Static
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
	  unify typ2 typ3
	else expr_wrong_static_err e

    | Rexpr_match (e1, patt_expr_list) ->
	let _typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr snd ctx patt_expr_list in
	typ2

    | Rexpr_when_match (e1,e2) ->
	let _typ1 = static_expr ML e1 in
	let typ2 = static_expr ctx e2 in
	typ2

    | Rexpr_while (e1,e2) ->
	let _typ1 = static_expr ML e1 in
	let typ2 = static_expr ctx e2 in
	typ2

    | Rexpr_for (_, e1, e2, _, e3) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if unify typ1 typ2 = Static
	then static_expr ctx e3
	else expr_wrong_static_err e

    | Rexpr_fordopar (_, e1, e2, _, e3) ->
	if ctx = Process
	then
	  let typ1 = static_expr ML e1 in
	  let typ2 = static_expr ML e2 in
	  if unify typ1 typ2 = Static
	  then 
	    begin match static_expr Process e3 with
	    | Static -> Dynamic Instantaneous
	    | ty -> ty
	    end
	  else expr_wrong_static_err e
	else expr_wrong_static_err e

    | Rexpr_seq e_list ->
	static_expr_list static_expr id ctx e_list

    | Rexpr_nothing ->
	if ctx = Process
	then Dynamic Instantaneous
	else expr_wrong_static_err e

    | Rexpr_pause ->
	if ctx = Process
	then Dynamic Noninstantaneous
	else expr_wrong_static_err e

    | Rexpr_halt ->
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
	  let ty = static_expr_list static_expr id ctx e_list in
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
	  begin match unify typ1 typ2 with
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
	if unify typ1 typ2 = Static
	then typ3
	else expr_wrong_static_err e

    | Rexpr_process (p) ->
	let _typ = static_expr Process p in
	Static

    | Rexpr_run (e1) ->
	if static_expr ML e1 = Static
	then Dynamic Dontknow
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
	   unify (Dynamic Dontknow) typ1)
	else
	  expr_wrong_static_err e

    | Rexpr_control (s, p) ->
	if ctx = Process
	then 
	  (static_conf s;
	   let typ1 = static_expr Process p in
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
	   unify (Dynamic Dontknow) typ1)
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
	    unify (Dynamic Dontknow) typ
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
  Sstatic.record (Ti_expr e);
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
	static_expr_list static_expr snd ML l
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


