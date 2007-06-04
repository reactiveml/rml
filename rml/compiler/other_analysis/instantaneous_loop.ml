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

(* file: instantaneous_loop.ml v.02 *)
(* created: 2006-08-29  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Print a warning if an expression may have an instantaneous loop or    *)
(* an instantaneous recursion.                                           *)
(* This analysis must be used after the static analysis.                 *)

open Misc
open Asttypes
open Reac_ast
open Reac_misc
open Def_static


module Env :
    sig
      type key = varpatt
      type t
      val empty: t
      val create: key -> int -> t
      val equal: t -> t -> bool
      val get: t -> key -> int option
      val add: t -> key -> int -> t
      val remove: t -> key -> t
      val plus: t -> int -> t
      val positive: t -> bool
      val min: t -> int option
      val append: t -> t -> t
      val string_of_t: t -> string
    end  =
  struct
    type key = varpatt
    type t = (key * int) list
		      
    let string_of_t env =
      "{ "^
      (List.fold_left 
	 (fun s (x,n) ->
	   (string_of_varpatt x)^
	   ":"^
	   (string_of_int n)^
	   "; "^ 
	   s)
	 "" env)^
      "}"

    let same key1 key2 =
      begin match key1, key2 with
      | Varpatt_local id1, Varpatt_local id2 -> 
	  Ident.same id1 id2
      | Varpatt_global gl1, Varpatt_global gl2 -> 
	  Global_ident.same gl1.Global.gi gl2.Global.gi
      | _ -> false
      end

    let empty = []

    let create x n = [(x,n)]

    let rec equal env1 env2 =
      begin match env1, env2 with
      | [], [] -> true
      | [], _ | _, [] -> false
      | (x,n)::env1', _ ->
	  if List.exists (fun (x',n') -> n = n' & same x x') env2 then
	    equal env1' env2
	  else false
      end
 
    let get env x =
      try
	let (x', n) = List.find (fun (y,_) -> same x y) env in 
	Some n 
      with Not_found -> None

    let add =
      let rec add acc env x n  =
	begin match env with
	| [] -> (x,n) :: acc
	| (x',n'):: env' -> 
	    if same x x' then
	      List.rev_append acc ((x, min n n') :: env')
	    else
	      add ((x',n')::acc) env' x n
	end
      in 
      add []

    let remove =
      let rec remove acc env x  =
	begin match env with
	| [] -> acc
	| (x',n'):: env' -> 
	    if same x x' then
	      List.rev_append acc env'
	    else
	      remove ((x',n')::acc) env' x
	end
      in 
      remove []
	
    let plus env i =
      List.rev_map (fun (x,n) -> (x,n+i)) env

    let positive env =
      List.for_all (fun (_,n) -> n > 0) env

    let min env =
      begin match env with
      | [] -> None
      | (_,n)::env' -> 
	  Some (List.fold_left (fun res (_,n) -> min res n) n env')
      end

    let append env1 env2 =
      List.fold_left
        (fun env (x,n) -> add env x n)
	env2 env1

  end


(* Warnings *)
let rec_warning expr =
  Printf.eprintf "%aWarning: This expression may produce an instantaneous recursion.\n"
    Location.print_oc expr.expr_loc

let loop_warning expr =
  Printf.eprintf "%aWarning: This expression may be an instantaneous loop.\n"
    Location.print_oc expr.expr_loc


let static_of_list filter l =
  begin match l with
  | [] -> assert false
  | [x] -> filter x
  | x :: l ->
      let typ = filter x in
      List.fold_left
	(fun typ x -> Static.max typ (filter x))
	typ l
  end

let id x = x

(* Build the product type of a list of expressions *)
let instantaneous_loop_expr_list analyse filter vars list =
  List.fold_left
    (fun ty_res x -> 
      let e = filter x in
      let ty_e = analyse vars e in
      Env.append ty_e ty_res)
    Env.empty
    list


(* Analyse an expression *)
let instantaneous_loop_expr =
  let rec analyse vars expr = 
    let env = 
      begin match expr.expr_desc with
      | Rexpr_local x ->
	  let id = Varpatt_local x in
	  begin match Env.get vars id with
	  | Some n -> 
	      if n <= 0 then rec_warning expr;
	      Env.create id n
	  | None -> 
	      Env.empty
	  end

	    
      | Rexpr_global x ->
	  let id = Varpatt_global x in
	  begin match Env.get vars id with
	  | Some n -> 
	      if n <= 0 then rec_warning expr;
	      Env.create id n
	  | None -> Env.empty
	  end
	    
      | Rexpr_constant _ -> Env.empty
	    
      | Rexpr_let (Nonrecursive, patt_expr_list, expr) ->
	  let patt_ty_list =
	    List.map 
	      (fun (p, e) -> 
		let patt =  vars_of_patt p in 
		let ty = analyse vars e in
		(patt, ty))
	      patt_expr_list
	  in
	  begin match 
	    static_of_list (fun (_, e) -> e.expr_static) patt_expr_list 
	  with
	  | Dynamic Noninstantaneous -> 
	      let _ = analyse Env.empty expr in
	      List.fold_left 
		(fun ty_res (_, ty) -> Env.append ty ty_res)
		Env.empty
		patt_ty_list
	  | _ -> 
	      let (vars', ty_prod, patt_list) =
		List.fold_left 
		  (fun (vars, ty_res, var_list) (p_list, ty) ->
		    begin match Env.min ty with
		    | None | Some 0 ->
	  	    (* if min(ty) = 0 we don't have to repeate the warning *)
			(vars, 
			 Env.append ty ty_res, 
			 List.rev_append p_list var_list)
		    | Some n -> 
			let vars' =
			  List.fold_left
			    (fun vars x -> Env.add vars x n)
			    vars p_list
			in
			(vars', 
			 Env.append ty ty_res,
			 List.rev_append p_list var_list)
		    end)
		  (vars, Env.empty, [])
		  patt_ty_list
	      in
	      let ty_expr = analyse vars' expr in
	      let ty_expr' = List.fold_left Env.remove ty_expr patt_list in
	      Env.append ty_expr' ty_prod
	  end 
      | Rexpr_let (Recursive, patt_expr_list, expr) ->
	  let rec_patt, vars' =
	    List.fold_left
	      (fun (rec_patt', vars') (p, _) -> 
		let rec_patt = vars_of_patt p in
		let vars'' = 
		  List.fold_left
		    (fun vars'' x -> Env.add vars'' x 0)
		    vars'
		    rec_patt
		in
		(List.rev_append rec_patt rec_patt', vars''))
	      ([], vars)
	      patt_expr_list
	  in

	  let patt_ty_list =
	    List.map 
	      (fun (p, e) -> 
		let patt =  vars_of_patt p in 
		let ty = analyse vars' e in
		(patt, ty))
	      patt_expr_list
	  in
	  begin match 
	    static_of_list (fun (_, e) -> e.expr_static) patt_expr_list 
	  with
	  | Dynamic Noninstantaneous -> 
	      let _ = analyse Env.empty expr in
	      List.fold_left 
		(fun ty_res (_, ty) -> Env.append ty ty_res)
		Env.empty
		patt_ty_list
	  | _ -> 
	      let (vars'', ty_prod) =
		List.fold_left 
		  (fun (vars, ty_res) (p_list, ty) ->
		    let ty' =
		      List.fold_left
			Env.remove
			ty
			p_list
		    in
		    begin match Env.min ty' with
		    | None | Some 0 -> 
			(vars, Env.append ty' ty_res)
		    | Some n -> 
			let vars' =
			  List.fold_left
			    (fun vars x -> Env.add vars x n)
			    vars p_list
			in
			(vars', Env.append ty' ty_res)
		    end)
		  (vars, Env.empty)
		  patt_ty_list
	      in
	      let ty_expr = analyse vars'' expr in
	      let ty_let = Env.append ty_expr ty_prod in
	      List.fold_left Env.remove ty_let rec_patt
	  end 
	    
      | Rexpr_function patt_expr_list ->
	  let vars' = Env.plus vars 1 in
	  instantaneous_loop_expr_list analyse snd vars' patt_expr_list
	    
      | Rexpr_apply (e, expr_list) ->
	  let ty = analyse vars e in
	  let ty' = Env.plus ty (- List.length expr_list) in
(* 	if not (Env.positive ty') then rec_warning expr; *)
	  let ty_args = instantaneous_loop_expr_list analyse id vars expr_list in
	  if not (Env.equal Env.empty ty_args) then rec_warning expr;
	  ty'
	    
      | Rexpr_tuple expr_list ->
	  instantaneous_loop_expr_list analyse id vars expr_list
	    
      | Rexpr_construct (const, None) -> Env.empty

      | Rexpr_construct (const, Some e) -> analyse vars e
	    
      | Rexpr_array expr_list ->
	  instantaneous_loop_expr_list analyse id vars expr_list
	    
      | Rexpr_record lbl_expr_list ->
	  instantaneous_loop_expr_list analyse snd vars lbl_expr_list
	    
      | Rexpr_record_access (e, lbl) -> 
	  analyse vars e
	    
      | Rexpr_record_update (e1, lbl, e2) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  Env.append ty1 ty2
	    
      | Rexpr_constraint (e, ty) ->
	  analyse vars e
	    
      | Rexpr_trywith (e, patt_expr_list) ->
	  let ty = analyse vars e in
	  let ty' = 
	    instantaneous_loop_expr_list analyse snd vars patt_expr_list 
	  in
	  Env.append ty' ty
	    
      | Rexpr_assert e ->
	  analyse vars e
	    
      | Rexpr_ifthenelse(e,e1,e2) ->
	  let ty = analyse vars e in
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  Env.append ty (Env.append ty1 ty2)
	    
      | Rexpr_match (e, patt_expr_list) ->
	  let ty = analyse vars e in
	  let ty' =
	    instantaneous_loop_expr_list analyse snd vars patt_expr_list
	  in
	  Env.append ty ty'
	    
      | Rexpr_when_match (e1,e2) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  Env.append ty1 ty2
	    
      | Rexpr_while (e1,e2) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  Env.append ty1 ty2
	    
      | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  let ty = analyse vars e in
	  Env.append ty1 (Env.append ty2 ty)
	    
      | Rexpr_seq e_list ->
	  let _, ty = 
	    List.fold_left
	      (fun (delayed, ty_res) e -> 
		if delayed then 
		  let _ty = analyse Env.empty e in
		  (true, ty_res)
		else 
		  let ty = analyse vars e in
		  (e.expr_static = Dynamic Noninstantaneous, 
		   Env.append ty ty_res))
	      (false, Env.empty) e_list 
	  in ty
	    
      | Rexpr_process e ->
	  analyse (Env.plus vars 1) e
	    
      | Rexpr_pre (pre_kind, e) ->
	  analyse vars e

      | Rexpr_last e ->
	  analyse vars e

      | Rexpr_default e ->
	  analyse vars e
	    
      | Rexpr_nothing -> Env.empty
	    
      | Rexpr_pause -> Env.empty

      | Rexpr_halt -> Env.empty
	    
      | Rexpr_emit (e, None) ->
	  analyse vars e
	    
      | Rexpr_emit (e1, Some e2) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  if not (Env.equal Env.empty ty2) then rec_warning expr;
	  ty1
	    
      | Rexpr_loop (None, e) ->
	  if e.expr_static <> Dynamic Noninstantaneous then loop_warning expr;
	  analyse vars e

      | Rexpr_loop (Some n, e) ->
	  let ty_n = analyse vars n in
	  let ty = analyse vars e in
	  Env.append ty_n ty
	    
      | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  let ty = analyse vars e in
	  Env.append ty1 (Env.append ty2 ty)
	    
      | Rexpr_par e_list ->
	  instantaneous_loop_expr_list analyse id vars e_list
	    
      | Rexpr_merge (e1, e2) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  Env.append ty1 ty2
	    
      | Rexpr_signal ((ident, tyexpr_opt), None, e) ->
	  analyse vars e
      | Rexpr_signal ((ident, tyexpr_opt), Some(e1,e2), e) ->
	  let ty1 = analyse vars e1 in
	  let ty2 = analyse vars e2 in
	  let ty2' = Env.plus ty2 (-2) in
	  if not (Env.positive ty2') then rec_warning e2;
	  let ty = analyse vars e in
	  Env.append ty1 (Env.append ty2' ty)
	    
      | Rexpr_run e ->
	  let ty = analyse vars e in
	  let ty' = Env.plus ty (-1) in
	  if not (Env.positive ty') then rec_warning expr;
	  ty'
	    
      | Rexpr_until (config, e, None) ->
	  let ty_config = config_analyse vars config in
	  let ty = analyse vars e in
	  Env.append ty_config ty
      | Rexpr_until (config, e, Some(p,e1)) ->
	  let ty_config = config_analyse vars config in
	  let ty = analyse vars e in
	  let _ = analyse Env.empty e1 in
	  Env.append ty_config ty

      | Rexpr_when (config, e) ->
	  let ty_config = config_analyse vars config in
	  let ty = analyse vars e in
	  Env.append ty_config ty
	    
      | Rexpr_control (config, e) ->
	  let ty_config = config_analyse vars config in
	  let ty = analyse vars e in
	  Env.append ty_config ty
	    
      | Rexpr_get (e,patt,e1) ->
	  let ty = analyse vars e in
	  let _ = analyse Env.empty e1 in
	  ty
	    
      | Rexpr_present (config, e1, e2) ->
	  let ty_config = config_analyse vars config in
	  let ty1 = analyse vars e1 in
	  let _ = analyse Env.empty e2 in 
	  Env.append ty_config ty1

      | Rexpr_await (immediate_flag, config) ->
	  config_analyse vars config
	    
      | Rexpr_await_val (Immediate, One, e, patt, e1) ->
	  let ty = analyse vars e in
	  let ty1 = analyse vars e1 in
	  Env.append ty ty1
      | Rexpr_await_val (immediate, kind, e, patt, e1) ->
	  let ty = analyse vars e in
	  let _ = analyse Env.empty e1 in
	  ty
      end
    in
(*     Printf.printf "%a : %s\n" *)
(*       Location.print_oc expr.expr_loc *)
(*       (Env.string_of_t env); *)
    env

    and config_analyse vars config =
      match config.conf_desc with
      | Rconf_present e -> 
	  analyse vars e

      | Rconf_and (c1, c2) ->
	  let ty1 = config_analyse vars c1 in
	  let ty2 = config_analyse vars c2 in
	  Env.append ty1 ty2

      | Rconf_or (c1, c2) ->
	  let ty1 = config_analyse vars c1 in
	  let ty2 = config_analyse vars c2 in
	  Env.append ty1 ty2
    in
    analyse 

      
let instantaneous_loop impl =
  match impl.impl_desc with
  | Rimpl_expr e -> 
      let _ty = instantaneous_loop_expr Env.empty e in ()

  | Rimpl_let (Nonrecursive, l) -> 
      let _ty = 
	instantaneous_loop_expr_list instantaneous_loop_expr snd Env.empty l
      in 
      ()
  | Rimpl_let (Recursive, l) -> 
      let vars =
	List.fold_left
	  (fun vars' (p, _) -> 
	    let rec_vars = vars_of_patt p in
	    List.fold_left
	      (fun vars'' x -> Env.add vars'' x 0)
	      vars'
	      rec_vars)
	  Env.empty
	  l
      in
      let _ty = 
	instantaneous_loop_expr_list instantaneous_loop_expr snd vars l 
      in 
      ()

  | Rimpl_signal (s_list) ->
      List.iter
	(fun (_, combine) ->
	  match combine with 
	  | Some(e1,e2) ->
	      let _ty1 = instantaneous_loop_expr Env.empty e1 in
	      let _ty2 = instantaneous_loop_expr Env.empty e2 in
	      ()
	  | None -> ())
	s_list

  | _ -> ()
