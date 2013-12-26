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

(* file: reac2lk.ml *)
(* created: 2004-08-09  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* The translation of Reac to Lk *)

open Asttypes
open Def_static
open Reac_ast
open Lk_ast
open Misc


let make_expr e loc =
  { kexpr_desc = e;
    kexpr_loc = loc; }

let make_proc e loc =
  { kproc_desc = e;
    kproc_loc = loc; }

let make_patt p loc =
  { kpatt_desc = p;
    kpatt_loc = loc; }

let make_te t loc =
  { kte_desc = t;
    kte_loc = loc; }

let make_conf c loc =
  { kconf_desc = c;
    kconf_loc = loc; }

let make_impl it loc =
  { kimpl_desc = it;
    kimpl_loc = loc; }

let make_intf it loc =
  { kintf_desc = it;
    kintf_loc = loc; }

let make_var s = Ident.create Ident.gen_var s Ident.Internal

(* Translation of type expressions *)
let rec translate_te typ =
  let ktyp =
    match typ.te_desc with
    | Rtype_var x -> Ktype_var x
    | Rtype_arrow (t1, t2) ->
	Ktype_arrow (translate_te t1, translate_te t2)
    | Rtype_product typ_list ->
	Ktype_product (List.map translate_te typ_list)
    | Rtype_constr (cstr, te_list) ->
	Ktype_constr (cstr, List.map translate_te te_list)
    | Rtype_process (t, _) ->
	Ktype_process (translate_te t)
  in
  make_te ktyp typ.te_loc


(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Rtype_abstract -> Ktype_abstract
  | Rtype_rebind typ -> Ktype_rebind (translate_te typ)
  | Rtype_variant constr_te_list ->
      let l =
	List.map
	  (fun (c, typ_opt) ->
	    let typ_opt =
	      match typ_opt with
	      | None -> None
	      | Some typ -> Some (translate_te typ)
	    in
	    (c, typ_opt))
	  constr_te_list
      in
      Ktype_variant l
  | Rtype_record l ->
      let l =
	List.map
	  (fun (lab, flag, typ) ->
	    (lab, flag, translate_te typ))
	  l
      in
      Ktype_record l

(* Translation of a pattern *)
let rec translate_pattern p =
  let kpatt =
    match p.patt_desc with
    | Rpatt_any -> Kpatt_any

    | Rpatt_var x ->
	begin
	  match x with
	  | Varpatt_global gl -> Kpatt_var (Kvarpatt_global gl)
	  | Varpatt_local id -> Kpatt_var (Kvarpatt_local id)
	end

    | Rpatt_alias (patt, x) ->
	let vp =
	  match x with
	  | Varpatt_global gl -> Kvarpatt_global gl
	  | Varpatt_local id ->  Kvarpatt_local id
	in
	Kpatt_alias (translate_pattern patt, vp)

    | Rpatt_constant im -> Kpatt_constant im

    | Rpatt_tuple l ->
	Kpatt_tuple (List.map translate_pattern l)

    | Rpatt_construct (constr, patt_opt) ->
	Kpatt_construct (constr, opt_map translate_pattern patt_opt)

    | Rpatt_or (p1, p2) ->
	Kpatt_or (translate_pattern p1, translate_pattern p2)

    | Rpatt_record l ->
	Kpatt_record (List.map (fun (l,p) -> (l,translate_pattern p)) l)

    | Rpatt_array l ->
	Kpatt_array (List.map translate_pattern l)

    | Rpatt_constraint (patt, typ) ->
	Kpatt_constraint (translate_pattern patt, translate_te typ)

  in
  make_patt kpatt p.patt_loc

(* Translation of ML expressions *)
let rec translate_ml e =
  let kexpr =
    match e.expr_desc with
    | Rexpr_local id -> Kexpr_local id

    | Rexpr_global gl -> Kexpr_global gl

    | Rexpr_constant im -> Kexpr_constant im

    | Rexpr_let (flag, patt_expr_list, expr) ->
	Kexpr_let (flag,
		   List.map
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     patt_expr_list,
		   translate_ml expr)

    | Rexpr_function  patt_expr_list ->
        Kexpr_function
          (List.map
             (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
             patt_expr_list)

    | Rexpr_apply (expr, expr_list) ->
	Kexpr_apply (translate_ml expr,
		     List.map translate_ml expr_list)

    | Rexpr_tuple expr_list ->
	Kexpr_tuple (List.map translate_ml expr_list)

    | Rexpr_construct (c, expr_opt) ->
	Kexpr_construct (c, opt_map translate_ml expr_opt)

    | Rexpr_array l ->
	Kexpr_array (List.map translate_ml l)

    | Rexpr_record l ->
	Kexpr_record (List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Rexpr_record_access (expr, label) ->
	Kexpr_record_access (translate_ml expr, label)

    | Rexpr_record_with (expr, l) ->
	Kexpr_record_with (translate_ml expr,
                           List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Rexpr_record_update (e1, label, e2) ->
	Kexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Rexpr_constraint (expr, typ) ->
	Kexpr_constraint (translate_ml expr, translate_te typ)

    | Rexpr_trywith (expr, l) ->
        Kexpr_trywith
          (translate_ml expr,
           List.map
             (fun (p,when_opt,e) ->
               translate_pattern p,
               opt_map translate_ml when_opt,
               translate_ml e)
             l)

    | Rexpr_assert expr -> Kexpr_assert (translate_ml expr)

    | Rexpr_ifthenelse (e1, e2, e3) ->
	Kexpr_ifthenelse (translate_ml e1,
			  translate_ml e2,
			  translate_ml e3)

    | Rexpr_match (expr, l) ->
	Kexpr_match (translate_ml expr,
		     List.map
                       (fun (p,when_opt,e) ->
                         translate_pattern p,
                         opt_map translate_ml when_opt,
                         translate_ml e)
		       l)

    | Rexpr_while(e1, e2) ->
	Kexpr_while (translate_ml e1, translate_ml e2)

    | Rexpr_for (id, e1, e2, flag, e3) ->
	Kexpr_for (id,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_ml e3)

    | Rexpr_seq (e1::e_list) ->
	let rec f acc l =
	  match l with
	  | [] -> assert false
	  | [e] ->
	      Kexpr_seq (acc, translate_ml e)
	  | e::l' ->
	      let acc' =
		make_expr
		  (Kexpr_seq (acc, translate_ml e))
		  Location.none
	      in
	      f acc' l'
	in f (translate_ml e1) e_list

    | Rexpr_process (p) ->
	let id = make_var "k" in
	let ctrl = make_var "ctrl" in
	let k_var = make_proc (Kproc_var id) Location.none in
	Kexpr_process (id, ctrl, translate_proc p k_var ctrl)

    | Rexpr_pre (flag,s) ->
	Kexpr_pre (flag, translate_ml s)

    | Rexpr_last s ->
	Kexpr_last (translate_ml s)

    | Rexpr_default s ->
	Kexpr_default (translate_ml s)

    | Rexpr_emit (s, None) -> Kexpr_emit (translate_ml s)

    | Rexpr_emit (s, Some e) ->
	Kexpr_emit_val (translate_ml s, translate_ml e)

    | Rexpr_signal ((s,typ), comb, e) ->
	Kexpr_signal ((s, opt_map translate_te typ),
		      opt_map
			(fun (k,e1,e2) ->
			  k, translate_ml e1, translate_ml e2) comb,
		      translate_ml e)

    | _ ->
	raise (Internal (e.expr_loc,
			 "Reac2lk.translate_ml: expr"))

  in
  make_expr kexpr e.expr_loc

(* Translation of Process expressions                                    *)
and translate_proc e k (ctrl: ident) =
  let kproc =
    begin match snd e.expr_static with
    | Def_static.Static ->
	Kproc_compute (translate_ml e, k)
    | Def_static.Dynamic _ ->
	begin match e.expr_desc with
	| Rexpr_nothing -> k.kproc_desc

	| Rexpr_pause kboi -> Kproc_pause (kboi, k, ctrl)

	| Rexpr_halt kboi -> Kproc_halt kboi

	| Rexpr_emit (s, None) -> Kproc_emit (translate_ml s, k)

	| Rexpr_emit (s, Some e) ->
	    Kproc_emit_val (translate_ml s, translate_ml e, k)

(* C_k[loop p] = loop (fun k' -> C_k'[p])                                  *)
	| Rexpr_loop (None, proc) ->
	    let id = make_var "k" in
	    let k_var = make_proc (Kproc_var id) Location.none in
	    Kproc_loop (id, translate_proc proc k_var ctrl)

(* C_k[loop_n n p] = loop_n n (fun k' -> C_k'[p]) k                        *)
	| Rexpr_loop (Some n, proc) ->
	    let id = make_var "k" in
	    let k_var = make_proc (Kproc_var id) Location.none in
	    Kproc_loop_n(id, translate_ml n, translate_proc proc k_var ctrl, k)

(* C_k[while e do p done] = while C[e] do (fun k' -> C_k'[p]) done.k       *)
	| Rexpr_while (expr, proc) ->
	    let id = make_var "k" in
	    let k_var = make_proc (Kproc_var id) Location.none in
	    Kproc_while (translate_ml expr,
			 (id, translate_proc proc k_var ctrl), k)

	| Rexpr_for (i, e1, e2, flag, proc) ->
	    let id = make_var "k" in
	    let k_var = make_proc (Kproc_var id) Location.none in
	    Kproc_for(i,
		      translate_ml e1,
		      translate_ml e2,
		      flag,
		      (id, translate_proc proc k_var ctrl),
		      k)

	| Rexpr_fordopar (i, e1, e2, flag, proc) ->
(* C_k[for i = e1 to e2 dopar e3 done] =                                   *)
(*   bind K = k in                                                         *)
(*     fordopar i e1 e2 (\j. (k3.join_par j.K))                            *)
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    let j_id = make_var "j" in
	    let join =
	      make_proc
		(Kproc_join_par (j_id, k_var))
		Location.none
	    in
	    Kproc_bind
	      (k_patt,
	       k,
	       make_proc
		 (Kproc_fordopar(i,
				 translate_ml e1,
				 translate_ml e2,
				 flag,
				 (j_id, translate_proc proc join ctrl),
				 k_var))
		 Location.none)

	| Rexpr_seq p_list ->
	    let kproc =
	      let rec f l =
		match l with
		| [] -> assert false
		| [p] -> translate_proc p k ctrl
                | ({ expr_static = (ctx, Def_static.Static) } as p)::l' ->
                    let k' = f l' in
                      make_proc (Kproc_seq (translate_ml p, k')) Location.none
		| p::l' ->
		    let k' = f l' in
		    translate_proc p k' ctrl
	      in f p_list
	    in
	    kproc.kproc_desc

(* C_k[p1||p2] =                                                           *)
(*   bind K = k in                                                         *)
(*     split (\j. (k1.join_par j.K, k2.join_par j.K))                      *)
(*                                                                         *)
(*
      | Rexpr_par p_list ->
	  let k_id = make_var "k" in
	  let k_var = make_proc (Kproc_var k_id) Location.none in
	  let k_patt =
	    make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	  in
	  let j_id = make_var "j" in
	  let k_list =
	    List.map
	      (fun p ->
		translate_proc p
		  (make_proc (Kproc_join_par (j_id, k_var)) Location.none)
		  ctrl)
	      p_list
	  in
	  Kproc_bind
	    (k_patt,
	     k,
	     make_proc (Kproc_split_par (j_id, k_list)) Location.none)
*)
(* C_k[p1||p2] =                                                           *)
(*   bind K = k in                                                         *)
(*     split (\j. bind Kj = join_par j.K in (k1.Kj, k2.Kj))                *)
	| Rexpr_par p_list ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    let kj_id = make_var "kj" in
	    let kj_var = make_proc (Kproc_var kj_id) Location.none in
	    let kj_patt =
	      make_patt (Kpatt_var (Kvarpatt_local kj_id)) Location.none
	    in
	    let j_id = make_var "j" in
	    let k_list =
	      List.map
		(fun p ->
		  translate_proc p kj_var ctrl)
		p_list
	    in
	    let join =
	      make_proc (Kproc_join_par (j_id, k_var)) Location.none
	    in
	    Kproc_bind
	      (k_patt,
	       k,
	       make_proc
		 (Kproc_split_par (j_id, kj_patt, join, k_list))
		 Location.none)


	| Rexpr_merge (p1, p2) ->
	    not_yet_implemented "merge"

(* C_k[signal s in p] =                                                    *)
(*   bind K = k in signal s in C_K[p]     avec K \not= s                   *)
	| Rexpr_signal ((s,typ), comb, proc) ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_signal
		    ((s, opt_map translate_te typ),
		     opt_map
		       (fun (k,e1,e2) ->
                         k, translate_ml e1, translate_ml e2) comb,
		     translate_proc proc k_var ctrl))
		 Location.none)

	| Rexpr_let (flag, patt_expr_list, proc) ->
	    translate_proc_let flag patt_expr_list proc k ctrl

	| Rexpr_run (expr) ->
	    Kproc_run (translate_ml expr, k, ctrl)

(* C_k[do p until s(x) -> p' done] =                                       *)
(*   bind K = k in                                                         *)
(*   start ctrl C[s] (fun ctrl' -> C_(end.k, ctrl')[p]) (x -> C_k[p'])     *)
	| Rexpr_until (proc,
                       [ {conf_desc = Rconf_present (_, patt_opt) } as s,
                         when_opt, proc_opt; ]) ->
     (* | Rexpr_until (s, proc, patt_proc_opt) -> *)
            let patt_proc_opt =
              match patt_opt, proc_opt with
              | None, None -> None
              | Some patt, Some proc -> Some (patt, proc)
              | None, Some proc ->
                  let patt = Reac_misc.make_patt Rpatt_any Location.none in
                  Some (patt, proc)
              | Some patt, None -> assert false
            in
            let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
            let ctrl_id = make_var "ctrl" in
	    let end_until =
	      make_proc
		(Kproc_end_until(ctrl_id, k_var))
		Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_start_until
		    (ctrl, translate_conf s, opt_map translate_ml when_opt,
		     (ctrl_id, translate_proc proc end_until ctrl_id),
		     begin match patt_proc_opt with
		     | None ->
			 (make_patt Kpatt_any Location.none, k_var)
		     | Some (patt,proc') ->
			 (translate_pattern patt,
			  translate_proc proc' k_var ctrl)
		     end))
		 Location.none)
	| Rexpr_until (proc, conf_when_opt_expr_opt_list) ->
            not_yet_implemented "Reac2lk.translate_proc(until)"

(* C_k[do p when s] =                                                      *)
(*   bind K = k in                                                         *)
(*   start ctrl C[s] (fun ctrl' -> C_(end.k, ctrl')[p])                    *)
	| Rexpr_when (s, proc) ->
            let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
            let ctrl_id = make_var "ctrl" in
	    let end_when =
	      make_proc
		(Kproc_end_when(ctrl_id, k_var))
		Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_start_when
		     (ctrl, translate_conf s,
		      (ctrl_id, translate_proc proc end_when ctrl_id)))
		 Location.none)

	| Rexpr_control (s, None, proc) ->
            let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
            let ctrl_id = make_var "ctrl" in
	    let end_control =
	      make_proc
		(Kproc_end_control(ctrl_id, k_var))
		Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_start_control
		    (ctrl, translate_conf s,
		     (ctrl_id, translate_proc proc end_control ctrl_id)))
		 Location.none)

	| Rexpr_control (s, Some _, proc) ->
	    Misc.not_yet_implemented "Reac2lk.translate_proc Rexpr_control"

(* C_k[let s<x> in p] =                                                    *)
(*   bind K = k in let s<x> in C_K[p]                                      *)
	| Rexpr_get (s, patt, proc) ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_get (translate_ml s,
			     translate_pattern patt,
			     translate_proc proc k_var ctrl,
			     ctrl))
		 Location.none)

	| Rexpr_present (s, p1, p2) ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_present (ctrl,
				 translate_conf s,
				 translate_proc p1 k_var ctrl,
				 translate_proc p2 k_var ctrl))
		 Location.none)

	| Rexpr_ifthenelse (expr, p1, p2) ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_ifthenelse (translate_ml expr,
				    translate_proc p1 k_var ctrl,
				    translate_proc p2 k_var ctrl))
		 Location.none)

	| Rexpr_match (expr, l) ->
	    let k_id = make_var "k" in
	    let k_var = make_proc (Kproc_var k_id) Location.none in
	    let k_patt =
	      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
	    in
	    Kproc_bind
	      (k_patt, k,
	       make_proc
		 (Kproc_match
		    (translate_ml expr,
		     List.map
                       (fun (p,when_opt,e) ->
                         (translate_pattern p,
                          opt_map translate_ml when_opt,
                          translate_proc e k_var ctrl))
		       l))
		 Location.none)

	| Rexpr_await (flag, s) ->
	    Kproc_await (flag, translate_conf s, k, ctrl)

	| Rexpr_await_val (flag1, flag2, conf, when_opt, proc) ->
            begin match conf.conf_desc with
            | Rconf_present (s, patt_opt) ->
	        let k_id = make_var "k" in
	        let k_var = make_proc (Kproc_var k_id) Location.none in
	        let k_patt =
                  make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
                in
                let patt =
                  match patt_opt with
                  | Some patt ->
	             translate_pattern patt
                  | None ->
                      make_patt Kpatt_any Location.none
	        in
	        Kproc_bind
	          (k_patt, k,
	           make_proc
		     (Kproc_await_val (flag1,
				       flag2,
				       translate_ml s,
				       patt,
                                       opt_map translate_ml when_opt,
				       translate_proc proc k_var ctrl,
				       ctrl))
		     Location.none)
            | Rconf_and (_, _) | Rconf_or (_, _) ->
                not_yet_implemented "Reac2lk.translate_proc(await_val) with config"
            end

        | Rexpr_local _
        | Rexpr_global _
        | Rexpr_constant _
        | Rexpr_function _
        | Rexpr_apply (_, _)
        | Rexpr_tuple _
        | Rexpr_construct (_, _)
        | Rexpr_array _
        | Rexpr_record _
        | Rexpr_record_access (_, _)
        | Rexpr_record_with (_, _)
        | Rexpr_record_update (_, _, _)
        | Rexpr_constraint (_, _)
        | Rexpr_trywith (_, _)
        | Rexpr_assert _
        | Rexpr_process _
        | Rexpr_pre (_, _)
        | Rexpr_last _
        | Rexpr_default _ ->
	    raise (Internal (e.expr_loc,
			     "Reac2lk.translate_proc: expr"))
	end
    end
  in
  make_proc kproc e.expr_loc

(* Translation of let definitions in a PROCESS context *)
and translate_proc_let =
  let rec is_static =
    List.for_all (fun (_, expr) -> snd expr.expr_static = Def_static.Static)
  in
  fun flag patt_expr_list proc k ctrl ->
    let k_id = make_var "k" in
    let k_var = make_proc (Kproc_var k_id) Location.none in
    let k_patt =
      make_patt (Kpatt_var (Kvarpatt_local k_id)) Location.none
    in
    if is_static patt_expr_list then
(* C_k[let x = e in p] =                                                   *)
(*   bind K = k in def x = e in C_K[p]     avec K \not= x                  *)
      Kproc_bind
	(k_patt,
	 k,
	 make_proc
	   (Kproc_def
	      (flag,
	       List.map
		 (fun (patt,expr) ->
		   translate_pattern patt, translate_ml expr)
		 patt_expr_list,
	       translate_proc proc k_var ctrl))
	   Location.none)
    else
      begin match patt_expr_list with
      | [patt, p1] ->
(* C_k[let x = p1 in p2] =                                                 *)
(*   bind K = k in (k1.(def_dyn x in k2.K))      avec K \not= x            *)
	  let def =
	    make_proc
	      (Kproc_def_dyn
		 (translate_pattern patt, translate_proc proc k_var ctrl))
	      Location.none
	  in
	  Kproc_bind
	    (k_patt,
	     k,
	     translate_proc p1 def ctrl)
      | _ ->
(* C_k[let x1 = p1 and x2 = p2 in body] =                                  *)
(*   bind K = def_and_dyn x1, x2 in k_body.k in                            *)
(*     split (\j,(vref1:t1),(vref2:t2),get_vrefs.                          *)
(*              k1.join j vref1 get_vrefs.K,                               *)
(*              k2.join j vref2 get_vrefs.K))                              *)
	  let def =
	    make_proc
	      (Kproc_def_and_dyn
		 ((List.map (fun (patt, _) -> translate_pattern patt)
		     patt_expr_list),
		  translate_proc proc k ctrl))
	      Location.none
	  in
	  let j_id = make_var "j" in
	  let vref_list =
	    List.map
	      (fun _ -> make_var "v_ref")
	      patt_expr_list
	  in
	  let get_vrefs_id = make_var "get_vrefs" in
	  let k_list =
	    List.map2
	      (fun v_ref (_, proc) ->
		let join =
		  make_proc
		    (Kproc_join_def (j_id, v_ref, get_vrefs_id, k_var))
		    Location.none
		in
		translate_proc proc join ctrl)
	      vref_list
	      patt_expr_list
	  in
	  Kproc_bind
	    (k_patt,
	     def,
	     make_proc
	       (Kproc_split_def (j_id,
				 List.map2
				   (fun vref (p, _) -> (vref, p.patt_type))
				   vref_list patt_expr_list,
				 get_vrefs_id,
				 k_list))
	       Location.none)
      end

(* Translation of event configurations *)
and translate_conf conf =
  let kconf =
    match conf.conf_desc with
    | Rconf_present (e, None) -> Kconf_present (translate_ml e)

    | Rconf_present (_, Some _) ->
        not_yet_implemented "Reac2lk.translate_conf (present) with pattern"

    | Rconf_and (c1,c2) ->
	Kconf_and (translate_conf c1, translate_conf c2)

    | Rconf_or (c1,c2) ->
	Kconf_or (translate_conf c1, translate_conf c2)

  in
  make_conf kconf conf.conf_loc


let translate_expr_or_process e =
  match snd (e.expr_static) with
    | Static -> translate_ml e
    | Dynamic _ ->
      let id = make_var "k" in
      let ctrl = make_var "ctrl" in
      let k_var = make_proc (Kproc_var id) Location.none in
      let p =
        make_expr
          (Kexpr_process (id, ctrl, translate_proc e k_var ctrl))
          Location.none
      in
      make_expr (Kexpr_exec p) Location.none

let translate_impl_item info_chan item =
  let kitem =
    match item.impl_desc with
    | Rimpl_expr e -> Kimpl_expr (translate_expr_or_process e)
    | Rimpl_let (flag, l) ->
	Kimpl_let (flag,
		   List.map
		     (fun (p,e) -> (translate_pattern p, translate_expr_or_process e))
		     l)
    | Rimpl_signal (l) ->
	Kimpl_signal
	  (List.map
	     (fun ((s, ty_opt), comb_opt) ->
	       (s, opt_map translate_te ty_opt),
	       opt_map
		 (fun (k,e1,e2) -> (k, translate_ml e1, translate_ml e2))
		 comb_opt)
	     l)
    | Rimpl_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Kimpl_type l
    | Rimpl_exn (name, typ) ->
	Kimpl_exn (name, opt_map translate_te typ)
    | Rimpl_exn_rebind (name, gl_name) ->
	Kimpl_exn_rebind(name, gl_name)
    | Rimpl_open s ->
	Kimpl_open s
  in
  make_impl kitem item.impl_loc


let translate_intf_item info_chan item =
  let kitem =
    match item.intf_desc with
    | Rintf_val (gl, typ) -> Kintf_val (gl, translate_te typ)

    | Rintf_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Kintf_type l

    | Rintf_exn (name, typ) ->
	Kintf_exn (name, opt_map translate_te typ)

    | Rintf_open m -> Kintf_open m

  in
  make_intf kitem item.intf_loc

