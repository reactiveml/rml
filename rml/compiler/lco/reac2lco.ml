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

(* file: reac2lco.ml *)
(* created: 2004-06-04  *)
(* author: Louis Mandel *)

(* $Id: reac2lco.ml,v 1.2 2005/03/14 09:58:54 mandel Exp $ *)

(* The translation of Reac to Lco *)

open Asttypes
open Def_static
open Reac_ast
open Lco_ast
open Global
open Global_ident
open Misc


let make_expr e loc =
  { coexpr_desc = e;
    coexpr_loc = loc; }

let make_proc e loc =
  { coproc_desc = e;
    coproc_loc = loc; }

let make_patt p loc =
  { copatt_desc = p;
    copatt_loc = loc; }

let make_te t loc =
  { cote_desc = t;
    cote_loc = loc; }

let make_conf c loc =
  { coconf_desc = c;
    coconf_loc = loc; }

let make_impl it loc =
  { coimpl_desc = it;
    coimpl_loc = loc; }

let make_intf it loc =
  { cointf_desc = it;
    cointf_loc = loc; }

let make_unit () =
  make_expr
    (Coexpr_constant Const_unit)
    Location.none

let make_nothing () =
  make_proc Coproc_nothing Location.none

let make_rmltop_instruction s =
  make_expr
    (Coexpr_global
       { gi = { qual = "Rmltop_global";
		id = Ident.create Ident.gen_var s Ident.Internal };
	 info = no_info(); })
    Location.none

(* Translation of type expressions *)
let rec translate_te typ =
  let cotyp =
    match typ.te_desc with
    | Rtype_var x -> Cotype_var x
    | Rtype_arrow (t1, t2) ->
	Cotype_arrow (translate_te t1, translate_te t2)
    | Rtype_product typ_list ->
	Cotype_product (List.map translate_te typ_list)
    | Rtype_constr (cstr, te_list) ->
	Cotype_constr (cstr, List.map translate_te te_list)
    | Rtype_process (t, _) ->
	Cotype_process (translate_te t)
  in
  make_te cotyp typ.te_loc

(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Rtype_abstract -> Cotype_abstract
  | Rtype_rebind typ -> Cotype_rebind (translate_te typ)
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
      Cotype_variant l
  | Rtype_record l ->
      let l =
	List.map
	  (fun (lab, flag, typ) ->
	    (lab, flag, translate_te typ))
	  l
      in
      Cotype_record l

(* Translation of a pattern *)
let rec translate_pattern p =
  let copatt =
    match p.patt_desc with
    | Rpatt_any -> Copatt_any

    | Rpatt_var x ->
	begin
	  match x with
	  | Varpatt_global gl -> Copatt_var (Covarpatt_global gl)
	  | Varpatt_local id -> Copatt_var (Covarpatt_local id)
	end

    | Rpatt_alias (patt, x) ->
	let vp =
	  match x with
	  | Varpatt_global gl -> Covarpatt_global gl
	  | Varpatt_local id ->  Covarpatt_local id
	in
	Copatt_alias (translate_pattern patt, vp)

    | Rpatt_constant im -> Copatt_constant im

    | Rpatt_tuple l ->
	Copatt_tuple (List.map translate_pattern l)

    | Rpatt_construct (constr, patt_opt) ->
	Copatt_construct (constr, opt_map translate_pattern patt_opt)

    | Rpatt_or (p1, p2) ->
	Copatt_or (translate_pattern p1, translate_pattern p2)

    | Rpatt_record l ->
	Copatt_record (List.map (fun (l,p) -> (l,translate_pattern p)) l)

    | Rpatt_array l ->
	Copatt_array (List.map translate_pattern l)

    | Rpatt_constraint (patt, typ) ->
	Copatt_constraint (translate_pattern patt, translate_te typ)

  in
  make_patt copatt p.patt_loc

(* Translation of ML expressions *)
let rec translate_ml e =
  let coexpr =
    match e.expr_desc with
    | Rexpr_local id -> Coexpr_local id

    | Rexpr_global gl -> Coexpr_global gl

    | Rexpr_constant im -> Coexpr_constant im

    | Rexpr_let (flag, patt_expr_list, expr) ->
	Coexpr_let (flag,
		    List.map
		      (fun (p,e) -> (translate_pattern p, translate_ml e))
		      patt_expr_list,
		    translate_ml expr)

    | Rexpr_function  patt_when_opt_expr_list ->
        Coexpr_function
          (List.map
             (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
             patt_when_opt_expr_list)

    | Rexpr_apply (expr, expr_list) ->
	Coexpr_apply (translate_ml expr,
		      List.map translate_ml expr_list)

    | Rexpr_tuple expr_list ->
	Coexpr_tuple (List.map translate_ml expr_list)

    | Rexpr_construct (c, expr_opt) ->
	Coexpr_construct (c, opt_map translate_ml expr_opt)

    | Rexpr_array l ->
	Coexpr_array (List.map translate_ml l)

    | Rexpr_record l ->
	Coexpr_record (List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Rexpr_record_access (expr, label) ->
	Coexpr_record_access (translate_ml expr, label)

    | Rexpr_record_with (expr, l) ->
	Coexpr_record_with (translate_ml expr,
                            List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Rexpr_record_update (e1, label, e2) ->
	Coexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Rexpr_constraint (expr, typ) ->
	Coexpr_constraint (translate_ml expr, translate_te typ)

    | Rexpr_trywith (expr, l) ->
        Coexpr_trywith
          (translate_ml expr,
           List.map
             (fun (p,when_opt,e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
             l)
    | Rexpr_assert expr -> Coexpr_assert (translate_ml expr)

    | Rexpr_ifthenelse (e1, e2, e3) ->
	Coexpr_ifthenelse (translate_ml e1,
			   translate_ml e2,
			   translate_ml e3)

    | Rexpr_match (expr, l) ->
        Coexpr_match
          (translate_ml expr,
           List.map
             (fun (p, when_opt, e) ->
               (translate_pattern p,
                opt_map translate_ml when_opt,
                translate_ml e))
             l)

    | Rexpr_while(e1, e2) ->
	Coexpr_while (translate_ml e1, translate_ml e2)

    | Rexpr_for (id, e1, e2, flag, e3) ->
	Coexpr_for (id,
		    translate_ml e1,
		    translate_ml e2,
		    flag,
		    translate_ml e3)

    | Rexpr_seq (e1::e_list) ->
	let rec f acc l =
	  match l with
	  | [] -> assert false
	  | [e] ->
	      Coexpr_seq (acc, translate_ml e)
	  | e::l' ->
	      let acc' =
		make_expr
		  (Coexpr_seq (acc, translate_ml e))
		  Location.none
	      in
	      f acc' l'
	in f (translate_ml e1) e_list

    | Rexpr_process (p) ->
	Coexpr_process (translate_proc p)

    | Rexpr_pre (flag,s) ->
	Coexpr_pre (flag, translate_ml s)

    | Rexpr_last s ->
	Coexpr_last (translate_ml s)

    | Rexpr_default s ->
	Coexpr_default (translate_ml s)

    | Rexpr_emit (s, None) -> Coexpr_emit (translate_ml s)

    | Rexpr_emit (s, Some e) ->
	Coexpr_emit_val (translate_ml s, translate_ml e)

    | Rexpr_signal ((s,typ), comb, e) ->
	Coexpr_signal ((s, opt_map translate_te typ),
		       opt_map
			 (fun (k,e1,e2) ->
			   k, translate_ml e1, translate_ml e2) comb,
		       translate_ml e)

    | _ ->
	raise (Internal (e.expr_loc,
			 "Reac2lco.translate_ml: expr"))

  in
  make_expr coexpr e.expr_loc

(* Translation of Process expressions                                    *)
and translate_proc p =
  let coproc =
    begin match snd p.expr_static with
    | Def_static.Static ->
	Coproc_compute (translate_ml p)
    | Def_static.Dynamic _ ->
	begin match p.expr_desc with
	| Rexpr_nothing -> Coproc_nothing

	| Rexpr_pause kboi -> Coproc_pause kboi

	| Rexpr_halt kboi -> Coproc_halt kboi

	| Rexpr_emit (s, None) -> Coproc_emit (translate_ml s)

	| Rexpr_emit (s, Some e) ->
	    Coproc_emit_val (translate_ml s, translate_ml e)

	| Rexpr_loop (n_opt, proc) ->
	    Coproc_loop (opt_map translate_ml n_opt, translate_proc proc)

	| Rexpr_while (expr, proc) ->
	    Coproc_while (translate_ml expr, translate_proc proc)

	| Rexpr_for (i, e1, e2, flag, proc) ->
	    Coproc_for(i,
		       translate_ml e1,
		       translate_ml e2,
		       flag,
		       translate_proc proc)

	| Rexpr_fordopar (i, e1, e2, flag, proc) ->
	    Coproc_fordopar(i,
			    translate_ml e1,
			    translate_ml e2,
			    flag,
			    translate_proc proc)

	| Rexpr_seq (p1::p_list) ->
	    let rec f acc l =
	      match l with
	      | [] -> assert false
	      | [p] ->
		  Coproc_seq (acc, translate_proc p)
	      | p::l' ->
		  let acc' =
		    make_proc
		      (Coproc_seq (acc, translate_proc p))
		      Location.none
		  in
		  f acc' l'
	    in f (translate_proc p1) p_list

(*
      | Rexpr_par p_list ->
	  let p_list' =
	    List.map (fun p -> translate_proc p) p_list
	  in
	  Coproc_par p_list'
*)
	| Rexpr_par [p1; p2] ->
	    Coproc_par [translate_proc p1; translate_proc p2]
	| Rexpr_par p_list ->
	    let p_list' =
	      List.map
		(fun p ->
		  if p.expr_type = Initialization.type_unit then
		    translate_proc p
		  else
		    if snd p.expr_static = Def_static.Static then
		      make_proc
			(Coproc_compute
			   (make_expr
			      (Coexpr_seq (translate_ml p, make_unit()))
			      Location.none))
			Location.none
		    else
		      make_proc
			(Coproc_seq (translate_proc p, make_nothing()))
			Location.none)
		p_list
	    in
	    Coproc_par p_list'

	| Rexpr_merge (p1, p2) ->
	    Coproc_merge (translate_proc p1,
			  translate_proc p2)

	| Rexpr_signal ((s,typ), comb, proc) ->
	    Coproc_signal ((s, opt_map translate_te typ),
			   opt_map
			     (fun (k,e1,e2) ->
			       k, translate_ml e1, translate_ml e2) comb,
			   translate_proc proc)

(*
   | Rexpr_let (Nonrecursive,[(patt, expr)], proc) ->
   Coproc_def ((translate_pattern patt, translate_ml expr),
   translate_proc proc)
   | Rexpr_let (flag, patt_expr_list, proc) ->
   Coproc_def (translate_proc_let flag patt_expr_list,
   translate_proc proc)
 *)
	| Rexpr_let (flag, patt_expr_list, proc) ->
	    translate_proc_let flag patt_expr_list proc

	| Rexpr_run (expr) ->
	    Coproc_run (translate_ml expr)

        | Rexpr_until (proc, conf_when_opt_expr_opt_list) ->
            let conf_when_opt_expr_opt_list =
              List.map
                (fun (conf, when_opt, proc_opt) ->
                  (translate_conf conf,
                   opt_map translate_ml when_opt,
                   opt_map translate_proc proc_opt))
                conf_when_opt_expr_opt_list
            in
            Coproc_until (translate_proc proc,
                          conf_when_opt_expr_opt_list)

	| Rexpr_when (conf, proc) ->
	    Coproc_when (translate_conf conf, translate_proc proc)

	| Rexpr_control (conf, proc_opt, proc) ->
	    Coproc_control (translate_conf conf,
			    opt_map translate_ml proc_opt,
			    translate_proc proc)

	| Rexpr_get (s, patt, proc) ->
	    Coproc_get (translate_ml s,
			translate_pattern patt,
			translate_proc proc)

	| Rexpr_present (conf, p1, p2) ->
	    Coproc_present (translate_conf conf,
			    translate_proc p1,
			    translate_proc p2)

	| Rexpr_ifthenelse (expr, p1, p2) ->
	    Coproc_ifthenelse (translate_ml expr,
			       translate_proc p1,
			       translate_proc p2)

	| Rexpr_match (expr, l) ->
	    Coproc_match (translate_ml expr,
			  List.map
                            (fun (p,when_opt,e) ->
                              (translate_pattern p,
                               opt_map translate_ml when_opt,
                               translate_proc e))
			    l)

	| Rexpr_await (flag, conf) -> Coproc_await (flag, translate_conf conf)

	| Rexpr_await_val (flag1, flag2, conf, when_opt, proc) ->
	    Coproc_await_val (flag1,
			      flag2,
			      translate_conf conf,
                              opt_map translate_ml when_opt,
			      translate_proc proc)

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
	    raise (Internal (p.expr_loc,
			     "Reac2lco.translate_proc: expr"))
        | Rexpr_seq [] ->
            assert false
	end
    end
  in
  make_proc coproc p.expr_loc

(* Translation of event configurations *)
and translate_conf conf =
  let coconf =
    match conf.conf_desc with
    | Rconf_present (e, patt_opt) ->
        Coconf_present (translate_ml e, opt_map translate_pattern patt_opt)

    | Rconf_and (c1,c2) ->
	Coconf_and (translate_conf c1, translate_conf c2)

    | Rconf_or (c1,c2) ->
	Coconf_or (translate_conf c1, translate_conf c2)

  in
  make_conf coconf conf.conf_loc

(* Translation of let definitions in a PROCESS context *)
and translate_proc_let =
  let rec is_static patt_expr_list =
    match patt_expr_list with
    | [] -> true
    | (_, expr) :: tl ->
	if snd expr.expr_static <> Def_static.Static then
	  false
	else
	  is_static tl
  in
  fun rec_flag patt_expr_list proc ->
    if is_static patt_expr_list then
      begin match rec_flag, patt_expr_list with
      | Nonrecursive, [(patt, expr)] ->
	  Coproc_def ((translate_pattern patt, translate_ml expr),
		      translate_proc proc)
      | _ ->
          (* x, C y = e1 and z = e2                              *)
          (* is translated in                                    *)
          (* x,y,z = let x, C y = e1 and z = e2 in x,y,z         *)
	  let vars =
	    List.fold_left
	      (fun vars (patt,_) -> (Reac_misc.vars_of_patt patt) @ vars)
	      [] patt_expr_list
	  in
	  let rexpr_and_copatt_of_var x =
	    match x with
	    | Varpatt_local id ->
		Reac_misc.make_expr (Rexpr_local id) Location.none,
		make_patt (Copatt_var (Covarpatt_local id)) Location.none
	    | Varpatt_global gl -> assert false
	  in
	  let rexpr_of_vars, copatt_of_vars =
	    List.fold_left
	      (fun (el,pl) var ->
		let e, p = rexpr_and_copatt_of_var var in
		e::el, p::pl)
	      ([],[])
	      vars
	  in
	  let body =
	    translate_ml
	      (Reac_misc.make_expr
		 (Rexpr_let(rec_flag,
			    patt_expr_list,
			    Reac_misc.make_expr
			      (Rexpr_tuple rexpr_of_vars)
			      Location.none))
		 Location.none)
	  in
	  Coproc_def
	    ((make_patt (Copatt_tuple copatt_of_vars) Location.none, body),
	     translate_proc proc)
      end
    else
      begin match patt_expr_list with
      | [(patt, expr)] ->
	  Coproc_def_dyn
	    ((translate_pattern patt, translate_proc expr),
	     translate_proc proc)
      | _ ->
(*
	  Coproc_def_and_dyn
	    (List.map
	       (fun (patt,expr) ->
		 (translate_pattern patt, translate_proc expr))
	       patt_expr_list,
	     translate_proc proc)
*)
          (*  let x1 = e1                                  *)
          (*  and x2 = e2                                  *)
          (*  in e                                         *)
          (*                                               *)
          (*  is translated into                           *)
          (*                                               *)
          (*  let v1, v2 = ref None, ref None in           *)
          (*  (let x1 = e1 in v1 := Some x1                *)
          (*   ||                                          *)
          (*   let x2 = e2 in v2 := Some x2);              *)
          (*  let x1, x2 =                                 *)
          (*    match !v1, !v2 with                        *)
          (*    | Some v1, Some v2 -> v1, v2               *)
          (*    | _ -> assert false                        *)
          (*  in e                                         *)
	  let ref_global =
	    Modules.find_value_desc (Initialization.pervasives_val "ref")
	  in
	  let set_global =
	    Modules.find_value_desc (Initialization.pervasives_val ":=")
	  in
	  let deref_global =
	    Modules.find_value_desc (Initialization.pervasives_val "!")
	  in
	  let id_array =
	    Array.init (List.length patt_expr_list)
	      (fun i -> Ident.create Ident.gen_var ("v"^(string_of_int i))
		  Ident.Internal)
	  in
	  let par =
	    Coproc_par
	      (List.fold_right2
		 (fun id (_, expr) expr_list ->
		   let local_id =
		     Ident.create Ident.gen_var "x" Ident.Internal
		   in
		   make_proc
		     (Coproc_def_dyn
			( (* let x_i = e_i *)
			 (make_patt
			    (Copatt_var (Covarpatt_local local_id))
			    Location.none,
			  translate_proc expr),
			  (* in ref_i := Some x1*)
			 make_proc
			   (Coproc_compute
			      (make_expr
				 (Coexpr_apply
				    (make_expr
				       (Coexpr_global set_global)
				       Location.none,
				     [make_expr (Coexpr_local id)
					Location.none;
				      make_expr
					(Coexpr_construct
					   (Initialization.some_constr_desc,
					    Some
					      (make_expr
						 (Coexpr_local local_id)
						 Location.none)))
					Location.none;]))
				 Location.none))
			   Location.none))
		     Location.none
		   :: expr_list)
		 (Array.to_list id_array) patt_expr_list [])
	  in
	  let let_match =
	    Coproc_def
	      ((make_patt
		  (Copatt_tuple
		     (List.fold_right
			(fun (patt, _) patt_list ->
			  (translate_pattern patt) :: patt_list)
			patt_expr_list []))
		  Location.none,

		make_expr
		  (Coexpr_match
		     ((make_expr
			 (Coexpr_tuple
			    (Array.fold_right
			       (fun id expr_list ->
				 make_expr
				   (Coexpr_apply
				      ((make_expr
					  (Coexpr_global deref_global)
					  Location.none,
					[make_expr (Coexpr_local id)
					   Location.none])))
				   Location.none
				 :: expr_list)
			       id_array []))
			 Location.none),
		      [(make_patt
			  (Copatt_tuple
			     (Array.fold_right
				(fun id patt_list ->
				  make_patt
				    (Copatt_construct
				       (Initialization.some_constr_desc,
					Some
					  (make_patt
					     (Copatt_var (Covarpatt_local id))
					     Location.none)))
				    Location.none
				  :: patt_list)
				id_array []))
			  Location.none,
                        None,
			make_expr
			  (Coexpr_tuple
			     (Array.fold_right
				(fun id expr_list ->
				  make_expr (Coexpr_local id) Location.none
				  :: expr_list)
				id_array []))
			  Location.none);
		       (make_patt (Copatt_any) Location.none,
                        None,
			make_expr (Coexpr_assert
				     (make_expr (Coexpr_constant
						   (Const_bool false))
					Location.none))
			  Location.none)]))
		  Location.none),
	       translate_proc proc)
	  in
	  Coproc_def
	    ((make_patt
		(Copatt_tuple
		   (Array.fold_right
		      (fun id patt_list ->
			(make_patt
			   (Copatt_var (Covarpatt_local id))
			   Location.none)
			:: patt_list)
		      id_array []))
		Location.none,
	      make_expr
		(Coexpr_tuple
		   (Array.fold_left
		      (fun expr_list id ->
			(make_expr
			   (Coexpr_apply
			      (make_expr
				 (Coexpr_global ref_global)
				 Location.none,
			       [ make_expr
				   (Coexpr_construct
				      (Initialization.none_constr_desc, None))
				   Location.none ]))
			   Location.none)
			:: expr_list)
		      [] id_array))
		Location.none),
	     make_proc
	       (Coproc_seq
		  (make_proc par Location.none,
		   make_proc let_match Location.none))
	       Location.none)
      end

let translate_expr_or_process e =
  match snd (e.expr_static) with
    | Static -> translate_ml e
    | Dynamic _ ->
        let p = make_expr (Coexpr_process (translate_proc e)) Location.none in
        make_expr (Coexpr_exec p) Location.none


let translate_impl_item info_chan item =
  let coitem =
    match item.impl_desc with
    | Rimpl_expr e -> Coimpl_expr (translate_expr_or_process e)
    | Rimpl_let (flag, l) ->
	Coimpl_let (flag,
		   List.map
		     (fun (p,e) -> (translate_pattern p, translate_expr_or_process e))
		     l)
    | Rimpl_signal (l) ->
	Coimpl_signal
	  (List.map
	     (fun ((s, ty_opt), comb_opt) ->
	       (s, opt_map translate_te ty_opt),
	       opt_map
		 (fun (k, e1,e2) ->(k, translate_ml e1, translate_ml e2))
		 comb_opt)
	     l)
    | Rimpl_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Coimpl_type l
    | Rimpl_exn (name, typ) ->
	Coimpl_exn (name, opt_map translate_te typ)
    | Rimpl_exn_rebind (name, gl_name) ->
	Coimpl_exn_rebind(name, gl_name)
    | Rimpl_open s ->
	Coimpl_open s
  in
  make_impl coitem item.impl_loc

let translate_intf_item info_chan item =
  let coitem =
    match item.intf_desc with
    | Rintf_val (gl, typ) -> Cointf_val (gl, translate_te typ)

    | Rintf_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in
	Cointf_type l

    | Rintf_exn (name, typ) ->
	Cointf_exn (name, opt_map translate_te typ)

    | Rintf_open m -> Cointf_open m

  in
  make_intf coitem item.intf_loc

