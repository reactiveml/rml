(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac2lk.ml                                                 *)
(*  Date de creation : 30/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* The translation of Reac to Lk *)

open Reac_ast
open Lk_ast
open Misc

let gen_k = new Ident.name_generator

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

let make_impl it loc =
  { kimpl_desc = it;
    kimpl_loc = loc; }

let make_intf it loc =
  { kintf_desc = it;
    kintf_loc = loc; }

let make_term () =
  make_proc Kproc_term Location.none

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
    | Rtype_process -> Ktype_process 
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
	Kexpr_function (List.map 
			  (fun (p,e) -> (translate_pattern p, translate_ml e))
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

    | Rexpr_record_update (e1, label, e2) ->
	Kexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Rexpr_constraint (expr, typ) ->
	Kexpr_constraint (translate_ml expr, translate_te typ)

    | Rexpr_trywith (expr, l) ->
	Kexpr_trywith (translate_ml expr,
		       List.map 
			 (fun (p,e) -> translate_pattern p, translate_ml e)
			 l)
    | Rexpr_assert expr -> Kexpr_assert (translate_ml expr)

    | Rexpr_ifthenelse (e1, e2, e3) ->
	Kexpr_ifthenelse (translate_ml e1,
			  translate_ml e2,
			  translate_ml e3)

    | Rexpr_match (expr, l) ->
	Kexpr_match (translate_ml expr,
		       List.map 
			 (fun (p,e) -> translate_pattern p, translate_ml e)
			 l)

    | Rexpr_when (e1, e2) ->
	Kexpr_when (translate_ml e1, translate_ml e2)

    | Rexpr_while(e1, e2) ->
	Kexpr_while (translate_ml e1, translate_ml e2)

    | Rexpr_for (id, e1, e2, flag, e3) ->
	Kexpr_for (id,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_ml e3)

    | Rexpr_seq (e1, e2) ->
	Kexpr_seq (translate_ml e1, translate_ml e2)

    | Rexpr_process (p) ->
	let k_id = Ident.create gen_k "k" Ident.Internal in
	let k_var = make_proc (Kproc_var k_id) Location.none in
	Kexpr_process (make_proc
			 (Kproc_abs (k_id, translate_proc k_var p))
			 Location.none)

    | Rexpr_pre (flag,s) ->
	Kexpr_pre (flag, translate_ml s)

    | Rexpr_emit s -> Kexpr_emit (translate_ml s)

    | Rexpr_emit_val (s, e) -> 
	Kexpr_emit_val (translate_ml s, translate_ml e)

  in
  make_expr kexpr e.expr_loc

(* Translation of Process expressions                                    *)
(* les noms sont uniques, on ne peut pas capturer des variables libres   *)
(* dans la continuation k                                                *)
and translate_proc k p =
  let kproc =
    match p.proc_desc with
    | Rproc_nothing -> k.kproc_desc

    | Rproc_pause -> Kproc_pause k

    | Rproc_compute e -> Kproc_compute (translate_ml e, k)

    | Rproc_emit s -> Kproc_emit (translate_ml s, k)

    | Rproc_emit_val (s, e) -> 
	Kproc_emit_val (translate_ml s, translate_ml e, k)

    | Rproc_loop proc -> Kproc_loop (translate_proc (make_term()) proc)

    | Rproc_while (expr, proc) ->
	Kproc_while (translate_ml expr, translate_proc (make_term()) proc, k)

    | Rproc_for (i, e1, e2, flag, proc) ->
	Kproc_for(i,
		  translate_ml e1,
		  translate_ml e2,
		  flag,
		  translate_proc (make_term()) proc,
		  k)

    | Rproc_seq (p1, p2) ->
	(translate_proc (translate_proc k p2) p1).kproc_desc

    | Rproc_par (p1, p2) ->
	Kproc_par (translate_proc (make_term()) p1,
		   translate_proc (make_term()) p2,
		   k)

    | Rproc_merge (p1, p2) ->
	Kproc_merge (translate_proc (make_term()) p1,
		     translate_proc (make_term()) p2,
		     k)

    | Rproc_signal ((s,typ), comb, proc) ->
	Kproc_signal ((s, opt_map translate_te typ),
		      opt_map 
			(fun (e1,e2) -> translate_ml e1, translate_ml e2) comb,
		      translate_proc k proc)

    | Rproc_def ((patt, expr), proc) ->
	Kproc_def ((translate_pattern patt, translate_ml expr),
		   translate_proc k proc)

    | Rproc_run (expr) ->
	Kproc_run (translate_ml expr, k)

    | Rproc_until (s, proc, patt_proc_opt) ->
	let k_id = Ident.create gen_k "k" Ident.Internal in
	let k_var = make_proc (Kproc_var k_id) Location.none in
	Kproc_apply
	  (make_proc
	     (Kproc_abs
		(k_id,
		 make_proc
		   (Kproc_until (translate_ml s, 
				 translate_proc (make_term()) proc, 
				 opt_map 
				   (fun (patt, proc) -> 
				     translate_pattern patt,
				     translate_proc k_var proc)
				   patt_proc_opt,
				 k_var))
		   Location.none))
	     Location.none,
	   k)

    | Rproc_when (s, proc) ->
	Kproc_when (translate_ml s, translate_proc (make_term()) proc, k)

    | Rproc_control (s, proc) ->
	Kproc_control (translate_ml s, translate_proc (make_term()) proc, k)

    | Rproc_get (s, patt, proc) ->
	Kproc_get (translate_ml s, 
		   translate_pattern patt, 
		   translate_proc k proc)

    | Rproc_present (s, p1, p2) ->
	let k_id = Ident.create gen_k "k" Ident.Internal in
	let k_var = make_proc (Kproc_var k_id) Location.none in
	Kproc_apply
	  (make_proc
	     (Kproc_abs
		(k_id,
		 make_proc 
		   (Kproc_present (translate_ml s, 
				   translate_proc k_var p1, 
				   translate_proc k_var p2))
		   Location.none))
	     Location.none,
	   k)

    | Rproc_ifthenelse (expr, p1, p2) ->
	let k_id = Ident.create gen_k "k" Ident.Internal in
	let k_var = make_proc (Kproc_var k_id) Location.none in
	Kproc_apply
	  (make_proc
	     (Kproc_abs
		(k_id,
		 make_proc 
		   (Kproc_ifthenelse (translate_ml expr,
				      translate_proc k_var p1, 
				      translate_proc k_var p2))
		   Location.none))
	     Location.none,
	   k)

    | Rproc_match (expr, l) ->
	Kproc_match (translate_ml expr,
		    List.map 
		       (fun (p,e) -> translate_pattern p, translate_proc k e)
		       l)

    | Rproc_await (flag, s) -> Kproc_await (flag, translate_ml s, k)

    | Rproc_await_val (flag1, flag2, s, patt, proc) ->
	Kproc_await_val (flag1, flag2,
			 translate_ml s, 
			 translate_pattern patt, 
			 translate_proc k proc)

  in
  make_proc kproc p.proc_loc

let translate_impl_item info_chan item =
  let kitem =
    match item.impl_desc with
    | Rimpl_expr e -> Kimpl_expr (translate_ml e)

    | Rimpl_let (flag, l) ->
	Kimpl_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     l)

    | Rimpl_signal (l) ->
	Kimpl_signal 
	  (List.map
	     (fun ((s, ty_opt), comb_opt) ->
	       (s, opt_map translate_te ty_opt),
	       opt_map 
		 (fun (e1,e2) -> (translate_ml e1, translate_ml e2)) 
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

