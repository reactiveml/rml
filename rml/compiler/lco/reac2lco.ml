(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac2lco.ml                                                *)
(*  Date de creation : 04/06/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* The translation of Reac to Lco *)

open Reac_ast
open Lco_ast
open Misc

let gen_k = new Ident.name_generator

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

let make_impl it loc =
  { coimpl_desc = it;
    coimpl_loc = loc; }

let make_intf it loc =
  { cointf_desc = it;
    cointf_loc = loc; }

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
    | Rtype_process ->
	Cotype_process 
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

    | Rexpr_function  patt_expr_list ->
	Coexpr_function (List.map 
			   (fun (p,e) -> (translate_pattern p, translate_ml e))
			   patt_expr_list)

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

    | Rexpr_record_update (e1, label, e2) ->
	Coexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Rexpr_constraint (expr, typ) ->
	Coexpr_constraint (translate_ml expr, translate_te typ)

    | Rexpr_trywith (expr, l) ->
	Coexpr_trywith (translate_ml expr,
			List.map 
			  (fun (p,e) -> translate_pattern p, translate_ml e)
			  l)
    | Rexpr_assert expr -> Coexpr_assert (translate_ml expr)

    | Rexpr_ifthenelse (e1, e2, e3) ->
	Coexpr_ifthenelse (translate_ml e1,
			   translate_ml e2,
			   translate_ml e3)

    | Rexpr_match (expr, l) ->
	Coexpr_match (translate_ml expr,
		      List.map 
			(fun (p,e) -> translate_pattern p, translate_ml e)
			l)

    | Rexpr_when (e1, e2) ->
	Coexpr_when (translate_ml e1, translate_ml e2)

    | Rexpr_while(e1, e2) ->
	Coexpr_while (translate_ml e1, translate_ml e2)

    | Rexpr_for (id, e1, e2, flag, e3) ->
	Coexpr_for (id,
		    translate_ml e1,
		    translate_ml e2,
		    flag,
		    translate_ml e3)

    | Rexpr_seq (e1, e2) ->
	Coexpr_seq (translate_ml e1, translate_ml e2)

    | Rexpr_process (p) ->
	Coexpr_process (translate_proc p)

    | Rexpr_pre (flag,s) ->
	Coexpr_pre (flag, translate_ml s)  

    | Rexpr_emit s -> Coexpr_emit (translate_ml s)

    | Rexpr_emit_val (s, e) -> 
	Coexpr_emit_val (translate_ml s, translate_ml e)
  in
  make_expr coexpr e.expr_loc

(* Translation of Process expressions                                    *)
and translate_proc p =
  let coproc =
    match p.proc_desc with
    | Rproc_nothing -> Coproc_nothing

    | Rproc_pause -> Coproc_pause 

    | Rproc_compute e -> Coproc_compute (translate_ml e)

    | Rproc_emit s -> Coproc_emit (translate_ml s)

    | Rproc_emit_val (s, e) -> 
	Coproc_emit_val (translate_ml s, translate_ml e)

    | Rproc_loop proc -> Coproc_loop (translate_proc proc)

    | Rproc_while (expr, proc) ->
	Coproc_while (translate_ml expr, translate_proc proc)

    | Rproc_for (i, e1, e2, flag, proc) ->
	Coproc_for(i,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_proc proc)

    | Rproc_fordopar (i, e1, e2, flag, proc) ->
	Coproc_fordopar(i,
			translate_ml e1,
			translate_ml e2,
			flag,
			translate_proc proc)

    | Rproc_seq (p1, p2) ->
	Coproc_seq(translate_proc p1, translate_proc p2)

    | Rproc_par (p1, p2) ->
	Coproc_par (translate_proc p1,
		    translate_proc p2)

    | Rproc_merge (p1, p2) ->
	Coproc_merge (translate_proc p1,
		     translate_proc p2)

    | Rproc_signal ((s,typ), comb, proc) ->
	Coproc_signal ((s, opt_map translate_te typ),
		       opt_map 
			 (fun (e1,e2) -> 
			   translate_ml e1, translate_ml e2) comb,
		       translate_proc proc)

    | Rproc_def ((patt, expr), proc) ->
	Coproc_def ((translate_pattern patt, translate_ml expr),
		    translate_proc proc)

    | Rproc_run (expr) ->
	Coproc_run (translate_ml expr)

    | Rproc_until (s, proc, patt_proc_opt) ->
	Coproc_until (translate_ml s, 
		      translate_proc proc,
		      opt_map 
			(fun (patt, proc) -> 
			  translate_pattern patt, translate_proc proc)
			patt_proc_opt)

    | Rproc_when (s, proc) ->
	Coproc_when (translate_ml s, translate_proc proc)

    | Rproc_control (s, proc) ->
	Coproc_control (translate_ml s, translate_proc proc)

    | Rproc_get (s, patt, proc) ->
	Coproc_get (translate_ml s, 
		    translate_pattern patt, 
		    translate_proc proc)
	  
    | Rproc_present (s, p1, p2) ->
	Coproc_present (translate_ml s, 
			translate_proc p1, 
			translate_proc p2)

    | Rproc_ifthenelse (expr, p1, p2) ->
	Coproc_ifthenelse (translate_ml expr, 
			   translate_proc p1, 
			   translate_proc p2)

    | Rproc_match (expr, l) ->
	Coproc_match (translate_ml expr,
		      List.map 
			(fun (p,e) -> translate_pattern p, translate_proc e)
			l)

    | Rproc_await (flag, s) -> Coproc_await (flag, translate_ml s)

    | Rproc_await_val (flag1, flag2, s, patt, proc) ->
	Coproc_await_val (flag1,
			  flag2,
			  translate_ml s, 
			  translate_pattern patt, 
			  translate_proc proc)
  in
  make_proc coproc p.proc_loc

let translate_impl_item info_chan item =
  let coitem =
    match item.impl_desc with
    | Rimpl_expr e -> Coimpl_expr (translate_ml e)
    | Rimpl_let (flag, l) ->
	Coimpl_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     l)
    | Rimpl_signal (l) ->
	Coimpl_signal 
	  (List.map
	     (fun ((s, ty_opt), comb_opt) ->
	       (s, opt_map translate_te ty_opt),
	       opt_map 
		 (fun (e1,e2) ->(translate_ml e1, translate_ml e2)) 
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

