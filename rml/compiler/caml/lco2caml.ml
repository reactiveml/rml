(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lco2caml.ml                                                *)
(*  Date de creation : 04/06/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* The translation of Lco to Caml *)

open Lco_ast
open Caml_ast
open Global
open Global_ident
open Asttypes
open Misc

let make_expr e loc =
  { cexpr_desc = e;
    cexpr_loc = loc; }

let make_patt p loc =
  { cpatt_desc = p;
    cpatt_loc = loc; }

let make_te t loc =
  { cte_desc = t;
    cte_loc = loc; }

let make_impl it loc =
  { cimpl_desc = it;
    cimpl_loc = loc; }

let make_intf it loc =
  { cintf_desc = it;
    cintf_loc = loc; }

let interpreteur_gen = new Ident.name_generator

let make_instruction s =
  make_expr 
    (Cexpr_global 
       { gi = { qual = interpreter_module; 
		id = Ident.create interpreteur_gen s Ident.Internal };
	 info = no_info; })
    Location.none

let make_rml_type s =
  make_te
    (Ctype_constr ({ gi = { qual = interpreter_module; 
			    id = Ident.create interpreteur_gen s Ident.Type };
		     info = no_info; }, []))
    Location.none


let make_patt_unit () =
  make_patt (Cpatt_constant Const_unit) Location.none

(* Translation of type expressions *)
let rec translate_te typ =
  let ctyp =
    match typ.cote_desc with
    | Cotype_var x -> Ctype_var x
    | Cotype_arrow (t1, t2) ->
	Ctype_arrow (translate_te t1, translate_te t2)
    | Cotype_product typ_list ->
	Ctype_product (List.map translate_te typ_list)
    | Cotype_constr (cstr, te_list) ->
	Ctype_constr (cstr, List.map translate_te te_list)
    | Cotype_process ->
	let unit_type = 
	  make_te 
	    (Ctype_constr 
	       (Modules.find_type_desc Initialization.unit_ident, []))
	    Location.none
	in
	Ctype_arrow  (unit_type, make_rml_type "proc")
  in
  make_te ctyp typ.cote_loc

let pattern_of_signal (s,t) = 
  let ps = 
    make_patt (Cpatt_var (Cvarpatt_local s)) Location.none
  in
  match t with 
  | None -> ps
  | Some t -> 
      make_patt (Cpatt_constraint(ps, translate_te t)) Location.none

let pattern_of_signal_global (s,t) = 
  let ps = 
    make_patt (Cpatt_var (Cvarpatt_global s)) Location.none
  in
  match t with 
  | None -> ps
  | Some t -> 
      make_patt (Cpatt_constraint(ps, translate_te t)) Location.none


(* Translation of type declatations *)
let rec translate_type_decl typ =
  match typ with
  | Cotype_abstract -> Ctype_abstract
  | Cotype_rebind t -> Ctype_rebind (translate_te t)
  | Cotype_variant constr_te_list ->
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
      Ctype_variant l
  | Cotype_record l ->
      let l = 
	List.map 
	  (fun (lab, flag, typ) ->
	    (lab, flag, translate_te typ))
	  l
      in
      Ctype_record l

(* Translation of a pattern *)
let rec translate_pattern p =
  let cpatt =
    match p.copatt_desc with
    | Copatt_any -> Cpatt_any
    | Copatt_var x -> 
	begin 
	  match x with
	  | Covarpatt_global gl -> Cpatt_var (Cvarpatt_global gl)
	  | Covarpatt_local id -> Cpatt_var (Cvarpatt_local id)
	end
    | Copatt_alias (patt, x) -> 
	let vp = 
	  match x with
	  | Covarpatt_global gl -> Cvarpatt_global gl
	  | Covarpatt_local id ->  Cvarpatt_local id
	in
	Cpatt_alias (translate_pattern patt, vp)
    | Copatt_constant im -> Cpatt_constant im
    | Copatt_tuple l ->
	Cpatt_tuple (List.map translate_pattern l)
    | Copatt_construct (constr, patt_opt) ->
	Cpatt_construct (constr, opt_map translate_pattern patt_opt)
    | Copatt_or (p1, p2) ->
	Cpatt_or (translate_pattern p1, translate_pattern p2)
    | Copatt_record l ->
	Cpatt_record (List.map (fun (l,p) -> (l,translate_pattern p)) l)
    | Copatt_array l ->
	Cpatt_array (List.map translate_pattern l)
    | Copatt_constraint (patt, typ) ->
	Cpatt_constraint (translate_pattern patt, translate_te typ)
  in
  make_patt cpatt p.copatt_loc

(* Translation of ML expressions *)
let rec translate_ml e =
  let cexpr =
    match e.coexpr_desc with
    | Coexpr_local id -> Cexpr_local id
    | Coexpr_global gl -> Cexpr_global gl
    | Coexpr_constant im -> Cexpr_constant im
    | Coexpr_let (flag, patt_expr_list, expr) ->
	Cexpr_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     patt_expr_list,
		   translate_ml expr)
    | Coexpr_function  patt_expr_list ->
	Cexpr_function (List.map 
			  (fun (p,e) -> (translate_pattern p, translate_ml e))
			  patt_expr_list)
    | Coexpr_apply (expr, expr_list) ->
	Cexpr_apply (translate_ml expr,
		     List.map translate_ml expr_list)
    | Coexpr_tuple expr_list ->
	Cexpr_tuple (List.map translate_ml expr_list)
    | Coexpr_construct (c, expr_opt) -> 
	Cexpr_construct (c, opt_map translate_ml expr_opt)
    | Coexpr_array l ->
	Cexpr_array (List.map translate_ml l)
    | Coexpr_record l ->
	Cexpr_record (List.map (fun (lab,e) -> lab, translate_ml e) l)
    | Coexpr_record_access (expr, label) ->
	Cexpr_record_access (translate_ml expr, label)
    | Coexpr_record_update (e1, label, e2) ->
	Cexpr_record_update (translate_ml e1, label, translate_ml e2)
    | Coexpr_constraint (expr, typ) ->
	Cexpr_constraint (translate_ml expr, translate_te typ)
    | Coexpr_trywith (expr, l) ->
	Cexpr_trywith (translate_ml expr,
		       List.map 
			 (fun (p,e) -> translate_pattern p, translate_ml e)
			 l)
    | Coexpr_assert expr -> Cexpr_assert (translate_ml expr)
    | Coexpr_ifthenelse (e1, e2, e3) ->
	Cexpr_ifthenelse (translate_ml e1,
			  translate_ml e2,
			  translate_ml e3)
    | Coexpr_match (expr, l) ->
	Cexpr_match (translate_ml expr,
		     List.map 
		       (fun (p,e) -> translate_pattern p, translate_ml e)
		       l)
    | Coexpr_when (e1, e2) ->
	Cexpr_when (translate_ml e1, translate_ml e2)
    | Coexpr_while(e1, e2) ->
	Cexpr_while (translate_ml e1, translate_ml e2)
    | Coexpr_for (id, e1, e2, flag, e3) ->
	Cexpr_for (id,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_ml e3)
    | Coexpr_seq (e1, e2) ->
	Cexpr_seq (translate_ml e1, translate_ml e2)
    | Coexpr_process (p) -> 
	Cexpr_function [make_patt_unit(), translate_proc p]
    | Coexpr_pre(flag, s) ->
	let kind = 
	  match flag with
	  | Status -> "status"
	  | Value -> "value"
	in
	Cexpr_apply
	  (make_instruction ("rml_pre_"^kind),
	   [embed_ml s])
    | Coexpr_emit (s) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit",
	   [embed_ml s;])
    | Coexpr_emit_val (s, e) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit_val",
	   [embed_ml s;
	    embed_ml e;])

  in
  make_expr cexpr e.coexpr_loc

(* Embedding of ML expressions in a process *)
and embed_ml e =
  make_expr
    (Cexpr_function [make_patt_unit(), translate_ml e])
    e.coexpr_loc

(* Translation of process *)
and translate_proc e =
  let cexpr =
    match e.coproc_desc with 
    | Coproc_nothing -> (make_instruction "rml_nothing").cexpr_desc

    | Coproc_pause -> (make_instruction "rml_pause").cexpr_desc

    | Coproc_compute (expr) ->
	Cexpr_apply 
	  (make_instruction "rml_compute",
	   [embed_ml expr;])

    | Coproc_emit (s) ->
	Cexpr_apply
	  (make_instruction "rml_emit",
	   [embed_ml s;])

    | Coproc_emit_val (s, e) ->
	Cexpr_apply
	  (make_instruction "rml_emit_val",
	   [embed_ml s;
	    embed_ml e;])

    | Coproc_loop k ->
	Cexpr_apply
	  (make_instruction "rml_loop",
	   [translate_proc k])

    | Coproc_while (e1, k) ->
	Cexpr_apply
	  (make_instruction "rml_while",
	   [embed_ml e1;
	    translate_proc k])

    | Coproc_for (i, e1, e2, flag, k) ->
	Cexpr_apply
	  (make_instruction "rml_for",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr 
	      (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	    make_expr
	      (Cexpr_function [make_patt 
				 (Cpatt_var (Cvarpatt_local i))
				 Location.none, 
			       translate_proc k])
	      Location.none;])

    | Coproc_fordopar (i, e1, e2, flag, k) ->
	Cexpr_apply
	  (make_instruction "rml_fordopar",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr 
	      (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	    make_expr
	      (Cexpr_function [make_patt 
				 (Cpatt_var (Cvarpatt_local i))
				 Location.none, 
			       translate_proc k])
	      Location.none;])

    | Coproc_seq (k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_seq",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_par (k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_par",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_merge (k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_merge",
	   [translate_proc k1;
	    translate_proc k2;])

    | Coproc_signal (s, None, k) ->
	Cexpr_apply
	  (make_instruction "rml_signal",
	   [make_expr 
	      (Cexpr_function [pattern_of_signal s, translate_proc k])
	      Location.none])

    | Coproc_signal (s, Some(e1,e2), k) ->
	Cexpr_apply
	  (make_instruction "rml_signal_combine",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr 
	      (Cexpr_function [pattern_of_signal s, translate_proc k])
	      Location.none])

    | Coproc_def ((patt, expr), k) ->
	Cexpr_apply
	  (make_instruction "rml_def",
	   [embed_ml expr;
	    make_expr
	      (Cexpr_function [translate_pattern patt, translate_proc k])
	      Location.none])

    | Coproc_run (expr) ->
	Cexpr_apply
	  (make_instruction "rml_run",
	   [embed_ml expr;])

    | Coproc_until (s, k, None) ->
	Cexpr_apply
	  (make_instruction "rml_until",
	   [embed_ml s;
	    translate_proc k])
    | Coproc_until (s, k, Some(patt, kh)) ->
	Cexpr_apply
	  (make_instruction "rml_until_handler",
	   [embed_ml s;
	    translate_proc k;
	    make_expr
	      (Cexpr_function [translate_pattern patt, translate_proc kh])
	      Location.none;])

    | Coproc_when (s, k) ->
	Cexpr_apply
	  (make_instruction "rml_when",
	   [embed_ml s;
	    translate_proc k])

    | Coproc_control (s, k) ->
	Cexpr_apply
	  (make_instruction "rml_control",
	   [embed_ml s;
	    translate_proc k])

    | Coproc_get (s, patt, k) ->
	Cexpr_apply
	  (make_instruction "rml_get",
	   [embed_ml s;
	    make_expr
	      (Cexpr_function [translate_pattern patt, translate_proc k])
	      Location.none])

    | Coproc_present (s, k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_present",
	   [embed_ml s;
	    translate_proc k1;
	    translate_proc k2])

    | Coproc_ifthenelse (expr, k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_if",
	   [embed_ml expr;
	    translate_proc k1;
	    translate_proc k2])

    | Coproc_match (expr, patt_proc_list) -> 
	Cexpr_apply
	  (make_instruction "rml_match",
	   [embed_ml expr;
	    make_expr
	      (Cexpr_function 
		 (List.map 
		    (fun (patt,k) -> translate_pattern patt, translate_proc k)
		    patt_proc_list))
	      Location.none])

    | Coproc_await (Nonimmediate, s) ->
	Cexpr_apply
	  (make_instruction "rml_await",
	   [embed_ml s])

    | Coproc_await (Immediate, s) ->
	Cexpr_apply
	  (make_instruction "rml_await_immediate",
	   [embed_ml s])

    | Coproc_await_val (flag1, flag2, s, patt, k) ->
	let im = 
	  match flag1 with
	  | Immediate -> "_immediate"
	  | Nonimmediate -> ""
	in
	let kind = 
	  match flag2 with
	  | One -> "_one"
	  | All -> "_all"
	in
	Cexpr_apply
	  (make_instruction ("rml_await"^im^kind),
	   [embed_ml s;
	    make_expr
	      (Cexpr_function [translate_pattern patt, translate_proc k])
	      Location.none])
  in
  make_expr cexpr e.coproc_loc

let translate_impl_item info_chan item =
  let citem =
    match item.coimpl_desc with
    | Coimpl_expr e -> Cimpl_expr (translate_ml e)
    | Coimpl_let (flag, l) ->
	Cimpl_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     l)
    | Coimpl_signal l ->
	Cimpl_let (Nonrecursive,
		   List.map
		     (function
		       | ((s,ty_opt), None) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal",
				 [make_expr 
				    (Cexpr_constant Const_unit)
				    Location.none]))
			     Location.none
		       | ((s,ty_opt), Some(e1,e2)) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal_combine",
				 [translate_ml e1;
				  translate_ml e2;]))
			     Location.none)
		     l)
    | Coimpl_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in 
	Cimpl_type l 
    | Coimpl_exn (name, typ) ->
	Cimpl_exn (name, opt_map translate_te typ)
    | Coimpl_exn_rebind (name, gl_name) ->
	Cimpl_exn_rebind(name, gl_name)
    | Coimpl_open s -> 
	Cimpl_open s
  in
  make_impl citem item.coimpl_loc

let translate_intf_item info_chan item =
  let citem =
    match item.cointf_desc with
    | Cointf_val (gl, typ) -> Cintf_val (gl, translate_te typ)

    | Cointf_type l -> 
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in 
	Cintf_type l
 
    | Cointf_exn (name, typ) ->
	Cintf_exn (name, opt_map translate_te typ)

    | Cointf_open m -> Cintf_open m

  in
  make_intf citem item.cointf_loc
