(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lk2caml.ml                                                 *)
(*  Date de creation : 15/08/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* The translation of Lk to Caml *)

open Lk_ast
open Caml_ast
open Caml_misc
open Global
open Global_ident
open Asttypes
open Misc

(* Version of the combinators generated *)
type version =
  | Combinator
  | Inline

let version = ref Combinator

(* Builds a step function of body : cexpr *)
let make_step_function cexpr loc =
  let step =
    Cexpr_fun
      ([make_patt Cpatt_any Location.none],
       (make_expr cexpr loc))
  in
  make_expr step loc


(* Creates the expression "()" *)
let make_unit = 
  make_expr (Cexpr_constant Const_unit) Location.none

(* Creates the expression "ref (Obj.magic())" *)
let ref_obj_magic () =
  make_expr
    (Cexpr_apply
       (make_expr 
	  (Cexpr_global 
	     { gi = { qual = "Pervasives"; 
		      id = Ident.create Ident.gen_var "ref" Ident.Internal };
	       info = no_info; })
	  Location.none,
	[make_expr
	   (Cexpr_apply
	      (make_expr 
		 (Cexpr_global 
		    { gi = 
		      { qual="Obj"; 
			id=Ident.create Ident.gen_var "magic" Ident.Internal };
		      info = no_info; })
		 Location.none,
	       [make_unit]))
	   Location.none]))
    Location.none

(* Creates the expression "!vref" *)
let deref vref =
  make_expr
    (Cexpr_apply
       (make_expr 
	  (Cexpr_global 
	     { gi = { qual = "Pervasives"; 
		      id = Ident.create Ident.gen_var "!" Ident.Internal };
	       info = no_info; })
	  Location.none,
	[make_expr_var_local vref]))
    Location.none

(* Creates an expr from list of kproc *)
let make_list_of_proc tr k_list =
  List.fold_right
    (fun k l ->
      make_expr
	(Cexpr_construct
	   (Initialization.cons_constr_desc, 
	    Some(make_expr (Cexpr_tuple([tr k;l])) Location.none)))
	Location.none)
    k_list
    (make_expr 
       (Cexpr_construct(Initialization.nil_constr_desc,None))
       Location.none)

(* Translation of type expressions *)
let rec translate_te typ =
  let ctyp =
    match typ.kte_desc with
    | Ktype_var x -> Ctype_var x

    | Ktype_arrow (t1, t2) ->
	Ctype_arrow (translate_te t1, translate_te t2)

    | Ktype_product typ_list ->
	Ctype_product (List.map translate_te typ_list)

    | Ktype_constr (cstr, te_list) ->
	Ctype_constr (cstr, List.map translate_te te_list)

    | Ktype_process t ->
	let proc_type = make_rml_type "process" [translate_te t] in
	proc_type.cte_desc

  in
  make_te ctyp typ.kte_loc

let pattern_of_signal (s,t) = 
  let ps = make_patt_var_local s in
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
  | Ktype_abstract -> Ctype_abstract

  | Ktype_rebind t -> Ctype_rebind (translate_te t)

  | Ktype_variant constr_te_list ->
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

  | Ktype_record l ->
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
    match p.kpatt_desc with
    | Kpatt_any -> Cpatt_any

    | Kpatt_var x -> 
	begin 
	  match x with
	  | Kvarpatt_global gl -> Cpatt_var (Cvarpatt_global gl)
	  | Kvarpatt_local id -> Cpatt_var (Cvarpatt_local id)
	end

    | Kpatt_alias (patt, x) -> 
	let vp = 
	  match x with
	  | Kvarpatt_global gl -> Cvarpatt_global gl
	  | Kvarpatt_local id ->  Cvarpatt_local id
	in
	Cpatt_alias (translate_pattern patt, vp)

    | Kpatt_constant im -> Cpatt_constant im

    | Kpatt_tuple l ->
	Cpatt_tuple (List.map translate_pattern l)

    | Kpatt_construct (constr, None) ->
	Cpatt_construct (constr, None)
    | Kpatt_construct (constr, Some patt) ->
	Cpatt_construct (constr, Some (translate_pattern patt))

    | Kpatt_or (p1, p2) ->
	Cpatt_or (translate_pattern p1, translate_pattern p2)

    | Kpatt_record l ->
	Cpatt_record (List.map (fun (l,p) -> (l,translate_pattern p)) l)

    | Kpatt_array l ->
	Cpatt_array (List.map translate_pattern l)

    | Kpatt_constraint (patt, typ) ->
	Cpatt_constraint (translate_pattern patt, translate_te typ)

  in
  make_patt cpatt p.kpatt_loc

(* Translation of ML expressions *)
let rec translate_ml e =
  let cexpr =
    match e.kexpr_desc with
    | Kexpr_local id -> Cexpr_local id

    | Kexpr_global gl -> Cexpr_global gl

    | Kexpr_constant im -> Cexpr_constant im

    | Kexpr_let (flag, patt_expr_list, expr) ->
	Cexpr_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     patt_expr_list,
		   translate_ml expr)

    | Kexpr_function  patt_expr_list ->
	Cexpr_function (List.map 
			  (fun (p,e) -> (translate_pattern p, translate_ml e))
			  patt_expr_list)

    | Kexpr_apply (expr, expr_list) ->
	Cexpr_apply (translate_ml expr,
		     List.map translate_ml expr_list)

    | Kexpr_tuple expr_list ->
	Cexpr_tuple (List.map translate_ml expr_list)

    | Kexpr_construct (c, None) -> Cexpr_construct (c, None)

    | Kexpr_construct (c, Some expr) -> 
	Cexpr_construct (c, Some (translate_ml expr))

    | Kexpr_array l ->
	Cexpr_array (List.map translate_ml l)

    | Kexpr_record l ->
	Cexpr_record (List.map (fun (lab,e) -> lab, translate_ml e) l)

    | Kexpr_record_access (expr, label) ->
	Cexpr_record_access (translate_ml expr, label)

    | Kexpr_record_update (e1, label, e2) ->
	Cexpr_record_update (translate_ml e1, label, translate_ml e2)

    | Kexpr_constraint (expr, typ) ->
	Cexpr_constraint (translate_ml expr, translate_te typ)

    | Kexpr_trywith (expr, l) ->
	Cexpr_trywith (translate_ml expr,
		       List.map 
			 (fun (p,e) -> translate_pattern p, translate_ml e)
			 l)

    | Kexpr_assert expr -> Cexpr_assert (translate_ml expr)

    | Kexpr_ifthenelse (e1, e2, e3) ->
	Cexpr_ifthenelse (translate_ml e1,
			  translate_ml e2,
			  translate_ml e3)

    | Kexpr_match (expr, l) ->
	Cexpr_match (translate_ml expr,
		     List.map 
		       (fun (p,e) -> translate_pattern p, translate_ml e)
		       l)

    | Kexpr_when_match (e1, e2) ->
	Cexpr_when_match (translate_ml e1, translate_ml e2)

    | Kexpr_while(e1, e2) ->
	Cexpr_while (translate_ml e1, translate_ml e2)

    | Kexpr_for (id, e1, e2, flag, e3) ->
	Cexpr_for (id,
		   translate_ml e1,
		   translate_ml e2,
		   flag,
		   translate_ml e3)

    | Kexpr_seq (e1, e2) ->
	Cexpr_seq (translate_ml e1, translate_ml e2)

    | Kexpr_process (k_id, ctrl, p) -> 
	Cexpr_fun ([make_patt_var_local k_id; make_patt_var_local ctrl;], 
		   translate_proc p)

    | Kexpr_pre(flag, s) ->
	let kind = 
	  match flag with
	  | Status -> "status"
	  | Value -> "value"
	in
	Cexpr_apply
	  (make_instruction ("rml_pre_"^kind),
	   [translate_ml s])

    | Kexpr_emit (s) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit",
	   [translate_ml s])

    | Kexpr_emit_val (s, e) ->
	Cexpr_apply
	  (make_instruction "rml_expr_emit_val",
	   [translate_ml s;
	    translate_ml e])


    | Kexpr_signal (s, None, e) ->
	Cexpr_let (Nonrecursive,
		   [pattern_of_signal s,
		    make_expr
		      (Cexpr_apply
			 (make_instruction "rml_global_signal",
			  [make_expr_unit()]))
		      Location.none],
		   translate_ml e)
    | Kexpr_signal (s, Some(e1,e2), e) ->
	Cexpr_let (Nonrecursive,
		   [pattern_of_signal s,
		    make_expr
		      (Cexpr_apply
			 (make_instruction "rml_global_signal_combine",
			  [translate_ml e1;
			   translate_ml e2;]))
		      Location.none],
		   translate_ml e)

  in
  make_expr cexpr e.kexpr_loc

(* Embedding of ML expressions in a process *)
and embed_ml e =
  make_expr
    (Cexpr_function [make_patt_unit(), translate_ml e])
    e.kexpr_loc

(* Translation of process *)
and translate_proc e =
  let cexpr =
    match e.kproc_desc with 
(* Tr(bind x = k1 in k2) =                                               *)
(*   let x = k1 in k2                                                    *)
    | Kproc_bind (patt, k1, k2) ->
	Cexpr_let
	  (Nonrecursive,
	  [translate_pattern patt, translate_proc k1],
	  translate_proc k2)

    | Kproc_var k -> Cexpr_local k
	  
    | Kproc_pause (k, ctrl) ->
	Cexpr_apply 
	  (make_instruction "rml_pause",
	   [translate_proc k;
	    make_expr_var_local ctrl])

    | Kproc_halt k ->
	Cexpr_apply 
	  (make_instruction "rml_halt",
	   [translate_proc k])
	  
    | Kproc_compute (expr, k) ->
	Cexpr_apply 
	  (make_instruction "rml_compute",
	   [embed_ml expr;
	    translate_proc k])
	  
    | Kproc_emit (s, k) ->
	Cexpr_apply
	  (make_instruction "rml_emit",
	   [embed_ml s;
	    translate_proc k])
	  
    | Kproc_emit_val (s, e, k) ->
	Cexpr_apply
	  (make_instruction "rml_emit_val",
	   [embed_ml s;
	    embed_ml e;
	    translate_proc k])
	  
    | Kproc_loop (k_id, k) -> 
	Cexpr_apply
	  (make_instruction "rml_loop",
	   [make_expr
	      (Cexpr_function [make_patt_var_local k_id, translate_proc k])
	      Location.none])

    | Kproc_loop_n (k_id, e, k, k') -> 
	Cexpr_apply
	  (make_instruction "rml_loop_n",
	   [embed_ml e;
	    make_expr
	      (Cexpr_function [make_patt_var_local k_id, translate_proc k])
	      Location.none;
	    translate_proc k'])
	  
    | Kproc_while (e1, (k_id, k1), k2) ->
	Cexpr_apply
	  (make_instruction "rml_while",
	   [embed_ml e1;
	    make_expr
	      (Cexpr_function [make_patt_var_local k_id, translate_proc k1])
	      Location.none;
	    translate_proc k2])
	  
    | Kproc_for (i, e1, e2, flag, (k_id,k1), k2) ->
	Cexpr_apply
	  (make_instruction "rml_for",
	   [embed_ml e1;
	    embed_ml e2;
	    make_expr 
	      (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	    make_expr
	      (Cexpr_fun 
		 ([ make_patt_var_local i;
		    make_patt_var_local k_id ],
		  translate_proc k1))
	      Location.none;
	    translate_proc k2])
	  
    | Kproc_fordopar (i, e1, e2, flag, (j_id, k), k') ->
	Cexpr_apply
	  (make_instruction "rml_fordopar",
	   [ embed_ml e1;
	     embed_ml e2;
	     make_expr 
	       (Cexpr_constant (Const_bool (flag = Upto))) Location.none;
	     make_expr
	       (Cexpr_fun
		  ([ make_patt_var_local j_id; 
		     make_patt_var_local i; ],
		   translate_proc k))
	       Location.none; 
	     translate_proc k'])
	  
(*
    | Kproc_split_par (j_id, [k1; k2]) ->
	Cexpr_apply
	  (make_instruction "rml_split_par",
	   [ make_expr
	       (Cexpr_fun
		  ([ make_patt_var_local j_id; ],
		   (make_expr 
		      (Cexpr_tuple [translate_proc k1; translate_proc k2])
		      Location.none)))
	       Location.none; ])
*)
(*
    | Kproc_split_par (j_id, k_list) ->
	Cexpr_apply
	  (make_instruction "rml_split_par",
	   [ make_expr
	       (Cexpr_constant (Const_int (List.length k_list)))
	       Location.none;
	     make_expr
	       (Cexpr_fun
		  ([ make_patt_var_local j_id; ],
		   make_list_of_proc translate_proc k_list))
	       Location.none; ])
*)
    | Kproc_split_par (j_id, kj_patt, join, k_list) ->
	Cexpr_apply
	  (make_instruction "rml_split_par",
	   [ make_expr
	       (Cexpr_constant (Const_int (List.length k_list)))
	       Location.none;
	     make_expr
	       (Cexpr_fun
		  ([ make_patt_var_local j_id; ],
		   (make_expr
		      (	Cexpr_let
			  (Nonrecursive,
			   [translate_pattern kj_patt, translate_proc join],
			   make_list_of_proc translate_proc k_list))
		      Location.none)))
	       Location.none; ])


    | Kproc_join_par (j_id, k) ->
	Cexpr_apply
	  (make_instruction "rml_join_par",
	   [ make_expr_var_local j_id;
	     translate_proc k ])

    | Kproc_signal (s, None, k) ->
	begin match !version with
	| Combinator ->
(* Tr(signal s in k) =                                                   *)
(*   signal (fun s -> k)                                                 *)
	    Cexpr_apply
	      (make_instruction "rml_signal",
	       [make_expr 
		  (Cexpr_function [pattern_of_signal s, translate_proc k])
		  Location.none])
	| Inline ->
(* Tr(signal s in k) =                                                   *)
(*   fun v -> let x = new_event() in k ()                                *)
	    let f =
	      make_step_function
		(Cexpr_let
		   (Nonrecursive, 
		    [pattern_of_signal s, 
		     make_expr 
		       (Cexpr_apply (make_instruction "rml_global_signal",
				     [make_expr_unit()]))
		       Location.none], 
		    make_expr 
		      (Cexpr_apply 
			 (translate_proc k,
			  [make_expr_unit()]))
		      Location.none)) 
		e.kproc_loc
	    in f.cexpr_desc
	end

    | Kproc_signal (s, Some(e1,e2), k) ->
	begin match !version with
	| Combinator ->
	    Cexpr_apply
	      (make_instruction "rml_signal_combine",
	       [embed_ml e1;
		embed_ml e2;
		make_expr 
		  (Cexpr_function [pattern_of_signal s, translate_proc k])
		  Location.none])
	| Inline ->
	    let f =
	      make_step_function
		(Cexpr_let
		   (Nonrecursive, 
		    [pattern_of_signal s, 
		     make_expr 
		       (Cexpr_apply (make_instruction "rml_global_signal_combine",
				     [ translate_ml e1;
				       translate_ml e2; ]))
		       Location.none], 
		    make_expr 
		      (Cexpr_apply 
			 (translate_proc k,
			  [make_expr_unit()]))
		      Location.none)) 
		e.kproc_loc
	    in f.cexpr_desc
	end


    | Kproc_def (flag, patt_expr_list, k) ->
	let f =
(* Tr(let x = e in k) =                                                  *)
(*   fun _ -> let x = e in k ()                                          *)
	  make_step_function
	    (Cexpr_let
	       (flag, 
		List.map 
		  (fun (p,e) -> (translate_pattern p, translate_ml e))
		  patt_expr_list, 
		make_expr 
		  (Cexpr_apply 
		     (translate_proc k,
		      [make_expr_unit()]))
		  Location.none)) 
	    e.kproc_loc
	in f.cexpr_desc


    | Kproc_split_def (j_id, vref_list, k_list) ->
(* Tr(split (\j. (\vref1. k1, \vref2. k2))) =                            *)
(*   split (\j. let vref1 = ref (Obj.magic())                            *)
(*              and vref1 = ref (Obj.magic()) in [k1; k2])               *)
	Cexpr_apply
	  (make_instruction "rml_split_par",
	   [  make_expr
	       (Cexpr_constant (Const_int (List.length k_list)))
	       Location.none;
	      make_expr
	       (Cexpr_fun
		  ([ make_patt_var_local j_id; ],
		   make_expr
		     (Cexpr_let 
			(Nonrecursive,
			 List.map
			   (fun vref -> 
			     (make_patt_var_local vref, ref_obj_magic()))
			   vref_list,
			 make_list
			   (List.map
			      (fun k -> translate_proc k)
			      k_list)))
		     Location.none))
	       Location.none; ])

    | Kproc_join_def (j_id, vref_id, vref_id_list, k) ->
(* Tr(join j vref [vref1;vref2] k) =                                    *)
(*   join j vref (fun () -> !vref1, !vref2) k                           *)
	Cexpr_apply
	  (make_instruction "rml_join_def",
	   [ make_expr_var_local j_id;
	     make_expr_var_local vref_id;
	     make_expr
	       (Cexpr_function 
		  [make_patt_unit(), 
		   make_expr
		     (Cexpr_tuple (List.map 
				     (fun vref_id -> deref vref_id)
				     vref_id_list))
		     Location.none])
	       Location.none;
	     translate_proc k ])

    | Kproc_def_dyn (patt, k) ->
(* Tr(def x in k) =                                                      *)
(*   fun v -> let x = v in k ()                                          *)
(* ce n'est pas traduit par (fun x -> k ()) pour avoir la generalisation *)
	let id = Ident.create Ident.gen_var "v" Ident.Internal in
	Cexpr_fun
	  ([make_patt_var_local id],
	   make_expr
	     (Cexpr_let
		(Nonrecursive, 
		 [(translate_pattern patt, make_expr_var_local id)], 
		 make_expr 
		   (Cexpr_apply 
		      (translate_proc k,
		       [make_expr_unit()]))
		   Location.none))
	     Location.none)

    | Kproc_def_and_dyn (patt_list, k) ->
(* Tr(def x and y in k) =                                                *)
(*   fun v -> let x,y = v in k ()                                        *)
	let id = Ident.create Ident.gen_var "v" Ident.Internal in
	Cexpr_fun
	  ([make_patt_var_local id],
	   make_expr
	     (Cexpr_let
		(Nonrecursive, 
		 [(make_patt 
		     (Cpatt_tuple
			(List.map translate_pattern patt_list))
		     Location.none, 
		   make_expr_var_local id)], 
		 make_expr 
		   (Cexpr_apply 
		      (translate_proc k,
		       [make_expr_unit()]))
		   Location.none))
	     Location.none)

    | Kproc_run (expr, k, ctrl) ->
	begin match !version with
	| Combinator ->
(* Tr(run e.k) =                                                         *)
(*   run (fun () -> e) k                                                 *)
	    Cexpr_apply
	      (make_instruction "rml_run",
	       [embed_ml expr;
		translate_proc k;
		make_expr_var_local ctrl])
	| Inline ->
(* Tr(run e.k) =                                                         *)
(*   fun _ -> e k ()                                                     *)
	    let f =
	      make_step_function
		(Cexpr_apply 
		   (translate_ml expr, 
		    [translate_proc k;
		     make_expr_var_local ctrl;
		     make_expr_unit();])) 
		e.kproc_loc
	    in f.cexpr_desc
	end


    | Kproc_start_until({kconf_desc = Kconf_present s}, 
			(ctrl, k1), (patt,k2)) ->
	Cexpr_apply
	  (make_instruction "rml_start_until",
	   [embed_ml s;
	    (make_expr 
	       (Cexpr_fun([make_patt_var_local ctrl], translate_proc k1))
	       Location.none);
	    (make_expr 
	       (Cexpr_fun([translate_pattern patt], translate_proc k2))
	       Location.none)])

    | Kproc_end_until(ctrl, k) ->
	Cexpr_apply
	  (make_instruction "rml_end_until",
	   [make_expr_var_local ctrl;
	    translate_proc k])

    | Kproc_start_when({kconf_desc = Kconf_present s}, (ctrl, k)) ->
	Cexpr_apply
	  (make_instruction "rml_start_when",
	   [embed_ml s;
	    (make_expr 
	       (Cexpr_fun([make_patt_var_local ctrl], translate_proc k))
	       Location.none)])

    | Kproc_end_when(ctrl, k) ->
	Cexpr_apply
	  (make_instruction "rml_end_when",
	   [make_expr_var_local ctrl;
	    translate_proc k])


    | Kproc_start_control({kconf_desc = Kconf_present s}, (ctrl, k)) ->
	Cexpr_apply
	  (make_instruction "rml_start_control",
	   [embed_ml s;
	    (make_expr 
	       (Cexpr_fun([make_patt_var_local ctrl], translate_proc k))
	       Location.none)])

    | Kproc_end_control(ctrl, k) ->
	Cexpr_apply
	  (make_instruction "rml_end_control",
	   [make_expr_var_local ctrl;
	    translate_proc k])
	

    | Kproc_get (s, patt, k, ctrl) ->
	Cexpr_apply
	  (make_instruction "rml_get",
	   [embed_ml s;
	    make_expr
	      (Cexpr_function 
		 [translate_pattern patt, translate_proc k])
	      Location.none;
	    make_expr_var_local ctrl])

    | Kproc_present (ctrl, {kconf_desc = Kconf_present s}, k1, k2) ->
	Cexpr_apply
	  (make_instruction "rml_present",
	   [make_expr_var_local ctrl;
	    embed_ml s;
	    translate_proc k1;
	    translate_proc k2])

    | Kproc_ifthenelse (expr, k1, k2) ->
	begin match !version with
	| Combinator ->
(* Tr(if expr then k1 else k2) =                                         *)
(*   rml_if (fun () -> e) k1 k2                                          *)
	    Cexpr_apply
	      (make_instruction "rml_if",
	       [embed_ml expr;
		translate_proc k1;
		translate_proc k2])

	| Inline ->
(* Tr(if expr then k1 else k2) =                                         *)
(*   fun _ -> if expr then k1() else k2()                                *)
	    let f =
	      make_step_function
		(Cexpr_ifthenelse
		   (translate_ml expr,
		    make_expr 
		      (Cexpr_apply (translate_proc k1, [make_expr_unit()]))
		      Location.none,
		    make_expr 
		      (Cexpr_apply (translate_proc k2, [make_expr_unit()]))
		      Location.none))
		e.kproc_loc
	    in f.cexpr_desc
	end

   | Kproc_match (expr, patt_proc_list) -> 
	begin match !version with
	| Combinator ->
(* Tr(match expr with | p1 -> k1 ... | pn -> kn) =                       *)
(*   rml_match (fun () -> e) (function | p1 -> k1 | ... | pn -> kn)      *)
	    Cexpr_apply
	      (make_instruction "rml_match",
	       [embed_ml expr;
		make_expr
		  (Cexpr_function 
		     (List.map 
			(fun (patt,k) -> 
			  translate_pattern patt, translate_proc k)
			patt_proc_list))
		  Location.none])
	| Inline ->
(* Tr(match expr with | p1 -> k1 ... | pn -> kn) =                       *)
(*   fun _ -> match expr with | p1 -> k1() ... | pn -> kn()              *)
 	    let f =
	      make_step_function
		(Cexpr_match
		   (translate_ml expr,
		    List.map
		      (fun (p,k) -> 
			(translate_pattern p,
			 make_expr 
			   (Cexpr_apply (translate_proc k, [make_expr_unit()]))
			   Location.none))
		      patt_proc_list))
		e.kproc_loc
	    in f.cexpr_desc
	end


    | Kproc_when_match (expr, k) ->
(* Tr(when e -> k) =                                                     *)
(*   when e -> k()                                                       *)
	Cexpr_when_match 
	  (translate_ml expr,
	   make_expr 
	     (Cexpr_apply (translate_proc k, [make_expr_unit()]))
	     Location.none)



    | Kproc_await (flag, {kconf_desc = Kconf_present s}, k, ctrl) ->
	let _immediate =
	  match flag with
	  | Nonimmediate -> ""
	  | Immediate -> "_immediate"
	in
	Cexpr_apply
	  (make_instruction ("rml_await"^_immediate),
	   [embed_ml s;
	    translate_proc k;
	    make_expr_var_local ctrl])

    | Kproc_await_val (flag1, flag2, s, patt, k, ctrl) ->
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
	let cpatt = translate_pattern patt in
	if Caml_misc.partial_match cpatt then 
	  begin
	    not_yet_implemented "await_{one|all}_match";
	  end
	else
	  Cexpr_apply
	    (make_instruction ("rml_await"^im^kind),
	     [embed_ml s;
	      make_expr
		(Cexpr_function [translate_pattern patt, translate_proc k])
		Location.none;
	      make_expr_var_local ctrl])

  in
  make_expr cexpr e.kproc_loc

and translate_conf c =
  let cexpr = 
    match c.kconf_desc with
    | Kconf_present (s) -> 
	Cexpr_apply
	  (make_instruction "is_present",
	   [embed_ml s])

    | Kconf_and(c1,c2) ->
	Cexpr_apply
	  (make_instruction "cfg_and",
	   [translate_conf c1;
	    translate_conf c2;])

    | Kconf_or(c1,c2) ->
	Cexpr_apply
	  (make_instruction "cfg_or",
	   [translate_conf c1;
	    translate_conf c2;])
  in
  make_expr cexpr c.kconf_loc


let translate_impl_item info_chan item =
  let citem =
    match item.kimpl_desc with
    | Kimpl_expr e -> Cimpl_expr (translate_ml e)

    | Kimpl_let (flag, l) ->
	Cimpl_let (flag,
		   List.map 
		     (fun (p,e) -> (translate_pattern p, translate_ml e))
		     l)

    | Kimpl_signal l ->
	Cimpl_let (Nonrecursive,
		   List.map
		     (function
		       | ((s,ty_opt), None) ->
			   pattern_of_signal_global (s, ty_opt),
			   make_expr
			     (Cexpr_apply
				(make_instruction "rml_global_signal",
				 [make_expr_unit()]))
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

    | Kimpl_type l ->
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in 
	Cimpl_type l 

    | Kimpl_exn (name, typ) ->
	Cimpl_exn (name, opt_map translate_te typ)

    | Kimpl_exn_rebind (name, gl_name) ->
	Cimpl_exn_rebind(name, gl_name)

    | Kimpl_open s -> 
	Cimpl_open s

  in
  make_impl citem item.kimpl_loc

let translate_intf_item info_chan item =
  let citem =
    match item.kintf_desc with
    | Kintf_val (gl, typ) -> Cintf_val (gl, translate_te typ)

    | Kintf_type l -> 
	let l =
	  List.map
	    (fun (name, param, typ) ->
	      (name, param, translate_type_decl typ))
	    l
	in 
	Cintf_type l
 
    | Kintf_exn (name, typ) ->
	Cintf_exn (name, opt_map translate_te typ)

    | Kintf_open m -> Cintf_open m

  in
  make_intf citem item.kintf_loc

