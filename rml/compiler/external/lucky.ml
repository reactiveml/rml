(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lucky.ml                                                   *)
(*  Date de creation : 21/03/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: lucky.ml,v 1.1 2005/03/29 10:18:21 mandel Exp $ *)

(* Code generation to import Lucky definitions *)

open Asttypes
open Parse_ident
open Parse_ast

let make_expr d =
  { pexpr_desc = d; 
    pexpr_loc = Location.none; 
    pexpr_static = Def_static.Dynamic; }

let make_ident id =
  { pident_id = id;
    pident_loc = Location.none; }

let make_simple_ident id =
  { psimple_id = id;
    psimple_loc = Location.none; }

let make_pattern patt =
  { ppatt_desc = patt;
    ppatt_loc = Location.none; }

let make_var_patt s =
  make_pattern
    (Ppatt_var { psimple_id = s;
		 psimple_loc = Location.none })

let make_impl d =
  { pimpl_desc = d;
    pimpl_loc = Location.none; }

let rename inputs outputs =
  let cpt = ref 0 in
  let inputs = List.map (fun (id,ty) -> incr cpt; (!cpt,id,ty)) inputs in
  let outputs = List.map (fun (id,ty) -> incr cpt; (!cpt,id,ty)) outputs in
  inputs, outputs


let lucky_constr_of_ty ty =
  match ty.pte_desc with
  | Ptype_constr (id, []) ->
      begin match id.pident_id with 
      | Pident "bool" -> make_ident (Pdot("Lucaml","B"))
      | Pident "int" -> make_ident (Pdot("Lucaml","I"))
      | Pident "float" -> make_ident (Pdot("Lucaml","F"))
      | Pident "event" -> make_ident (Pdot("Lucaml","B"))
      | _ -> Lucky_errors.not_implemented_type ty
      end
  | _ -> Lucky_errors.not_implemented_type ty

let is_valued ty = 
  match ty.pte_desc with
  | Ptype_constr (id, []) ->
      begin match id.pident_id with 
      | Pident "event" -> false
      | _ -> true
      end
  | _ -> true

let get_inputs = 
  let get_input (n,id,ty) =
    make_var_patt ("evtval_"^
		   id.psimple_id^
		   "_in_"^(string_of_int n)),
    make_expr 
      (Pexpr_construct 
	 (lucky_constr_of_ty ty,
	  Some 
	    (make_expr 
	       (Pexpr_pre
		  ((if is_valued ty then Value else Status),
		   make_expr 
		     (Pexpr_ident 
			(make_ident 
			   (Pident ("evt_"^
				    id.psimple_id^
				    "_in_"^(string_of_int n))))))))))
  in
  fun inputs -> 
    List.map get_input inputs

let make_step inputs =
  let input_to_string_val (n,id,ty) = 
    make_expr
      (Pexpr_tuple
	 [make_expr (Pexpr_constant (Const_string id.psimple_id));
	  make_expr (Pexpr_ident (make_ident (Pident 
						("evtval_"^
						 id.psimple_id^
						 "_in_"^(string_of_int n)))))])
  in
  let inputs_to_arg inputs =
    List.fold_right 
      (fun input acc ->
	make_expr 
	  (Pexpr_construct(make_ident (Pident "::"),
			   Some(make_expr
				  (Pexpr_tuple 
				     [input_to_string_val input;
				      acc])))))
      inputs
      (make_expr (Pexpr_construct ((make_ident (Pident "[]")), None)))
  in
  make_expr
    (Pexpr_apply 
       (make_expr
	  (Pexpr_ident (make_ident (Pident "step"))),
	[inputs_to_arg inputs]))

let emit_outputs =
  let emit_output (n,id,ty) =
    make_pattern 
      (Ppatt_tuple 
	 [make_pattern (Ppatt_constant (Const_string id.psimple_id));
	  make_pattern (Ppatt_construct
			  (lucky_constr_of_ty ty,
			   Some 
			     (make_var_patt "x")))]),
    if is_valued ty then 
      make_expr (Pexpr_emit_val 
		   (make_expr 
		      (Pexpr_ident(make_ident 
				     (Pident ("evt_"^
					      id.psimple_id^
					      "_out_"^(string_of_int n))))),
		    make_expr 
		      (Pexpr_ident(make_ident (Pident "x")))))
    else
      make_expr 
	(Pexpr_ifthenelse
	   (make_expr (Pexpr_ident(make_ident (Pident "x"))),
	    make_expr (Pexpr_emit
			 (make_expr 
			    (Pexpr_ident
			       (make_ident 
				  (Pident ("evt_"^
					   id.psimple_id^
					   "_out_"^(string_of_int n))))))),
	    None))

  in
  fun outputs ->
  make_expr
    (Pexpr_apply
       (make_expr (Pexpr_ident (make_ident (Pdot("List","iter")))),
	[make_expr (Pexpr_function 
		      ((List.map emit_output outputs)
		       @[make_pattern (Ppatt_any),
			 make_expr (Pexpr_constant (Const_unit))]));
	 make_expr (Pexpr_ident (make_ident (Pident "lucky_out")))]))
      

let patt_of_in_out_puts patt_of_in_out_put inputs =
  match inputs with
  | [] -> make_pattern (Ppatt_constant Const_unit)
  | [input] -> patt_of_in_out_put input
  | _ ->
      make_pattern 
	(Ppatt_tuple
	   (List.map (fun input -> patt_of_in_out_put input)
	      inputs))
    
let patt_of_inputs =
  let patt_of_input (n,id,ty) = 
    make_var_patt ("evt_"^
		   id.psimple_id^
		   "_in_"^(string_of_int n))
  in
  patt_of_in_out_puts patt_of_input

let patt_of_outputs =
  let patt_of_output (n,id,ty) = 
    make_var_patt ("evt_"^
		   id.psimple_id^
		   "_out_"^(string_of_int n))
  in
  patt_of_in_out_puts patt_of_output

let expr_of_files files =
  List.fold_right 
    (fun file acc ->
      make_expr 
	(Pexpr_construct(make_ident (Pident "::"),
			 Some(make_expr
				(Pexpr_tuple 
				   [make_expr 
				      (Pexpr_constant (Const_string file));
				    acc])))))
    files
    (make_expr (Pexpr_construct ((make_ident (Pident "[]")), None)))


(*
   the translation of 
   
   (id, ["in1",ty1; ...; "inn",tyn], [out1, tym; ...; outm, tym], ["f1"; ...])

   let process id 
               (evt_in1_1, ..., evt_inn_n) 
               (evt_out1_n_pl_1, ..., evt_outm_n_pl_m) =
     let state = ref (Lucaml.make 
		     0
		     false
		     ""
		     false
		     ["f1"; ...]) in
     let step = Lucaml.step_se Lucaml.StepInside state in
     loop
       pause;
       let val_in_1 = Lucaml.F (pre ?evt_in1_1) in
       ...
       let val_in_n = Lucaml.B (pre ?evt_inn_n) in
       let lucky_out, lucky_local = 
         step ["in1",val_in_1; ...; "inn",val_in_n] 
       in
       List.iter
         (function
	  | ("out1",Lucaml.I x) -> emit evt_out1_n_pl_m x
          ...
	  | ("outn",Lucaml.F x) -> emit evt_outm_n_pl_m x)
         lucky_out
     end

*)

let lucky_to_parse (id,inputs,outputs,files) =
  let inputs, outputs = rename inputs outputs in
  let main_loop = 
    make_expr 
      (Pexpr_loop
	 (make_expr
	    (Pexpr_seq 
	       ((make_expr Pexpr_pause),
		(make_expr 
		   (Pexpr_let
		      (Nonrecursive,
		       get_inputs inputs,
		       make_expr 
			 (Pexpr_let 
			    (Nonrecursive,
			     [make_pattern (Ppatt_tuple
					      [make_var_patt "lucky_out";
					       make_var_patt "lucky_loc";]),
			      make_step inputs],
			     emit_outputs outputs))
		      ))
		)))))
  in
  let body =
    make_expr
      (Pexpr_let
	 (Nonrecursive,
	  [make_var_patt "state",
	   make_expr
             (Pexpr_apply
		(make_expr (Pexpr_ident (make_ident (Pident "ref"))),
		 [make_expr 
		    (Pexpr_apply
		       (make_expr
			  (Pexpr_ident (make_ident (Pdot ("Lucaml_nolbl","make")))),
			[make_expr 
(*			   (Pexpr_constant (Const_int 0)); *)
			   (Pexpr_apply 
			      (make_expr
				 (Pexpr_ident 
				    (make_ident (Pdot("Random","int")))),
			       [make_expr 
				  (Pexpr_constant (Const_int max_int))]));
			 make_expr
			   (Pexpr_constant (Const_bool false));
			 make_expr
			   (Pexpr_constant (Const_string ""));
			 make_expr
			   (Pexpr_constant (Const_bool false));
			 expr_of_files files;]))]))],
	  make_expr
	    (Pexpr_let
	       (Nonrecursive,
		[make_var_patt "step", 
		 make_expr 
		   (Pexpr_apply
		      (make_expr
			 (Pexpr_ident 
			    (make_ident (Pdot ("Lucaml_nolbl","step_se")))),
			  [make_expr
			     (Pexpr_ident (make_ident (Pident "step_mode")));
			   make_expr
			     (Pexpr_ident (make_ident (Pident "state")))]))],
		main_loop))))
  in
  Pimpl_let
    (Nonrecursive,
     [{ ppatt_desc = Ppatt_var id;
	ppatt_loc = id.psimple_loc; },
      make_expr
	(Pexpr_function
	   [make_var_patt "step_mode",
	    make_expr
	      (Pexpr_function
		 [patt_of_inputs inputs,
		  make_expr
		    (Pexpr_function
		       [patt_of_outputs outputs,
			make_expr (Pexpr_process body)
		      ])
		])
	  ])
    ])
