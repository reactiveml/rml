(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : reac2reac.ml                                               *)
(*  Date de creation : 07/09/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Source to source transformations *)

open Asttypes
open Reac_ast
open Def_types
open Reac_misc

(* Generic source to source transformation for Reac_ast *)
let expr_map  =
  let rec config_map f config =
    let loc = config.conf_loc in
    match config.conf_desc with
    | Rconf_present e -> 
	let e' = f e in
	make_conf (Rconf_present e') loc 
    | Rconf_and (c1, c2) ->
	let c1' = config_map f c1 in
	let c2' = config_map f c2 in
	make_conf (Rconf_and (c1', c2')) loc
    | Rconf_or (c1, c2) ->
	let c1' = config_map f c1 in
	let c2' = config_map f c2 in
	make_conf (Rconf_or (c1', c2')) loc
  in
  let rec expr_map f expr =
    let typ = expr.expr_type in
    let static = expr.expr_static in
    let loc = expr.expr_loc in
    let expr' =
      match expr.expr_desc with
      | Rexpr_local _ -> f expr
	    
      | Rexpr_global _ -> f expr
	    
      | Rexpr_constant _ -> f expr
	    
      | Rexpr_let (rec_flag, patt_expr_list, expr) ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  let expr' = expr_map f expr in
	  f (make_expr_all (Rexpr_let (rec_flag, patt_expr_list', expr'))
	       typ static loc)
	    
      | Rexpr_function patt_expr_list ->
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr_all (Rexpr_function patt_expr_list') typ static loc)
	    
      | Rexpr_apply (e, expr_list) ->
	  let e' = expr_map f e in
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all (Rexpr_apply (e', expr_list')) typ static loc)
	    
      | Rexpr_tuple expr_list ->
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all (Rexpr_tuple expr_list') typ static loc)
	    
      | Rexpr_construct (const, None) ->
	  f expr
      | Rexpr_construct (const, Some e) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_construct (const, Some e')) typ static loc)
	    
      | Rexpr_array expr_list ->
	  let expr_list' = 
	    List.map (fun e -> expr_map f e) expr_list
	  in
	  f (make_expr_all (Rexpr_array expr_list') typ static loc)
	    
      | Rexpr_record lbl_expr_list ->
	  let lbl_expr_list' =
	    List.map (fun (lbl,e) -> (lbl, expr_map f e)) lbl_expr_list
	  in
	  f (make_expr_all (Rexpr_record lbl_expr_list') typ static loc)
	    
      | Rexpr_record_access (e, lbl) -> 
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_record_access (e', lbl)) typ static loc)
	    
      | Rexpr_record_update (e1, lbl, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_record_update (e1', lbl, e2')) 
	       typ static loc)
	    
      | Rexpr_constraint (e, ty) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_constraint (e',ty)) typ static loc)
	    
      | Rexpr_trywith (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr_all (Rexpr_trywith(e', patt_expr_list')) typ static loc)
	    
      | Rexpr_assert e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_assert e') typ static loc)
	    
      | Rexpr_ifthenelse(e,e1,e2) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_ifthenelse(e',e1',e2')) typ static loc)
	    
      | Rexpr_match (e, patt_expr_list) ->
	  let e' = expr_map f e in
	  let patt_expr_list' =
	    List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
	  in
	  f (make_expr_all (Rexpr_match (e', patt_expr_list')) typ static loc)
	    
      | Rexpr_when_match (e1,e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_when_match (e1', e2')) typ static loc)
	    
      | Rexpr_while (e1,e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_while (e1', e2')) typ static loc)
	    
      | Rexpr_for (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all ( Rexpr_for (ident, e1', e2', direction_flag, e')) 
	       typ static loc)
	    
      | Rexpr_seq e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr_all (Rexpr_seq e_list') typ static loc)
	    
      | Rexpr_process e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_process e') typ static loc)
	    
      | Rexpr_pre (pre_kind, e) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_pre (pre_kind, e')) typ static loc)
	    
      | Rexpr_nothing ->
	  f expr
	    
      | Rexpr_pause ->
	  f expr

      | Rexpr_halt ->
	  f expr
	    
      | Rexpr_emit e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_emit e') typ static loc)
	    
      | Rexpr_emit_val (e1, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_emit_val (e1', e2')) typ static loc)
	    
      | Rexpr_loop (n_opt, e) ->
	  let n_opt' = Misc.opt_map (expr_map f) n_opt in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_loop (n_opt', e')) typ static loc)
	    
      | Rexpr_fordopar (ident, e1, e2, direction_flag, e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all 
	       (Rexpr_fordopar (ident, e1', e2', direction_flag, e')) 
	       typ static loc)
	    
      | Rexpr_par e_list ->
	  let e_list' = List.map (fun e -> expr_map f e) e_list in
	  f (make_expr_all (Rexpr_par e_list') typ static loc)
	    
      | Rexpr_merge (e1, e2) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_merge (e1',e2')) typ static loc)
	    
      | Rexpr_signal (id_tyexpr_opt, None, e) ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_signal (id_tyexpr_opt, None, e')) 
	       typ static loc)
      | Rexpr_signal (id_tyexpr_opt, Some(e1,e2), e) ->
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_signal (id_tyexpr_opt, Some(e1',e2'), e')) 
	       typ static loc)
	    
      | Rexpr_run e ->
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_run e') typ static loc)
	    
      | Rexpr_until (config, e, None) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr_all ( Rexpr_until (config', e', None)) typ static loc)
      | Rexpr_until (config, e, Some(p,e1)) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr_all ( Rexpr_until (config', e', Some(p,e1'))) 
	       typ static loc)
	    
      | Rexpr_when (config, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_when (config',e')) typ static loc)
	    
      | Rexpr_control (config, e) ->
	  let config' = config_map f config in
	  let e' = expr_map f e in
	  f (make_expr_all (Rexpr_control (config', e')) typ static loc)
	    
      | Rexpr_get (e,patt,e1) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr_all (Rexpr_get (e',patt,e1')) typ static loc)
	    
      | Rexpr_present (config, e1, e2) ->
	  let config' = config_map f config in
	  let e1' = expr_map f e1 in
	  let e2' = expr_map f e2 in
	  f (make_expr_all (Rexpr_present (config', e1', e2')) typ static loc)
	    
      | Rexpr_await (immediate_flag, config) ->
	  let config' = config_map f config in
	  f (make_expr_all (Rexpr_await (immediate_flag, config'))
	       typ static loc)
	    
      | Rexpr_await_val (immediate, kind, e, patt, e1) ->
	  let e' = expr_map f e in
	  let e1' = expr_map f e1 in
	  f (make_expr_all (Rexpr_await_val (immediate, kind, e', patt, e1')) 
	       typ static loc)
    in
    expr'.expr_type <- expr.expr_type;
    expr'.expr_static <- expr.expr_static;
    expr'
  in
  expr_map

let impl_map f impl =
  let loc = impl.impl_loc in
  match impl.impl_desc with
  | Rimpl_expr e -> make_impl (Rimpl_expr (expr_map f e)) loc
  | Rimpl_let (rec_flag, patt_expr_list) ->
      let patt_expr_list' =
	List.map (fun (p,e) -> (p, expr_map f e)) patt_expr_list
      in
      make_impl (Rimpl_let (rec_flag, patt_expr_list')) loc

  | Rimpl_signal decl_list ->
      let decl_list' =
	List.map
	  (function    
	    | (id_tyexpr_opt, Some(e1,e2)) ->
		let e1' = expr_map f e1 in
		let e2' = expr_map f e2 in
		(id_tyexpr_opt, Some(e1',e2'))
	    | decl -> decl)
	  decl_list
      in
      make_impl (Rimpl_signal decl_list') loc

  | _ -> impl



(* Translate binary seq and par to n-ary operators *)
let binary2nary e =
  let e' =
    match e.expr_desc with
    | Rexpr_seq e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_seq e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
      in Rexpr_seq (f [] e_list)
	  
    | Rexpr_par e_list ->
	let rec f left l =
	  match l with
	  | { expr_desc = Rexpr_par e_list' } :: l' ->
	      let left' = left @ e_list' in
	      f left' l'
	  | x :: l' ->
	      let left' = left @ [x] in
	      f left' l'
	  | [] -> left
	in Rexpr_par (f [] e_list)
    | e -> e
  in { e with expr_desc = e' }


(* Translate reactive seq to combinatorial seq *)
let dynamic2static e =
  begin match e.expr_desc with
  | Rexpr_seq e_list ->
      let rec f left l =
	match l with
	| ({ expr_static = Def_static.Static } as e1) 
	  :: ({ expr_static = Def_static.Static } as e2) :: l' ->
	    let e' =  
	      make_expr 
		(Rexpr_seq [e1;e2]) 
		(Location.concat e1.expr_loc e2.expr_loc)
	    in
	    e'.expr_static <- Def_static.Static;
	    f left (e'::l')
	| x :: l' ->
	    let left' = left @ [x] in
	    f left' l'
	| [] -> left
      in 
      begin match f [] e_list with
      | [] -> assert false
      | [e] -> e
      | e_list' ->
	  let e' = make_expr (Rexpr_seq e_list') e.expr_loc in
	  e'.expr_static <- Def_static.Dynamic;
	  e'
      end
  | _ -> e
  end


(* Translate for to loop_n *)
let for2loop_n expr = 
  begin match expr.expr_desc with
  | Rexpr_for(ident, e1, e2, direction_flag, e) 
    when expr.expr_static = Def_static.Dynamic->
      let fv = expr_free_vars e in
      if is_free (Varpatt_local ident) fv then
	begin
	  let n = 
	    let minus = 
	      make_expr
		(Rexpr_global 
		   (Modules.pfind_value_desc 
		      (Parse_ident.Pident ("-"))))
		Location.none
	    in
	    minus.expr_static <- Def_static.Static;
	    minus.expr_type <- 
	      Types.arrow 
		Initialization.type_int
		(Types.arrow 
		   Initialization.type_int 
		   Initialization.type_int);
            let e' = 
	      make_expr
		(Rexpr_apply
		   (minus,
		    begin match direction_flag with
		    | Upto -> [e2; e1]
		    | Downto -> [e1; e2]
		    end))
		Location.none
	    in
	    e'.expr_static <- Def_static.Static;
	    e'.expr_type <- Initialization.type_int;
	    let plus = 
	      make_expr
		(Rexpr_global 
		   (Modules.pfind_value_desc 
		      (Parse_ident.Pident ("+"))))
		Location.none
	    in
	    plus.expr_static <- Def_static.Static;
	    plus.expr_type <- 
	      Types.arrow 
		Initialization.type_int
		(Types.arrow 
		   Initialization.type_int 
		   Initialization.type_int);
            let one =  
	      make_expr
		(Rexpr_constant (Const_int 1))
		Location.none
	    in
	    one.expr_static <- Def_static.Static;
	    one.expr_type <- Initialization.type_int;
	    let n = 
	      make_expr
		(Rexpr_apply (plus, [e'; one]))
		Location.none
	    in
	    n.expr_static <- Def_static.Static;
	    n.expr_type <- Initialization.type_int;
	    n
	  in
	  let loop_n =
	    make_expr
	      (Rexpr_loop(Some n, e))
	      expr.expr_loc
	  in
	  loop_n.expr_static <- Def_static.Dynamic;
	  loop_n.expr_type <- Initialization.type_unit;
	  loop_n
	end
      else
	expr
  | _ -> expr
  end


let print_static e = 
  Location.print_oc stderr e.expr_loc;
  if e.expr_static = Def_static.Dynamic then
    prerr_string " : Dynamic"
  else
    prerr_string " : Static";
  prerr_newline ();
  e
