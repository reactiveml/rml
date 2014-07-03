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

(* file: caml_misc.ml *)
(* created: 2005-08-03  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Functions on Caml AST *)

open Asttypes
open Def_types
open Caml_ast
open Global
open Global_ident
open Misc

(* Building functions *)

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

let make_instruction s =
  make_expr
    (Cexpr_global
       { gi = { qual = !interpreter_module;
		id = Ident.create Ident.gen_var s Ident.Internal };
	 info = no_info(); })
    Location.none

let make_module_value mod_name val_name =
  make_expr
    (Cexpr_global
       { gi = { qual = mod_name;
		id = Ident.create Ident.gen_var val_name Ident.Internal };
	 info = no_info(); })
    Location.none


let make_rml_type s ty_list =
  make_te
    (Ctype_constr ({ gi = { qual = !interpreter_module;
			    id = Ident.create Ident.gen_type s Ident.Type };
		     info = no_info(); }, ty_list))
    Location.none

let make_patt_any () =
  make_patt Cpatt_any Location.none

let make_patt_unit () =
  make_patt (Cpatt_constant Const_unit) Location.none

let make_expr_unit () =
  make_expr (Cexpr_constant Const_unit) Location.none

let make_patt_var_local id =
  make_patt (Cpatt_var (Cvarpatt_local id)) Location.none

let make_expr_var_local id =
  make_expr (Cexpr_local id) Location.none

let make_list c_list =
  List.fold_right
    (fun e acc ->
      make_expr
	(Cexpr_construct
	   (Initialization.cons_constr_desc,
	    Some
	      (make_expr
		 (Cexpr_tuple [e; acc])
		 Location.none)))
	Location.none)
    c_list
    (make_expr
       (Cexpr_construct (Initialization.nil_constr_desc, None))
       Location.none)

let make_raise_RML () =
  make_expr
    (Cexpr_apply
       (make_expr
	  (Cexpr_global
	     { gi = { qual = pervasives_module;
		      id = Ident.create Ident.gen_var "raise" Ident.Val_ML };
	       info = no_info(); })
	  Location.none,
	[make_expr
	   (Cexpr_construct
	      ({ gi = { qual = !interpreter_module;
			id = Ident.create Ident.gen_constr
			  "RML" Ident.Internal };
		 info = no_info(); },
	       None))
	   Location.none]))
    Location.none

let make_rml_exec_hook () =
  let n_hook =
    if !number_of_instant >= 0 then
      let n_hook =
        make_module_value !Misc.rml_machine_module "n_hook"
      in
      let n =
        make_expr (Cexpr_constant (Const_int !number_of_instant)) Location.none
      in
      [ make_expr (Cexpr_apply (n_hook, [n])) Location.none ]
    else []
  in
  let sampling_hook =
    if !sampling >= 0.0 then
      let sampling_hook =
        make_module_value !Misc.rml_machine_module "sampling_hook"
      in
      let s =
        make_expr (Cexpr_constant (Const_float !sampling)) Location.none
      in
      [ make_expr (Cexpr_apply (sampling_hook, [s])) Location.none ]
    else []
  in
  let debug_hook =
    if !with_debug then
      [ make_module_value !Misc.rml_machine_module "debug_hook" ]
    else
      []
  in
  let thread_hook =
    if !with_thread then
      [ make_module_value "Rml_async_body" "boi_hook" ]
    else
      []
  in
  make_list (n_hook @ sampling_hook @ debug_hook @ thread_hook)


(* Creates the expression "()" *)
let make_unit () =
  make_expr (Cexpr_constant Const_unit) Location.none

(* Creates the expression "true" *)
let make_true () =
  make_expr (Cexpr_constant (Const_bool true)) Location.none

(* Creates the expression "false" *)
let make_false () =
  make_expr (Cexpr_constant (Const_bool false)) Location.none

(* Creates the expression "ref e" *)
let make_ref e =
  make_expr
    (Cexpr_apply
       (make_expr
	  (Cexpr_global
	     { gi = { qual = "Pervasives";
		      id = Ident.create Ident.gen_var "ref" Ident.Internal };
	       info = no_info(); })
	  Location.none,
	[e]))
    Location.none

(* Creates the expression "!vref" *)
let deref vref =
  make_expr
    (Cexpr_apply
       (make_expr
	  (Cexpr_global
	     { gi = { qual = "Pervasives";
		      id = Ident.create Ident.gen_var "!" Ident.Internal };
	       info = no_info(); })
	  Location.none,
	[make_expr_var_local vref]))
    Location.none


(* Creates the expression "Obj.magic ()" *)
let make_magic () =
  Cexpr_apply
    (make_expr
       (Cexpr_global
	  { gi = { qual="Obj";
		   id=Ident.create Ident.gen_var "magic" Ident.Internal };
	    info = no_info(); })
       Location.none,
     [make_unit()])

let make_magic_expr () =
  make_expr (make_magic ()) Location.none

(* Creates the pattern "None" *)
let make_patt_none () =
  let none =
    { gi = { qual="Pervasives";
             id=Ident.create Ident.gen_var "None" Ident.Internal; };
      info = no_info(); }
  in
  make_patt (Cpatt_construct (none, None)) Location.none


(* Creates the pattern "Some p" *)
let make_patt_some p =
  let some =
    { gi = { qual="Pervasives";
             id=Ident.create Ident.gen_var "Some" Ident.Internal; };
      info = no_info(); }
  in
  make_patt (Cpatt_construct (some, Some p)) Location.none

(* Translation of type expressions *)
let rec ctype_expr_of_type_expr typ =
  let ctyp =
    match typ.type_desc with
    | Type_var -> Ctype_any

    | Type_arrow (t1, t2) ->
	Ctype_arrow (ctype_expr_of_type_expr t1, ctype_expr_of_type_expr t2)

    | Type_product typ_list ->
	Ctype_product (List.map ctype_expr_of_type_expr typ_list)

    | Type_constr (cstr, te_list) ->
	let cstr_desc =
	  try
	    Modules.find_type_desc cstr.gi
	  with Modules.Desc_not_found -> assert false
	in
	Ctype_constr (cstr_desc, List.map ctype_expr_of_type_expr te_list)

    | Type_process (t, _) ->
	let proc_type = make_rml_type "process" [ctype_expr_of_type_expr t] in
	proc_type.cte_desc

    | Type_link t ->
	(ctype_expr_of_type_expr t).cte_desc

  in
  make_te ctyp Location.none


(* Create a dummy value from a type *)
let rec make_dummy t =
  let expr =
    begin match t.type_desc with
    | Type_var -> make_magic ()

    | Type_arrow _ | Type_process _ ->
	Cexpr_function
	  [make_patt Cpatt_any Location.none,
           None,
	   make_expr
	     (Cexpr_assert
		(make_expr (Cexpr_constant (Const_bool false)) Location.none))
	     Location.none]

    | Type_product te_list ->
	Cexpr_tuple (List.map make_dummy te_list)

    | Type_constr (cstr, te_list)  ->
	begin try
	  let type_desc = Modules.find_type_desc cstr.gi in
	  if type_desc = Initialization.type_desc_int then
	    Cexpr_constant (Const_int 4012)

	  else if type_desc = Initialization.type_desc_bool then
	    Cexpr_constant (Const_bool false)

	  else if type_desc = Initialization.type_desc_float then
	    Cexpr_constant (Const_float 4012.0)

	  else if type_desc = Initialization.type_desc_char then
	    Cexpr_constant (Const_char '?')

	  else if type_desc = Initialization.type_desc_string then
	    Cexpr_constant (Const_string "?")

	  else if type_desc = Initialization.type_desc_unit then
	    Cexpr_constant (Const_unit)

	  else if type_desc = Initialization.type_desc_exn then
	    Cexpr_construct
	      ({ gi =
		 { qual = !interpreter_module;
		   id = Ident.create Ident.gen_constr "RML" Ident.Internal };
		 info = no_info(); },
	       None)

	  else if type_desc = Initialization.type_desc_array then
	    Cexpr_array []

	  else if type_desc = Initialization.type_desc_event then
	    Cexpr_constraint (make_magic_expr(), ctype_expr_of_type_expr t)

	  else if type_desc = Initialization.type_desc_list then
	    Cexpr_construct (Initialization.nil_constr_desc, None)

	  else if type_desc = Initialization.type_desc_option then
	    Cexpr_construct (Initialization.none_constr_desc, None)

	  else
	    Cexpr_constraint (dummy_of_type_description type_desc,
			      ctype_expr_of_type_expr t)
	with _ ->
	  Cexpr_constraint (make_magic_expr(), ctype_expr_of_type_expr t)
	end

    | Type_link te ->
	(make_dummy te).cexpr_desc
    end
  in
  make_expr expr Location.none

and dummy_of_type_description t =
  let expr =
    begin match (Global.info t).type_kind with
    | Type_abstract -> make_magic_expr()

    | Type_rebind te -> make_dummy te

    | Type_variant (_::_ as cstr_list) ->
	let cstr =
	  try
	    List.find
	      (fun cstr -> (Global.info cstr).cstr_arg = None)
	      cstr_list
	  with Not_found -> List.hd cstr_list
	in
	let arg =
	  match (Global.info cstr).cstr_arg with
	  | None -> None
	  | Some te -> Some (make_dummy te)
	in
	make_expr (Cexpr_construct(cstr, arg)) Location.none

    | Type_variant [] -> make_magic_expr()

    | Type_record lbl_list ->
	let body =
	  List.map
	    (fun lbl -> (lbl, make_dummy (Global.info lbl).lbl_res))
	    lbl_list
	in
	make_expr (Cexpr_record body) Location.none
    end
  in
  expr


(* Test if an expression is an ML value*)
let rec is_value e =
  match e.cexpr_desc with
  | Cexpr_local _  | Cexpr_global _ | Cexpr_constant _
  | Cexpr_function _ | Cexpr_fun _ ->
      true

  | Cexpr_let(_, patt_expr_list, expr) ->
      (is_value expr)
	&&
      (List.for_all (fun (_, e) -> is_value e) patt_expr_list)

  | Cexpr_tuple expr_list ->
      List.for_all is_value expr_list

  | Cexpr_construct (_, None) -> true
  | Cexpr_construct (_, Some expr) -> is_value expr

  | Cexpr_constraint (expr, _) -> is_value expr

  | Cexpr_trywith (expr, patt_expr_list) ->
      (is_value expr)

  | Cexpr_assert expr -> false

  | Cexpr_ifthenelse (e1, e2, e3) ->
      (is_value e1) && (is_value e2) && (is_value e3)

  | Cexpr_match (expr, patt_expr_list) ->
      (is_value expr)
	&&
      (List.for_all
         (fun (_, when_opt, e) -> when_opt = None && is_value e)
         patt_expr_list)

  | _ -> false

(* Test if an expression is an ML constant *)
let rec is_constant e =
  match e.cexpr_desc with
  | Cexpr_local _  | Cexpr_global _ | Cexpr_constant _ ->
      true
  | Cexpr_tuple expr_list ->
      List.for_all is_constant expr_list

  | Cexpr_construct (_, None) -> true
  | Cexpr_construct (_, Some expr) -> is_constant expr

  | Cexpr_constraint (expr, _) -> is_constant expr

  | _ -> false


(* Test is a pattern is a partial matching *)
let rec partial_match patt =
  match patt.cpatt_desc with
  | Cpatt_any -> false
  | Cpatt_var _ -> false
  | Cpatt_alias (p, _) -> partial_match p
  | Cpatt_constant Const_unit -> false
  | Cpatt_constant _ -> true
  | Cpatt_tuple patt_list -> List.exists partial_match patt_list
  | Cpatt_construct _ -> true
  | Cpatt_or (p1, p2) -> (partial_match p1) && (partial_match p2)
  | Cpatt_record _ -> true
  | Cpatt_array _ -> true
  | Cpatt_constraint (p, _) -> partial_match p

