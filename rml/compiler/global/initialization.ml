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

(* file: initialization.ml *)

(* Warning: *)
(* This file is based on the original version of initialization.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6 *)

(* first modification: 2004-05-06  *)
(* modified by: Louis Mandel *)

(* $Id$ *)

(* the initial module *)

open Misc
open Ident
open Global_ident
open Def_types
open Global


let pervasives_type id =
  { qual = pervasives_module;
    id = Ident.create Ident.gen_type id Ident.Type }
let interpreter_type id =
  { qual = !interpreter_module;
    id = Ident.create Ident.gen_type id Ident.Type }
let pervasives_constr id =
  { qual = pervasives_module;
    id = Ident.create Ident.gen_constr id Ident.Constr }
let pervasives_val id =
  { qual = pervasives_module;
    id = Ident.create Ident.gen_var id Ident.Val_ML }


let abstract_type id = { type_constr =
			 { gi = id;
			   info = Some { constr_abbr = Constr_notabbrev}};
			 type_kind = Type_abstract;
			 type_arity = 0; }
let type_desc id =
  { gi = id;
    info = Some (abstract_type id) }

(* int *)
let int_ident = pervasives_type "int"
let type_desc_int = type_desc int_ident
let type_int = Def_types.constr_notabbrev int_ident []

(* bool *)
let bool_ident = pervasives_type "bool"
let type_desc_bool = type_desc bool_ident
let type_bool = Def_types.constr_notabbrev bool_ident []

(* float *)
let float_ident = pervasives_type "float"
let type_desc_float = type_desc float_ident
let type_float = Def_types.constr_notabbrev float_ident []

(* char *)
let char_ident = pervasives_type "char"
let type_desc_char = type_desc char_ident
let type_char = Def_types.constr_notabbrev char_ident []

(* string *)
let string_ident = pervasives_type "string"
let type_desc_string = type_desc string_ident
let type_string = Def_types.constr_notabbrev string_ident []

(* unit *)
let unit_ident = pervasives_type "unit"
let type_desc_unit = type_desc unit_ident
let type_unit =  Def_types.constr_notabbrev unit_ident []

(* exn *)
let exn_ident = pervasives_type "exn"
let type_desc_exn = type_desc exn_ident
let type_exn = Def_types.constr_notabbrev exn_ident []

(* array *)
let array_ident = pervasives_type "array"
let type_desc_array =
  { gi = array_ident;
    info = Some { type_constr = { gi = array_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 1; } }
let type_array = Def_types.constr_notabbrev array_ident [Def_types.new_generic_var()]

(* event *)
let event_ident = pervasives_type "event"

let type_desc_event =
  { gi = event_ident;
    info = Some { type_constr = { gi = event_ident;
				  info = Some{ constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 4; } }

let is_event ty = match ty.type_desc with
  | Type_constr (c, _) ->
      c.gi.qual = event_ident.qual &&
      c.gi.id.name = event_ident.id.name
  | _ -> false

(* usages *)
let affine_ident = pervasives_type "affine"
let type_desc_affine = type_desc affine_ident
let type_affine = Def_types.constr_notabbrev affine_ident []

let is_affine ty = match ty.type_desc with
  | Type_constr (c, []) ->
      c.gi.qual = affine_ident.qual &&
      c.gi.id.name = affine_ident.id.name
  | _ -> false

let neutral_ident = pervasives_type "neutral"
let type_desc_neutral = type_desc neutral_ident
let type_neutral = Def_types.constr_notabbrev neutral_ident []

let is_neutral ty = match ty.type_desc with
  | Type_constr (c, []) ->
      c.gi.qual = neutral_ident.qual &&
      c.gi.id.name = neutral_ident.id.name
  | _ -> false

let zero_ident = pervasives_type "zero"
let type_desc_zero = type_desc zero_ident
let type_zero = Def_types.constr_notabbrev zero_ident []

let is_zero ty = match ty.type_desc with
  | Type_constr (c, []) ->
      c.gi.qual = zero_ident.qual &&
      c.gi.id.name = zero_ident.id.name
  | _ -> false

let zero1_ident = pervasives_type "zero1"
let type_desc_zero1 = type_desc zero1_ident
let type_zero1 = Def_types.constr_notabbrev zero1_ident []

let is_zero1 ty = match ty.type_desc with
  | Type_constr (c, []) ->
      c.gi.qual = zero1_ident.qual &&
      c.gi.id.name = zero1_ident.id.name
  | _ -> false

let uvar_ident = pervasives_type "var"
let type_desc_uvar = type_desc uvar_ident
let type_uvar = Def_types.constr_notabbrev uvar_ident []

let is_uvar ty = match ty.type_desc with
  | Type_constr (c, []) ->
      c.gi.qual = uvar_ident.qual &&
      c.gi.id.name = uvar_ident.id.name
  | _ -> false

let is_usage ty =
  is_zero ty || is_zero1 ty || is_affine ty || is_neutral ty || is_uvar ty

(* list *)
let list_ident = pervasives_type "list"

let nil_ident = pervasives_constr "[]"
let nil_constr_desc =
  let var = Def_types.new_generic_var() in
  let nil_constr =
    { cstr_arg = None;
      cstr_res = Def_types.constr_notabbrev list_ident [var] }
  in
  { gi = nil_ident;
    info = Some nil_constr; }

let cons_ident = pervasives_constr "::"
let cons_constr_desc =
  let var = Def_types.new_generic_var() in
  let var_list = Def_types.constr_notabbrev list_ident [var] in
  let cons_constr =
    { cstr_arg = Some (Def_types.product [var; var_list]);
      cstr_res = var_list; }
  in
  { gi = cons_ident;
    info = Some cons_constr; }

let type_desc_list =
  { gi = list_ident;
    info = Some { type_constr = { gi = list_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_variant [nil_constr_desc; cons_constr_desc];
		  type_arity = 1; } }

let type_list = Def_types.constr_notabbrev list_ident [Def_types.new_generic_var()]

(* option *)
let option_ident = pervasives_type "option"

let none_ident = pervasives_constr "None"
let none_constr_desc =
  let var = Def_types.new_generic_var() in
  let none_constr =
    { cstr_arg = None;
      cstr_res = Def_types.constr_notabbrev option_ident [var] }
  in
  { gi = none_ident;
    info = Some none_constr; }

let some_ident = pervasives_constr "Some"
let some_constr_desc =
  let var = Def_types.new_generic_var() in
  let some_constr =
    { cstr_arg = Some var;
      cstr_res = Def_types.constr_notabbrev option_ident [var]; }
  in
  { gi = some_ident;
    info = Some some_constr; }

let type_desc_option =
  { gi = option_ident;
    info = Some { type_constr = { gi = option_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_variant [none_constr_desc; some_constr_desc];
		  type_arity = 1; } }

let type_option = Def_types.constr_notabbrev option_ident [Def_types.new_generic_var()]


let list_of_type_desc =
  [ type_desc_int;
    type_desc_bool;
    type_desc_float;
    type_desc_char;
    type_desc_string;
    type_desc_unit;
    type_desc_exn;
    type_desc_array;
    type_desc_list;
    type_desc_option;
    type_desc_event;
    type_desc_affine;
    type_desc_neutral;
    type_desc_zero;
    type_desc_uvar;
]

let list_of_constr_desc =
  [ cons_constr_desc;
    nil_constr_desc;
    none_constr_desc;
    some_constr_desc; ]


let load_initial_modules () =
  List.iter Modules.add_type list_of_type_desc;
  List.iter Modules.add_constr list_of_constr_desc


