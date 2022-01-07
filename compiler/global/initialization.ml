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

open Rml_misc
open Global_ident
open Def_types
open Global


let stdlib_type id =
  { qual = stdlib_module;
    id = Rml_ident.create Rml_ident.gen_type id Rml_ident.Type }
let interpreter_type id =
  { qual = !interpreter_module;
    id = Rml_ident.create Rml_ident.gen_type id Rml_ident.Type }
let stdlib_constr id =
  { qual = stdlib_module;
    id = Rml_ident.create Rml_ident.gen_constr id Rml_ident.Constr }
let stdlib_val id =
  { qual = stdlib_module;
    id = Rml_ident.create Rml_ident.gen_var id Rml_ident.Val_ML }


let abstract_type id = { type_constr =
			 { gi = id;
			   info = Some { constr_abbr = Constr_notabbrev}};
			 type_kind = Type_abstract;
			 type_arity = 0; }
let type_desc id =
  { gi = id;
    info = Some (abstract_type id) }

(* int *)
let int_ident = stdlib_type "int"
let type_desc_int = type_desc int_ident
let type_int = Rml_types.constr_notabbrev int_ident []

(* bool *)
let bool_ident = stdlib_type "bool"
let type_desc_bool = type_desc bool_ident
let type_bool = Rml_types.constr_notabbrev bool_ident []

(* float *)
let float_ident = stdlib_type "float"
let type_desc_float = type_desc float_ident
let type_float = Rml_types.constr_notabbrev float_ident []

(* char *)
let char_ident = stdlib_type "char"
let type_desc_char = type_desc char_ident
let type_char = Rml_types.constr_notabbrev char_ident []

(* string *)
let string_ident = stdlib_type "string"
let type_desc_string = type_desc string_ident
let type_string = Rml_types.constr_notabbrev string_ident []

(* unit *)
let unit_ident = stdlib_type "unit"
let type_desc_unit = type_desc unit_ident
let type_unit =  Rml_types.constr_notabbrev unit_ident []

(* exn *)
let exn_ident = stdlib_type "exn"
let type_desc_exn = type_desc exn_ident
let type_exn = Rml_types.constr_notabbrev exn_ident []

(* array *)
let array_ident = stdlib_type "array"
let type_desc_array =
  { gi = array_ident;
    info = Some { type_constr = { gi = array_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 1; } }
let type_array = Rml_types.constr_notabbrev array_ident [Rml_types.new_generic_var()]

(* event *)
(* let event_ident = interpreter_type "event" *)
let event_ident = stdlib_type "event"

let type_desc_event =
  { gi = event_ident;
    info = Some { type_constr = { gi = event_ident;
				  info = Some{ constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 2; } }
let type_event = Rml_types.constr_notabbrev event_ident [Rml_types.new_generic_var();
						     Rml_types.new_generic_var(); ]


(* list *)
let list_ident = stdlib_type "list"

let nil_ident = stdlib_constr "[]"
let nil_constr_desc =
  let var = Rml_types.new_generic_var() in
  let nil_constr =
    { cstr_arg = None;
      cstr_res = Rml_types.constr_notabbrev list_ident [var] }
  in
  { gi = nil_ident;
    info = Some nil_constr; }

let cons_ident = stdlib_constr "::"
let cons_constr_desc =
  let var = Rml_types.new_generic_var() in
  let var_list = Rml_types.constr_notabbrev list_ident [var] in
  let cons_constr =
    { cstr_arg = Some (Rml_types.product [var; var_list]);
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

let type_list = Rml_types.constr_notabbrev list_ident [Rml_types.new_generic_var()]

(* option *)
let option_ident = stdlib_type "option"

let none_ident = stdlib_constr "None"
let none_constr_desc =
  let var = Rml_types.new_generic_var() in
  let none_constr =
    { cstr_arg = None;
      cstr_res = Rml_types.constr_notabbrev option_ident [var] }
  in
  { gi = none_ident;
    info = Some none_constr; }

let some_ident = stdlib_constr "Some"
let some_constr_desc =
  let var = Rml_types.new_generic_var() in
  let some_constr =
    { cstr_arg = Some var;
      cstr_res = Rml_types.constr_notabbrev option_ident [var]; }
  in
  { gi = some_ident;
    info = Some some_constr; }

let type_desc_option =
  { gi = option_ident;
    info = Some { type_constr = { gi = option_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_variant [none_constr_desc; some_constr_desc];
		  type_arity = 1; } }

let type_option = Rml_types.constr_notabbrev option_ident [Rml_types.new_generic_var()]


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
]

let list_of_constr_desc =
  [ cons_constr_desc;
    nil_constr_desc;
    none_constr_desc;
    some_constr_desc; ]


let load_initial_modules () =
  List.iter Modules.add_type list_of_type_desc;
  List.iter Modules.add_constr list_of_constr_desc


