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
let type_int = Types.constr_notabbrev int_ident []

(* bool *)
let bool_ident = pervasives_type "bool"
let type_desc_bool = type_desc bool_ident
let type_bool = Types.constr_notabbrev bool_ident []

(* float *)
let float_ident = pervasives_type "float"
let type_desc_float = type_desc float_ident
let type_float = Types.constr_notabbrev float_ident []

(* char *)
let char_ident = pervasives_type "char"
let type_desc_char = type_desc char_ident
let type_char = Types.constr_notabbrev char_ident []

(* string *)
let string_ident = pervasives_type "string"
let type_desc_string = type_desc string_ident
let type_string = Types.constr_notabbrev string_ident []

(* unit *)
let unit_ident = pervasives_type "unit"
let type_desc_unit = type_desc unit_ident
let type_unit =  Types.constr_notabbrev unit_ident []

(* exn *)
let exn_ident = pervasives_type "exn"
let type_desc_exn = type_desc exn_ident
let type_exn = Types.constr_notabbrev exn_ident []

(* array *)
let array_ident = pervasives_type "array"
let type_desc_array =
  { gi = array_ident;
    info = Some { type_constr = { gi = array_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 1; } }
let type_array = Types.constr_notabbrev array_ident [Types.new_generic_var()]

(* event *)
(* let event_ident = interpreter_type "event" *)
let event_ident = pervasives_type "event"

let type_desc_event =
  { gi = event_ident;
    info = Some { type_constr = { gi = event_ident;
				  info = Some{ constr_abbr=Constr_notabbrev} };
		  type_kind = Type_abstract;
		  type_arity = 2; } }
let type_event = Types.constr_notabbrev event_ident [Types.new_generic_var();
						     Types.new_generic_var(); ]


(* list *)
let list_ident = pervasives_type "list"

let nil_ident = pervasives_constr "[]"
let nil_constr_desc =
  let var = Types.new_generic_var() in
  let nil_constr =
    { cstr_arg = None;
      cstr_res = Types.constr_notabbrev list_ident [var] }
  in
  { gi = nil_ident;
    info = Some nil_constr; }

let cons_ident = pervasives_constr "::"
let cons_constr_desc =
  let var = Types.new_generic_var() in
  let var_list = Types.constr_notabbrev list_ident [var] in
  let cons_constr =
    { cstr_arg = Some (Types.product [var; var_list]);
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

let type_list = Types.constr_notabbrev list_ident [Types.new_generic_var()]

(* option *)
let option_ident = pervasives_type "option"

let none_ident = pervasives_constr "None"
let none_constr_desc =
  let var = Types.new_generic_var() in
  let none_constr =
    { cstr_arg = None;
      cstr_res = Types.constr_notabbrev option_ident [var] }
  in
  { gi = none_ident;
    info = Some none_constr; }

let some_ident = pervasives_constr "Some"
let some_constr_desc =
  let var = Types.new_generic_var() in
  let some_constr =
    { cstr_arg = Some var;
      cstr_res = Types.constr_notabbrev option_ident [var]; }
  in
  { gi = some_ident;
    info = Some some_constr; }

let type_desc_option =
  { gi = option_ident;
    info = Some { type_constr = { gi = option_ident;
				  info = Some { constr_abbr=Constr_notabbrev} };
		  type_kind = Type_variant [none_constr_desc; some_constr_desc];
		  type_arity = 1; } }

let type_option = Types.constr_notabbrev option_ident [Types.new_generic_var()]


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


