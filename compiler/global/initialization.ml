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

open Asttypes
open Misc
open Compiler_options
open Ident
open Global_ident
open Clocks
open Types
open Global

let zero_arity =
  mk_kind_prod ~clock:0 ~carrier:0 ~carrier_row:0
    ~effect:0 ~effect_row:0 ~react:0
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
			ty_info = Some { constr_abbr = Constr_notabbrev};
      ck_info = None };
			 type_kind = Type_abstract;
			 type_arity = 0; }
let abstract_type_clock id =
  { clock_constr = { gi = id;
                     ty_info = None;
                     ck_info = Some { Clocks.constr_abbr =
                         Clocks.Constr_abbrev ([], Clocks_utils.static) } };
    clock_kind = Clock_rebind Clocks_utils.static;
    clock_arity = zero_arity;
    clock_def_arity = zero_arity; }
let type_desc id =
  { gi = id;
    ty_info = Some (abstract_type id);
    ck_info = Some (abstract_type_clock id) }

(* int *)
let int_ident = pervasives_type "int"
let type_desc_int = type_desc int_ident
let type_int = Types_utils.constr_notabbrev int_ident []

(* bool *)
let bool_ident = pervasives_type "bool"
let type_desc_bool = type_desc bool_ident
let type_bool = Types_utils.constr_notabbrev bool_ident []

(* float *)
let float_ident = pervasives_type "float"
let type_desc_float = type_desc float_ident
let type_float = Types_utils.constr_notabbrev float_ident []

(* char *)
let char_ident = pervasives_type "char"
let type_desc_char = type_desc char_ident
let type_char = Types_utils.constr_notabbrev char_ident []

(* string *)
let string_ident = pervasives_type "string"
let type_desc_string = type_desc string_ident
let type_string = Types_utils.constr_notabbrev string_ident []

(* unit *)
let unit_ident = pervasives_type "unit"
let type_desc_unit = type_desc unit_ident
let type_unit =  Types_utils.constr_notabbrev unit_ident []

(* exn *)
let exn_ident = pervasives_type "exn"
let type_desc_exn = type_desc exn_ident
let type_exn = Types_utils.constr_notabbrev exn_ident []

(* array *)
let array_ident = pervasives_type "array"
let array_arity = { zero_arity with k_clock = 1 }
let type_desc_array =
  { gi = array_ident;
    ty_info = Some { type_constr = { gi = array_ident;
				                             ty_info = Some { constr_abbr=Constr_notabbrev };
                                     ck_info = None  };
		                 type_kind = Type_abstract;
		                 type_arity = 1; };
    ck_info = Some { clock_constr = { gi = array_ident;
                                     ty_info = None;
				                             ck_info = Some { Clocks.constr_abbr= Clocks.Constr_notabbrev } };
		                 clock_kind = Clock_abstract;
		                 clock_arity = array_arity;
                     clock_def_arity = array_arity; } }

let type_array = Types_utils.constr_notabbrev array_ident [Types_utils.new_generic_var()]
let clock_array = Clocks_utils.constr_notabbrev array_ident [Kclock (Clocks_utils.new_generic_clock_var())]

(* event *)
(* let event_ident = interpreter_type "event" *)
let event_ident = pervasives_type "event"
let event_arity = { zero_arity with k_clock = 2; k_carrier_row = 1 }

let type_desc_event =
  { gi = event_ident;
    ty_info = Some { type_constr = { gi = event_ident;
				                             ty_info = Some{ constr_abbr=Constr_notabbrev};
                                     ck_info = None };
		                 type_kind = Type_abstract;
		                 type_arity = 2; };
    ck_info = Some { clock_constr = { gi = event_ident;
                                     ty_info = None;
				                             ck_info = Some{ Clocks.constr_abbr = Clocks.Constr_notabbrev} };
		                 clock_kind = Clock_abstract;
                     clock_def_arity = event_arity;
		                 clock_arity = event_arity; } }

let type_event = Types_utils.constr_notabbrev event_ident [Types_utils.new_generic_var();
						     Types_utils.new_generic_var(); ]
let clock_event = Clocks_utils.constr_notabbrev event_ident
  [Kclock (Clocks_utils.new_generic_clock_var());
   Kclock (Clocks_utils.new_generic_clock_var());
   Kcarrier_row (Clocks_utils.new_generic_carrier_row_var ()) ]


(* memory *)
let memory_ident = pervasives_type "mevent"
let memory_gather = pervasives_val "memory_gather"
let memory_arity = { zero_arity with k_clock = 1; k_carrier_row = 1 }

let type_desc_memory =
  { gi = memory_ident;
    ty_info = Some { type_constr = { gi = memory_ident;
				                             ty_info = Some{ constr_abbr=Constr_notabbrev};
                                     ck_info = None };
		                 type_kind = Type_abstract;
		                 type_arity = 1; };
    ck_info = Some { clock_constr = { gi = memory_ident;
                                     ty_info = None;
				                             ck_info = Some{ Clocks.constr_abbr = Clocks.Constr_notabbrev} };
		                 clock_kind = Clock_abstract;
                     clock_def_arity = memory_arity;
		                 clock_arity = memory_arity; } }

let type_memory = Types_utils.constr_notabbrev memory_ident [Types_utils.new_generic_var();
						     Types_utils.new_generic_var(); ]
let clock_memory = Clocks_utils.constr_notabbrev memory_ident
  [Kclock (Clocks_utils.new_generic_clock_var());
   Kcarrier_row (Clocks_utils.new_generic_carrier_row_var ()) ]



(* clock *)
let clock_ident = pervasives_type "clock"

let type_desc_clock =
  { gi = clock_ident;
    ty_info = Some { type_constr = { gi = clock_ident;
                                  ty_info = Some{ constr_abbr=Constr_notabbrev};
                                  ck_info = None };
                  type_kind = Type_abstract;
                  type_arity = 0; };
    ck_info = None; }
let type_clock = Types_utils.constr_notabbrev clock_ident []

(* list *)
let list_ident = pervasives_type "list"

let nil_ident = pervasives_constr "[]"
let nil_constr_ty_desc =
  let var = Types_utils.new_generic_var() in
  let nil_constr =
    { cstr_arg = None;
      cstr_res = Types_utils.constr_notabbrev list_ident [var] }
  in
  { gi = nil_ident;
    ty_info = Some nil_constr;
    ck_info = None }
let nil_constr_ck_desc =
  let ck_var = Clocks_utils.new_generic_clock_var() in
  let nil_constr_ck =
    { Clocks.cstr_arg = None;
      Clocks.cstr_res = Clocks_utils.constr_notabbrev list_ident [Kclock ck_var] }
  in
  { gi = nil_ident;
    ty_info = None;
    ck_info = Some nil_constr_ck }
let nil_constr_desc =
  { gi = nil_ident;
    ty_info = nil_constr_ty_desc.ty_info;
    ck_info = nil_constr_ck_desc.ck_info }

let cons_ident = pervasives_constr "::"
let cons_constr_ty_desc =
  let var = Types_utils.new_generic_var() in
  let var_list = Types_utils.constr_notabbrev list_ident [var] in
  let cons_constr =
    { cstr_arg = Some (Types_utils.product [var; var_list]);
      cstr_res = var_list; }
  in
  { gi = cons_ident;
    ty_info = Some cons_constr;
    ck_info = None }
let cons_constr_ck_desc =
  let ck_var = Clocks_utils.new_generic_clock_var() in
  let ck_var_list = Clocks_utils.constr_notabbrev list_ident [Kclock ck_var] in
  let cons_constr_ck =
    { Clocks.cstr_arg = Some (Clocks_utils.product [ck_var; ck_var_list]);
      Clocks.cstr_res = ck_var_list; }
  in
  { gi = cons_ident;
    ty_info = None;
    ck_info = Some cons_constr_ck }
let cons_constr_desc =
  { gi = cons_ident;
    ty_info = cons_constr_ty_desc.ty_info;
    ck_info = cons_constr_ck_desc.ck_info }

let list_arity = { zero_arity with k_clock = 1 }
let type_desc_list =
  { gi = list_ident;
    ty_info = Some { type_constr = { gi = list_ident;
				                             ty_info = Some { constr_abbr=Constr_notabbrev};
                                     ck_info = None };
		                 type_kind = Type_variant [nil_constr_ty_desc; cons_constr_ty_desc];
		                 type_arity = 1; };
    ck_info  = Some { clock_constr = { gi = list_ident;
                                      ty_info = None;
				                              ck_info = Some { Clocks.constr_abbr=Clocks.Constr_notabbrev} };
		                  clock_kind = Clock_variant [nil_constr_ck_desc; cons_constr_ck_desc];
                      clock_def_arity = list_arity;
		                  clock_arity = list_arity; } }

let type_list = Types_utils.constr_notabbrev list_ident [Types_utils.new_generic_var()]
let clock_list = Clocks_utils.constr_notabbrev list_ident [Kclock (Clocks_utils.new_generic_clock_var())]

(* option *)
let option_ident = pervasives_type "option"

let none_ident = pervasives_constr "None"
let none_constr_ty_desc =
  let var = Types_utils.new_generic_var() in
  let none_constr =
    { cstr_arg = None;
      cstr_res = Types_utils.constr_notabbrev option_ident [var] }
  in
  { gi = none_ident;
    ty_info = Some none_constr;
    ck_info = None; }
let none_constr_ck_desc =
  let var = Clocks_utils.new_generic_clock_var() in
  let none_constr =
    { Clocks.cstr_arg = None;
      Clocks.cstr_res = Clocks_utils.constr_notabbrev option_ident [Kclock var] }
  in
  { gi = none_ident;
    ty_info = None;
    ck_info = Some none_constr; }
let none_constr_desc =
  { gi = none_ident;
    ty_info = none_constr_ty_desc.ty_info;
    ck_info = none_constr_ck_desc.ck_info; }


let some_ident = pervasives_constr "Some"
let some_constr_ty_desc =
  let var = Types_utils.new_generic_var() in
  let some_constr =
    { cstr_arg = Some var;
      cstr_res = Types_utils.constr_notabbrev option_ident [var]; }
  in
  { gi = some_ident;
    ty_info = Some some_constr;
    ck_info = None }
let some_constr_ck_desc =
  let var = Clocks_utils.new_generic_clock_var() in
  let some_constr =
    { Clocks.cstr_arg = Some var;
      Clocks.cstr_res = Clocks_utils.constr_notabbrev option_ident [Kclock var]; }
  in
  { gi = some_ident;
    ty_info = None;
    ck_info = Some some_constr; }
let some_constr_desc =
  { gi = some_ident;
    ty_info = some_constr_ty_desc.ty_info;
    ck_info = some_constr_ck_desc.ck_info; }

let option_arity = { zero_arity with k_clock = 1 }
let type_desc_option =
  { gi = option_ident;
    ty_info = Some { type_constr = { gi = option_ident;
				                             ty_info = Some { constr_abbr=Constr_notabbrev};
                                     ck_info = None };
		                 type_kind = Type_variant [none_constr_ty_desc; some_constr_ty_desc];
		                 type_arity = 1; };
    ck_info = Some { clock_constr = { gi = option_ident;
                                      ty_info = None;
				                              ck_info = Some { Clocks.constr_abbr = Clocks.Constr_notabbrev} };
		                 clock_kind = Clock_variant [none_constr_ck_desc; some_constr_ck_desc];
                     clock_def_arity = option_arity;
		                 clock_arity = option_arity; } }

let type_option = Types_utils.constr_notabbrev option_ident [Types_utils.new_generic_var()]
let clock_option = Clocks_utils.constr_notabbrev option_ident [Kclock (Clocks_utils.new_generic_clock_var())]

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
    type_desc_memory;
    type_desc_clock;
]

let list_of_constr_desc =
  [ cons_constr_desc;
    nil_constr_desc;
    none_constr_desc;
    some_constr_desc; ]


let load_initial_modules () =
  List.iter Modules.add_type list_of_type_desc;
  List.iter Modules.add_constr list_of_constr_desc


