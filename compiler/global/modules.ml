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

(* file: modules.ml *)

(* Warning: *)
(* This file is based on the original version of modules.ml *)
(* from the CamlLight 0.75 distribution, INRIA              *)

(* first modification: 2004-04-26  *)
(* modified: Louis Mandel *)

(* $Id$ *)

open Rml_misc
open Global_ident
open Global
open Parse_ident
open Def_modules

let name_of_module    md = md.mod_name
and values_of_module  md = md.mod_values
and constrs_of_module md = md.mod_constrs
and labels_of_module  md = md.mod_labels
and types_of_module   md = md.mod_types

(* The table of module interfaces already loaded in memory *)

let module_table = (Hashtbl.create 1 : (string, module0) Hashtbl.t)

let new_module nm =
  let md =
    { mod_name = nm;
      mod_values = Hashtbl.create 1;
      mod_constrs = Hashtbl.create 1;
      mod_types = Hashtbl.create 1;
      mod_labels = Hashtbl.create 1}
  in
  Hashtbl.add module_table nm md; md


(* To load an interface from a file *)

let read_module basename filename =
  let ic = open_in_bin filename in
  try
    let md = (input_value ic : module0) in
    close_in ic;
    md
  with End_of_file | Failure _ ->
    close_in ic;
    Format.fprintf !err_fmt "Corrupted compiled interface file %s.\n\
                       Please recompile %s.rml first.\n"
      filename basename;
    raise Error


let load_module modname =
  let name = String.uncapitalize_ascii modname in
  let rzi = find_in_path (name ^ ".rzi") in
  match rzi with
  | Some rzi ->
      read_module name rzi
  | None ->
      try
        let md = List.assoc name Rzi.known_modules in
        (Marshal.from_string md 0 : module0)
      with Not_found ->
        Format.fprintf !err_fmt
          "Cannot find the compiled interface file %s.rzi.\n" name;
        raise Error

(* To find an interface by its name *)

let find_module modname =
  try
    Hashtbl.find module_table modname
  with Not_found ->
    let md = load_module modname in
    Hashtbl.add module_table modname md; md

(* To remove the in-memory image of an interface *)

let kill_module name =
  Hashtbl.remove module_table name


(* The table of all opened modules. Associate to each unqualified name
   the corresponding descriptor from the right opened module. *)

let opened_modules = ref
    { mod_name = "";
      mod_values = Hashtbl.create 1;
      mod_constrs = Hashtbl.create 1;
      mod_types = Hashtbl.create 1;
      mod_labels = Hashtbl.create 1 }
let opened_modules_names = ref ([]: string list)
let used_opened_modules = ref (Hashtbl.create 1: (string, bool ref) Hashtbl.t)

let reset_opened_modules () =
  opened_modules :=
    { mod_name = "";
      mod_values = Hashtbl.create 1;
      mod_constrs = Hashtbl.create 1;
      mod_types = Hashtbl.create 1;
      mod_labels = Hashtbl.create 1};
  opened_modules_names := [];
  used_opened_modules := Hashtbl.create 1

(* Open a module and add its definitions to the table of opened modules. *)

let add_table t1 t2 =
  let rev_t1 = Hashtbl.fold (fun k x acc -> (k,x)::acc) t1 [] in
  List.iter (fun (k, x) -> Hashtbl.add t2 k x) rev_t1


let open_module modname =
  let module0 = find_module modname in
  add_table module0.mod_values (!opened_modules).mod_values;
  add_table module0.mod_constrs (!opened_modules).mod_constrs;
  add_table module0.mod_types (!opened_modules).mod_types;
  add_table module0.mod_labels (!opened_modules).mod_labels;
  opened_modules_names := modname :: !opened_modules_names;
  Hashtbl.add !used_opened_modules modname (ref false)

(* The current state of the compiler *)

let defined_module = ref (new_module "")

let start_compiling_interface modname =
  defined_module := new_module modname;
  reset_opened_modules();
  List.iter open_module !default_used_modules;;

let start_compiling_implementation modname _intf =
  start_compiling_interface modname

let compiled_module_name () =
  !defined_module.mod_name

let defined_global name desc =
  { gi = { qual=compiled_module_name(); id=name }; info = desc }

(* Additions to the module being compiled *)

let add_global_info sel_fct glob =
  let tbl = sel_fct !defined_module in
  Hashtbl.add tbl (Rml_ident.name glob.gi.id) glob

let add_global_info_list sel_fct glob_list =
  List.iter (add_global_info sel_fct) glob_list

let add_value = add_global_info values_of_module
and add_constr = add_global_info constrs_of_module
and add_label = add_global_info labels_of_module
and add_type = add_global_info types_of_module


(* Find the descriptor for a reference to a global identifier.
   If the identifier is qualified (mod.name), just look into module mod.
   If the identifier is not qualified, look inside the current module,
   then inside the table of opened modules.
   returns true if the name is imported *)

exception Desc_not_found

let pfind_desc sel_fct pident =
  match pident with
  | Pdot (mod_name, s) ->
      begin try
        Hashtbl.find (sel_fct (find_module mod_name)) s
      with Not_found ->
        raise Desc_not_found
      end
  | Pident s ->
      try
        Hashtbl.find (sel_fct !defined_module) s
      with Not_found ->
        try
          let res = Hashtbl.find (sel_fct !opened_modules) s in
          (* Record the module as actually used *)
          (Hashtbl.find !used_opened_modules res.gi.qual) := true;
          res
        with Not_found ->
          raise Desc_not_found


let pfind_value_desc = pfind_desc values_of_module
and pfind_constr_desc = pfind_desc constrs_of_module
and pfind_label_desc = pfind_desc labels_of_module
and pfind_type_desc = pfind_desc types_of_module

let find_desc sel_fct gident =
  try
    Hashtbl.find (sel_fct (find_module gident.qual)) (Rml_ident.name gident.id)
  with Not_found ->
    raise Desc_not_found

let find_value_desc = find_desc values_of_module
and find_constr_desc = find_desc constrs_of_module
and find_label_desc = find_desc labels_of_module
and find_type_desc = find_desc types_of_module

(* To write the interface of the module currently compiled *)

let write_compiled_interface oc =
  output_value oc !defined_module
