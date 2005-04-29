(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : modules.ml                                                 *)
(*  Date de creation : 26/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from the Caml Light sources, INRIA                  *)
(*************************************************************************)

(* $Id: modules.ml,v 1.1.1.1 2005/01/23 17:55:36 mandel Exp $ *)

open Misc
open Global_ident
open Def_types
open Global
open Parse_ident

(* Informations associated with module names *)

type module0 =
    { mod_name: string;                      (* name of the module *)
      mod_values: (string, value_type_description global) Hashtbl.t;
                                             (* table of values *)
      mod_constrs: 
	(string, constructor_type_description global) Hashtbl.t;
                                             (* table of constructors *)
      mod_labels: (string, label_type_description global) Hashtbl.t;
                                             (* table of labels *)
      mod_types: (string, type_description global) Hashtbl.t;
                                             (* table of type constructors *)
    }
      
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
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.rml first.\n"
      filename basename;
    raise Error


let use_extended_interfaces = ref false
    
let load_module modname =
  let name = String.uncapitalize modname in
  try
    let fullname = find_in_path (name ^ ".rzi") in
    let extname = fullname ^ "x" in
    read_module name
      (if !use_extended_interfaces && Sys.file_exists extname
      then extname else fullname)
  with Cannot_find_file _ ->
    Printf.eprintf "Cannot find the compiled interface file %s.rzi.\n" name;
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
  Hashtbl.iter (Hashtbl.add t2) t1
    
let open_module modname =
  let module0 = find_module modname in
  add_table module0.mod_values (!opened_modules).mod_values;
  add_table module0.mod_constrs (!opened_modules).mod_constrs;
  add_table module0.mod_types (!opened_modules).mod_types;
  add_table module0.mod_labels (!opened_modules).mod_labels;
  opened_modules_names := modname :: !opened_modules_names;
  Hashtbl.add !used_opened_modules modname (ref false)

(* The current state of the compiler *)

let default_used_modules = ref ([] : string list)

let defined_module = ref (new_module "")

let start_compiling_interface modname =
  defined_module := new_module modname;
  reset_opened_modules();
  List.iter open_module !default_used_modules;;

let start_compiling_implementation modname intf =
  start_compiling_interface modname

let compiled_module_name () =
  !defined_module.mod_name

let defined_global name desc =
  { gi = { qual=compiled_module_name(); id=name }; info = desc }

(* Additions to the module being compiled *)

let add_global_info sel_fct glob =
  let tbl = sel_fct !defined_module in
  Hashtbl.add tbl (Ident.name glob.gi.id) glob

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
    Hashtbl.find (sel_fct (find_module gident.qual)) (Ident.name gident.id)
  with Not_found ->
    raise Desc_not_found

let find_value_desc = find_desc values_of_module
and find_constr_desc = find_desc constrs_of_module
and find_label_desc = find_desc labels_of_module
and find_type_desc = find_desc types_of_module

(* To write the interface of the module currently compiled *)

let write_compiled_interface oc =
  output_value oc !defined_module



