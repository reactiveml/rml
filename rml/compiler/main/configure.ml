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

(* file: main.ml *)
(* created: 2006-08-07  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Misc

(* add a file in the list of file to compile. *)
let add_to_compile file =
  to_compile := file :: !to_compile

(* set the output directory *)
let set_output_dir dir =
  output_dir := Some dir

(* standard libraries and includes *)
let set_standard_lib dir =
  standard_lib := Version.stdlib ^ "/" ^ dir

let set_init_stdlib () =
  let standard_lib = try Sys.getenv "RMLLIB" with Not_found -> !standard_lib in
  load_path := standard_lib :: !load_path

let set_stdlib p =
  load_path := [p]
and add_include d =
  load_path := d :: !load_path;;

(* where is the standard library *)
let locate_stdlib () =
  try
    Sys.getenv "RMLLIB"
  with
    Not_found -> !standard_lib

(* standard pervasives module *)
let set_init_pervasives () =
  default_used_modules := [pervasives_module]

let set_no_pervasives () =
  default_used_modules := []

let add_stdlib_thread () =
  add_include (locate_stdlib () ^ "/thread")

(* show version *)
let show_v () =
  Printf.printf "The ReactiveML compiler, version %s\n" version;
  print_string "Standard library: "; print_string (locate_stdlib ());
  print_newline ()


let show_version () =
  Printf.printf "%s\n" version

let show_where () =
  Printf.printf "%s\n" (locate_stdlib ())

(* sets the simulation process *)
let set_simulation_process n = simulation_process := n

(* sets the number of instant to execute *)
let set_number_of_instant n = number_of_instant := n

(* sets the sampling rate *)
let set_sampling n = sampling := n


(* print information *)
let set_print_info s =
  match s with
    "type" -> print_type := true
  | "all" -> print_type := true
  | _ -> raise (Arg.Bad ("don't know what to do with " ^ s))

let set_verbose () =
  print_type := true

let set_save_types () =
  save_types := true

let unset_reactivity_warning () =
  reactivity_warning := false
let set_old_instantaneous_loop_warning () =
  old_instantaneous_loop_warning := true
let unset_no_reactivity_simpl () =
  reactivity_simplify := false


(* Select the runtime *)
let set_runtime s =
  match s with
  | "Lco_rewrite" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_rewrite_record";
      set_interpreter_module "Lco_rewrite_record";
      set_rml_machine_module "Rml_machine";
      set_standard_lib "lco_rewrite";
      set_translation Lco
  | "Lco_ctrl_tree" | "Lco" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_ctrl_tree_record";
      set_interpreter_module "Lco_ctrl_tree_record";
      set_rml_machine_module "Rml_machine";
      set_standard_lib "lco";
      set_translation Lco
  | "Lco_ctrl_tree_class" | "Lco_class" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_ctrl_tree_class";
      set_interpreter_module "Lco_ctrl_tree_class";
      set_rml_machine_module "Rml_machine";
      set_standard_lib "lco_class";
      set_translation Lco
(*
  | "Lco_ctrl_tree_thread_safe" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Thread_implem";
      set_interpreter_module "Lco_ctrl_tree_thread_safe_record";
      set_translation Lco
*)
  | "Lco_ctrl_tree_n" | "Lco_n" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_ctrl_tree_n_record";
      set_interpreter_module "Lco_ctrl_tree_n_record";
      set_rml_machine_module "Rml_machine";
      set_standard_lib "lco_n";
      set_translation Lco
(*
  | "ctrl_tree_debug" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Lco_ctrl_tree_debug";
      set_translation Lco
*)
  | "Lk" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Implem_lk_record";
      set_interpreter_module "Lk_record";
      set_rml_machine_module "Rml_machine";
      set_standard_lib "lk";
      set_translation Lk
(*
  | "Lk_threaded" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Thread_implem";
      set_interpreter_module "Lk_threaded_record";
      set_translation Lk
*)
  | "Lco_toplevel" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_ctrl_tree_record";
      set_interpreter_module "Lco_ctrl_tree_record";
      set_rml_machine_module "Rmltop_reactive_machine";
      set_standard_lib "lco";
      set_translation Lco

  | "Lco_toplevel_alt" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem_lco_ctrl_tree_record";
      set_interpreter_module "Lco_ctrl_tree_record";
      set_rml_machine_module "Rmltop_alt_reactive_machine";
      set_standard_lib "lco";
      set_translation Lco

  | "Rmltop" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Rmltop_implem";
      set_interpreter_module "Machine_controler_machine";
      set_rml_machine_module "Rml_machine";
      set_translation Lco

  | "Rmltop_alt" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Rmltop_alt_implem";
      set_interpreter_module "Machine_controler_machine";
      set_rml_machine_module "Rml_machine";
      set_translation Lco


  | _ -> raise (Arg.Bad ("don't know what to do with " ^ s))

(* sets the display of the parse code *)
let set_dparse () = dparse := true

(* sets the display of the timing information of the compiler *)
let set_dtime () = dtime := true

(* sets the display of the reactivity effects *)
let set_dreactivity () = dreactivity := true

(* sets the interactive mode *)
let set_interactive () =
  interactive := true

(* suspend the nary optimization *)
let set_no_nary () = nary_optimization := false
(* suspend the static optimization *)
let set_no_static () = static_optimization := false
(* suspend the for optimization *)
let set_no_for () = for_optimization := false

(* the string of messages *)
let doc_v = "Print compiler version and location of standard library and exit"
and doc_version = "Print compiler version and exit"
and doc_where = "Print location of standard library and exit"
and doc_stdlib = "<dir> Directory for the standard library"
and doc_no_pervasives = "(undocumented)"
and doc_no_const_opt = "(undocumented)"
and doc_no_nary = "(undocumented)"
and doc_no_static = "(undocumented)"
and doc_no_for = "(undocumented)"
and doc_compilation = "Compile only (produces a .rzi file)"
and doc_libraries = "<dir> Add <dir> to the list of include directories"
and doc_simulation = "<proc> Executes the process <proc>."
and doc_number_of_instant = "<n> Executes the main process <n> instants"
and doc_sampling = "<rate> Sets the sampling rate to <rate> seconds"
and doc_verbose = "Print types"
and doc_save_types = "Save type information in <filename>.?annot"
and doc_no_reactivity_warning = "Remove reactivity analysis warnings"
and doc_dreactivity = "Display reactivity effects in process types"
and doc_no_reactivity_simpl = "Do not simplify reactivity effects"
and doc_old_loop_warning = "Set the old instantaneous loop and recursion analysis"
and doc_interactive = "Read programs on stdin and output on stdout"
and doc_d = "<dir> Generate files in directory <dir>, rather than in current directory"
and doc_runtime =
(*"<interpreter> select the runtime according to <interpreter>:\n"*)
   "(undocumented)\n" ^
   "\t Lco_rewrite\n" ^
   "\t Lco_ctrl_tree | Lco (default)\n" ^
   "\t Lco_ctrl_tree_n | Lco_n \n" ^
   "\t Lco_ctrl_tree_class\n" ^
   "\t Lk"
and doc_thread = "Generate code that supports the Rml_async library"
and doc_debug = "Print instant numbers on the error output"

and doc_dparse = "(undocumented)"
and doc_dtime = "(undocumented)"
and errmsg =
"\nrmlc - The ReactiveML Compiler
Usage: rmlc [options] -s <process> <file>.rml
  <process> : name of the main process
  <file>.rml : file containing the source of <process>
  Ouputs the file <file>.ml.
  This file should compile with:
  ocamlc -I `rmlc -where` unix.cma rmllib.cma <obj_files> <file>.ml

Options are:"

(* the main function: parse the command line *)
let configure () =
  set_init_stdlib ();
  set_init_pervasives ()

