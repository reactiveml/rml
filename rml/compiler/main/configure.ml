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

(* standard libraries and includes *)
let set_init_stdlib () =
  let standard_lib = try Sys.getenv "RMLLIB" with Not_found -> standard_lib in
  load_path := [standard_lib]

let set_stdlib p =
  load_path := [p]
and add_include d =
  load_path := d :: !load_path;;

(* where is the standard library *)
let locate_stdlib () =
  try
    Sys.getenv "RMLLIB"
  with
    Not_found -> standard_lib 

(* standard pervasives module *)
let set_init_pervasives () =
  default_used_modules := [pervasives_module]

let set_no_pervasives () = 
  default_used_modules := []

(* show version *)
let show_v () =
  Printf.printf "The Reactive ML compiler, version %s\n" version;
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

let unset_instantaneous_loop_warning () =
  instantaneous_loop_warning := false

(* Select the runtime *)
let set_runtime s =
  match s with
  | "Lco_rewrite" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Rec_implem";
      set_interpreter_module "Lco_rewrite_record";
      set_translation Lco
  | "Lco_ctrl_tree" | "Lco" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem";
      set_interpreter_module "Lco_ctrl_tree_record";
      set_translation Lco
  | "Lco_ctrl_tree_class" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem";
      set_interpreter_module "Lco_ctrl_tree_class";
      set_translation Lco
  | "Lco_ctrl_tree_thread_safe" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Thread_implem";
      set_interpreter_module "Lco_ctrl_tree_thread_safe_record";
      set_translation Lco
  | "Lco_ctrl_tree_n" | "Lco_n" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implem";
      set_interpreter_module "Lco_ctrl_tree_n_record";
      set_translation Lco
(*
  | "ctrl_tree_debug" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Lco_ctrl_tree_debug";
      set_translation Lco
*)
  | "Lk" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Implem";
      set_interpreter_module "Lk_record";
      set_translation Lk

  | "Lk_threaded" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Thread_implem";
      set_interpreter_module "Lk_threaded_record";
      set_translation Lk

  | "Rmltop" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Rmltop_implem";
      set_interpreter_module "Machine_controler_machine";
      set_translation Lco


  | _ -> raise (Arg.Bad ("don't know what to do with " ^ s))

(* sets the display of the parse code *)
let set_dparse () = dparse := true

(* sets the display of the timing information of the compiler *)
let set_dtime () = dtime := true

(* sets the interactive mode *)
let set_interactive () = 
(*
  interpreter_module := "Rml_interactive";
*)
  set_interpreter_intf "Lco_interpreter";
  set_interpreter_impl "Implem";
  set_interpreter_module "Lco_ctrl_tree_record";
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
and doc_no_loop_warning = "Remove instantaneous loop and recursion warnings"
and doc_interactive = "Read programs on stdin and output on stdout"
and doc_runtime = 
(*"<interpreter> select the runtime according to <interpreter>:\n"*)
   "(undocumented)\n" ^
   "\t Lco_rewrite\n" ^
   "\t Lco_ctrl_tree (default)\n" ^
   "\t Lco_ctrl_tree_class\n" ^
   "\t Lk"

and doc_dparse = "(undocumented)"
and doc_dtime = "(undocumented)"
and errmsg = 
"\nrmlc - The Reactive ML Compiler
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
  set_init_pervasives ();
  try
    Arg.parse 
      [ "-stdlib", Arg.String set_stdlib, doc_stdlib;
	"-v", Arg.Unit show_v, doc_v;    
	"-version", Arg.Unit show_version, doc_version;
	"-where", Arg.Unit show_where, doc_where;
	"-c",Arg.Set no_link, doc_compilation;
	"-I",Arg.String add_include,doc_libraries;
	"-s", Arg.String set_simulation_process, doc_simulation;
	"-n", Arg.Int set_number_of_instant, doc_number_of_instant;
	"-sampling", Arg.Float set_sampling, doc_sampling;
	"-i", Arg.Unit set_verbose, doc_verbose;
	"-dtypes", Arg.Unit set_save_types, doc_save_types;
	"-no_loop_warning", Arg.Unit unset_instantaneous_loop_warning, doc_no_loop_warning;
	"-runtime", Arg.String set_runtime, doc_runtime;
	"-interactive", Arg.Unit set_interactive, doc_interactive;
	"-nopervasives", Arg.Unit set_no_pervasives, doc_no_pervasives;
	"-no_nary_opt", Arg.Unit set_no_nary, doc_no_nary;
	"-no_static_opt", Arg.Unit set_no_static, doc_no_static;
	"-no_for_opt", Arg.Unit set_no_for, doc_no_for;
	"-no_const_opt", Arg.Clear const_optimization, doc_no_const_opt; 
	"-dparse", Arg.Unit set_dparse, doc_dparse;
	"-dtime", Arg.Unit set_dtime, doc_dtime;
      ]	
      add_to_compile
      errmsg;
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2
;;


let _ = 
  Printexc.catch configure ();
  to_compile := List.rev !to_compile

