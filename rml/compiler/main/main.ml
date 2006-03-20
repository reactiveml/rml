(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : main.ml                                                    *)
(*  Date de creation : 06/05/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from Lucid Synchron                                 *)
(*************************************************************************)

(* $Id: main.ml,v 1.2 2005/04/30 16:49:15 mandel Exp $ *)

open Misc
open Modules
open Compiler

(* list of object files passed on the command line *)
let object_files = ref [] 

let compile file =
  if Filename.check_suffix file ".rml"
  then
    let filename = Filename.chop_suffix file ".rml" in
    let modname = Filename.basename filename in
    compile_implementation (String.capitalize modname) filename;
    object_files := modname::!object_files
  else if Filename.check_suffix file ".rmli"
  then 
    let filename = Filename.chop_suffix file ".rmli" in
    compile_interface (String.capitalize (Filename.basename filename)) filename
  else if Filename.check_suffix file ".mli"
  then
    let filename = Filename.chop_suffix file ".mli" in
    compile_scalar_interface 
      (String.capitalize (Filename.basename filename)) filename
  else 
    raise (Arg.Bad ("don't know what to do with " ^ file))

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

(* Select the runtime *)
let set_runtime s =
  match s with
  | "Lco_rewrite" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implantation";
      set_interpreter_module "Lco_rewrite_record";
      set_translation Lco
(*
  | "rewrite_debug" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Lk_rewrite_debug";
      set_translation Lk
*)
  | "Lco_ctrl_tree" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Implantation";
      set_interpreter_module "Lco_ctrl_tree_record";
      set_translation Lco
(*
  | "ctrl_tree_debug" -> 
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Lco_ctrl_tree_debug";
      set_translation Lco
*)
  | "Lk" ->
      set_interpreter_intf "Lk_interpreter";
      set_interpreter_impl "Lk_interpreter";
      set_translation Lk

  | "Rmltop" ->
      set_interpreter_intf "Lco_interpreter";
      set_interpreter_impl "Rmltop_implantation";
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
      set_interpreter_impl "Implantation";
      set_interpreter_module "Lco_ctrl_tree_record";
  interactive := true

(* suspend the nary optimization *)
let set_no_nary () = nary_optimization := false

(* the string of messages *)
let doc_v = "Print compiler version and location of standard library and exit"
and doc_version = "Print compiler version and exit"
and doc_where = "Print location of standard library and exit"
and doc_stdlib = "<dir> Directory for the standard library"
and doc_no_pervasives = "(undocumented)"
and doc_no_const_let = "(undocumented)"
and doc_no_nary = "(undocumented)"
and doc_compilation = "Compile only (produces a .rzi file)"
and doc_libraries = "<dir> Add <dir> to the list of include directories"
and doc_simulation = "<proc> Executes the process <proc>."
and doc_number_of_instant = "<n> Executes the main process <n> instants"
and doc_sampling = "<rate> Sets the sampling rate to <rate> seconds"
and doc_verbose = "Print types"
and doc_save_types = "Save type information in <filename>.rannot"
and doc_interactive = "Read programs on stdin and output on stdout"
and doc_runtime = 
(*"<interpreter> select the runtime according to <interpreter>:\n"*)
   "(undocumented)\n" ^
   "\t Lco_rewrite\n" ^
   "\t Lco_ctrl_tree (default)\n" ^
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
  ocamlc -I `rmlc -where` unix.cma rml_interpreter.cma <obj_files> <file>.ml

Options are:"

(* the main function: parse the command line *)
let main () =
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
	"-runtime", Arg.String set_runtime, doc_runtime;
	"-interactive", Arg.Unit set_interactive, doc_interactive;
	"-nopervasives", Arg.Unit set_no_pervasives, doc_no_pervasives;
	"-nonary", Arg.Unit set_no_nary, doc_no_nary;
	"-noconstlet", Arg.Clear const_let, doc_no_const_let;
	"-dparse", Arg.Unit set_dparse, doc_dparse;
	"-dtime", Arg.Unit set_dtime, doc_dtime;
      ]	
      compile
      errmsg;
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2
;;

Printexc.catch main (); 
(* this is strange, but is required to avoid a bug in ocaml 3.04 *)
Format.set_formatter_out_channel stdout; 
if !interactive then Interactive.compile () else ();
if !dtime then Diagnostic.print stderr;
exit 0;;

