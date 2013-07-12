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

include Version

(* standard module *)
let pervasives_module = "Pervasives"
let rml_pervasives_module = "Rml_pervasives"
let interpreter_module = ref "Rml_machine"
let interpreter_impl = ref "Rml_machine.Lco_ctrl_tree_seq_interpreter"
let machine_module = ref "Rml_machine.Machine"

(* List of file to compile *)
let to_compile = ref ([] : string list)

let default_used_modules = ref ([] : string list)

(* interpreter *)
let set_interpreter_impl s = interpreter_impl := s
let set_interpreter_module s = interpreter_module := s
let set_machine_module s = machine_module := s

(* different translations *)
type translations = Lco | Lco_fsharp | Rml_print | Rpml2Rml

let translation = ref Lco

let set_translation t = translation := t

(* load paths *)
let load_path = ref ([] : string list)

(* no link *)
let no_link = ref false

(* simulation process *)
let simulation_process = ref ""

(* test definition *)
let test_name = ref ""

(* verbose *)
let verbose = ref false
let print_type = ref false
let save_types = ref false
let instantaneous_loop_warning = ref true

(* dparse *)
let dparse = ref false

(* dtime *)
let dtime = ref false

(*
let display_time =
  let last = ref 0.0 in
  fun s ->
    if !dtime then
      let current = Sys.time() in
      Printf.printf "%s\t%f\t%f\n"
	s
	(current -. !last)
	current;
      last := current
*)

(* interactive *)
let interactive = ref false

(* optimization *)
let nary_optimization = ref true
let static_optimization = ref true
let for_optimization = ref true
let const_optimization = ref true

let no_clocking = ref false
let no_reactivity = ref false
let show_reactivity = ref false
let no_clock_effects = ref false
let use_row_clocking = ref false

let warning_are_errors = ref false

(* add a file in the list of file to compile. *)
let add_to_compile file =
  to_compile := file :: !to_compile

(* standard libraries and includes *)
let stdlib = ref None
let set_init_stdlib () =
  let standard_lib =
    match !stdlib with
      | None ->
        (try
           Sys.getenv "RMLLIB"
         with
           | Not_found -> if !use_row_clocking then standard_lib^"/row" else standard_lib)
      | Some stdlib -> stdlib
  in
  load_path := !load_path@[standard_lib]

let set_stdlib p =
  stdlib := Some p
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
  default_used_modules := [pervasives_module; rml_pervasives_module]

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
  Printf.printf "%s%s\n" (locate_stdlib ())
    (if !use_row_clocking then "/row" else "")

(* sets the simulation process *)
let set_simulation_process n = simulation_process := n

let set_test_name n = test_name := n

(* print information *)
let set_print_info s =
  match s with
    "type" -> print_type := true
  | "all" -> print_type := true
  | _ -> raise (Arg.Bad ("Invalid print argument " ^ s))

let set_verbose () =
  print_type := true

let set_save_types () =
  save_types := true

let unset_instantaneous_loop_warning () =
  instantaneous_loop_warning := false

(* Select the runtime *)
let set_runtime s =
  match s with
    | "Lco" ->
      set_interpreter_impl "Rml_machine.Lco_ctrl_tree_seq_interpreter";
      set_translation Lco

    | "Lco_new" ->
      set_interpreter_impl "Rml_machine.Lco_ctrl_tree_seq_list_interpreter";
      set_translation Lco

    | "Lco_top_alt" ->
      set_interpreter_impl "Rmltop_alt_implem";
      set_translation Lco

    | "Lco_mpi" ->
      set_interpreter_impl "Rml_machine_mpi.Lco_ctrl_tree_mpi_interpreter";
      set_machine_module "Rml_machine_mpi.Machine";
      set_translation Lco

    | "Lco_mpi_buffer" ->
      set_interpreter_impl "Rml_machine_mpi.Lco_ctrl_tree_mpi_buffer_interpreter";
      set_machine_module "Rml_machine_mpi.Machine";
      set_translation Lco

   | "Lco_mpi_c" ->
      set_interpreter_impl "Rml_machine_mpi.Lco_ctrl_tree_mpi_c_interpreter";
      set_machine_module "Rml_machine_mpi.Machine";
      set_translation Lco

    | "Lco_mpi_new" ->
      set_interpreter_impl "Rml_machine_mpi.Lco_ctrl_tree_mpi_new_interpreter";
      set_machine_module "Rml_machine_mpi.Machine";
      set_translation Lco


    | "Rml_print" -> set_translation Rml_print
    | "Rpml2rml" -> set_translation Rpml2Rml

    | "Fsharp_Lco" ->
        set_interpreter_module "LcoSeq";
        set_machine_module "SeqMachine";
        set_translation Lco_fsharp

    | "Fsharp_LcoThread" ->
        set_interpreter_module "LcoThread";
        set_machine_module "ThreadMachine";
        set_translation Lco_fsharp

    | "Fsharp_LcoRmlThread" ->
        set_interpreter_module "LcoRmlThread";
        set_machine_module "RmlThreadMachine";
        set_translation Lco_fsharp

    | _ -> raise (Arg.Bad ("Invalid runtime:" ^ s))

(* sets the display of the parse code *)
let set_dparse () = dparse := true

(* sets the display of the timing information of the compiler *)
let set_dtime () = dtime := true

(* sets the interactive mode *)
let set_interactive () =
(*
  interpreter_module := "Rml_interactive";
*)
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
and doc_test_name = "<name> Sets the name of the unit test"
and doc_verbose = "Print types"
and doc_save_types = "Save type information in <filename>.?annot"
and doc_no_loop_warning = "Remove instantaneous loop and recursion warnings"
and doc_interactive = "Read programs on stdin and output on stdout"
and doc_no_clocking = "Don't check clocks"
and doc_row_clocking = "Use clocking with rows (experimental)"
and doc_no_reactivity = "Do not check for reactivity"
and doc_show_reactivity = "Display reactivity effects"
and doc_runtime =
(*"<interpreter> select the runtime according to <interpreter>:\n"*)
   "(undocumented)\n" ^
   "\t Lco_rewrite\n" ^
   "\t Lco_ctrl_tree | Lco (default)\n" ^
   "\t Lco_ctrl_tree_n | Lco_n \n" ^
   "\t Lco_ctrl_tree_class\n" ^
   "\t Lk"

and doc_dparse = "(undocumented)"
and doc_dtime = "(undocumented)"
and doc_warning_are_errors = "(undocumented)"
and doc_no_clock_effects = "(undocumented)"
and errmsg =
"\nrmlc - The Reactive ML Compiler
Usage: rmlc [options] -s <process> <file>.rml
  <process> : name of the main process
  <file>.rml : file containing the source of <process>
  Ouputs the file <file>.ml.
  This file should compile with:
  ocamlc -I `rmlc -where` unix.cma rmllib.cma <obj_files> <file>.ml

Options are:"

let _ = set_init_pervasives ()
