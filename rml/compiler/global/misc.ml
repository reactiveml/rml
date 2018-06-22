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

(* file: misc.ml *)
(* created: 2004-04-02  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* version of the compiler *)
let version = Version.version

exception Error

exception Internal of Location.t * string

exception Cannot_find_file of string

let std_fmt = ref Format.std_formatter
let err_fmt = ref Format.err_formatter

let () = Format.pp_set_max_boxes !std_fmt max_int
let () = Format.pp_set_margin !std_fmt max_int
let () = Format.pp_set_max_boxes !err_fmt max_int
let () = Format.pp_set_margin !err_fmt max_int

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Error

let not_yet_implemented msg =
  prerr_string ">> Not yet implemented: "; prerr_endline msg; raise Error


let print_DEBUG msg =
  prerr_string ">> DEBUG: "; prerr_endline msg

(* standard module *)
let pervasives_module = "Pervasives"
let interpreter_module = ref ""
let interpreter_intf = ref ""
let interpreter_impl = ref ""
let rml_machine_module = ref ""

let standard_lib = ref Version.stdlib

(* List of file to compile *)
let to_compile = ref ([] : string list)

let default_used_modules = ref ([] : string list)

(* directory in which output files are generated *)
let output_dir = ref None

let make_output_filename filename =
  match !output_dir with
  | None -> filename
  | Some dir -> Filename.concat dir (Filename.basename filename)

(* interpreter *)
let set_interpreter_intf s = interpreter_intf := s
let set_interpreter_impl s = interpreter_impl := s
let set_interpreter_module s = interpreter_module := s
let set_rml_machine_module s = rml_machine_module := s

(* different translations *)
type translations = Lk | Lco

let translation = ref Lco

let set_translation t = translation := t

(* load paths *)
let load_path = ref ([] : string list)

(* no link *)
let no_link = ref false

(* simulation process *)
let simulation_process = ref ""

(* number_of_instant to execute *)
let number_of_instant = ref (-1)

(* samplin rate *)
let sampling = ref (-. 1.0)

(* threads *)
let with_thread = ref false

(* debug *)
let with_debug = ref false

(* verbose *)
let print_type = ref false
let save_types = ref false
let reactivity_warning = ref true
let reactivity_simplify = ref true
let dreactivity = ref false
let old_instantaneous_loop_warning = ref false

(* dparse *)
let dparse = ref false

(* dtime *)
let dtime = ref false

(* interactive *)
let interactive = ref false

(* optimization *)
let nary_optimization = ref true
let static_optimization = ref true
let for_optimization = ref true
let const_optimization = ref true

(* File operations *)

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

let find_in_path filename =
  if Sys.file_exists filename then
    Some filename
  else if not(Filename.is_implicit filename) then
    None
  else
    let rec find = function
      [] ->
        None
    | a::rest ->
        let b = Filename.concat a filename in
          if Sys.file_exists b then Some b else find rest
    in find !load_path

let opt_map f = function
    Some x -> Some (f x)
  | None -> None

let opt_iter f = function
    Some x -> f x
  | None -> ()

(* association table with memoization *)
class name_assoc_table f =
  object
    val mutable counter = 0
    val mutable assoc_table: (int * string) list = []
    method name var =
      try
        List.assq var assoc_table
      with
        not_found ->
          let n = f counter in
          counter <- counter + 1;
          assoc_table <- (var,n) :: assoc_table;
          n
    method reset =
      counter <- 0;
      assoc_table <- []
  end

(* converting integers into variable names *)
(* variables are printed 'a, 'b *)
let int_to_letter bound i =
  if i < 26
  then String.make 1 (Char.chr (i+bound))
  else String.make 1 (Char.chr ((i mod 26) + bound)) ^ string_of_int (i/26)

let int_to_alpha i = int_to_letter 97 i

(* for infix operators, print parenthesis around *)
let is_an_infix_or_prefix_operator op =
  if op = "" then false
  else
    let c = String.get op 0 in
    not (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))


(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s


module Diagnostic =
  struct
    open Format

    let diag_list = ref []
    let global_time = ref 0.0

    let round f = float(truncate(100.0 *. f)) /. 100.0

    let print fmt =
      (* prints the diagnostic given by one entry *)
      let print_entry (name, time) =
        let average = 100.0 *. !time /. !global_time in
        pp_print_tab fmt ();
        pp_print_string fmt name;
        pp_print_string fmt ":";
        pp_print_tab fmt ();
        pp_print_float fmt (round !time);
        pp_print_string fmt "s";
        pp_print_tab fmt ();
        pp_print_float fmt (round average);
        pp_print_string fmt "%"
      in

      (* prints a diagnostic of the execution *)
      (* set_max_boxes max_int; *)
      (* set_formatter_out_channel oc; *)

      pp_open_tbox fmt ();
      pp_set_tab fmt ();
      pp_print_tbreak fmt 30 0;
      pp_set_tab fmt ();
      pp_print_tbreak fmt 30 0;
      pp_set_tab fmt ();
      pp_print_string fmt "\n";
      pp_print_string fmt "====================================\
                           ==============================\n";
      pp_print_string fmt "        Summary of execution time\n";
      List.iter print_entry (List.rev !diag_list);
      pp_print_string fmt "\n";
      print_entry ("Total", global_time);
      pp_print_string fmt "\n";
      pp_print_string fmt "====================================\
                           ==============================\n";
      pp_close_tbox fmt ();
      pp_print_flush fmt ()
  end

(* every step of the compiler takes its own timer *)
module Timer =
  functor (Name: sig val name: string end) ->
  struct
    open Diagnostic

    let exec_time = ref 0.0
    let accumulated_time = ref 0.0
    let start_time = ref 0.0;;

    (* add the entry to the list *)
    diag_list := (Name.name, accumulated_time) :: !diag_list

    (* start counting *)
    let start () = start_time := Sys.time ()

    (* counting *)
    let time () =
      let t = Sys.time () in
      exec_time := t -. !start_time;
      accumulated_time := !accumulated_time +. !exec_time;
      global_time := !global_time +. !exec_time
  end

(* List of visited nodes in a graph *)
let mk_visited () =
  let visited_list = ref [] in
  let visited k =
    if List.memq k !visited_list then true
    else (visited_list := k::!visited_list; false)
  in
  visited_list, visited
