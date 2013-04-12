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

(* file: compiler.ml *)

(* Warning: *)
(* This file is based on the original version of compiler.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6     *)

(* first modification: 2004-05-06 *)
(* modified by: Louis Mandel      *)

(* $Id$ *)

open Misc
open Errors
open Compiler_options
open Compiler_utils

(* compiling a file. Two steps. *)
(* - Front-end: - Parsing
                - Typing
                - Static analysis
                - emission of the intermediate rml code *)
(* - Back-end: - generation of caml code for the whole file *)

(* [info_chan] is the channel where static informations are emitted *)
(* [out_chan] is the channel where the generated code is emitted *)

(* front-end *)
let compile_implementation_front_end info_chan itf has_intf impl_list =
  let compile_one_phrase impl =
    (* producing rml code (and openning of modules) *)
    let rml_code = Parse2reac.translate_impl_item info_chan impl in
    (*let rml_code = silent_pass "Merge translation" true Merge.impl rml_code in*)

    let rml_code = silent_pass "Binary to n-ary parallel operator"
      !nary_optimization Binary2nary.impl rml_code in

    (* typing *)
    let rml_code = silent_pass "Typing" true (Typing.impl info_chan) rml_code in
    if not has_intf then Typing.check_nongen_values rml_code;

    (* clocking *)
    let rml_code = silent_pass "Clocking" (not !Compiler_options.no_clocking)
      (Clocking.impl info_chan has_intf) rml_code in

    let rml_code = silent_pass "Reactivity analysis" (not !Compiler_options.no_reactivity)
      (Reactivity.impl info_chan) rml_code in

    (* static analysis *)
    let rml_code = silent_pass "Static analysis" true (Static_analysis.impl info_chan) rml_code in

    let rml_code = silent_pass "Static optimization"
      !static_optimization Dynamic2static.impl rml_code in

    let rml_code = silent_pass "Print Annotations" true Annot.impl rml_code in

    (* for option *)
    (*let rml_code = silent_pass "For optimization" !for_optimization For2loopn.impl rml_code in*)

    rml_code
  in

  (* compilation of the whole file *)
  let rml_table = List.map compile_one_phrase impl_list in
  (* write interface *)
  Modules.write_compiled_interface itf;
  (* we return the rml code *)
  rml_table


(* the main functions *)
let compile_implementation module_name filename =
  (* input and output files *)
  let source_name = filename ^ ".rml"
  and obj_interf_name = filename ^ ".rzi"
  and tannot_name = filename ^ ".tannot"
  and sannot_name = filename ^ ".sannot"
  and module_name = String.capitalize filename  in

  let ic = open_in source_name in
  let itf = open_out_bin obj_interf_name in
  let info_chan = stdout in

  try
    (* load predefined base types *)
    Modules.start_compiling_interface module_name;
    Initialization.load_initial_modules ();

    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source_name;

    (* parsing of the file *)
    let decl_list =
      if !Compiler_options.use_row_clocking then
        Parse_row.implementation lexbuf
      else
        Parse.implementation lexbuf
    in

    let has_intf = Sys.file_exists (filename ^ ".rmli") || Sys.file_exists (filename ^ ".mli") in
    (* front-end *)
    let intermediate_code = compile_implementation_front_end info_chan itf has_intf decl_list in
    close_out itf;

    (* back-end *)
    if not !no_link then
      Reac2code.compile_impl info_chan filename module_name intermediate_code;

      (* write types annotation *)
    Annot.Stypes.dump tannot_name;
    Annot.Sstatic.dump sannot_name;

    close_in ic;
  with
    x ->
      Annot.Stypes.dump tannot_name;
      Annot.Sstatic.dump sannot_name;
      close_in ic;
      raise x


(* compiling an interface *)
(* front-end *)
let compile_interface_front_end info_chan itf intf_list =
  let compile_one_phrase phr =
    (* producing rml code (and openning of modules) *)
    let rml_code = Parse2reac.translate_intf_item info_chan phr in
    (* typing *)
    let rml_code = silent_pass "Typing interface" true (Typing.intf info_chan) rml_code in
    (* clocking *)
    let rml_code = silent_pass "Clocking interface" (not !Compiler_options.no_clocking)
      (Clocking.intf info_chan) rml_code in
    rml_code
  in
  (* compilation of the whole file *)
  let rml_table = List.map compile_one_phrase intf_list in
  (* write interface *)
  Modules.write_compiled_interface itf;
  close_out itf;
  (* we return the rml code *)
  rml_table

(* the main functions *)
let compile_interface parse module_name filename filename_end =
  (* input and output files *)
  let source_name = filename ^ filename_end
  and obj_interf_name = filename ^ ".rzi"
  and module_name = String.capitalize (Filename.basename filename) in

  let ic = open_in source_name in
  let itf = open_out_bin obj_interf_name in
  let info_chan = stdout in

  try
    (* load predefined base types *)
    Modules.start_compiling_interface module_name;
    Initialization.load_initial_modules ();

    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source_name;

    (* parsing of the file *)
    let decl_list = parse lexbuf in
    (* front-end *)
    let intermediate_code = compile_interface_front_end info_chan itf decl_list in
    (* back-end *)
    if (not !no_link) then
      Reac2code.compile_intf info_chan filename module_name intermediate_code;

   close_in ic;

  with
    x ->
      close_in ic;
      raise x


(* compiling a scalar interface *)
let compile_scalar_interface module_name filename =
  no_link := true;
  let parse = if !Compiler_options.use_row_clocking then Parse_row.interface else Parse.interface in
  compile_interface parse module_name filename ".mli"

(* compiling a Reactive ML interface *)
let compile_interface module_name filename =
  let parse = if !Compiler_options.use_row_clocking then Parse_row.interface else Parse.interface in
  compile_interface parse module_name filename ".rmli"

