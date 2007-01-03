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

(* file: interactive.ml *)
(* created: 2005-06-11  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* compile a list of declarations *)
let compile_decl_list module_name itf info_chan out_chan decl_list =
  (* expend externals *)
  let decl_list = List.map External.expend decl_list in
  (* front-end *)
  let intermediate_code = 
    Compiler.compile_implementation_front_end info_chan itf decl_list 
  in
  (* the implementation *)
  Compiler.compile_implementation_back_end info_chan out_chan module_name 
    intermediate_code


(* the main function *)
let compile () =
  let filename = Filename.temp_file "rml_" "" in
  let module_name = String.capitalize filename in
  let ic = stdin in
  let itf = open_out_bin (filename ^ ".rzi") in
  let info_chan = stderr in
  let out_chan = stdout in

  (* Initialization *)
  Modules.start_compiling_interface module_name;
  Initialization.load_initial_modules();

  let lexbuf = Lexing.from_channel ic in
  (* Compilation loop *)
  while true do
    begin
      try 
	Location.init lexbuf ""; 
	let decl_list = Parse.interactive lexbuf in 
	compile_decl_list module_name itf info_chan out_chan decl_list
      with x ->
	Errors.report_error Format.err_formatter x;
	output_string out_chan "();;\n"
    end;
    flush out_chan;
    flush info_chan
  done

