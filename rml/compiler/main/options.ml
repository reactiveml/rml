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

(* file: options.ml *)
(* created: 2006-08-07  *)
(* author: Louis Mandel *)

open Misc
open Configure

let _ =
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

let _ =
  to_compile := List.rev !to_compile
