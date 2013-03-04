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

(* file: errors.ml *)

(* Warning: *)
(* This file is based on the original version of errors.ml *)
(* from the Objective Caml 3.07 distribution, INRIA        *)

(* first modification: 2004-05-08  *)
(* modified by: Louis Mandel *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

let report_error ppf exn =
  let report ppf = function
    | Lexer.Error(err, loc) ->
	Location.print ppf loc;
	Lexer.report_error ppf err
    | Syntaxerr.Error err ->
	Syntaxerr.report_error ppf err

    | Misc.Error -> ()
    | Misc.Internal (loc,msg) ->
	if loc = Location.none
      	then fprintf ppf "@.Internal error: %s. \nPlease report it." msg
	else
	  fprintf ppf "@.%aInternal error: %s. \nPlease report it."
	    Location.print loc msg
    | Warnings.Errors (n) ->
	fprintf ppf "@.Error: %d error-enabled warnings occurred." n
    | x -> fprintf ppf "@]"; raise x
  in
  fprintf ppf "@[%a@]@." report exn

let unbound_main main =
  eprintf "The main process \"%s\" is unbound" main;
  raise Misc.Error

let bad_type_main main main_ty =
  eprintf
    "The main process \"%s\" must have type unit process.\n"
	  main;
(*   Types_printer.output main_ty.Def_types.value_typ.Def_types.ts_desc; *)
  raise Misc.Error

