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

(* file: static_errors.ml *)
(* created: 2004-04-26  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Misc
open Reac

(* Printing of error messages during the "static" analysis *)

let expr_wrong_static_err expr =
  Format.eprintf
(*  "%aThis expression is not static but it is used in a static context.\n" *)
    "%aThis expression must be instantaneous.\n"
    Location.print expr.e_loc;
  raise Error

let impl_wrong_static_err impl =
  Format.eprintf
(*  "%aThis expression is not static but it is used in a static context.\n" *)
    "%aThis expression must be instantaneous.\n"
    Location.print impl.impl_loc;
  raise Error

(* Type clash *)
let unify_err exp actual_k expected_k =
  Printf.eprintf
    "%aThis expression is a %s process,\n\
    but it should be a %s process.\n"
    Location.print_oc exp.e_loc
    (Static.string_of_instantaneous actual_k)
    (Static.string_of_instantaneous expected_k);
  raise Error
