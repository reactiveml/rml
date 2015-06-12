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
open Reac_ast

(* Printing of error messages during the "static" analysis *)

let expr_wrong_static_err fmt expr =
  Format.fprintf fmt
    "@[%aThis expression must be instantaneous.@]@."
    Location.print expr.expr_loc;
  raise Error

let impl_wrong_static_err fmt impl =
  Format.fprintf fmt
    "@[%aThis expression must be instantaneous.@]@."
    Location.print impl.impl_loc;
  raise Error

(* Type clash *)
let unify_err fmt exp actual_k expected_k =
  Format.fprintf fmt
    "@[%aThis expression is a %s process,@.
    but it should be a %s process.@]@."
    Location.print exp.expr_loc
    (Def_static.string_of_instantaneous actual_k)
    (Def_static.string_of_instantaneous expected_k);
  raise Error
