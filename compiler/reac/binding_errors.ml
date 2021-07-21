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

(* file: bindings_errors.ml *)
(* created: 2004-04-27  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Misc
open Parse_ast
open Parse_ident

let unbound_variable_err x loc =
  Format.fprintf !err_fmt
    "%aUnbound value %s.\n"
    Location.print loc
    (string_of_parseident x);
  raise Error

let unbound_label_err lbl loc =
  Format.fprintf !err_fmt
    "%aUnbound label %s.\n"
    Location.print loc
    (string_of_parseident lbl);
  raise Error

let unbound_constr_err cstr loc =
  Format.fprintf !err_fmt
    "%aUnbound constructor %s.\n"
    Location.print loc
    (string_of_parseident cstr);
  raise Error

let unbound_type_err typ loc =
  Format.fprintf !err_fmt
    "%aUnbound type constructor %s.\n"
    Location.print loc
    (string_of_parseident typ);
  raise Error

let multiply_bound_variable_err x loc =
  Format.fprintf !err_fmt
    "%aThe variable %s is bound several times in this matching.\n"
    Location.print loc
    x;
  raise Error

let orpat_vars loc =
  Format.fprintf !err_fmt
    "%aVariables must occur on both sides of this | pattern.\n"
    Location.print loc;
  raise Error

let orconfig_vars loc =
  Format.fprintf !err_fmt
    "%aVariables must occur on both branchs of this \\/ configuration.\n"
    Location.print loc;
  raise Error

let event_config_err loc =
  Format.fprintf !err_fmt
    "%aIt is not allowed to get the value of a signal in this context.\n"
    Location.print loc;
  raise Error
