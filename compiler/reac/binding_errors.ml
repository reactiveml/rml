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
  Format.eprintf
    "%aUnbound value %s.\n"
    Location.print loc
    (string_of_parseident x);
  raise Error

let unbound_label_err lbl loc =
  Format.eprintf
    "%aUnbound label %s.\n"
    Location.print loc
    (string_of_parseident lbl);
  raise Error

let unbound_constr_err cstr loc =
  Format.eprintf
    "%aUnbound constructor %s.\n"
    Location.print loc
    (string_of_parseident cstr);
  raise Error

let unbound_type_err typ loc =
  Format.eprintf
    "%aUnbound type constructor %s.\n"
    Location.print loc
    (string_of_parseident typ);
  raise Error

let multiply_bound_variable_err x loc =
  Format.eprintf
    "%aThe variable %s is bound several times in this matching.\n"
    Location.print loc
    x;
  raise Error

let orpat_vars loc =
  Format.eprintf
    "%aVariables must occur on both sides of this | pattern.\n"
    Location.print loc;
  raise Error

let event_config_err loc =
  Format.eprintf
    "%aEvent configuration outside of a await, present, until, control or when.\n"
    Location.print loc;
  raise Error

let constr_wrong_arity_err cstr
    (found_ck, found_car, found_eff, found_r) (exp_ck, exp_car, exp_eff, exp_r) loc =
  Format.eprintf
    "%aThe constructor %s expects %d type variables, %d clock variables \
     and %d effect variables  and %d reactivity variables \
     but was given  %d type variables, %d clock variables \
     and %d effect variables and %d reactivity variables.\n"
    Location.print loc
    (string_of_parseident cstr)
    exp_ck exp_car exp_eff exp_r
    found_ck found_car found_eff found_r;
  raise Error
