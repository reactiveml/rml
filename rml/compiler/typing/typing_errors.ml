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

(* file: typing_errors.ml *)

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* created: 2004-05-13  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Printing of error messages during typing *)

open Misc
open Def_types
open Types
open Reac_ast

(* type clash *)
let expr_wrong_type_err exp actual_ty expected_ty =
  Printf.eprintf
    "%aThis expression has type %a,\n\
    but is used with type %a.\n"
    Location.print_oc exp.expr_loc
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let patt_wrong_type_err patt actual_ty expected_ty =
  Printf.eprintf
    "%aThis pattern has type %a,\n\
    but is used with type %a.\n"
    Location.print_oc patt.patt_loc
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let event_wrong_type_err evt actual_ty expected_ty =
  Printf.eprintf
    "The event %s has type %a,\n\
    but is used with type %a.\n"
    (Ident.name evt)
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let emit_wrong_type_err loc actual_ty expected_ty =
  Printf.eprintf
    "%aThe emitted value has type %a,\n\
    but is used with type %a.\n"
    Location.print_oc loc
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let run_wrong_type_err loc actual_ty expected_ty =
  Printf.eprintf
    "%aThis expression has type %a,\n\
    but is used with type %a.\n"
    Location.print_oc loc
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let var_wrong_type_err loc actual_ty expected_ty =
  Printf.eprintf
    "%aThis pattern has type %a,\n\
    but is used with type %a.\n"
    Location.print_oc loc
    Types_printer.output actual_ty
    Types_printer.output expected_ty;
  raise Error

let usage_wrong_type_err loc _ _ =
  Printf.eprintf
    "%aThe affine signal is used more than once.\n"
    Location.print_oc loc;
  raise Error

let application_of_non_function_err exp ty =
  begin try
    let _ = filter_arrow ty in
    Printf.eprintf
      "%aThis function is applied to too many arguments.\n"
      Location.print_oc exp.expr_loc
  with Unify ->
    Printf.eprintf
      "%aThis expression is not a function, it cannot be applied.\n"
      Location.print_oc exp.expr_loc
  end;
  raise Error

let non_event_err exp =
  Printf.eprintf
    "%aThis expression is not an event.\n"
    Location.print_oc exp.expr_loc;
  raise Error

let non_event_err2 conf =
  Printf.eprintf
    "%aThis expression is not an event.\n"
    Location.print_oc conf.conf_loc;
  raise Error

(* typing errors *)
(* unbound *)
let unbound_typ_err name loc =
  Printf.eprintf "%aThe type variable \'%s is unbound.\n"
    Location.print_oc loc name;
  raise Error

let unbound_typ_constr_err gr loc =
  Printf.eprintf "%aThe type constructor %a is unbound.\n"
    Location.print_oc loc
    Global_ident.print_oc gr;
  raise Error

let unbound_global_ident_err gr loc =
  Printf.eprintf "%aThe name %a is unbound.\n"
    Location.print_oc loc
    Global_ident.print_oc gr;
  raise Error

let unbound_ident_err n loc =
  Printf.eprintf "%aThe name %s is unbound.\n"
    Location.print_oc loc
    (Ident.name n);
  raise Error

let unbound_constructor_err c loc =
  Printf.eprintf "%aThe constructor %a is unbound.\n"
    Location.print_oc loc
    Global_ident.print_oc c;
  raise Error

let unbound_label_err label loc =
  Printf.eprintf "%aThe label %a is unbound.\n"
    Location.print_oc loc
    Global_ident.print_oc label;
  raise Error

(* arity *)
let constr_arity_err gr loc =
  Printf.eprintf "%aThe value constructor %a expects 1 argument, \
                     but is here applied to 0 argument.\n"
    Location.print_oc loc
    Global_ident.print_oc gr;
  raise Error

let constr_arity_err_2 gr loc =
  Printf.eprintf "%aThe value constructor %a expects 0 argument, \
                     but is here applied to 1 argument.\n"
    Location.print_oc loc
    Global_ident.print_oc gr;
  raise Error

let type_constr_arity_err gr arit' arit loc =
  Printf.eprintf "%aThe type constructor %a expects %d argument(s), \
                     but is here given %d argument(s).\n"
    Location.print_oc loc
    Global_ident.print_oc gr
    arit'
    arit;
  raise Error

(* bound several times *)
let non_linear_pattern_err pat n =
  Printf.eprintf
    "%aThe variable %s is bound several times in this pattern.\n"
    Location.print_oc pat.patt_loc n;
  raise Error

let non_linear_record_err label loc =
  Printf.eprintf
    "%aThe label %a is defined several times\n"
    Location.print_oc loc
    Global_ident.print_oc label;
  raise Error

let repeated_constructor_definition_err s loc =
  Printf.eprintf "%aTwo constructors are named %s\n"
    Location.print_oc loc s;
  raise Error


let repeated_label_definition_err s loc =
  Printf.eprintf "%aTwo labels are named %s\n"
    Location.print_oc loc s;
  raise Error

let orpat_vars loc s =
  Format.eprintf
    "%aVariable %s must occur on both sides of this | pattern.\n"
    Location.print loc s;
  raise Error


(* label *)
let label_not_mutable_err exp lbl =
  Printf.eprintf "%aThe label %a is not mutable.\n"
    Location.print_oc exp.expr_loc
    Global_ident.print_oc lbl;
  raise Error

(* Top level *)
let cannot_generalize_err expr =
  Printf.eprintf
    "%aThe type of this expression, %a,\n\
    contains type variables that cannot be generalized"
    Location.print_oc expr.expr_loc
    Types_printer.output expr.expr_type;
  raise Error
;;

(* Warnings *)
let partial_apply_warning loc =
  Printf.eprintf "%aWarning: this function application is partial,\n\
           maybe some arguments are missing.\n"
    Location.print_oc loc

let not_unit_type_warning expr =
  Printf.eprintf "%aWarning: this expression should have type unit.\n"
    Location.print_oc expr.expr_loc

