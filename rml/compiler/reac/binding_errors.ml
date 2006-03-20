(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : bindings_errors.ml                                         *)
(*  Date de creation : 27/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: binding_errors.ml,v 1.1.1.1 2005/01/23 17:55:36 mandel Exp $ *)

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
