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

(* file: def_modules.ml *)

open Def_types
open Global

(* Informations associated with module names *)

type module0 =
    { mod_name: string;                      (* name of the module *)
      mod_values: (string, value_type_description global) Hashtbl.t;
                                             (* table of values *)
      mod_constrs:
	(string, constructor_type_description global) Hashtbl.t;
                                             (* table of constructors *)
      mod_labels: (string, label_type_description global) Hashtbl.t;
                                             (* table of labels *)
      mod_types: (string, type_description global) Hashtbl.t;
                                             (* table of type constructors *)
    }
