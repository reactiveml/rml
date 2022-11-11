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

(* file: global.ml *)

(* Warning: *)
(* This file is based on the original version of globals.ml *)
(* from the CamlLight 0.75 distribution, INRIA              *)

(* first modification: 2004-04-23 *)
(* modified by: Louis Mandel      *)

(* $Id$ *)

open Rml_misc

(* values in the symbol table *)

type 'a global =
  { mutable gi: Global_ident.qualified_ident;
    mutable info: 'a option }

let little_name_of_global g = Global_ident.little_name g.gi


let no_info() = None

let gi gl = gl.gi
let info gl =
  match gl.info with
  | None -> assert false
  | Some i -> i
