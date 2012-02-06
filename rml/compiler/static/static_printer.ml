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

(* file: static_printer.ml *)
(* created: 2006-08-31  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Asttypes
open Reac_ast
open Def_static

let print ty =
  Format.open_box 0;
  Format.print_string (string_of_static ty);
  Format.close_box ()

let _ = Format.set_max_boxes max_int

let output oc ty =
  Format.set_formatter_out_channel oc;
  Format.print_string "  ";
  print ty;
  Format.print_flush ()

