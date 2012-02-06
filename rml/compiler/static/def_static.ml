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

(* file: def_static.mli *)
(* created: 2004-04-23  *)
(* author: Louis Mandel *)

(* $Id$ *)

type context = Process | ML

type static = Static | Dynamic of instantaneous
and instantaneous = Instantaneous | Noninstantaneous | Dontknow



(* For debug *)
let string_of_instantaneous k =
  match k with
  | Instantaneous -> "-"
  | Noninstantaneous -> "+"
  | Dontknow -> "+/-"

let string_of_static typ =
  match typ with
  | Static -> "Static"
  | Dynamic k -> "Dynamic("^(string_of_instantaneous k)^")"

