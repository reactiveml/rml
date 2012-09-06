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

open Usages

val usage_of_type : Def_types.type_expression -> usage
val type_of_usage : usage -> Def_types.type_expression
val mk_t :
  Location.t ->
  Def_types.type_expression ->
  Def_types.type_expression ->
  signal_usage

exception Unify of Usages.usage * Usages.usage

val unify :
  Def_types.type_expression ->
  Def_types.type_expression ->
  unit
