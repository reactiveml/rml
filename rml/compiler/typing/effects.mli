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

type key = Ident.t
type effects

val empty : effects
val is_empty : effects -> bool

val mem : key -> effects -> bool
val find : key -> effects -> signal_usage
val iter : (key -> signal_usage -> unit) -> effects -> unit

val add :
  key ->
  Location.t ->
  usage ->
  usage ->
  effects ->
  effects

val singleton :
  key ->
  Location.t ->
  usage ->
  usage ->
  effects

val merge : effects -> effects -> effects
val flatten : effects list -> effects

val apply :
  signal_usage ->
  effects ->
  effects

val update_loc : effects -> Location.t -> effects

val print : effects -> unit
val print_l : effects list -> unit
