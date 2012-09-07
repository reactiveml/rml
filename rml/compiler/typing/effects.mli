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

type key
type t

val id : Ident.t -> key
val var : Ident.t -> key
val new_id : string -> Ident.t

val empty : t
val is_empty : t -> bool

val iter : (key -> signal_usage -> unit) -> t -> unit

val add :
  key ->
  Location.t ->
  usage ->
  usage ->
  t ->
  t

val singleton :
  Ident.t ->
  Location.t ->
  usage ->
  usage ->
  t

val merge : t -> t -> t
val flatten : t list -> t

val apply :
  signal_usage ->
  t ->
  t

val gen :
  (Ident.t -> bool) ->
  t ->
  t

val update_loc : t -> Location.t -> t

val print : t -> unit
