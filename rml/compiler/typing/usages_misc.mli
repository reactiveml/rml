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

val string_of_usage : usage -> string
val usage_of_type : Def_types.type_expression -> usage
val type_of_usage : usage -> Def_types.type_expression
val string_of_signal_usage : signal_usage -> string
val mk_t :
  Location.t ->
  Def_types.type_expression ->
  Def_types.type_expression ->
  signal_usage

module Table :
  sig
    type key = int
    type 'a t

    val empty : signal_usage t
    val is_empty : signal_usage t -> bool

    val mem : key -> signal_usage t -> bool
    val find : key -> signal_usage t -> signal_usage

    val add :
      key ->
      Location.t ->
      Def_types.type_expression ->
      Def_types.type_expression ->
      signal_usage t ->
      signal_usage t

    val singleton :
      key ->
      Location.t ->
      Def_types.type_expression ->
      Def_types.type_expression ->
      signal_usage t

    val merge : signal_usage t -> signal_usage t -> signal_usage t
    val flatten : signal_usage t list -> signal_usage t

    val print : signal_usage t -> unit
  end
