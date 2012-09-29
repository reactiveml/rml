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

type 'a loc = { mutable loc : Location.t; node : 'a }
val mk_loc : Location.t -> 'a -> 'a loc

(* Usages *)
type usage = Affine | Neutral | Zero | Zero_1 | Var
type signal_usage

val string_of_usage : usage -> string
val desc_of_usage : usage -> string
val string_of_signal_usage : signal_usage -> string

exception Forbidden_usage of Location.t * Location.t
exception Forbidden_signal_usage of signal_usage * signal_usage

(* Operations on usages *)
val add_u : usage loc -> usage loc -> usage
val add_s : signal_usage -> signal_usage -> signal_usage

val compare : usage -> usage -> int

val max_u : usage -> usage -> usage
val max_s : signal_usage -> signal_usage -> signal_usage

val mk_null : signal_usage
val mk_su : Location.t -> usage -> usage -> signal_usage
val km_su : signal_usage -> Location.t * usage * usage
val km_s : signal_usage -> usage * usage

val update_loc : Location.t -> signal_usage -> signal_usage

val send_u : Location.t -> bool -> signal_usage
val await_u : Location.t -> bool -> signal_usage

val compatible_usage : signal_usage -> signal_usage -> bool
