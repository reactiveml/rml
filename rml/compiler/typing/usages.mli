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

type 'a loc = { loc : Location.t; node : 'a }
type 'a loc_step = Location.t -> 'a

(* Usages *)
type usage = Affine | Neutral | Zero
type signal_usage

exception Forbidden_usage of Location.t * Location.t

(* Operations on usages *)
val add_u : usage loc -> usage loc -> usage
val add_s : signal_usage -> signal_usage -> signal_usage

val mk_zero : signal_usage
val mk_su : Location.t -> usage -> usage -> signal_usage
val km_su : signal_usage -> Location.t * usage * usage

val send_u : Location.t -> bool -> signal_usage
val await_u : Location.t -> bool -> signal_usage
