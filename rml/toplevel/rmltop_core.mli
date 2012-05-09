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

val sampling : float option ref

val debug : bool ref
val debug_status : unit -> string
val toggle_debug : Format.formatter -> unit
val print_DEBUG : ('a, out_channel, unit) format -> 'a

val print_help : unit -> unit
val add_terminator : string -> string

val eval_ocaml_phrase  : Format.formatter -> bool -> string -> bool
val eval_ocaml_phrases : Format.formatter -> bool -> string list -> unit

val eval : Format.formatter -> string -> unit
val controller_react : unit -> unit option

val init : unit -> unit
