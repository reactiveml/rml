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

(* Usages *)
type usage = Affine | Neutral | Zero
type signal_usage = usage * usage

exception Forbidden_usage

(* Operations on usages *)
val add_u : usage -> usage -> usage
val add_s : signal_usage -> signal_usage -> signal_usage
val usage_of_type : Def_types.type_expression -> usage

module Table :
  sig
    type key = int
    type 'a t

    val empty : signal_usage t

    val mem : key -> signal_usage t -> bool
    val find : key -> signal_usage t -> signal_usage

    val add : key -> Def_types.type_expression -> Def_types.type_expression -> signal_usage t -> signal_usage t
    val singleton : key -> Def_types.type_expression -> Def_types.type_expression -> signal_usage t

    val merge : signal_usage t -> signal_usage t -> signal_usage t
    val flatten : signal_usage t list -> signal_usage t

    val print : signal_usage t -> unit
  end
