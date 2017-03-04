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

(* file: asttypes.mli *)

(* Warning: *)
(* This file is based on the original version of asttypes.mli *)
(* from the Objective Caml 3.07 distribution, INRIA *)

(* first modification: 2004-04-23 *)
(* modified by: Louis Mandel      *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Types used in the a.s.t. *)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type immediate_flag = Immediate | Nonimmediate

type mutable_flag = Mutable | Immutable

type await_kind = All | One

type pre_kind = Status | Value

type signal_kind = Default | Memory

type continue_begin_of_instant = K_boi | K_not_boi

type immediate =
  | Const_unit
  | Const_bool of bool
  | Const_int of int
  | Const_float of float
  | Const_char of char
  | Const_string of string
  | Const_bytes of string
