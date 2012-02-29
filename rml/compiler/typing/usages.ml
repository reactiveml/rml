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

type usage =
  | Affine
  | Neutral
  | Zero

type signal_usage = usage * usage

exception Forbidden_usage

let add_u u1 u2 = match u1, u2 with
  | Zero, u | u, Zero -> u
  | Neutral, Neutral -> Neutral
  | _ -> raise Forbidden_usage

let add_s (u1, u2) (v1, v2) =
  add_u u1 v1,
  add_u u2 v2

let addable u1 u2 =
  try
    ignore (add_u u1 u2);
    true
  with Forbidden_usage -> false

let mk_zero = Zero, Zero

let mk_su su1 su2 = su1, su2
