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

(* file: rmltop_directives.ml *)
(* author: Louis Mandel *)
(* created: 2005-10-25  *)

let set ref n =
  Rmltop_global.lock();
  ref := Some n;
  Rmltop_global.unlock()

let set_sampling n =  Rmltop_global.sampling := n

let set_sampled = set Rmltop_global.sampled
let set_suspend_resume = set Rmltop_global.suspend_resume 
let set_step_by_step = set Rmltop_global.step_by_step 
let set_step = set Rmltop_global.step 
let set_add = set Rmltop_global.add 
