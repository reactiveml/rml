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

(* file: rmltop_global.ml *)
(* author: Louis Mandel *)
(* created: 2005-09-23  *)

type 'a rml_process = 'a Implem.Lco_ctrl_tree_record.process

let global_mutex = Mutex.create ()
let lock () = Mutex.lock global_mutex
let unlock () = Mutex.unlock global_mutex

let suspend = ref None
let resume = ref None
let step = ref None
let to_run = ref []
let sampling = ref 0.01

let set ref n =
  lock();
  ref := Some n;
  unlock()

let set_suspend = set suspend
let set_resume = set resume
let set_step = set step
let add_to_run p =
  lock ();
  to_run := p :: !to_run;
  unlock ()

let set_sampling n =  sampling := n

let enter_step_by_step_mode = ref (fun () -> ())
let exit_step_by_step_mode = ref (fun () -> ())
