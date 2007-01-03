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

open Implantation;;

type 'a rml_process = 'a Lco_ctrl_tree_record.process

let sampling = ref 0.01

let sampled = ref None
let suspend_resume = ref None
let step_by_step = ref None
let step = ref None
let add = ref None

let global_mutex = Mutex.create ()
let lock () = Mutex.lock global_mutex
let unlock () = Mutex.unlock global_mutex

let rml_nothing =  
  fun () -> Lco_ctrl_tree_record.rml_nothing

let rml_halt = 
(*  (fun () -> Lco_ctrl_tree_record.rml_loop Lco_ctrl_tree_record.rml_pause) *)
  fun () -> Lco_ctrl_tree_record.rml_halt

let combine_process p q =
  fun () ->
    Lco_ctrl_tree_record.rml_par
      (Lco_ctrl_tree_record.rml_run (fun () -> q))
      (Lco_ctrl_tree_record.rml_run (fun () -> p))

let rml_react, add_process = 
  let step, add =
    Lco_ctrl_tree_record.rml_make_unit rml_halt
  in
  (fun () -> ignore (step())),
  add
