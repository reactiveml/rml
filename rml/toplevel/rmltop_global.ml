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

type 'a rml_process =
    'a Implem_lco_ctrl_tree_record.Lco_ctrl_tree_record.process

let global_mutex = Mutex.create ()
let lock () = Mutex.lock global_mutex
let unlock () = Mutex.unlock global_mutex

let suspend = ref None
let resume = ref None
let step = ref None
let to_run = ref []
let sampling = ref 0.01

let print_prompt () = print_string "# "

type 'a sync_point = 'a ref * Mutex.t * Condition.t

let create_sp () =
  let r = ref (Obj.magic ()) in
  let m = Mutex.create () in
  let c = Condition.create () in
  Mutex.lock m;
  (r, m, c)

let push_sp (r, m, c) v =
  Mutex.lock m;
  r := v;
  Mutex.unlock m;
  Condition.signal c

let wait_sp (r, m, c) =
  Condition.wait c m;
  Mutex.unlock m;
  !r

let add_to_run p =
  lock ();
  to_run := p :: !to_run;
  unlock ()
