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

(* file: rmltop_reactive_machine.ml *)
(* created: 2007-12-03  *)
(* author: Louis Mandel *)

module Interpretor = Implem_lco_ctrl_tree_record.Lco_ctrl_tree_record

let rml_react_unsafe =
  let react =
    Interpretor.rml_make_exec_process
      Rmltop_machine_body.main
  in
  fun l ->
    match react l with
    | None -> ()
    | Some () -> assert false


let rml_react x =
  Rmltop_global.lock();
  begin try rml_react_unsafe x
  with e -> Rmltop_global.unlock(); raise e
  end;
  Rmltop_global.unlock()


let sampling_hook min () = ()
let n_hook n () = ()
let debug_hook () = ()

let rml_exec boi_hook p =
  let res = ref None in
  let p' () =
    Interpretor.rml_def_dyn
      (Interpretor.rml_run (fun () -> p))
      (fun v -> Interpretor.rml_compute (fun () -> res := Some v))
  in
  let rec exec () =
    List.iter (fun hook -> hook ()) boi_hook;
    rml_react_unsafe [];
    match !res with
    | None -> exec ()
    | Some v -> v
  in
  rml_react_unsafe [p'];
  match !res with
  | None -> exec ()
  | Some v -> v
