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

let rml_react_unsafe =
  let react =
    Implem.Lco_ctrl_tree_record.rml_make_exec_process
      Rmltop_alt_machine_body.main
  in
  fun l ->
    match react l with
    | None -> ()
    | Some () -> assert false

let rml_react x =
  Rmltop_alt_global.lock();
  begin try rml_react_unsafe x
  with e -> Rmltop_alt_global.unlock(); raise e
  end;
  Rmltop_alt_global.unlock()
