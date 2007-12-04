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

let rml_react = 
  let step =
    Implantation.Lco_ctrl_tree_record.rml_make Rmltop_machine_body.exec_process
  in
  (fun () -> ignore (step()))

let emit_add p = 
  Implantation.Lco_ctrl_tree_record.rml_expr_emit_val
    Rmltop_machine_body.add
    ((Obj.magic p): unit Rmltop_global.rml_process)
