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

(* file: rmltop_main.ml *)
(* author: Louis Mandel *)
(* created: 2005-10-25  *)

let exec_machine_controller () =
  let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
  let debut = ref 0.0 in
  let sleep = ref 0.0 in
  let react =
    Rmltop_implem.Machine_controler_machine.rml_make
      Rmltop_controller.controller
  in
  while true do
    let _ = debut := Sys.time() in
    let _ = react () in
    let _ =
      sleep := !Rmltop_global.sampling -. ((Sys.time()) -. !debut);
      if !sleep > 0.001 then
	begin try Thread.delay !sleep
	with Unix.Unix_error _ -> () end
      else
        Thread.yield ()
    in ()
  done

let start () =
  Thread.create exec_machine_controller ()

let controller = start ()
