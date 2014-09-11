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

(* file: async_body.rml *)
(* created: 2013-07-22  *)
(* author: Louis Mandel *)

let mutex = Mutex.create ()
let atomic f x =
  Mutex.lock mutex;
  let v = f x in
  Mutex.unlock mutex;
  v

let exec_boi_list = ref ([] : (unit -> unit) list)


let exec_boi f =
  atomic (fun f -> exec_boi_list := f :: !exec_boi_list) f

let boi_hook () =
  Thread.yield ();
  match !exec_boi_list with
  | [] -> ()
  | _ ->
      let l =
        atomic (fun () -> let l = !exec_boi_list in exec_boi_list := []; l) ()
      in
      List.iter (fun f -> f ()) (List.rev l)

let spawn f =
  ignore (Thread.create f ())

let yield = Thread.yield
