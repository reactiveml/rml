(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2015-07-02  *)
(* file: rml_machine_hook.ml *)

let n_hook =
  let first = ref true in
  let cpt = ref 0 in
  (fun n ->
    if !first then (first := false; cpt := n);
    let hook () =
      if !cpt > 0 then decr cpt
      else exit 0
    in hook)

let debug_hook =
  let instant = ref 0 in
  (fun () ->
    incr instant;
    Format.eprintf "************ Instant %d ************@." !instant)
