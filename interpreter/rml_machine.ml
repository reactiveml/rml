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
(* created: 2005-09-13  *)
(* file: rml_machine.ml *)



module type Interpretor_type =
  sig
    type 'a process
    val rml_make: 'a process -> (unit -> 'a option)
  end


module M =
  functor (Interpretor: Interpretor_type) ->
  struct

    let rml_exec_first = ref true

    let rml_exec boi_hook p =
      let react = Interpretor.rml_make p in
      match boi_hook with
      | [] ->
          let rec exec () =
	    match react () with
	    | None -> exec()
	    | Some v -> v
          in exec ()
      | l ->
          let hook =
            match l with
            | [] -> (fun () -> ())
            | [f] -> f
            | [f1; f2] -> (fun () -> f1 (); f2 ())
            | [f1; f2; f3] -> (fun () -> f1 (); f2 (); f3 ())
            | [f1; f2; f3; f4] -> (fun () -> f1 (); f2 (); f3 (); f4 ())
            | l -> (fun () -> List.iter (fun f -> f ()) l)
          in
          let rec exec () =
	    match react () with
	    | None -> hook (); exec()
	    | Some v -> v
          in
          if !rml_exec_first then (rml_exec_first := false; hook ());
          exec ()


  end

