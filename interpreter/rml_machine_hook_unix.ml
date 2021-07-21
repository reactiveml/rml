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

(*
let wait_next_instant =
  let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
  fun debut fin min ->
    (* incr instant; *)
    let diff = min -. (fin -. debut) in
    if diff > 0.001 then (
      ignore (Unix.setitimer
		Unix.ITIMER_REAL
		{Unix.it_interval = 0.0; Unix.it_value = diff});
      Unix.pause();
      false)
    else true
*)

(*
let rec wait_next_instant debut fin min =
  let diff = min -. (fin -. debut) in
  if diff > 0.0 then
    begin try
      ignore (Unix.select [] [] [] diff); false
    with Unix.Unix_error (Unix.EINTR ,_, _) ->
      wait_next_instant debut (Unix.gettimeofday ()) min
    end
  else true
*)

let wait_next_instant =
  let retard = ref 0.0 in
  let rec wait_next_instant debut fin min_ =
    let diff = (min_ -. (fin -. debut)) -. !retard in
    if diff > 0.0 then
      begin try
        retard := 0.0;
        ignore (Unix.select [] [] [] diff); false
      with Unix.Unix_error (Unix.EINTR ,_, _) ->
        wait_next_instant debut (Unix.gettimeofday ()) min_
      end
    else
      begin
        retard := min (-. diff) min_;
        (* Format.eprintf "XXXXXX retard : %f@." !retard; *)
        true
      end
  in
  wait_next_instant


let sampling_hook =
  let first = ref true in
  let starting = ref 0.0 in
  (fun min ->
    let hook () =
      if !first then
        (first := false;
         starting := Unix.gettimeofday ())
      else
        let ending = Unix.gettimeofday () in
        ignore (wait_next_instant !starting ending min);
        starting := Unix.gettimeofday ()
    in
    hook)
