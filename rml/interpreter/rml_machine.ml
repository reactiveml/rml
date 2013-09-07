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


module M =
  functor (Interpretor: Interpretor_type) ->
  struct

    let sampling_hook =
      let starting = ref (Unix.gettimeofday ()) in
      (fun min ->
        let hook () =
          let ending = Unix.gettimeofday () in
          ignore (wait_next_instant !starting ending min);
          starting := Unix.gettimeofday ()
        in
        hook)

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


    let rml_exec_n_sampling p n min =
      let instant = ref 0 in
      let react = Interpretor.rml_make p in
      let rec exec n =
	if n > 0 then
	  let _ =
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
	    incr instant
	  in
	  let debut = Unix.gettimeofday () in
	  let v = react () in
	  let fin = Unix.gettimeofday () in
          if wait_next_instant debut fin min then begin
	    print_string "Instant ";
	    print_int !instant;
	    print_string " : depassement !";
	    (* print_float (-. !diff); *)
	    print_newline()
          end;
	  match v with
	  | None -> exec (n-1)
	  | v -> v
	else
	  None
      in exec n


  end

