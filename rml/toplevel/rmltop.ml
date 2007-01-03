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

(* file: rmltop.ml *)
(* created: 2005-06-11  *)
(* author: Louis Mandel *)


let print_DEBUG s =
  print_string s;
  print_newline()


let end_of_phrase = Str.regexp ";;"
let begin_of_directive = Str.regexp "^\b*#"  

let search r s =
  try 
    let _ = Str.search_forward r s 0 in 
    true 
  with
  | Not_found -> false

let read_phrase ch =
  let rec aux line previous_lines =
    if search end_of_phrase line then
      List.rev ((line^"\n") :: previous_lines)
    else
      let new_line = input_line ch in
      aux new_line ((line^"\n") :: previous_lines)
  in
  fun s ->
    aux s []

let string_of_char c =
  let s = "a" in
  s.[0] <- c;
  s

let extract_directive s =
  let rec aux s res =
    match s.[0] with
    | ('a' .. 'z' | '_') as c -> 
	aux 
	  (String.sub s 1 ((String.length s) - 1)) 
	  (res ^ (string_of_char c))
    | c -> res
  in aux s ""

let main_loop rmltop_in rmlc_in rmlc_out ocaml_in =
  while true do
    let s = input_line rmltop_in in
    if search begin_of_directive s then
      let i = String.index s '#' in
      let s' = Str.string_after s (i+1) in
      let directive = 
	match (*String.sub s' 0 3*) extract_directive s' with
	| "run" ->
	    let proc = Str.string_after s' 3 in
	    "Rmltop_directives.set_add "::(read_phrase rmltop_in proc)
	| "step_by_step" ->
	    ["Rmltop_directives.set_step_by_step ();; \n"]
	| "step" ->
	    ["Rmltop_directives.set_step ();; \n"]
	| "suspend" ->
	    ["Rmltop_directives.set_suspend_resume ();; \n"]
	| "resume" ->
	    ["Rmltop_directives.set_suspend_resume ();; \n"]
	| "sampled" ->
	    ["Rmltop_directives.set_sampled ();; \n"]
	| "sampling" ->
	    let n = Str.string_after s' 8 in
	    "Rmltop_directives.set_sampling "::(read_phrase rmltop_in n)
	| _ ->
	    read_phrase rmltop_in s' 
      in
      (* send directive to OCaml *)
      List.iter (fun line -> output_string ocaml_in line) directive;
      flush ocaml_in;
    else
      let phrase = read_phrase rmltop_in s in
      (* send phrase to rmlc *)
      List.iter (fun line -> output_string rmlc_in line) phrase;
      flush rmlc_in;
      (* read the compiled phrase *)
      let ocaml_phrase = read_phrase rmlc_out "" in
      (* send the compiled phrase to OCaml *)
      List.iter (fun line -> output_string ocaml_in line) ocaml_phrase;
      flush ocaml_in;
  done

(*
let run_machine ocaml_in =
  output_string ocaml_in "Rmltop_main.machine();;\n";
  flush ocaml_in

let run_machine_sampling ocaml_in sampling =
  output_string ocaml_in ("Rml_interactive.machine_sampling "^
			  (string_of_float sampling)^";;\n");
  flush ocaml_in
*)
let init ocaml_in =
  output_string ocaml_in ("open Implantation;;\n");
  output_string ocaml_in ("();;\n");
  flush ocaml_in  

      
let print_intro () =
  print_string "        ReactiveML version ";
  let version_ch = Unix.open_process_in "rmlc -version" in
  let version = input_line version_ch in
  print_string version;
  close_in version_ch;
  print_newline()

let rmlc = ref "rmlc -i -interactive"
let ocaml = 
  ref 
(*"ocaml -I +threads -I `rmlc -where` unix.cma threads.cma rml_interactive.cmo "*)
    "ocaml -I +threads -I `rmlc -where` -I `rmlc -where`/toplevel unix.cma threads.cma rml_interpreter.cma rmltop_global.cmo rmltop_implantation.cmo rmltop_machine_controler.cmo rmltop_directives.cmo rmltop_main.cmo "

let sampling = ref None
    
let main s =
  let _ = print_intro() in
  (* fork the ReactiveML compiler *)
  let rmlc_out, rmlc_in = Unix.open_process !rmlc in
  (* fork the OCaml toplevel *)
  let ocaml_in = Unix.open_process_out (!ocaml ^ s) in
  (* start the machine *)
  begin match !sampling with
  | None -> (*run_machine ocaml_in*) 
      ()
  | Some n -> (*run_machine_sampling ocaml_in n*)
      Rmltop_directives.set_sampling n
  end;
  init ocaml_in;
  main_loop stdin rmlc_in rmlc_out ocaml_in

let usage = ""

let _ = 
  Arg.parse
    [ "-sampling", Arg.Float (fun x -> if x >= 0.0 then sampling := Some x), 
      "<rate> Sets the sampling rate to <rate> seconds";
      "--", Arg.Rest (fun x -> ocaml := !ocaml ^ x), 
      "Sends all others options to the Ocaml toplevel"]
    (fun x -> ocaml := !ocaml ^ x)
    usage;
  main ""

(*
  let args = 
    let tmp = ref "" in
    for i = 1 to Array.length Sys.argv - 1 do
      tmp := !tmp^" "^Sys.argv.(i)
    done;
    !tmp
  in
  main args
*)
