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

let print_DEBUG s = print_string s; print_newline()

let main_loop rmltop_in rmlc_in rmlc_out ocaml_in =
  let rmltop_in_lexbuf = Lexing.from_channel rmltop_in in
  let rmlc_out_lexbuf = Lexing.from_channel rmlc_out in
  while true do
    try
      let ocaml_phrase =
	match Rmltop_lexer.phrase rmltop_in_lexbuf with
	| Rmltop_lexer.Rml_phrase s ->
	    let phrase = [ s; ";;" ] in
	    (* send phrase to rmlc *)
	    List.iter (fun line -> output_string rmlc_in line) phrase;
	    flush rmlc_in;
	    (* read the compiled phrase *)
	    let ocaml_phrase = Rmltop_lexer.expr rmlc_out_lexbuf in
	    [ "let () = Rmltop_global.lock() ;;\n";
	      ocaml_phrase;";;\n";
	      "let () = Rmltop_global.unlock();;\n";
	      "let () = Rmltop_global.print_prompt();;" ]
	| Rmltop_lexer.OCaml_phrase s ->
	    [ s; ";;\n";
	      "let () = Rmltop_global.print_prompt();;" ]
	| Rmltop_lexer.Run s -> 
	    (* add "(process ( run (...); ()));;" *)
	    let phrase = 
	      [ "let () = Rmltop_global.to_run := ";
		"(process ( run (";s;"); ())) :: !Rmltop_global.to_run ;;" ]
	    in
	    (* send phrase to rmlc *)
	    List.iter (fun line -> output_string rmlc_in line) phrase;
	    flush rmlc_in;
	    (* read the compiled phrase *)
	    let ocaml_phrase = Rmltop_lexer.expr rmlc_out_lexbuf in
	    [ "let () = Rmltop_global.lock();; \n";
	      ocaml_phrase;";;\n";
	      "let () = Rmltop_global.unlock();; \n";
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Exec s ->
	    (* add "(process ( ...; ()));;" *)
	    let phrase = 
	      [ "let () = Rmltop_global.to_run := ";
		"(process (";s;"; ())) :: !Rmltop_global.to_run;;" ] 
	    in
	    (* send phrase to rmlc *)
	    List.iter (fun line -> output_string rmlc_in line) phrase;
	    flush rmlc_in;
	    (* read the compiled phrase *)
	    let ocaml_phrase = Rmltop_lexer.expr rmlc_out_lexbuf in
	    [ "let () = Rmltop_global.lock();;\n";
	      ocaml_phrase;";;\n";
	      "let () = Rmltop_global.unlock();;\n";
	      "let () = Rmltop_global.print_prompt () ;;"]
	| Rmltop_lexer.Step None ->
	    [ "let () = Rmltop_directives.set_step 1 ;;\n";
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Step (Some n) ->
	    [ "let () = Rmltop_directives.set_step "; string_of_int n; ";;\n"; 
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Suspend ->
	    [ "let () = Rmltop_directives.set_suspend () ;;\n";
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Resume ->
	    [ "let () = Rmltop_directives.set_resume () ;;\n";
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Sampling n ->
            [ "let () = Rmltop_directives.set_sampling ";
	      string_of_float n;";;\n";
	      "let () = Rmltop_global.print_prompt ();;" ]
	| Rmltop_lexer.Quit -> exit 0
      in
      (* send to OCaml *)
      List.iter (fun line -> output_string ocaml_in line) ocaml_phrase;
      output_string ocaml_in "\n";
      flush ocaml_in
    with
    | Rmltop_lexer.EOF -> exit 0
    | Rmltop_lexer.Syntax_error -> 
	Printf.fprintf stderr "Syntax error\n";
	flush stderr;
	output_string ocaml_in "let () = Rmltop_global.print_prompt ();;\n\n";
	flush ocaml_in
  done

let init ocaml_in =
  output_string ocaml_in ("open Implem;;\n");
  output_string ocaml_in ("let () = Rmltop_global.print_prompt ();;\n");
  flush ocaml_in  
      
let print_intro () =
  print_string "        ReactiveML version ";
  let version_ch = Unix.open_process_in "rmlc -version" in
  let version = input_line version_ch in
  print_string version;
  close_in version_ch;
  print_newline()

let rmlc = ref "rmlc -i -interactive -I `rmlc -where`/toplevel"
let ocaml = 
  ref 
(*"ocaml -I +threads -I `rmlc -where` unix.cma threads.cma rml_interactive.cmo "*)
    "TERM=norepeat ocaml -noprompt -I +threads -I `rmlc -where` -I `rmlc -where`/toplevel unix.cma threads.cma rmllib.cma rmltop_global.cmo rmltop_implem.cmo rmltop_machine_body.cmo rmltop_reactive_machine.cmo rmltop_controller.cmo rmltop_directives.cmo rmltop_main.cmo "

let sampling = ref None
    
let main s =
  let _ = print_intro() in
  (* fork the ReactiveML compiler *)
  let rmlc_out, rmlc_in = Unix.open_process !rmlc in
  at_exit (fun () -> ignore (Unix.close_process (rmlc_out, rmlc_in)));
  (* fork the OCaml toplevel *)
  let ocaml_in = Unix.open_process_out (!ocaml ^ s) in
  at_exit (fun () -> ignore (Unix.close_process_out ocaml_in));
  (* start the machine *)
  begin match !sampling with
  | None -> ()
  | Some n -> Rmltop_directives.set_sampling n
  end;
  init ocaml_in;
  main_loop stdin rmlc_in rmlc_out ocaml_in

let usage = ""

let _ = 
  Arg.parse
    [ "-sampling", Arg.Float (fun x -> if x >= 0.0 then sampling := Some x), 
      "<rate> Sets the sampling rate to <rate> seconds";
      "--", Arg.Rest (fun x -> ocaml := !ocaml ^ " " ^ x), 
      "Sends all others options to the Ocaml toplevel"]
    (fun x -> ocaml := !ocaml ^ " " ^ x)
    usage;
  main ""
