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

let debug = ref false
let print_DEBUG x =
  if !debug
  then Printf.eprintf x
  else Printf.ifprintf stderr x

let show_help = ref false
let print_help () =
  print_endline "Toplevel directives:";
  let list =
    ["#run e;;",      "spawn the execution of a process e.";
     "#exec e;;",     "execute a reactive expression e.";
     "#suspend;;",    "suspend the simulation.";
     "#step;;",       "execute one instant of the system. (*)";
     "#step n;;",     "execute n instants of the system. (*)";
     "#resume;;",     "go back to the sampled mode.";
     "#sampling p;;", "change the sampling rate.";
     "#quit;;",       "exit the toplevel";
     "# e;;",         "execute e in the OCaml toplevel."
    ] in
  List.iter
    (fun (dir, msg) -> Printf.printf "  %-13s  %s\n" dir msg)
    list;
  Printf.printf "  (*): Can be used only while the simulation is suspended.\n\n";
  flush stdout

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

let init_rml = [
  "open Implem;;";
  "let () = Rmltop_global.print_prompt ();;";
]

let print_intro () =
  print_string "        ReactiveML version ";
  print_string Version.version;
  print_newline();
  if !show_help then print_help ()

let init_directory_paths () =
  let (//) = Filename.concat in
  let stdlib = Filename.dirname !Ocamlbuild_pack.Ocamlbuild_where.libdir in
  let add_dir dir =
    Topdirs.dir_directory dir;
    print_DEBUG "Added %s directory to search path.\n" dir
  in
  List.iter add_dir
    [ stdlib // "threads";
      Version.stdlib;
      Version.stdlib // "toplevel";
    ]

let init_toplevel () =
  init_directory_paths ();
  let load_file file =
    print_DEBUG "Trying to load %s... %!" file;
    if Topdirs.load_file Format.err_formatter file then
      print_DEBUG "done%s%!" "\n"
    else
      Printf.eprintf "Cannot find file %s.\n%!" file
  in
  List.iter load_file
    [ "unix.cma";
      "threads.cma";
      "rmllib.cma";
      "rmltop_global.cmo";
      "rmltop_implem.cmo";
      "rmltop_machine_body.cmo";
      "rmltop_reactive_machine.cmo";
      "rmltop_controller.cmo";
      "rmltop_directives.cmo";
      "rmltop_main.cmo"
    ]

let get_error str =
  let r_error = Pcre.regexp "File _*, line ([0-9]+ as line), characters ([0-9]+ as ofs_a)-([0-9]+ as ofs_b)" in
  if Pcre.pmatch ~rex:r_error str then
    try
      let pos = String.index str '\n' + 1 in
      String.sub str pos (String.length str - pos)
    with Not_found ->
      ""
  else str

let eval_command ?(silent=false) command =
  let buffer = Buffer.create 512 in
  let pp = Format.formatter_of_buffer buffer in
  Format.pp_set_margin pp max_int;
  try
    let _ =
      Toploop.execute_phrase (not silent) pp
        (!Toploop.parse_toplevel_phrase (Lexing.from_string (command ^ ";;")))
    in
    (true, Buffer.contents buffer)
  with exn ->
    let save = !Toploop.parse_use_file in
    Toploop.parse_use_file := (fun _ -> raise exn);
    ignore (Toploop.use_silently pp "/dev/null");
    Toploop.parse_use_file := save;
    (false, get_error (Buffer.contents buffer))

let eval_phrase ?(silent=false) phrase =
  let success, message = eval_command ~silent phrase in
  if not success then
    Printf.eprintf "%s\n%!" message
  else
    Printf.printf "%s\n%!" message

let sampling = ref None

let main s =
  let _ = print_intro() in
  (* start the machine *)
  begin match !sampling with
  | None -> ()
  | Some n -> Rmltop_directives.set_sampling n
  end;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  init_toplevel ();
  Sys.catch_break true;
  List.iter
    (fun phrase ->
      eval_phrase ~silent:(not !debug) phrase
    )
    init_rml;
  try
    let buf = Buffer.create 512 in
    Printf.printf "# %!";
    while true do
      let line = read_line () in
      let len = String.length line in
      let tail = if len < 2 then "" else String.sub line (len-2) 2 in
      if len = 0 || tail = ";;" then begin
        if len <> 0 then begin
          let () = Buffer.add_string buf line in
          let phrase = Buffer.contents buf in
          Buffer.reset buf;
          eval_phrase phrase;
        end;
        Printf.printf "# %!";
      end
    done
  with
    | End_of_file -> exit 0
    | Sys.Break -> Printf.eprintf "Interrupted.\n%!"
    | exn -> Printf.eprintf "%s\n%!" (Printexc.to_string exn)

let _ =
  Arg.parse (Arg.align
    [ "-sampling", Arg.Float (fun x -> if x >= 0.0 then sampling := Some x),
      "<rate> Sets the sampling rate to <rate> seconds";
      "-i", Arg.Set show_help, " List known rml directives at startup";
      "-debug", Arg.Set debug, " Enable debug output";
    ])
    (fun _ -> ())
    "";
  main ""
