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

let (//) = Filename.concat
let stdlib = Filename.dirname !Ocamlbuild_pack.Ocamlbuild_where.libdir

let include_dir = ref
  [ stdlib // "threads";
    Rmlcompiler.Version.stdlib;
    Rmlcompiler.Version.stdlib // "toplevel";
  ]
let include_obj = ref ["stdlib.cma"; "threads.cma"]
let add_include_dir inc = include_dir := inc :: !include_dir
let add_include_obj inc = include_obj := inc :: !include_obj

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

let rec eval_phrases ?(silent=false) = function
  | [] -> ()
  | phrase :: phrases ->
      let success, message = eval_command ~silent phrase in
      if not success then
        Printf.eprintf "%s\n%!" message
      else begin
        Printf.printf "%s\n%!" message;
        eval_phrases ~silent phrases
      end

let translate_and_eval_phrase rml_phrase =
  let rml_phrase = Lexing.from_string rml_phrase in
  try
    let rml_translation = Rmltop_lexer.phrase rml_phrase in
    match rml_translation with
    | Rmltop_lexer.Rml_phrase s ->
      let ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
      let () = Rmltop_global.lock () in
      let () = eval_phrases ocaml_phrases in
      Rmltop_global.unlock ()

    | Rmltop_lexer.OCaml_phrase s ->
      eval_phrases [ s ]

    | Rmltop_lexer.Run s ->
      (* add "(process ( run (...); ()));;" *)
      let ocaml_phrases = Rmlcompiler.Interactive.translate_phrase
	("let ()= Rmltop_global.to_run:=(process(run(" ^s^ ");()))::!Rmltop_global.to_run;;")
      in
      let () = Rmltop_global.lock() in
      let () = eval_phrases ocaml_phrases in
      Rmltop_global.unlock ()

    | Rmltop_lexer.Exec s ->
      (* add "(process ( ...; ()));;" *)
      let ocaml_phrases = Rmlcompiler.Interactive.translate_phrase
	("let () = Rmltop_global.to_run := (process ("^s^"; ())) :: !Rmltop_global.to_run;;")
      in
      let () = Rmltop_global.lock () in
      let () = eval_phrases ocaml_phrases in
      Rmltop_global.unlock ()

    | Rmltop_lexer.Step None ->
      eval_phrases [ "let () = Rmltop_directives.set_step 1 ;;"; ]

    | Rmltop_lexer.Step (Some n) ->
      eval_phrases [ "let () = Rmltop_directives.set_step "^ (string_of_int n) ^ ";;"; ]

    | Rmltop_lexer.Suspend ->
      eval_phrases [ "let () = Rmltop_directives.set_suspend () ;;"; ]

    | Rmltop_lexer.Resume ->
      eval_phrases [ "let () = Rmltop_directives.set_resume () ;;"; ]

    | Rmltop_lexer.Sampling n ->
      eval_phrases [ "let () = Rmltop_directives.set_sampling "^(string_of_float n)^";;"; ]

    | Rmltop_lexer.Quit -> exit 0
  with
    | Rmltop_lexer.EOF -> Printf.eprintf "Got an EOF! Exiting...%!"; exit 0
    | Rmltop_lexer.Syntax_error -> ()

let init_rml = [
  "open Implem;;";
]

let print_intro () =
  print_string "        ReactiveML version ";
  print_string Rmlcompiler.Version.version;
  print_newline();
  if !show_help then print_help ()

let load_dir dir =
  Topdirs.dir_directory dir;
  print_DEBUG "Added %s directory to search path.\n" dir

let load_file file =
  print_DEBUG "Trying to load %s... %!" file;
  if Topdirs.load_file Format.err_formatter file then
  print_DEBUG "done%s%!" "\n"
  else
  Printf.eprintf "Cannot find file %s.\n%!" file

let init_toplevel () =
  List.iter load_dir !include_dir;
  List.iter load_file !include_obj

let load_script file =
  (* TODO *)
  ()

let load_ocamlinit () =
  if Sys.file_exists ".ocamlinit" then
    load_script ".ocamlinit"
  else
    try
      let home_init = Filename.concat (Sys.getenv "HOME") ".ocamlinit" in
      if Sys.file_exists home_init then load_script home_init
    with Not_found -> ()

let main s =
  let _ = print_intro() in
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  init_toplevel ();
  Sys.catch_break true;
  eval_phrases ~silent:(not !debug) init_rml;
  try
    let buf = Buffer.create 512 in
    Rmltop_global.print_prompt ();
    while true do
      let line = read_line () in
      let len = String.length line in
      let tail = if len < 2 then "" else String.sub line (len-2) 2 in
      if len = 0 || tail = ";;" then begin
        if len <> 0 then begin
          let () = Buffer.add_string buf line in
          let phrase = Buffer.contents buf in
          Buffer.reset buf;
          translate_and_eval_phrase phrase;
        end;
        Rmltop_global.print_prompt ();
      end
    done
  with
    | End_of_file -> exit 0
    | Sys.Break -> Printf.eprintf "Interrupted.\n%!"
    | exn -> Printf.eprintf "%s\n%!" (Printexc.to_string exn)

let _ =
  Arg.parse (Arg.align
    [ "-sampling", Arg.Float (fun x -> if x >= 0.0 then Rmltop_directives.set_sampling x),
      "<rate> Sets the sampling rate to <rate> seconds";
      "-i", Arg.Set show_help, " List known rml directives at startup";
      "-debug", Arg.Set debug, " Enable debug output";
      "-I", Arg.String add_include_dir, "<dir>  Add <dir> to the list of include directories";
    ])
    add_include_obj
    "";
  main ""
