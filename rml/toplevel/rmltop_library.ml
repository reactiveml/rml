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

let get_error s =
  try
    let i = String.index s ',' in
    String.sub s (i+2) (String.length s - i - 2)
  with Not_found -> s

let eval_command ?(silent=false) fmt command =
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
    Errors.report_error fmt exn;
    (false, get_error (Buffer.contents buffer))

let print_message fmt message =
  if String.length message <> 0 then
    Format.fprintf fmt "@[%s@]@." message

let rec eval_phrases ?(silent=false) fmt = function
  | [] -> ()
  | phrase :: phrases ->
      let success, message = eval_command ~silent fmt phrase in
      print_message fmt message;
      if success then
        eval_phrases ~silent fmt phrases

let sampling : string option ref = ref None
let set_sampling fmt f =
  eval_phrases ~silent:true fmt [ "let () = Rmltop_global.set_sampling "^ f ^";;"; ]

let translate_and_eval_phrase fmt rml_phrase =
  let rml_phrase = Lexing.from_string rml_phrase in
  try
    let rml_translation = Rmltop_lexer.phrase rml_phrase in
    match rml_translation with
    | Rmltop_lexer.Rml_phrase s ->
      let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
      begin match error with
      | Some error -> Format.fprintf fmt "@[%s@]@." error
      | None ->
        eval_phrases ~silent:true fmt [ "Rmltop_global.lock ();;" ];
        eval_phrases fmt ocaml_phrases;
        eval_phrases fmt ~silent:true [ "Rmltop_global.unlock ();;" ]
      end

    | Rmltop_lexer.OCaml_phrase s ->
      eval_phrases fmt [ s ]

    | Rmltop_lexer.Run s ->
      (* add "(process ( run (...); ()));;" *)
      let s = Printf.sprintf
        "let () = Rmltop_global.add_to_run (process (run( %s );()));;"
        s in
      let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
      begin match error with
      | Some error -> Format.fprintf fmt "@[%s@]@." error
      | None -> eval_phrases ~silent:true fmt ocaml_phrases
      end

    | Rmltop_lexer.Exec s ->
      (* add "(process ( ...; ()));;" *)
      let s = Printf.sprintf
        "let () = Rmltop_global.add_to_run (process (%s; ()));;"
        s in
      let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
      begin match error with
      | Some error -> Format.fprintf fmt "@[%s@]@." error
      | None -> eval_phrases ~silent:true fmt ocaml_phrases
      end

    | Rmltop_lexer.Step None ->
      eval_phrases ~silent:true fmt [ "let () = Rmltop_global.set_step 1 ;;"; ]

    | Rmltop_lexer.Step (Some n) ->
      eval_phrases ~silent:true fmt [ "let () = Rmltop_global.set_step "^ n ^ ";;"; ]

    | Rmltop_lexer.Suspend ->
      eval_phrases ~silent:true fmt [ "let () = Rmltop_global.set_suspend () ;;"; ]

    | Rmltop_lexer.Resume ->
      eval_phrases ~silent:true fmt [ "let () = Rmltop_global.set_resume () ;;"; ]

    | Rmltop_lexer.Sampling f ->
      set_sampling fmt f

    | Rmltop_lexer.Quit -> exit 0

    | Rmltop_lexer.Help -> print_help ()
  with
    | Rmltop_lexer.EOF -> Format.fprintf fmt "Got an EOF! Exiting...%!"; exit 0
    | Rmltop_lexer.Syntax_error -> ()

let translate_phrase fmt rml_phrase =
  let phrase = Lexing.from_string rml_phrase in
  try
    let rml_translation = Rmltop_lexer.phrase phrase in
    match rml_translation with
    | Rmltop_lexer.Rml_phrase s ->
        let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
        begin match error with
        | Some error -> raise Not_found
        | None -> ocaml_phrases
        end

    | Rmltop_lexer.OCaml_phrase s ->
        [ s ]

    | Rmltop_lexer.Run s ->
        (* add "(process ( run (...); ()));;" *)
        let s = Printf.sprintf
          "let () = Rmltop_global.add_to_run (process (run( %s );()));;"
          s in
        let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
        begin match error with
        | Some error -> raise Not_found
        | None -> ocaml_phrases
        end

    | Rmltop_lexer.Exec s ->
        (* add "(process ( ...; ()));;" *)
        let s = Printf.sprintf
          "let () = Rmltop_global.add_to_run (process (%s; ()));;"
          s in
        let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
        begin match error with
        | Some error -> raise Not_found
        | None -> ocaml_phrases
        end

    | Rmltop_lexer.Step None ->
        [ "let () =
                 Rmltop_reactive_machine.rml_react (Rmltop_controller.get_to_run ());;";
        ]

    | Rmltop_lexer.Step (Some n) ->
        let s = Printf.sprintf "Rmltop_reactive_machine.rml_react ([process (
                 for i = 1 to %s do
                      Rmltop_reactive_machine.rml_react (Rmltop_controller.get_to_run ()) ;
                      pause
                 done)]);;" n
        in
        let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
        begin match error with
        | Some error -> raise Not_found
        | None -> ocaml_phrases
        end

    | Rmltop_lexer.Suspend ->
        [ "let () = print_endline \"Not implemented in JS.\" ;;"; ]

    | Rmltop_lexer.Resume ->
        [ "let () = print_endline \"Not implemented in JS.\" ;;"; ]

    | Rmltop_lexer.Sampling f ->
        [ "let () = print_endline \"Not implemented in JS.\" ;;"; ]

    | Rmltop_lexer.Quit ->
        [ "let () = print_endline \"Not implemented in JS.\" ;;"; ]

    | Rmltop_lexer.Help ->
        print_help (); []
  with
    | Rmltop_lexer.EOF -> raise End_of_file
    | Rmltop_lexer.Syntax_error -> []

