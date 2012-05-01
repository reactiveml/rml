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

let sampling : float option ref = ref None

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

let eval_command fmt verbose command =
  try
    let lb = Lexing.from_string (command ^ ";;") in
    let _ =
      Toploop.execute_phrase verbose fmt
        (!Toploop.parse_toplevel_phrase lb)
    in
    true
  with
    | exn ->
        Errors.report_error fmt exn;
        false

let rec eval_phrases fmt verbose = function
  | [] -> ()
  | phrase :: phrases ->
      let success = eval_command fmt verbose phrase in
      if success then
        eval_phrases fmt verbose phrases

let parse rml =
  let lb = Lexing.from_string rml in
  Rmltop_lexer.phrase lb

let eval fmt parse rml_phrase =
  let rec aux fmt directive =
    try
      match directive with
      | Rmltop_lexer.Rml_phrases l ->
          List.iter (aux fmt)  l
      | Rmltop_lexer.Rml_phrase s ->
          let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
          begin match error with
            | Some error -> Format.fprintf fmt "@[%s@]@." error
            | None ->
                Rmltop_global.lock ();
                eval_phrases fmt true ocaml_phrases;
                Rmltop_global.unlock ()
          end

      | Rmltop_lexer.OCaml_phrase s ->
          eval_phrases fmt true [ s ]

      | Rmltop_lexer.Run s ->
          (* add "(process ( run (...); ()));;" *)
          let s = Printf.sprintf
            "let () = Rmltop_global.add_to_run (process (run( %s );()));;"
            s in
          let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
          begin match error with
            | Some error -> Format.fprintf fmt "@[%s@]@." error
            | None -> eval_phrases fmt false ocaml_phrases
          end

      | Rmltop_lexer.Exec s ->
          (* add "(process ( ...; ()));;" *)
          let s = Printf.sprintf
            "let () = Rmltop_global.add_to_run (process (%s; ()));;"
            s in
          let error, ocaml_phrases = Rmlcompiler.Interactive.translate_phrase s in
          begin match error with
            | Some error -> Format.fprintf fmt "@[%s@]@." error
            | None -> eval_phrases fmt true ocaml_phrases
          end

      | Rmltop_lexer.Step None -> Rmltop_global.set_step 1

      | Rmltop_lexer.Step (Some n) -> Rmltop_global.set_step n

      | Rmltop_lexer.Suspend -> Rmltop_global.set_suspend ()

      | Rmltop_lexer.Resume -> Rmltop_global.set_resume ()

      | Rmltop_lexer.Sampling f -> Rmltop_global.set_sampling f

      | Rmltop_lexer.Quit -> exit 0

      | Rmltop_lexer.Help -> print_help ()
    with
      | Rmltop_lexer.EOF -> Format.fprintf fmt "Got an EOF! Exiting...%!"; exit 0
      | Rmltop_lexer.Syntax_error -> ()
  in
  aux fmt (parse rml_phrase)
