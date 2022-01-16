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

(* file: location.ml *)

(* Warning: *)
(* This file is based on the original version of location.ml *)
(* from the Objective Caml 3.07 distribution, INRIA          *)

(* first modification: 2004-05-06 *)
(* modified by: Louis Mandel      *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Lexing

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let init lexbuf fname =
  lexbuf.lex_curr_pos <- 0;
  lexbuf.lex_buffer_len <- 0;
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let none = { loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = true };;

let concat loc1 loc2 =
  { loc_start = loc1.loc_start;
    loc_end = loc2.loc_end;
    loc_ghost = loc1.loc_ghost || loc2.loc_ghost; };;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;
(*
let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;*)

let loc_of_pos (ps, pe) = {
  loc_start = ps;
  loc_end = pe;
  loc_ghost = false;
};;

let gloc_of_pos (ps, pe) = {
    loc_start = ps;
    loc_end = pe;
    loc_ghost = true;
};;

let input_name = ref ""
let input_lexbuf = ref (None : lexbuf option)

let num_loc_lines = ref 0 (* number of lines already printed after input *)

(* Print the location in some way or another *)

open Format

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  match Sys.os_type with
  | "MacOS" -> ("File \"", "\"; line ", "; characters ", " to ", "", "### ")
  | _ -> ("File \"", "\", line ", ", characters ", "-", ":", "")

(* return file, line, char from the given position *)
let get_pos_info pos =
  let (filename, linenum, linebeg) =
    if pos.pos_fname = "" && !input_name = "" then
      ("", -1, 0)
    else if pos.pos_fname = "" then
      Linenum.for_position !input_name pos.pos_cnum
    else
      (pos.pos_fname, pos.pos_lnum, pos.pos_bol)
  in
  (filename, linenum, pos.pos_cnum - linebeg)
;;

let print ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "" then begin
    fprintf ppf "Characters %i-%i:@."
      loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s%s%s%i" msg_file file msg_line line;
    fprintf ppf "%s%i" msg_chars startchar;
    fprintf ppf "%s%i%s@.%s" msg_to endchar msg_colon msg_head
  end

(* let print_oc oc loc = *)
(*   print (formatter_of_out_channel oc) loc *)

let print_warning loc ppf w =
  if Warnings.is_active w then begin
    let printw ppf w =
      let n = Warnings.print ppf w in
      num_loc_lines := !num_loc_lines + n
    in
    fprintf ppf "%a" print loc;
    fprintf ppf "Warning: %a@." printw w;
    pp_print_flush ppf ();
    incr num_loc_lines;
  end
;;

let prerr_warning loc w = print_warning loc err_formatter w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines
