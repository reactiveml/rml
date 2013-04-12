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

(* file: parse.ml *)

(* Warning: *)
(* This file is based on the original version of parse.ml *)
(* from the Objective Caml 3.07 distribution, INRIA       *)

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

(* Entry points in the parser *)

open Location

(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer_row.token lexbuf with
      Parser_row.SEMISEMI | Parser_row.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer_row.Error (Lexer_row.Unterminated_comment, _) -> ()
    | Lexer_row.Error (Lexer_row.Unterminated_string, _) -> ()
    | Lexer_row.Error (Lexer_row.Unterminated_string_in_comment, _) -> ()
    | Lexer_row.Error (Lexer_row.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser_row.SEMISEMI
  || Parsing.is_current_lookahead Parser_row.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    let ast = parsing_fun Lexer_row.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Lexer_row.Error(Lexer_row.Unterminated_comment, _) as err -> raise err
  | Lexer_row.Error(Lexer_row.Unterminated_string, _) as err -> raise err
  | Lexer_row.Error(Lexer_row.Unterminated_string_in_comment, _) as err -> raise err
  | Lexer_row.Error(Lexer_row.Illegal_character _, _) as err ->
      if !Location.input_name = "" then skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err ->
      if !Location.input_name = "" then maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = ""
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
;;

let implementation = wrap Parser_row.implementation
and interface = wrap Parser_row.interface
and interactive = wrap Parser_row.interactive
