(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : syntaxerr.ml                                               *)
(*  Date de creation : 05/05/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from OCaml                                          *)
(*************************************************************************)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Auxiliary type for reporting syntax errors *)

open Format

type error =
    Unclosed of Location.t * string * Location.t * string
  | Other of Location.t

exception Error of error
exception Escape_error

let report_error ppf = function
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      fprintf ppf "%aSyntax error: '%s' expected@."
        Location.print closing_loc closing;
      fprintf ppf "%aThis '%s' might be unmatched"
        Location.print opening_loc opening 
  | Other loc ->
      fprintf ppf "%aSyntax error" Location.print loc


