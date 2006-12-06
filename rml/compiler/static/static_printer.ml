(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : static_printer.ml                                          *)
(*  Date de creation : 31/08/2006                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

open Asttypes
open Reac_ast
open Def_static

let print ty = 
  Format.open_box 0;
  Format.print_string (string_of_static ty);
  Format.close_box ()

let _ = Format.set_max_boxes max_int

let output oc ty = 
  Format.set_formatter_out_channel oc;
  Format.print_string "  ";
  print ty;
  Format.print_flush ()
