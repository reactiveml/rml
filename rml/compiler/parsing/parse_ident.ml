(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : parse_ident.ml                                             *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : inspired by OCaml                                         *)
(*************************************************************************)

(* $Id: parse_ident.ml,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

(* Identifier used in parse_ast *)

type t =
  | Pident of string
  | Pdot of string * string

let string_of_parseident p =
  match p with
  | Pident s -> s
  | Pdot (s1,s2) -> s1^"."^s2
