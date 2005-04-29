(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : global.ml                                                  *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : inspired by CamlLight                                     *)
(*************************************************************************)

(* $Id: global.ml,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

open Misc

(* values in the symbol table *)

type 'a global =
  { mutable gi: Global_ident.qualified_ident;
    mutable info: 'a }

let no_info = Obj.magic()

let little_name_of_global g = Global_ident.little_name g.gi
