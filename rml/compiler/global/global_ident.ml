(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : global_ident.ml                                            *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from Lucid Synchrone (names.ml)                     *)
(*************************************************************************)

(* $Id: global_ident.ml,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

(* global names *)

type qualified_ident =
    { qual: string;
      id: Ident.t }

let same i1 i2 =
  (Ident.same i1.id i2.id) && (i1.qual = i2.qual)

let name i = i.qual ^ "." ^ (Ident.name i.id)

let little_name i = Ident.name i.id

let print ppf i = Format.fprintf ppf "%s@? " (name i)

let print_oc oc i = print (Format.formatter_of_out_channel oc) i

