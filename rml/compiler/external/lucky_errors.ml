(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : lucky_errors.ml                                            *)
(*  Date de creation : 22/03/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: lucky_errors.ml,v 1.1 2005/03/29 10:18:21 mandel Exp $ *)

open Misc
open Parse_ast

(* Printing of error messages about Lucky import *)

let not_implemented_type ty =
  Printf.eprintf
    "%aThis type cannot be used in a Lucky process."
    Location.print_oc ty.pte_loc;
  raise Error
