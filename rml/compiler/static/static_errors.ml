(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : static_errors.ml                                           *)
(*  Date de creation : 26/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: static_errors.ml,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

open Misc
open Reac_ast

(* Printing of error messages during the "static" analysis *)

let expr_wrong_static_err expr =
  Format.eprintf
    "%aThis expression is not static but it is used in a static context.\n"
    Location.print expr.expr_loc;
  raise Error

let impl_wrong_static_err impl =
  Format.eprintf
    "%aThis expression is not static but it is used in a static context.\n"
    Location.print impl.impl_loc;
  raise Error

