(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : def_static.mli                                             *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: def_static.mli,v 1.1.1.1 2005/01/23 17:55:37 mandel Exp $ *)

type context = Process | ML

type static = Static | Dynamic 

(* For debug *)
let string_of_static typ = 
  match typ with
  | Static -> "Static"
  | Dynamic -> "Dynamic"


