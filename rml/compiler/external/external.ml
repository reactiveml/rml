(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : external.ml                                                *)
(*  Date de creation : 23/03/2005                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id: external.ml,v 1.1 2005/03/29 10:18:21 mandel Exp $ *)

open Parse_ast

let expend impl =
  match impl.pimpl_desc with
  | Pimpl_lucky (id,inputs,outputs,files) -> 
      { pimpl_desc = Lucky.lucky_to_parse (id,inputs,outputs,files);
	pimpl_loc = impl.pimpl_loc; }
  | _ -> impl
