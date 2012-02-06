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

(* file: external.ml *)
(* created: 2005-03-23  *)
(* author: Louis Mandel *)

(* $Id$ *)

open Parse_ast

let expend impl =
  match impl.pimpl_desc with
  | Pimpl_lucky (id,inputs,outputs,files) ->
      { pimpl_desc = Lucky.lucky_to_parse (id,inputs,outputs,files);
	pimpl_loc = impl.pimpl_loc; }
  | _ -> impl
