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

(* file: ident.ml *)

(* Warning: *)
(* This file is based on the original version of ident.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6  *)

(* first modification: 2004-04-23  *)
(* modified by: Louis Mandel *)

(* $Id$ *)

type kind =
    Val_ML | Val_RML | Sig
  | Constr | Label | Exn | Type | Internal

type t = { id: int; name: string; kind: kind; }

let compare i1 i2 = compare i1.id i2.id

let name i = i.name

let string_of_kind = function
  | Val_ML -> "val_ml"
  | Val_RML -> "val_rml"
  | Sig -> "sig"
  | Constr -> "cstr"
  | Label -> "lbl"
  | Exn -> "exn"
  | Type -> "ty"
  | Internal -> "loc"

let unique_name i =
  match String.get i.name 0 with
  | '=' | '<' | '>' | '@' | '^' | '|' | '&' | '+' | '-' | '*' | '/' | '$' | '%'
  | '!' | '?' | '~' ->
      "__" ^ (string_of_kind i.kind) ^ "_" ^ (string_of_int i.id)
  | _ ->
      i.name ^ "__" ^ (string_of_kind i.kind) ^ "_" ^ (string_of_int i.id)

(* generating names *)
class name_generator =
  object
    val mutable counter = 0
    method name =
      counter <- counter + 1;
      counter
    method reset =
      counter <- 0
    method init i =
      counter <- i
  end

let gen_var = new name_generator
let gen_constr = new name_generator
let gen_type = new name_generator
let gen_label = new name_generator



let create gen s k =
  {id = gen#name;
   name = s;
   kind = k; }

let same i1 i2 = i1.id = i2.id
