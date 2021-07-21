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

(* file: global_ident.ml *)

(* Warning: *)
(* This file is based on the original version of names.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6  *)

(* first modification: 2004-04-23 *)
(* modified by: Louis Mandel      *)

(* $Id$ *)


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

