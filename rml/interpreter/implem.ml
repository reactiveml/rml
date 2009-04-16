(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2005-09-13  *)
(* file: implem.ml *)


module Lco_ctrl_tree_record = Lco_ctrl_tree.Rml_interpreter(Sig_env.Record)
(* module Lco_rewrite_record = Lco_rewrite.Rml_interpreter(Sig_env.Record) *)

module Lco_ctrl_tree_n_record = Lco_ctrl_tree_n.Rml_interpreter(Sig_env.Record)

module Lco_ctrl_tree_class = Lco_ctrl_tree.Rml_interpreter(Sig_env.Class)

module Lk_record = Lk_implem.Lk_interpreter(Sig_env.Record)


