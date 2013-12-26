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

(* file: rmltop_implem.ml *)
(* author: Louis Mandel *)
(* created: 2005-10-25  *)

module Sig_env (* : S *) =
  struct

    type kind = Default | Memory

    type ('a, 'b) t =
	{ mutable status: int;
	  mutable value: 'b;
	  mutable pre_status: int;
	  mutable last: 'b;
	  default: 'b;
	  combine: ('a -> 'b -> 'b);
          kind: kind; }

    let instant = ref 0
    let absent = -2

    let create default combine =
      { status = absent;
	value = default;
	pre_status = absent;
	last = default;
	default = default;
	combine = combine;
        kind = Default; }

    let create_memory default combine =
      { status = absent;
        value = default;
        pre_status = absent;
        last = default;
        default = default;
        combine = combine;
        kind = Memory; }

(* -------------------------- Access functions -------------------------- *)
    let default n = n.default
    let status n = n.status = !instant

    let value n = n.value

    let pre_status n =
      if n.status = !instant
      then n.pre_status = !instant - 1
      else n.status = !instant - 1

    let last n =
      if n.status = !instant
      then n.last
      else n.value

    let pre_value n =
      if n.status = !instant
      then
	if n.pre_status = !instant - 1
	then n.last
	else n.default
      else
	if n.status = !instant - 1
	then n.value
	else n.default

    let one n =
      match n.value with
      | x :: _ -> x
      | _ -> assert false

(***************************************)
(* emit                                *)
(***************************************)
    let emit n v =
      if n.status <> !instant
      then
	(n.pre_status <- n.status;
	 n.last <- n.value;
	 n.status <- !instant;
         begin match n.kind with
         | Default -> n.value <- n.combine v n.default
         | Memory -> n.value <- n.combine v n.value
         end)
      else
	n.value <- n.combine v n.value

(***************************************)
(* next                                *)
(***************************************)
    let next () = incr instant

  end

module Machine_controler_machine = Lco_ctrl_tree.Rml_interpreter(Sig_env)
