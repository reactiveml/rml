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

module Event

type clock = int ref
type ('a, 'b, 'cd) t =
    { clock : clock;
        clock_domain : 'cd;
        mutable status: int;
        mutable value: 'b;
        mutable pre_status: int;
        mutable last: 'b;
        mutable _default: 'b;
        combine: ('a -> 'b -> 'b); }

let absent = -2

let create cd ck _default combine =
    { clock = ck;
    clock_domain = cd;
    status = absent;
    value = _default;
    pre_status = absent;
    last = _default;
    _default = _default;
    combine = combine; }

(* -------------------------- Access functions -------------------------- *)
let _default n = n._default
let status n = n.status = !(n.clock)

let value n = n.value

let pre_status n =
    if n.status = !(n.clock)
    then n.pre_status = !(n.clock) - 1
    else n.status = !(n.clock) - 1

let last n =
    if n.status = !(n.clock)
    then n.last
    else n.value

let pre_value n =
    if n.status = !(n.clock) then
      if n.pre_status = !(n.clock) - 1
      then n.last
      else n._default
    else
      if n.status = !(n.clock) - 1
      then n.value
      else n._default

let one n =
    match n.value with
    | x :: _ -> x
    | _ -> assert false

let emit n v =
    if n.status <> !(n.clock) then
     (n.pre_status <- n.status;
      n.last <- n.value;
      n.status <- !(n.clock);
      n.value <- n.combine v n._default)
    else
      n.value <- n.combine v n.value

let clock_domain n =
    n.clock_domain

let init_clock () =
    ref 0

let next ck =
    incr ck
