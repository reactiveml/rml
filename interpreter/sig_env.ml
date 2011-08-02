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
(* created: 2005-08-28  *)
(* file: sig_env.ml *)

module type S =
  sig
    type clock
    type ('a, 'b, 'cd) t

    val create: 'cd -> clock -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b, 'cd) t
    val status: ('a, 'b, _) t ->bool
    val value: ('a, 'b, _) t -> 'b
    val pre_status: ('a, 'b, _) t -> bool
    val pre_value: ('a, 'b, _) t -> 'b
    val last: ('a, 'b, _) t -> 'b
    val default: ('a, 'b, _) t -> 'b
    val one: ('a, 'a list, _) t -> 'a

    val emit: ('a, 'b, _) t -> 'a -> unit

    val clock_domain : ('a, 'b, 'cd) t -> 'cd
    val init_clock : unit -> clock
    val next: clock -> unit
  end

module Record  (*: S*)  =
  struct
    type clock = int ref
    type ('a, 'b, 'cd) t =
        { clock : clock;
          clock_domain : 'cd;
          mutable status: int;
          mutable value: 'b;
          mutable pre_status: int;
          mutable last: 'b;
          mutable default: 'b;
          combine: ('a -> 'b -> 'b); }

    let absent = -2

    let create cd ck default combine =
      { clock = ck;
        clock_domain = cd;
        status = absent;
        value = default;
        pre_status = absent;
        last = default;
        default = default;
        combine = combine; }

(* -------------------------- Access functions -------------------------- *)
    let default n = n.default
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
      if n.status = !(n.clock)
      then
        if n.pre_status = !(n.clock) - 1
        then n.last
        else n.default
      else
        if n.status = !(n.clock) - 1
        then n.value
        else n.default

    let one n =
      match n.value with
      | x :: _ -> x
      | _ -> assert false

    let emit n v =
      if n.status <> !(n.clock)
      then
        (n.pre_status <- n.status;
         n.last <- n.value;
         n.status <- !(n.clock);
         n.value <- n.combine v n.default)
      else
        n.value <- n.combine v n.value

    let clock_domain n =
      n.clock_domain

    let init_clock () =
      ref 0

    let next ck =
      incr ck
  end