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

(*
module type CLOCK_INDEX = sig
  type t

  val get : t -> int
  val init_clock : unit -> clock_index
  val next: clock_index -> unit
end

*)

open Rml_types

module type S =
  sig
    type clock
    type clock_index
    type ('a, 'b) t

    (* create ck kind default combine *)
    val create: clock -> signal_kind -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) t
    val status: ('a, 'b) t -> bool
    val value: ('a, 'b) t -> 'b
    val pre_status: ('a, 'b) t -> bool
    val pre_value: ('a, 'b) t -> 'b
    val last: ('a, 'b) t -> 'b
    val default: ('a, 'b) t -> 'b
    val one: ('a, 'a list) t -> 'a

    val emit: ('a, 'b) t -> 'a -> unit
    val reset: ('a, 'b) t -> unit

    val set_value : ('a, 'b) t -> 'b -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t -> unit
    val set_clock : ('a, 'b) t -> clock -> unit

    val init_clock : unit -> clock
    val next: clock -> unit
    val get : clock -> clock_index
    val set : clock -> clock_index -> unit
    val equal : clock_index -> clock_index -> bool
    val print_clock_index : Format.formatter -> clock_index -> unit
   (* val remote_emit :  ('a, 'b) t -> 'a -> unit *)
  end

module Record : S  =
  struct
    type clock = int ref
    type clock_index = int
    type ('a, 'b) t =
        { mutable clock : clock;
          kind : signal_kind;
          mutable status: int;
          mutable reset : bool;
          mutable value: 'b;
          mutable pre_status: int;
          mutable last: 'b;
          mutable default: 'b;
          combine: ('a -> 'b -> 'b); }

    let absent = -2

    let create ck kind default combine =
      { clock = ck;
        kind = kind;
        status = absent;
        reset = false;
        value = default;
        pre_status = absent;
        last = default;
        default = default;
        combine = combine; }

(* -------------------------- Access functions -------------------------- *)
    let default n = n.default
    let status n =
      n.status = !(n.clock)

    let value n = n.value

    let pre_status n =
      if n.status = !(n.clock)
      then n.pre_status = !(n.clock) - 1
      else n.status = !(n.clock) - 1

    let last n =
      if n.reset
      then n.default
      else (
        if n.status = !(n.clock)
        then n.last
        else n.value
      )

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
         n.last <- if n.reset then n.default else n.value;
         n.status <- !(n.clock);
         n.reset <- false;
         n.value <- if n.kind = Memory then n.combine v n.last else n.combine v n.default)
      else
        n.value <- n.combine v n.value

    let set_value n v =
      if n.status <> !(n.clock)
      then
        (n.pre_status <- n.status;
         n.last <- if n.reset then n.default else n.value;
         n.status <- !(n.clock);
         n.reset <- false;
         n.value <- v)
      else
        n.value <- v

    let copy n new_n =
      n.status <- new_n.status;
      n.value <- new_n.value;
      n.pre_status <- new_n.pre_status;
      n.last <- new_n.last

    let reset n =
      n.reset <- true

    let init_clock () =
      ref 0

    let set_clock n ck =
      n.clock <- ck

    let next ck =
      incr ck

    let get ck = !ck
    let set ck v = ck := v

    let equal ck1 ck2 =
      ck1 = ck2

    let print_clock_index ff c =
      Format.fprintf ff "%d" c
  end

(*
module DistributedRecord  (*: S*)  =
  struct
    type clock_index = int ref
    type ('a, 'b) t =
        { clock : clock_index;
          mutex : Mutex.t;
          mutable status: int;
          mutable value: 'b;
          mutable pre_status: int;
          mutable last: 'b;
          mutable default: 'b;
          combine: ('a -> 'b -> 'b); }

    let absent = -2

    let create ck default combine =
      { clock = ck;
        status = absent;
        mutex = Mutex.create ();
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
         Mutex.lock n.lock;
         n.value <- n.combine v n.default;
         Mutex.unlock n.lock;)
      else (
        Mutex.lock n.lock;
        n.value <- n.combine v n.value;
        Mutex.unlock n.lock
      )

    let remote_emit = emit

    let init_clock () =
      ref 0

    let next ck =
      incr ck
  end
  *)
