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

(* file: lco_interpreter.mli *)
(* author: Louis Mandel *)
(* created: 2004-06-04  *)
open Runtime

module type LCO_RUNTIME =
  sig
    type clock
    type region
    type clock_domain
    type ('a, 'b) event
    type event_cfg

    val react : clock_domain -> unit
    val on_current_instant : clock_domain -> unit step -> unit
  end

module type S =
  sig
    module R : LCO_RUNTIME

    type 'a expr
    and 'a process
    and clock_expr = R.clock Rml_types.clock
    and region_expr = clock_expr
    type 'a memory

    val rml_make: R.clock_domain -> 'a option ref -> 'a process -> unit step
    val rml_make_n: R.clock_domain -> 'a option ref -> 'a process list -> unit step list

    val rml_pre_status: ('a, 'b) R.event -> bool
    val rml_pre_value: ('a, 'b) R.event -> 'b
    val rml_last: ('a, 'b) R.event -> 'b
    val rml_default: ('a, 'b) R.event -> 'b
    val rml_clock : ('a, 'b) R.event -> clock_expr

    val rml_expr_emit: (unit, 'b) R.event -> unit
    val rml_expr_emit_val: ('a, 'b) R.event -> 'a -> unit

    val rml_global_signal: Rml_types.signal_kind -> ('a, 'a list) R.event
    val rml_global_signal_combine: Rml_types.signal_kind -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) R.event

    type event_cfg_gen = unit -> R.event_cfg
    val cfg_present': ('a,'b) R.event -> event_cfg_gen
    val cfg_present: (unit -> ('a,'b) R.event) -> event_cfg_gen
    val cfg_and: event_cfg_gen -> event_cfg_gen -> event_cfg_gen
    val cfg_or: event_cfg_gen -> event_cfg_gen -> event_cfg_gen

    val rml_nothing: unit expr
    val rml_compute: (unit -> 'a) -> 'a expr
    val rml_pause : (unit -> clock_expr) -> unit expr
    val rml_pause' : clock_expr -> unit expr
    val rml_weak_pause : (unit -> clock_expr) -> unit expr
    val rml_weak_pause' : clock_expr -> unit expr
    val rml_halt: 'a expr
    val rml_emit': (unit, 'b) R.event -> unit expr
    val rml_emit: (unit -> (unit, 'b) R.event) -> unit expr
    val rml_emit_val': ('a, 'b) R.event -> (unit -> 'a) -> unit expr
    val rml_emit_val: (unit -> ('a, 'b) R.event) -> (unit -> 'a) -> unit expr
    val rml_get: (unit -> ('a, 'b) R.event) -> ('b -> 'c expr) -> 'c expr
    val rml_await': ('a, 'b) R.event -> unit expr
    val rml_await: (unit -> ('a, 'b) R.event) -> unit expr
    val rml_await_conf: event_cfg_gen -> unit expr
    val rml_await_immediate': ('a, 'b) R.event -> unit expr
    val rml_await_immediate: (unit -> ('a, 'b) R.event) -> unit expr
    val rml_await_immediate_conf: event_cfg_gen -> unit expr
    val rml_await_all': ('a, 'b) R.event -> ('b -> 'c expr) -> 'c expr
    val rml_await_all: (unit -> ('a, 'b) R.event) -> ('b -> 'c expr) -> 'c expr
    val rml_await_all_match':
        ('a, 'b) R.event -> ('b -> bool) -> ('b -> 'c expr) -> 'c expr
    val rml_await_all_match:
        (unit -> ('a, 'b) R.event) -> ('b -> bool) -> ('b -> 'c expr) -> 'c expr
    val rml_await_one':
        ('a , 'a list) R.event -> ('a -> 'c expr) -> 'c expr
    val rml_await_one:
        (unit -> ('a , 'a list) R.event) -> ('a -> 'c expr) -> 'c expr
    val rml_await_immediate_one':
        ('a , 'a list) R.event -> ('a -> 'c expr) -> 'c expr
    val rml_await_immediate_one:
        (unit -> ('a , 'a list) R.event) -> ('a -> 'c expr) -> 'c expr
    val rml_present': ('a, 'b) R.event -> 'c expr -> 'c expr -> 'c expr
    val rml_present: (unit -> ('a, 'b) R.event) -> 'c expr -> 'c expr -> 'c expr
    val rml_present_conf: event_cfg_gen -> 'a expr -> 'a expr -> 'a expr
    val rml_seq: 'a expr -> 'b expr -> 'b expr
    val rml_par: 'a expr -> 'b expr -> unit expr
    val rml_loop: 'a expr -> unit expr
    val rml_loop_n: (unit -> int) -> 'a expr -> unit expr
    val rml_while: (unit -> bool) -> 'a expr -> unit expr
    val rml_for:
        (unit -> int) -> (unit -> int) -> bool -> (int -> 'a expr) ->
          unit expr
    val rml_fordopar:
        (unit -> int) -> (unit -> int) -> bool -> (int -> 'a expr) ->
          unit expr
    val rml_signal:
      Rml_types.signal_kind -> clock_expr -> region_expr ->
      clock_expr option -> (('a, 'a list) R.event -> 'b expr) -> 'b expr
    val rml_signal_combine:
      Rml_types.signal_kind -> clock_expr -> region_expr ->
      (unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
      clock_expr option -> (('a, 'b) R.event -> 'c expr) -> 'c expr
    val rml_def: (unit -> 'a) -> ('a -> 'b expr) -> 'b expr
    val rml_def_dyn: 'a expr -> ('a -> 'b expr) -> 'b expr
(*    val rml_def_and_dyn: expr array -> (value array -> expr) -> expr *)
    val rml_match: (unit -> 'a) -> ('a -> 'b expr) -> 'b expr
    val rml_run: (unit -> 'a process) -> 'a expr
    val rml_until': Rml_types.pause_kind -> ('a, 'b) R.event -> unit expr -> unit expr
    val rml_until: Rml_types.pause_kind -> (unit -> ('a, 'b) R.event) -> unit expr -> unit expr
    val rml_until_conf: Rml_types.pause_kind -> event_cfg_gen -> unit expr -> unit expr
    val rml_until_handler':
        Rml_types.pause_kind -> ('a, 'b) R.event -> 'c expr -> ('b -> 'c expr) -> 'c expr
    val rml_until_handler:
        Rml_types.pause_kind -> (unit -> ('a, 'b) R.event) -> 'c expr -> ('b -> 'c expr) -> 'c expr
    val rml_until_handler_match':
        Rml_types.pause_kind -> ('a, 'b) R.event -> ('b -> bool) -> 'c expr -> ('b -> 'c expr) -> 'c expr
    val rml_until_handler_match:
        Rml_types.pause_kind -> (unit -> ('a, 'b) R.event) -> ('b -> bool) ->
          'c expr -> ('b -> 'c expr) -> 'c expr
    val rml_control': ('a, 'b) R.event -> 'c expr -> 'c expr
    val rml_control: (unit -> ('a, 'b) R.event) -> 'c expr -> 'c expr
    val rml_control_match': ('a, 'b) R.event -> ('b -> bool) -> 'c expr -> 'c expr
    val rml_control_match:
        (unit -> ('a, 'b) R.event) -> ('b -> bool) -> 'c expr -> 'c expr
    val rml_control_conf: event_cfg_gen -> 'c expr -> 'c expr
    val rml_when': ('a, 'b) R.event -> 'c expr -> 'c expr
    val rml_when: (unit -> ('a, 'b) R.event) -> 'c expr -> 'c expr
    val rml_when_conf: event_cfg_gen -> 'c expr -> 'c expr
    val rml_if: (unit -> bool) -> 'a expr -> 'a expr -> 'a expr

    val rml_par_n : unit expr list -> unit expr
(*
    val rml_seq_n : expr list -> expr
*)

    val rml_newclock : (int -> int* int) option -> int option -> (clock_expr -> unit expr) -> unit expr

    val rml_top_clock : clock_expr
    val rml_base_clock : clock_expr
  end



