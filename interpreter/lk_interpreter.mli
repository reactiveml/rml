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
(* created: 2005-08-31  *)
(* file: lk_interpreter.mli *)


module type S =
   functor (R : Runtime.R) ->
    sig
      type event_cfg
      and 'a process = 'a R.step -> R.control_tree -> unit R.step
      and join_point

      val rml_make: 'a option ref -> 'a process -> unit R.step

      val rml_global_signal: unit -> ('a, 'a list) R.event
      val rml_global_signal_combine: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) R.event
      val rml_expr_emit_pure: (unit, 'b) R.event -> unit
      val rml_expr_emit: ('a, 'b) R.event -> 'a -> unit
      val rml_pre_status: ('a, 'b) R.event -> bool
      val rml_pre_value: ('a, 'b) R.event -> 'b
      val rml_last: ('a, 'b) R.event -> 'b
      val rml_default: ('a, 'b) R.event -> 'b

      val rml_compute: (unit -> 'a) -> 'a R.step -> 'b R.step
      val rml_compute_v: 'a -> 'a R.step -> 'b R.step

      val rml_pause: unit R.step -> R.control_tree -> 'a R.step
      val rml_halt: 'a R.step

      val rml_pause_kboi: unit R.step -> R.control_tree -> 'a R.step
      val rml_halt_kboi: 'a R.step

      val rml_emit:
        (unit -> ('a, 'b) R.event) -> (unit -> 'a) -> unit R.step -> 'c R.step
      val rml_emit_v_e:
        ('a, 'b) R.event -> (unit -> 'a) -> unit R.step -> 'c R.step
      val rml_emit_e_v:
        (unit -> ('a, 'b) R.event) -> 'a -> unit R.step -> 'c R.step
      val rml_emit_v_v:
        ('a, 'b) R.event -> 'a -> unit R.step -> 'c R.step

      val rml_emit_pure:
        (unit -> (unit, 'a) R.event) -> unit R.step -> 'b R.step
      val rml_emit_pure_v:
        (unit, 'a) R.event -> unit R.step -> 'b R.step


      val rml_await_immediate:
        (unit -> ('a, 'b) R.event) -> unit R.step -> R.control_tree -> 'c R.step
      val rml_await_immediate_v:
        ('a, 'b) R.event -> unit R.step -> R.control_tree -> 'c R.step

      val rml_await:
        (unit -> ('a, 'b) R.event) -> unit R.step -> R.control_tree -> 'c R.step
      val rml_await_v:
        ('a, 'b) R.event -> unit R.step -> R.control_tree -> 'c R.step

      val rml_get:
        (unit -> ('a, 'b) R.event) -> ('b -> unit R.step) -> R.control_tree ->
          'c R.step
      val rml_get_v:
        ('a, 'b) R.event -> ('b -> unit R.step) -> R.control_tree ->
          'c R.step

      val rml_await_immediate_one:
        (unit -> ('a, 'a list) R.event) -> ('a -> unit R.step) -> R.control_tree ->
          'b R.step
      val rml_await_immediate_one_v:
        ('a, 'a list) R.event -> ('a -> unit R.step) -> R.control_tree ->
          'b R.step

      val rml_await_one:
        (unit -> ('a, 'a list) R.event) -> ('a -> unit R.step) -> R.control_tree ->
          'b R.step
      val rml_await_one_v:
        ('a, 'a list) R.event -> ('a -> unit R.step) -> R.control_tree ->
          'b R.step

      val rml_await_all:
        (unit -> ('a, 'b) R.event) -> ('b -> unit R.step) -> R.control_tree ->
          'c R.step
      val rml_await_all_v:
        ('a, 'b) R.event -> ('b -> unit R.step) -> R.control_tree ->
          'c R.step

      val rml_await_all_match:
        (unit -> ('a, 'b) R.event) -> ('b -> bool) -> ('b -> unit R.step) ->
          R.control_tree -> 'c R.step
      val rml_await_all_match_v:
        ('a, 'b) R.event -> ('b -> bool) -> ('b -> unit R.step) ->
          R.control_tree -> 'c R.step

      val rml_present:
          R.control_tree -> (unit -> ('a, 'b) R.event) ->
            unit R.step -> unit R.step -> 'c R.step
      val rml_present_v:
          R.control_tree -> ('a, 'b) R.event ->
            unit R.step -> unit R.step -> 'c R.step

      val rml_signal: (('a, 'a list) R.event -> unit R.step) -> 'b R.step

      val rml_signal_combine:
          (unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
            (('a, 'b) R.event -> unit R.step) -> 'c R.step
      val rml_signal_combine_v_e:
          'b -> (unit -> ('a -> 'b -> 'b)) ->
            (('a, 'b) R.event -> unit R.step) -> 'c R.step
      val rml_signal_combine_e_v:
          (unit -> 'b) -> ('a -> 'b -> 'b) ->
            (('a, 'b) R.event -> unit R.step) -> 'c R.step
      val rml_signal_combine_v_v:
          'b -> ('a -> 'b -> 'b) ->
            (('a, 'b) R.event -> unit R.step) -> 'c R.step

      val rml_split_par:
          int -> (join_point -> (unit R.step) list) -> 'a R.step

      val rml_join_par:
          join_point -> unit R.step -> 'a R.step

      val rml_join_def:
          join_point -> 'a ref -> (unit -> 'b) -> 'b R.step -> 'a R.step

      val rml_loop: ('a R.step -> unit R.step) ->  unit R.step

      val rml_loop_n:
          (unit -> int) -> ('a R.step -> unit R.step) -> unit R.step ->  unit R.step
      val rml_loop_n_v:
          int -> ('a R.step -> unit R.step) -> unit R.step ->  unit R.step

      val rml_match: (unit -> 'a) -> ('a -> unit R.step) -> 'b R.step
      val rml_match_v: 'a -> ('a -> unit R.step) -> 'b R.step

      val rml_run: (unit -> 'a process) -> 'a R.step -> R.control_tree -> 'b R.step
      val rml_run_v: 'a process -> 'a R.step -> R.control_tree -> 'b R.step

      val rml_if: (unit -> bool) -> unit R.step -> unit R.step -> 'a R.step
      val rml_if_v: bool -> unit R.step -> unit R.step -> 'a R.step

      val rml_while:
          (unit -> bool) -> ('a R.step -> unit R.step) -> unit R.step -> 'b R.step

      val rml_for:
          (unit -> int) -> (unit -> int) -> bool ->
            (int -> 'a R.step -> unit R.step) -> unit R.step -> 'b R.step

      val rml_fordopar:
          (unit -> int) -> (unit -> int) -> bool ->
            (join_point -> int -> unit R.step) -> unit R.step -> 'b R.step

      val rml_start_until:
          R.control_tree -> (unit -> ('a, 'b) R.event) ->
            (R.control_tree -> unit R.step) -> ('b -> unit R.step) -> 'c R.step
      val rml_start_until_v:
          R.control_tree -> ('a, 'b) R.event ->
            (R.control_tree -> unit R.step) -> ('b -> unit R.step) -> 'c R.step
      val rml_end_until:
          R.control_tree -> 'a R.step -> 'a R.step

      val rml_start_control:
          R.control_tree -> (unit -> ('a, 'b) R.event) ->
            (R.control_tree -> unit R.step) -> 'c R.step
      val rml_start_control_v:
          R.control_tree -> ('a, 'b) R.event ->
            (R.control_tree -> unit R.step) -> 'c R.step
      val rml_end_control:
          R.control_tree -> 'a R.step -> 'a R.step

      val rml_start_when:
          R.control_tree -> (unit -> ('a, 'b) R.event) ->
            (R.control_tree -> unit R.step) -> 'c R.step
      val rml_start_when_v:
          R.control_tree -> ('a, 'b) R.event ->
            (R.control_tree -> unit R.step) -> 'c R.step
      val rml_end_when:
          R.control_tree -> 'a R.step -> 'a R.step
    end
