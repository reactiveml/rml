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
  functor (Event : Sig_env.S) ->
    sig

      exception RML

      type ('a, 'b) event
      (* and event_cfg *)
      and 'a step = 'a -> unit
      and control_tree
      and 'a process = 'a step -> control_tree -> unit step
      and join_point

      val rml_global_signal: unit -> ('a, 'a list) event
      val rml_global_signal_combine: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event
      val rml_global_signal_memory_combine:
          'b -> ('a -> 'b -> 'b) -> ('a, 'b) event
      val rml_expr_emit_pure: (unit, 'b) event -> unit
      val rml_expr_emit: ('a, 'b) event -> 'a -> unit
      val rml_pre_status: ('a, 'b) event -> bool
      val rml_pre_value: ('a, 'b) event -> 'b
      val rml_last: ('a, 'b) event -> 'b
      val rml_default: ('a, 'b) event -> 'b

      val rml_compute: (unit -> 'a) -> 'a step -> 'b step
      val rml_compute_v: 'a -> 'a step -> 'b step

      val rml_pause: unit step -> control_tree -> 'a step
      val rml_halt: 'a step

      val rml_pause_kboi: unit step -> control_tree -> 'a step
      val rml_halt_kboi: 'a step

      val rml_emit:
        (unit -> ('a, 'b) event) -> (unit -> 'a) -> unit step -> 'c step
      val rml_emit_v_e:
        ('a, 'b) event -> (unit -> 'a) -> unit step -> 'c step
      val rml_emit_e_v:
        (unit -> ('a, 'b) event) -> 'a -> unit step -> 'c step
      val rml_emit_v_v:
        ('a, 'b) event -> 'a -> unit step -> 'c step

      val rml_emit_pure:
        (unit -> (unit, 'a) event) -> unit step -> 'b step
      val rml_emit_pure_v:
        (unit, 'a) event -> unit step -> 'b step


      val rml_await_immediate:
        (unit -> ('a, 'b) event) -> unit step -> control_tree -> 'c step
      val rml_await_immediate_v:
        ('a, 'b) event -> unit step -> control_tree -> 'c step

      val rml_await:
        (unit -> ('a, 'b) event) -> unit step -> control_tree -> 'c step
      val rml_await_v:
        ('a, 'b) event -> unit step -> control_tree -> 'c step

      val rml_get:
        (unit -> ('a, 'b) event) -> ('b -> unit step) -> control_tree ->
	  'c step
      val rml_get_v:
        ('a, 'b) event -> ('b -> unit step) -> control_tree ->
	  'c step

      val rml_await_immediate_one:
        (unit -> ('a, 'a list) event) -> ('a -> unit step) -> control_tree ->
	  'b step
      val rml_await_immediate_one_v:
        ('a, 'a list) event -> ('a -> unit step) -> control_tree ->
	  'b step

      val rml_await_one:
        (unit -> ('a, 'a list) event) -> ('a -> unit step) -> control_tree ->
	  'b step
      val rml_await_one_v:
        ('a, 'a list) event -> ('a -> unit step) -> control_tree ->
	  'b step

      val rml_await_all:
        (unit -> ('a, 'b) event) -> ('b -> unit step) -> control_tree ->
	  'c step
      val rml_await_all_v:
        ('a, 'b) event -> ('b -> unit step) -> control_tree ->
	  'c step

      val rml_await_all_match:
        (unit -> ('a, 'b) event) -> ('b -> bool) -> ('b -> unit step) ->
	  control_tree -> 'c step
      val rml_await_all_match_v:
        ('a, 'b) event -> ('b -> bool) -> ('b -> unit step) ->
	  control_tree -> 'c step

      val rml_await_one_match:
        (unit -> ('a, 'b list) event) -> ('b -> bool) -> ('b -> unit step) ->
	  control_tree -> 'c step
      val rml_await_one_match_v:
        ('a, 'b list) event -> ('b -> bool) -> ('b -> unit step) ->
	  control_tree -> 'c step

      val rml_present:
	  control_tree -> (unit -> ('a, 'b) event) ->
	    unit step -> unit step -> 'c step
      val rml_present_v:
	  control_tree -> ('a, 'b) event ->
	    unit step -> unit step -> 'c step

      val rml_signal: (('a, 'a list) event -> unit step) -> 'b step

      val rml_signal_combine:
	  (unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_memory_combine:
	  (unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_combine_v_e:
	  'b -> (unit -> ('a -> 'b -> 'b)) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_memory_combine_v_e:
	  'b -> (unit -> ('a -> 'b -> 'b)) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_combine_e_v:
	  (unit -> 'b) -> ('a -> 'b -> 'b) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_memory_combine_e_v:
	  (unit -> 'b) -> ('a -> 'b -> 'b) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_combine_v_v:
	  'b -> ('a -> 'b -> 'b) ->
	    (('a, 'b) event -> unit step) -> 'c step
      val rml_signal_memory_combine_v_v:
	  'b -> ('a -> 'b -> 'b) ->
	    (('a, 'b) event -> unit step) -> 'c step

      val rml_split_par:
	  int -> (join_point -> (unit step) list) -> 'a step

      val rml_join_par:
	  join_point -> unit step -> 'a step

      val rml_join_def:
	  join_point -> 'a ref -> (unit -> 'b) -> 'b step -> 'a step

      val rml_loop: ('a step -> unit step) ->  unit step

      val rml_loop_n:
	  (unit -> int) -> ('a step -> unit step) -> unit step ->  unit step
      val rml_loop_n_v:
	  int -> ('a step -> unit step) -> unit step ->  unit step

      val rml_match: (unit -> 'a) -> ('a -> unit step) -> 'b step
      val rml_match_v: 'a -> ('a -> unit step) -> 'b step

      val rml_run: (unit -> 'a process) -> 'a step -> control_tree -> 'b step
      val rml_run_v: 'a process -> 'a step -> control_tree -> 'b step

      val rml_if: (unit -> bool) -> unit step -> unit step -> 'a step
      val rml_if_v: bool -> unit step -> unit step -> 'a step

      val rml_while:
	  (unit -> bool) -> ('a step -> unit step) -> unit step -> 'b step

      val rml_for:
	  (unit -> int) -> (unit -> int) -> bool ->
	    (int -> 'a step -> unit step) -> unit step -> 'b step

      val rml_fordopar:
	  (unit -> int) -> (unit -> int) -> bool ->
	    (join_point -> int -> unit step) -> unit step -> 'b step

      val rml_start_until:
	  control_tree -> (unit -> ('a, 'b) event) ->
	    (control_tree -> unit step) -> ('b -> unit step) -> 'c step
      val rml_start_until_v:
	  control_tree -> ('a, 'b) event ->
	    (control_tree -> unit step) -> ('b -> unit step) -> 'c step
      val rml_end_until:
	  control_tree -> 'a step -> 'a step

      val rml_start_control:
	  control_tree -> (unit -> ('a, 'b) event) ->
	    (control_tree -> unit step) -> 'c step
      val rml_start_control_v:
	  control_tree -> ('a, 'b) event ->
	    (control_tree -> unit step) -> 'c step
      val rml_end_control:
	  control_tree -> 'a step -> 'a step

      val rml_start_when:
	  control_tree -> (unit -> ('a, 'b) event) ->
	    (control_tree -> unit step) -> 'c step
      val rml_start_when_v:
	  control_tree -> ('a, 'b) event ->
	    (control_tree -> unit step) -> 'c step
      val rml_end_when:
	  control_tree -> 'a step -> 'a step

      val rml_make: 'a process -> (unit -> 'a option)
    end
