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
(* file: lk_implem.ml *)



module Rml_interpreter =
  functor (R : Runtime.CONTROL_TREE_R with type 'a step = 'a -> unit) ->
  struct

    type 'b expr = R.control_tree -> 'b R.step
    and 'a process = 'a R.step -> unit expr
    and join_point = int ref

    let dummy_step _ = ()

    open R

(* ------------------------------------------------------------------------ *)
    let rml_pre_status evt = R.Event.pre_status evt

    let rml_pre_value evt = R.Event.pre_value evt

    let rml_last evt = R.Event.last evt

    let rml_default evt = R.Event.default evt


(* ------------------------------------------------------------------------ *)
  (*  let rml_global_signal () = R.Event.new_evt R.top_clock_domain

    let rml_global_signal_combine def comb =
      R.Event.new_evt_combine R.top_clock_domain def comb *)

(* ------------------------------------------------------------------------ *)
(**************************************)
(* compute                            *)
(**************************************)

    let rml_compute_v v k _ =
      k v

    let rml_compute e k _ =
      k (e())

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause k ctrl _ =
      R.on_next_instant ctrl k

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi k ctrl _ =
      raise RML

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt _ =
      ()

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rml_halt_kboi _ =
      ()

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_v_v v1 v2 k _ =
      R.Event.emit v1 v2;
      k ()

    let rml_emit_v_e v1 e2 k _ =
      rml_emit_v_v v1 (e2()) k ()

    let rml_emit_e_v e1 v2 k _ =
      rml_emit_v_v (e1()) v2 k ()

    let rml_emit_e_e e1 e2 k _ =
      rml_emit_v_v (e1()) (e2()) k ()

    let rml_emit = rml_emit_e_e

    let rml_emit_pure_v v1 =
      rml_emit_v_v v1 ()

    let rml_emit_pure_e e1 =
      rml_emit_v_v (e1()) ()

    let rml_emit_pure = rml_emit_pure_e

    let rml_expr_emit evt v =
       R.Event.emit evt v

    let rml_expr_emit_pure evt = rml_expr_emit evt ()


(**************************************)
(* await_immediate                    *)
(**************************************)

    let rml_await_immediate_v evt k ctrl _ =
      on_event evt ctrl k ()

    let rml_await_immediate expr_evt k ctrl _ =
      let evt = expr_evt() in
      rml_await_immediate_v evt k ctrl ()

(**************************************)
(* get                                *)
(**************************************)

    let step_get evt f ctrl _ =
      let step_get_eoi _ =
        let v = if R.Event.status evt then R.Event.value evt else R.Event.default evt in
        R.on_next_instant ctrl (f v)
      in
      R.on_eoi R.top_clock_domain step_get_eoi

    let rml_get_v = step_get

    let rml_get expr_evt f ctrl _ =
      step_get (expr_evt()) f ctrl ()

(**************************************)
(* await_immediate_one                *)
(**************************************)

    let rml_await_immediate_one_v evt f ctrl _ =
      let f _ =
        let x = R.Event.one evt in
        f x ()
      in
      R.on_event evt ctrl f ()

    let rml_await_immediate_one expr_evt f ctrl _ =
      rml_await_immediate_one_v (expr_evt()) f ctrl ()


(**************************************)
(* present                            *)
(**************************************)
    let rml_present_v evt k_1 k_2 ctrl _ =
      on_event_or_next evt k_1 () R.top_clock_domain ctrl k_2

    let rml_present expr_evt k_1 k_2 ctrl _ =
      rml_present_v (expr_evt ()) k_1 k_2 ctrl ()


(**************************************)
(* await_all_match                    *)
(**************************************)
    let rml_await_all_match_v evt matching k ctrl _ =
      let await_eoi _ =
        let v = R.Event.value evt in
        if matching v then
          R.on_next_instant ctrl (k v)
        else
          raise Wait_again
      in
      R.on_event_at_eoi evt ctrl await_eoi


    let rml_await_all_match expr_evt matching k ctrl _ =
      rml_await_all_match_v (expr_evt ()) matching k ctrl ()


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p _ =
      p (R.Event.new_evt R.top_clock_domain) ()

    let rml_signal_combine_v_v default comb p _ =
      p (R.Event.new_evt_combine R.top_clock_domain default comb) ()

    let rml_signal_combine_v_e default comb p _ =
      rml_signal_combine_v_v default (comb()) p ()

    let rml_signal_combine_e_v default comb p _ =
      rml_signal_combine_v_v (default()) comb p ()

    let rml_signal_combine default comb p _ =
      rml_signal_combine_v_v (default()) (comb()) p ()


(**************************************)
(* par                                *)
(**************************************)
    let rml_split_par n f _ =
      let j = ref n in
      let k_list = f j in
      R.on_current_instant_list R.top_clock_domain k_list

    let rml_join_par j k _ =
      decr j;
      if !j <= 0 then
        k ()

(**************************************)
(* join_def                           *)
(**************************************)
    let rml_join_def j v_ref get_values k v =
      decr j;
      v_ref := v;
      if !j <= 0 then
        k (get_values())


(**************************************)
(* loop                               *)
(**************************************)
    let rml_loop p =
      let f_1 = ref dummy_step in
      let f_loop = p (fun _ -> !f_1 ()) in
      f_1 := f_loop;
      f_loop

(**************************************)
(* loop_n                             *)
(**************************************)
    let rml_loop_n_v n p k =
      let cpt = ref 0 in
      let f_1 = ref dummy_step in
      let f_loop =
        p
          (fun _ ->
            if !cpt > 0 then
              (decr cpt; !f_1 ())
            else
              k ())
        in
        f_1 := f_loop;
        fun _ ->
          if n > 0 then
            (cpt := n - 1;
             f_loop ())
          else
            k ()

    let rml_loop_n e p k _ =
      let n = e() in
      rml_loop_n_v n p k ()

(**************************************)
(* match                              *)
(**************************************)

    let rml_match_v e f _ =
      f e ()

    let rml_match e f _ =
      let k = f (e()) in
      k ()

(**************************************)
(* run                                *)
(**************************************)

    let rml_run_v p k ctrl _ =
      p k ctrl ()

    let rml_run e k ctrl _ =
      (e ()) k ctrl ()

(**************************************)
(* if                                 *)
(**************************************)

    let rml_if_v e k_1 k_2 _ =
      if e then
        k_1 ()
      else
        k_2 ()

    let rml_if e k_1 k_2 _ =
      rml_if_v (e()) k_1 k_2 ()


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p k =
      let f_body = ref dummy_step in
      let f_while _ =
        if e()
        then !f_body ()
        else k ()
      in
      f_body := p f_while;
      f_while

(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun k ->
        let rec f_for i v2 =
          fun _ ->
            incr i;
            if cmp !i v2
            then p !i (f_for i v2) ()
            else k ()
        in
        let f_for_init =
          fun _ ->
            let i = ref (e1()) in
            let v2 = e2() in
            if cmp !i v2
            then p !i (f_for i v2) ()
            else k ()
        in
        f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)

    let rml_fordopar e1 e2 dir p k _ =
      if dir then
        begin
          let min = e1() in
          let max = e2() in
          let j = ref (max - min + 1) in
          if !j <= 0 then
            k ()
          else
            begin
              for i = max downto min do
                let f = p j i in
                R.on_current_instant R.top_clock_domain f
              done
            end
        end
      else
        begin
          let max = e1() in
          let min = e2() in
          let j = ref (max - min + 1) in
          if !j <= 0 then
            k ()
          else
            begin
              for i = min to max do
                let f = p j i in
                R.on_current_instant R.top_clock_domain f
              done
            end
        end

(**************************************)
(* until                              *)
(**************************************)
    let rml_start_until_v evt p k ctrl _ =
      let new_ctrl =
        new_ctrl ~cond:(fun () -> R.Event.status evt)
          (Kill_handler (fun () -> let v = R.Event.value evt in k v))
      in
      R.start_ctrl ctrl new_ctrl;
      p new_ctrl ()

    let rml_start_until expr_evt p k ctrl _ =
      rml_start_until_v (expr_evt ()) p k ctrl ()

    let rml_end_until = R.end_ctrl

(**************************************)
(* control                            *)
(**************************************)
   let rml_start_control_v evt p ctrl _ =
      let new_ctrl = new_ctrl ~cond:(fun () -> R.Event.status evt) Susp in
      R.start_ctrl ctrl new_ctrl;
      p new_ctrl ()

   let rml_start_control expr_evt p ctrl _ =
     rml_start_control_v (expr_evt()) p ctrl ()

   let rml_end_control = R.end_ctrl


(**************************************)
(* when                               *)
(**************************************)
    let rml_start_when_v evt p ctrl _ =
      let dummy = ref (fun _ -> assert false) in
      let new_ctrl = R.new_ctrl ~cond:(fun () -> R.Event.status evt) When in
      let rec when_act _ =
        R.wake_up_ctrl new_ctrl R.top_clock_domain;
        R.on_next_instant ctrl f_when
      and f_when _ =
        R.on_event evt ctrl when_act ()
      in
      dummy := f_when;
      R.start_ctrl ctrl new_ctrl;
      R.set_suspended new_ctrl true;
      R.on_next_instant new_ctrl (p new_ctrl);
      f_when ()

    let rml_start_when expr_evt p ctrl _ =
      rml_start_when_v (expr_evt()) p ctrl ()

    let rml_end_when = R.end_ctrl



(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

    let rml_await expr_evt k ctrl _ =
      rml_await_immediate expr_evt (rml_pause k ctrl) ctrl ()

    let rml_await_v evt k ctrl _ =
      rml_await_immediate_v evt (rml_pause k ctrl) ctrl ()

    let rml_await_all expr_evt p ctrl _ =
      let evt = expr_evt () in
      rml_await_immediate_v evt (step_get evt p ctrl) ctrl ()

    let rml_await_all_v evt p ctrl _ =
      rml_await_immediate_v evt (step_get evt p ctrl) ctrl ()

    let rml_await_one expr_evt p ctrl _ =
      let pause_p x =
        rml_pause (fun () -> p x ()) ctrl
      in
      rml_await_immediate_one expr_evt pause_p ctrl ()

    let rml_await_one_v evt p ctrl _ =
      let pause_p x =
        rml_pause (fun () -> p x ()) ctrl
      in
      rml_await_immediate_one_v evt pause_p ctrl ()


(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make result p =
      p (fun x -> result := Some x) (R.control_tree R.top_clock_domain)


    exception RML
    module R = R
  end

module Lk_seq_interpreter =
  Rml_interpreter(Seq_runtime.SeqRuntime)
