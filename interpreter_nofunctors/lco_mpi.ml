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
(* created: 2009-03-03  *)
(* file: lco_ctrl_tree_n *)

(* Remark: taken from                                         *)
(*            interpreter_without_scope_extrusion_control.ml  *)

(* Description :                                                      *)
(*   On a une liste next associee a chaque noeud de l'arbre de        *)
(*   control.                                                         *)
(*   Marche avec Scope Extrusion                                      *)
(*   Ajout des valeurs pour traiter def_dyn et def_and_dyn            *)
(*   Ajout de until_match et await_match                              *)
(*   Ajout des configurations evenementielles                         *)
(*   Parametrisation par le foncteur "Event"                          *)
(*   Suppression du type "value" et des "Obj.magic"                   *)
(*   Suppression de exec                                              *)
(*   Gestion du parallele n-aire                                      *)

open Runtime_options

    type join_point = int ref option
    and 'a expr = 'a Distributed_runtime_nofunct.step -> Distributed_runtime_nofunct.control_tree -> join_point -> Distributed_runtime_nofunct.clock_domain -> unit Distributed_runtime_nofunct.step
    and 'a process = unit -> 'a expr

    type ('a, 'b) event = ('a, 'b) Distributed_runtime_nofunct.event
    type event_cfg_gen = unit -> Distributed_runtime_nofunct.event_cfg
    type clock_expr = Distributed_runtime_nofunct.clock Types.clock
    type region_expr = clock_expr

    let unit_value = ()
    let dummy_step _ = ()

    open Distributed_runtime_nofunct
    open Types

    let eval_clock_expr current_cd ce = match ce with
      | CkLocal -> Distributed_runtime_nofunct.clock current_cd
      | CkTop -> Distributed_runtime_nofunct.top_clock ()
      | CkExpr e -> e

    let ensure_clock_expr ce = match ce with
      | CkExpr e -> e
      | CkTop -> Distributed_runtime_nofunct.top_clock ()
      | CkLocal -> (* should be rejected by compiler *)
          print_debug "Error: Unexpected local clock@.";
          raise Types.RML

(* ------------------------------------------------------------------------ *)
    let rml_pre_status evt = Distributed_runtime_nofunct.Event.pre_status evt

    let rml_pre_value evt = Distributed_runtime_nofunct.Event.pre_value evt

    let rml_last evt = Distributed_runtime_nofunct.Event.last evt

    let rml_default evt = Distributed_runtime_nofunct.Event.default evt

(* ------------------------------------------------------------------------ *)

    let rml_global_signal ce re =
      let ck = ensure_clock_expr ce in
      let r = Distributed_runtime_nofunct.Event.region_of_clock (ensure_clock_expr re) in
      Distributed_runtime_nofunct.Event.new_evt ck r

    let rml_global_signal_combine ce re default combine =
      let ck = ensure_clock_expr ce in
      let r = Distributed_runtime_nofunct.Event.region_of_clock (ensure_clock_expr re) in
      Distributed_runtime_nofunct.Event.new_evt_combine ck r default combine

(* ------------------------------------------------------------------------ *)

    let cfg_present' evt _ =
      Distributed_runtime_nofunct.Event.cfg_present evt
    let cfg_present expr_evt _ =
      let evt = expr_evt () in
      Distributed_runtime_nofunct.Event.cfg_present evt
    let cfg_and ev1 ev2 _ =
      Distributed_runtime_nofunct.Event.cfg_and (ev1 ()) (ev2 ())
    let cfg_or ev1 ev2 _ =
      Distributed_runtime_nofunct.Event.cfg_or (ev1 ()) (ev2 ())

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k ctrl jp cd _ ->
        f_k unit_value

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k ctrl jp cd _ ->
        let v = e () in
        f_k v

(**************************************)
(* pause                              *)
(**************************************)

    let rml_pause_at' pause_ce =
      fun f_k ctrl jp cd _ ->
        let pause_ck = eval_clock_expr cd pause_ce in
        Distributed_runtime_nofunct.on_eoi pause_ck (fun () -> Distributed_runtime_nofunct.on_next_instant ctrl f_k)

    let rml_pause_at e =
      fun f_k ctrl jp cd _ ->
        rml_pause_at' (e ()) f_k ctrl jp cd unit_value

    let rml_pause =
      fun f_k ctrl jp cd _ ->
        Distributed_runtime_nofunct.on_next_instant ctrl f_k

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k ctrl jp cd _ ->
        ()

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' evt e =
      fun f_k ctrl jp cd _ ->
        Distributed_runtime_nofunct.Event.emit evt (e());
        f_k unit_value

    let rml_emit_val expr_evt e =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_emit_val' evt e f_k ctrl jp cd unit_value

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val evt v =
      Distributed_runtime_nofunct.Event.emit evt v

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)

    let rml_await_immediate' evt =
      fun f_k ctrl jp cd _ ->
        Distributed_runtime_nofunct.on_event evt ctrl f_k unit_value

    let rml_await_immediate expr_evt =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_await_immediate' evt f_k ctrl jp cd unit_value

(**************************************)
(* await_immediate_conf               *)
(**************************************)

    let rml_await_immediate_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        Distributed_runtime_nofunct.on_event_cfg (expr_cfg ()) ctrl f_k unit_value

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' evt p =
      fun f_k ctrl jp cd _ ->
        let f_get _ =
          let x = if Distributed_runtime_nofunct.Event.status evt then Distributed_runtime_nofunct.Event.value evt else Distributed_runtime_nofunct.Event.default evt in
          let f_body = p x f_k ctrl jp cd in
          Distributed_runtime_nofunct.on_next_instant ctrl f_body
        in
        Distributed_runtime_nofunct.on_eoi (Distributed_runtime_nofunct.Event.clock evt) f_get

    let rml_get expr_evt p =
      let evt = expr_evt() in
      rml_get' evt p

(**************************************)
(* await_immediate_one                *)
(**************************************)

    let rml_await_immediate_one' evt p =
      fun f_k ctrl jp cd _ ->
        let f _ =
          let x = Distributed_runtime_nofunct.Event.one evt in
          p x f_k ctrl jp cd unit_value
        in
        Distributed_runtime_nofunct.on_event evt ctrl f unit_value

     let rml_await_immediate_one expr_evt p =
       fun f_k ctrl jp cd _ ->
         let evt = expr_evt() in
         rml_await_immediate_one' evt p f_k ctrl jp cd unit_value

(**************************************)
(* await_all_match                    *)
(**************************************)

     let rml_await_all_match' evt matching p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = Distributed_runtime_nofunct.Event.value evt in
           if matching v then
             Distributed_runtime_nofunct.on_next_instant ctrl (p v f_k ctrl jp cd)
           else
             raise Wait_again
         in
         Distributed_runtime_nofunct.on_event_at_eoi evt ctrl await_eoi

    let rml_await_all_match expr_evt matching p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_await_all_match' evt matching p f_k ctrl jp cd unit_value

(**************************************)
(* await                              *)
(**************************************)

    let rml_await' evt =
      fun f_k ctrl jp cd _ ->
        let await_eoi _ = Distributed_runtime_nofunct.on_next_instant ctrl f_k in
        Distributed_runtime_nofunct.on_event_at_eoi evt ctrl await_eoi

    let rml_await expr_evt =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_await' evt f_k ctrl jp cd unit_value

    let rml_await_all' evt p =
      rml_await_all_match' evt (fun _ -> true) p

    let rml_await_all expr_evt p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_await_all_match' evt (fun _ -> true) p f_k ctrl jp cd unit_value

    let rml_await_one' evt p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = Distributed_runtime_nofunct.Event.one evt in
           Distributed_runtime_nofunct.on_next_instant ctrl (p v f_k ctrl jp cd)
         in
         Distributed_runtime_nofunct.on_event_at_eoi evt ctrl await_eoi

    let rml_await_one expr_evt p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_await_one' evt p f_k ctrl jp cd unit_value

    let rml_await_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        Distributed_runtime_nofunct.on_event_cfg_at_eoi (expr_cfg ()) ctrl (fun () -> Distributed_runtime_nofunct.on_next_instant ctrl f_k)

(**************************************)
(* present                            *)
(**************************************)

    let rml_present' evt p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          Distributed_runtime_nofunct.on_event_or_next evt f_1 unit_value cd ctrl f_2

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_present' evt p_1 p_2 f_k ctrl jp cd unit_value

(**************************************)
(* present_conf                       *)
(**************************************)

    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          Distributed_runtime_nofunct.on_event_cfg_or_next (expr_cfg ()) f_1 unit_value cd ctrl f_2

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_2 = p_2 f_k ctrl jp cd in
        let f_1 = p_1 (fun x -> f_2 ()) ctrl None cd in
        f_1

(**************************************)
(* par                                *)
(**************************************)
(* Utilisation de Obj.magic pour le pb de la generalisation des *)
(* applications partielles.                                     *)

    let join_n cpt =
      fun f_k ctrl jp cd _ ->
        decr cpt;
        if !cpt = 0 then
          f_k unit_value

    let rml_par p_1 p_2 =
      fun f_k ctrl jp cd ->
        let cpt, f_1, f_2 =
          match jp with
            | None ->
              let cpt = ref 2 in
              let j = join_n cpt f_k ctrl jp cd in
              let f_1 = p_1 (Obj.magic j: 'a step) ctrl (Some cpt) cd in
              let f_2 = p_2 (Obj.magic j: 'b step) ctrl (Some cpt) cd in
              cpt, f_1, f_2
            | Some cpt ->
              let f_1 = p_1 (Obj.magic f_k: 'a step) ctrl (Some cpt) cd in
              let f_2 = p_2 (Obj.magic f_k: 'b step) ctrl (Some cpt) cd in
              incr cpt;
              cpt, f_1, f_2
        in
        fun () ->
          Distributed_runtime_nofunct.on_current_instant cd f_2;
          f_1 unit_value

(**************************************)
(* loop                               *)
(**************************************)
(*
let rec rml_loop p f_k ctrl _ =
  p (rml_loop p f_k ctrl) ctrl unit_value
*)

(*
let rml_loop p =
  fun f_k ctrl ->
    let rec f_1 = lazy (p f ctrl)
    and f =
      fun _ ->
        Lazy.force f_1 unit_value
    in
    f
*)

    let rml_loop p =
      fun f_k ctrl jp cd ->
        let f_1 = ref dummy_step in
        let f_loop = p (fun _ -> !f_1 unit_value) ctrl None cd in
        f_1 := f_loop;
        f_loop

(**************************************)
(* loop_n                             *)
(**************************************)

    let rml_loop_n e p =
      fun f_k ctrl jp cd ->
        let cpt = ref 0 in
        let f_1 = ref dummy_step in
        let f_loop =
          p
            (fun _ ->
              if !cpt > 0 then
                (decr cpt; !f_1 unit_value)
              else
                f_k unit_value)
            ctrl None cd
        in
        f_1 := f_loop;
        fun _ ->
          let n = e() in
          if n > 0 then
            (cpt := n - 1;
             f_loop unit_value)
          else
            f_k unit_value


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal ce re p =
      fun f_k ctrl jp cd _ ->
        let ck = eval_clock_expr cd ce in
        let r = Distributed_runtime_nofunct.Event.region_of_clock (eval_clock_expr cd re) in
        p (Distributed_runtime_nofunct.Event.new_evt ck r) f_k ctrl jp cd unit_value

    let rml_signal_combine ce re default comb p =
      fun f_k ctrl jp cd _ ->
        let ck = eval_clock_expr cd ce in
        let r = Distributed_runtime_nofunct.Event.region_of_clock (eval_clock_expr cd re) in
        let evt = Distributed_runtime_nofunct.Event.new_evt_combine ck r (default()) (comb()) in
        p evt f_k ctrl jp cd unit_value

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k ctrl jp cd _ ->
        p (e()) f_k ctrl jp cd unit_value

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k ctrl jp cd ->
        p1 (fun v -> p2 v f_k ctrl None cd unit_value) ctrl jp cd

(**************************************)
(* def_and_dyn                        *)
(**************************************)
(*
    let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
        fun f_k ctrl jp cd ->
          fun x ->
            value_array.(i) <- x;
            decr cpt;
            if !cpt = 0 then
              let f = p3 value_array f_k ctrl jp cd in
              f unit_value
            else
              sched()
      in
      fun p_array p3 ->
        fun f_k ctrl jp cd ->
          let n = Array.length p_array in
          let cpt = ref n in
          let value_array = Array.make n (Obj.magic()) in
          let step_init =
            fun _ ->
              cpt := n;
              for i = 0 to n - 1 do
                let f =
                  p_array.(i)
                    (join_n cpt value_array p3 i f_k ctrl jp cd)
                    ctrl (Some cpt)
                in
                current := f :: !current
               done;
              sched()
          in step_init
*)

(**************************************)
(* match                              *)
(**************************************)

    let rml_match e p =
      fun f_k ctrl jp cd _ ->
        p (e()) f_k ctrl jp cd unit_value


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k ctrl jp cd _ ->
        (e ()) () f_k ctrl jp cd unit_value


(**************************************)
(* if                                 *)
(**************************************)

    let rml_if e p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          if e() then
            f_1 unit_value
          else
            f_2 unit_value

(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k ctrl jp cd ->
        let f_body = ref dummy_step in
        let f_while _ =
          if e()
          then !f_body unit_value
          else f_k unit_value
        in
        f_body := p f_while ctrl None cd;
        f_while


(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k ctrl jp cd ->
        let rec f_for i v2 _ =
          incr i;
          if cmp !i v2
          then p !i (f_for i v2) ctrl None cd unit_value
          else f_k unit_value
        in
        let f_for_init _ =
          let i = ref (e1()) in
          let v2 = e2() in
          if cmp !i v2
          then p !i (f_for i v2) ctrl None cd unit_value
          else f_k unit_value
        in
        f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)

    let rml_fordopar e1 e2 dir p =
      fun f_k ctrl jp cd ->
        let cpt = ref 0 in
        let j = join_n cpt f_k jp ctrl cd in
        let f_fordopar _ =
          if dir then
            begin
              let min = e1() in
              let max = e2() in
              cpt := max - min + 1;
              if !cpt <= 0 then
                f_k unit_value
              else
                for i = max downto min do
                  let f = p i j ctrl (Some cpt) cd in
                  Distributed_runtime_nofunct.on_current_instant cd f
                done
            end
          else
            begin
              let max = e1() in
              let min = e2() in
              cpt := max - min + 1;
              if !cpt <= 0 then
                f_k unit_value
              else
                for i = min to max do
                  let f = p i j ctrl (Some cpt) cd in
                  Distributed_runtime_nofunct.on_current_instant cd f
                done
            end
        in
        f_fordopar

    let rml_par_n p_list =
      fun f_k ctrl jp cd ->
        let nb = List.length p_list in
        let cpt, f_list =
          match jp with
            | None ->
              let cpt = ref nb in
              let j = join_n cpt f_k ctrl jp cd in
              let f_list = List.rev_map (fun p -> p j ctrl (Some cpt) cd) p_list in
              cpt, f_list
            | Some cpt ->
              let f_list = List.rev_map (fun p -> p f_k ctrl (Some cpt) cd) p_list in
              cpt := !cpt + nb - 1;
              cpt, f_list
        in
        fun _ ->
          Distributed_runtime_nofunct.on_current_instant_list cd f_list

    let rml_seq_n =
      let rec fold p_list k ctrl jp cd =
        match p_list with
        | [] -> k
        | [ p ] -> p k ctrl jp cd
        | p::p_list -> p (fold p_list k ctrl jp cd) ctrl None cd
      in
      fun p_list ->
        fun f_k ctrl jp cd ->
          let f =
            (* List.fold_right (fun p -> fun k -> p k ctrl None) p_list f_k  *)
            fold p_list f_k ctrl jp cd
          in f



(**************************************)
(* until                              *)
(**************************************)

    let rml_until expr_evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
        fun _ ->
          let evt = expr_evt () in
          Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt);
          start_ctrl cd ctrl new_ctrl;
          f unit_value

    let rml_until' evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt);
        let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
        fun _ ->
          start_ctrl cd ctrl new_ctrl;
          f unit_value

    let rml_until_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
        fun _ ->
          let evt_cfg = expr_cfg () in
          Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.cfg_status ~only_at_eoi:true evt_cfg);
          start_ctrl cd ctrl new_ctrl;
          f unit_value

(**************************************)
(* until handler                      *)
(**************************************)

    let rml_until_handler_local expr_evt matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let evt = ref (Obj.magic() : ('a, 'b) event) in
        let handler _ =
          let x = Distributed_runtime_nofunct.Event.value !evt in
          p_handler x f_k ctrl jp cd
        in
        let new_ctrl = new_ctrl (Kill_handler handler) in
        let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
        fun _ ->
          evt := expr_evt ();
          begin match matching_opt with
            | None ->
              Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true !evt)
            | Some matching ->
              Distributed_runtime_nofunct.set_condition new_ctrl
                (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true !evt
                  && matching (Distributed_runtime_nofunct.Event.value !evt))
          end;
          start_ctrl cd ctrl new_ctrl;
          f unit_value

    let rml_until_handler_local' evt matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let handler _ =
          let x = Distributed_runtime_nofunct.Event.value evt in
          p_handler x f_k ctrl jp cd
        in
        let new_ctrl = new_ctrl (Kill_handler handler) in
        let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
        begin match matching_opt with
        | None ->
            Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt)
        | Some matching ->
            Distributed_runtime_nofunct.set_condition new_ctrl
              (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt
                && matching (Distributed_runtime_nofunct.Event.value evt))
        end;
        fun () ->
          start_ctrl cd ctrl new_ctrl;
          f ()

    let rml_until_handler expr_evt p p_handler =
      rml_until_handler_local expr_evt None p p_handler

    let rml_until_handler' evt p p_handler =
      rml_until_handler_local' evt None p p_handler

    let rml_until_handler_match expr_evt matching p p_handler =
      rml_until_handler_local expr_evt (Some matching) p p_handler

    let rml_until_handler_match' evt matching p p_handler =
      rml_until_handler_local' evt (Some matching) p p_handler


(**************************************)
(* control                            *)
(**************************************)
    let step_control_static cond p f_k ctrl jp cd =
      let new_ctrl = new_ctrl Susp in
      let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
      Distributed_runtime_nofunct.set_condition new_ctrl cond;
      fun () ->
        start_ctrl cd ctrl new_ctrl;
        f ()

    let step_control cond p f_k ctrl jp cd =
      let new_ctrl = new_ctrl Susp in
      let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
      fun () ->
        Distributed_runtime_nofunct.set_condition new_ctrl (cond ());
        start_ctrl cd ctrl new_ctrl;
        f ()

    let rml_control' evt p =
      fun f_k ctrl jp cd ->
        step_control_static (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt) p
          f_k ctrl jp cd

    let rml_control expr_evt p =
      fun f_k ctrl jp cd ->
        let cond () =
          let evt = expr_evt () in
          fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt
        in
        step_control cond p f_k ctrl jp cd

    let rml_control_match' evt matching p =
      fun f_k ctrl jp cd ->
        step_control_static
          (fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt
            && matching (Distributed_runtime_nofunct.Event.value evt)) p f_k ctrl jp cd

    let rml_control_match expr_evt matching p =
      fun f_k ctrl jp cd ->
        let cond () =
          let evt = expr_evt () in
          fun () -> Distributed_runtime_nofunct.Event.status ~only_at_eoi:true evt
            && matching (Distributed_runtime_nofunct.Event.value evt)
        in
        step_control cond p f_k ctrl jp cd

    let rml_control_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let cond () =
          let evt_cfg = expr_cfg () in
          fun () -> Distributed_runtime_nofunct.Event.cfg_status ~only_at_eoi:true evt_cfg
        in
        step_control cond p f_k ctrl jp cd

(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl jp cd p evt =
      let dummy = ref dummy_step in
      let new_ctrl = Distributed_runtime_nofunct.new_ctrl When in
      let rec when_act _ =
        Distributed_runtime_nofunct.wake_up_ctrl new_ctrl cd;
        Distributed_runtime_nofunct.on_next_instant ctrl f_when
      and f_when _ =
        Distributed_runtime_nofunct.on_event evt ctrl when_act unit_value
      in
      let f = p (Distributed_runtime_nofunct.end_ctrl new_ctrl f_k) new_ctrl None cd in
      dummy := f_when;
      fun () ->
        Distributed_runtime_nofunct.start_ctrl cd ctrl new_ctrl;
        Distributed_runtime_nofunct.set_condition new_ctrl (fun () -> Distributed_runtime_nofunct.Event.status evt);
        Distributed_runtime_nofunct.set_suspended new_ctrl true;
        Distributed_runtime_nofunct.on_next_instant new_ctrl f;
        f_when ()

    let rml_when expr_evt p =
      fun f_k ctrl jp cd ->
        let f = step_when f_k ctrl jp cd p in
        fun _ ->
          let evt = expr_evt () in
          f evt unit_value

    let rml_when' evt p =
      fun f_k ctrl jp cd ->
        step_when f_k ctrl jp cd p evt

    let rml_when_conf expr_cfg =
      fun f_k ctrl ->
        fun _ -> print_debug "Unimplemented when_conf@."; raise Types.RML

(**************************************)
(* clock domain                       *)
(**************************************)

    let rml_newclock sch p =
      fun f_k ctrl jp cd ->
        Distributed_runtime_nofunct.new_clock_domain cd ctrl (fun cd ctrl f_k -> p (CkExpr (Distributed_runtime_nofunct.clock cd)) f_k ctrl None cd) sch f_k

    let rml_top_clock = CkTop
    let rml_local_clock = CkLocal

(**************************************)
(* pauseclock                         *)
(**************************************)
    let rml_pauseclock' pause_ce =
      fun f_k ctrl jp cd _ ->
        let pause_ck = eval_clock_expr cd pause_ce in
        Distributed_runtime_nofunct.set_pauseclock cd pause_ck;
        rml_pause_at' pause_ce f_k ctrl jp cd ()

    let rml_pauseclock e =
      fun f_k ctrl jp cd _ ->
        rml_pauseclock' (e ()) f_k ctrl jp cd unit_value

(**************************************)
(* rml_make                           *)
(**************************************)

    let rml_make cd result p =
     (* Function to create the last continuation of a toplevel process *)
      let jp, join_end =
        let term_cpt = ref 0 in
        Some term_cpt,
        fun () ->
          incr term_cpt;
          let f x =
            decr term_cpt;
            if !term_cpt <= 0 then
              result := Some x
          in f
      in
      p () (join_end()) (control_tree cd) jp cd

    let rml_make_n cd result pl =
      let jp, join_end =
        let term_cpt = ref 0 in
        Some term_cpt,
        fun () ->
          term_cpt := !term_cpt + (List.length pl);
          let f x =
            decr term_cpt;
            if !term_cpt <= 0 then
              result := Some x
          in f
      in
      List.map (fun p -> p () (join_end ()) (control_tree cd) jp cd) pl

    module R = Distributed_runtime_nofunct

(*
module Lco_ctrl_tree_mpi_buffer_interpreter =
  Rml_interpreter(Distributed_runtime.MpiBufferedRuntime)
*)
