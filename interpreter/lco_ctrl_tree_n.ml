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

module Rml_interpreter =
  functor (R : Runtime.R with type 'a Step.t = 'a -> unit) ->
  struct
    exception RML = R.RML

    type event_cfg = bool -> (unit -> bool) * R.waiting_list list

    type join_point = int ref option
    and 'a expr = 'a R.Step.t -> R.control_tree -> join_point -> R.clock_domain -> unit R.Step.t
    and 'a process = unit -> 'a expr

    let unit_value = ()
    let dummy_step _ = ()

    open R

(* ------------------------------------------------------------------------ *)
    let rml_pre_status (n, _, _) = Event.pre_status n

    let rml_pre_value (n, _, _) = Event.pre_value n

    let rml_last (n, _, _) = Event.last n

    let rml_default (n, _, _) = Event.default n

(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine = new_evt_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' (n,wa,wp) =
      fun is_long_wait ->
        (fun () -> Event.status n),
        [ if is_long_wait then wa else wp ]

    let cfg_present evt_expr =
      fun is_long_wait ->
        let evt = evt_expr() in
        cfg_present' evt is_long_wait

    let cfg_and c1 c2 =
      fun is_long_wait ->
        let is_true1, evt_list1 = c1 is_long_wait in
        let is_true2, evt_list2 = c2 is_long_wait in
        (fun () -> is_true1() && is_true2()),
        List.rev_append evt_list1 evt_list2

    let cfg_or c1 c2 =
      fun is_long_wait ->
        let is_true1, evt_list1 = c1 is_long_wait in
        let is_true2, evt_list2 = c2 is_long_wait in
        (fun () -> is_true1() || is_true2()),
        List.rev_append evt_list1 evt_list2



(* ------------------------------------------------------------------------ *)

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k ctrl jp cd ->
        let f_nothing =
          fun _ ->
            f_k unit_value
        in f_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k ctrl jp cd ->
        let f_compute =
          fun _ ->
            let v = e cd () in
            f_k v
        in f_compute

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause_at' pause_cd =
      fun f_k ctrl jp cd ->
        let add_next () = R.add_next f_k ctrl.next in
        let f_pause _ =
          R.add_weoi pause_cd add_next
        in f_pause

    let rml_pause_at e =
      rml_pause_at' (e ())

    let rml_pause_top f_k ctrl jp cd =
      rml_pause_at' R.top_clock_domain f_k ctrl jp cd

    let rml_pause =
      fun f_k ctrl jp cd ->
        let f_pause =
          fun _ ->
            (* Format.eprintf "Pause @."; *)
            R.add_next f_k ctrl.next
        in f_pause

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi =
      fun f_k ctrl jp cd ->
        let f_pause =
          fun _ ->
            R.add_next f_k ctrl.next_boi
        in f_pause

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k ctrl jp cd ->
        let f_halt =
          fun _ -> ()
        in f_halt

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rml_halt_kboi = rml_halt

(**************************************)
(* emit                               *)
(**************************************)
    let step_emit f_k ctrl cd (n,wa,wp) e _ =
      R.Event.emit n (e());
      R.add_current_waiting_list wa cd;
      R.add_current_waiting_list wp cd;
      f_k unit_value

    let rml_emit_val expr_evt e =
      fun f_k ctrl jp cd ->
        let f_emit_val =
          fun _ ->
            let evt = expr_evt() in
            step_emit f_k ctrl cd evt e unit_value
        in f_emit_val

    let rml_emit_val' evt e =
      fun f_k ctrl jp cd ->
        let f_emit_val =
          step_emit f_k ctrl cd evt e
        in f_emit_val

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val cd (n,wa,wp) v =
      R.Event.emit n v;
      R.add_current_waiting_list wa cd;
      R.add_current_waiting_list wp cd

    let rml_expr_emit cd evt =
      rml_expr_emit_val cd evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate f_k ctrl cd (n,wa,wp) =
      let w = if R.is_toplevel ctrl then wa else wp in
      if R.is_toplevel ctrl then
        let rec f_await_top =
          fun _ ->
            if R.Event.status n
            then
              f_k unit_value
            else
              R.add_waiting f_await_top w
        in f_await_top
      else
        let rec f_await_not_top =
          fun _ ->
            if R.Event.status n
            then
              f_k unit_value
            else
              if is_eoi cd
              then
                R.add_next f_await_not_top ctrl.next
              else
                (R.add_waiting f_await_not_top w;
                 add_weoi_waiting_list cd w)
        in f_await_not_top

(*
    let step_await_immediate f_k ctrl cd (n,wa,wp) =
      let rec wake_up _ =
        if ctrl.alive && not ctrl.susp then
          (*ctrl is activated, run continuation*)
          f_k unit_value
        else ((*ctrl is not active, wait end of instant*)
          R.add_waiting eoi_await ctrl.wp;
          R.add_weoi_waiting_list cd ctrl.wp
        )
      and eoi_await _ =
        if is_eoi cd then
          (*ctrl was not activated, await the signal again *)
          R.add_waiting wake_up wa
        else (* ctrl was activated, signal is present*)
          f_k unit_value
      in
      let rec await _ =
        if R.Event.status n then
          f_k unit_value
        else
          R.add_waiting wake_up wa
      in
      await
*)

    let rml_await_immediate expr_evt =
      fun f_k ctrl jp cd ->
        let f_await =
          fun _ ->
            let evt = expr_evt() in
            step_await_immediate f_k ctrl cd evt unit_value
        in f_await

    let rml_await_immediate' evt =
      fun f_k ctrl jp cd ->
        let f_await =
          step_await_immediate f_k ctrl cd evt
        in f_await

(**************************************)
(* await_immediate_conf               *)
(**************************************)
    let rml_await_immediate_conf expr_cfg =
      fun f_k ctrl jp cd ->
        if R.is_toplevel ctrl then
          let f_await_top =
            fun _ ->
              let is_true, w_list = expr_cfg true in
              if is_true() then
                f_k unit_value
              else
                let ref_f = ref None in
                let f w step_wake_up =
                  if is_true() then
                    (ref_f := None;
                     f_k unit_value)
                  else
                    R.add_waiting step_wake_up w
                in
                let gen_step w =
                  let rec step_wake_up _ =
                    match !ref_f with
                    | None -> ()
                    | Some f -> f w step_wake_up
                  in step_wake_up
                in
                ref_f := Some f;
                List.iter (fun w -> R.add_waiting (gen_step w) w) w_list
          in f_await_top
        else
          let f_await_not_top =
            fun _ ->
              let is_true, w_list = expr_cfg false in
              if is_true() then
                f_k unit_value
              else
                let ref_f = ref None in
                let rec f w step_wake_up =
                  if is_true() then
                    (ref_f := None;
                     f_k unit_value)
                  else
                    if is_eoi cd
                    then
                      R.add_next step_wake_up ctrl.next
                    else
                      (R.add_waiting step_wake_up w;
                       add_weoi_waiting_list cd w)
                in
                let gen_step w =
                  let rec step_wake_up _ =
                    match !ref_f with
                    | None -> ()
                    | Some f -> f w step_wake_up
                  in step_wake_up
                in
                ref_f := Some f;
                List.iter
                  (fun w ->
                    R.add_waiting (gen_step w) w;
                    add_weoi_waiting_list cd w)
                  w_list
          in f_await_not_top

(**************************************)
(* get                                *)
(**************************************)
    let step_get f_k ctrl jp cd (n,_,_) p =
      let rec f_get =
        fun _ ->
          if is_eoi cd
          then
            let x =
              if R.Event.status n
              then R.Event.value n
              else R.Event.default n
            in
            let f_body = p x f_k ctrl jp cd in
            R.add_next f_body ctrl.next
          else
            R.add_weoi cd f_get
      in f_get

    let rml_get expr_evt p =
      fun f_k ctrl jp cd ->
        let f_get =
          fun _ ->
            let evt = expr_evt() in
            step_get f_k ctrl jp cd evt p unit_value
        in f_get

    let rml_get' evt p =
      fun f_k ctrl jp cd ->
        let f_get =
            step_get f_k ctrl jp cd evt p
        in f_get




(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one f_k ctrl jp cd (n,wa,wp) p =
      let w = if R.is_toplevel ctrl then wa else wp in
      let f_await_one =
        if R.is_toplevel ctrl then
          let rec f_await_one_top =
            fun _ ->
              if R.Event.status n
              then
                let x = R.Event.one n in
                p x f_k ctrl jp cd unit_value
              else
                R.add_waiting f_await_one_top w
          in f_await_one_top
        else
          let rec f_await_one_not_top =
            fun _ ->
              if R.Event.status n
              then
                let x = R.Event.one n in
                p x f_k ctrl jp cd unit_value
              else
                if is_eoi cd
                then
                  R.add_next f_await_one_not_top ctrl.next
                else
                  (R.add_waiting f_await_one_not_top w;
                   add_weoi_waiting_list cd w)
          in f_await_one_not_top
      in f_await_one

     let rml_await_immediate_one expr_evt p =
      fun f_k ctrl jp cd ->
      let f_await_one =
        fun _ ->
          let evt = expr_evt() in
          step_await_immediate_one f_k ctrl jp cd evt p unit_value
      in f_await_one

    let rml_await_immediate_one' evt p =
      fun f_k ctrl jp cd ->
        step_await_immediate_one f_k ctrl jp cd evt p


(**************************************)
(* await_all_match                    *)
(**************************************)
    let step_await_all_match f_k ctrl jp cd (n,wa,wp) matching p =
      let w = if R.is_toplevel ctrl then wa else wp in
      let sig_cd = R.Event.clock_domain n in
      let f_await_all_match =
        if R.is_toplevel ctrl then
          let rec f_await_top =
            fun _ ->
              if is_eoi sig_cd
              then
                let v = R.Event.value n in
                if R.Event.status n && matching v
                then
                  let x = v in
                  let f_body = p x f_k ctrl jp cd in
                  R.add_next f_body ctrl.next
                else
                  R.add_waiting f_await_top w
              else
                if R.Event.status n
                then
                  R.add_weoi sig_cd f_await_top
                else
                  R.add_waiting f_await_top w
          in f_await_top
        else
          let rec f_await_not_top =
            fun _ ->
              if is_eoi cd
              then
                let v = R.Event.value n in
                if is_eoi sig_cd && R.Event.status n && matching v
                then
                  let x = v in
                  let f_body = p x f_k ctrl jp cd in
                  R.add_next f_body ctrl.next
                else
                  R.add_next f_await_not_top ctrl.next
              else
                (R.add_waiting f_await_not_top w;
                 add_weoi_waiting_list cd w)
          in f_await_not_top
      in f_await_all_match


    let rml_await_all_match expr_evt matching p =
      fun f_k ctrl jp cd ->
        let f_await_all_match =
          fun _ ->
            let evt = expr_evt() in
            step_await_all_match f_k ctrl jp cd evt matching p unit_value
        in f_await_all_match

    let rml_await_all_match' evt matching p =
      fun f_k ctrl jp cd ->
        step_await_all_match f_k ctrl jp cd evt matching p

(**************************************)
(* present                            *)
(**************************************)

    let step_present f_k ctrl cd (n,_,wp) f_1 f_2 =
      let rec f_present =
        fun _ ->
          if R.Event.status n
          then
            f_1 unit_value
          else
            if is_eoi cd
            then
              R.add_next f_2 ctrl.next
            else
              (R.add_waiting f_present wp;
               add_weoi_waiting_list cd wp)
      in f_present

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        let rec f_present =
          fun _ ->
            let evt = expr_evt () in
            step_present f_k ctrl cd evt f_1 f_2 unit_value
        in f_present

    let rml_present' evt p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        step_present f_k ctrl cd evt f_1 f_2

(**************************************)
(* present_conf                       *)
(**************************************)
    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl jp cd ->
        fun _ ->
          let f_1 = p_1 f_k ctrl jp cd in
          let f_2 = p_2 f_k ctrl jp cd in
          let is_true, w_list = expr_cfg false in
          if is_true ()
          then
            f_1 unit_value
          else
            let ref_f = ref None in
            let f w step_wake_up =
              if is_true() then
                (ref_f := None;
                 f_1 unit_value)
              else
                if is_eoi cd
                then
                  R.add_next f_2 ctrl.next
                else
                  (R.add_waiting step_wake_up w;
                   add_weoi_waiting_list cd w)
            in
            let gen_step w =
              let rec step_wake_up _ =
              match !ref_f with
              | None -> ()
              | Some f -> f w step_wake_up
              in step_wake_up
            in
            ref_f := Some f;
            List.iter
              (fun w ->
                R.add_waiting (gen_step w) w;
                add_weoi_waiting_list cd w)
              w_list

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
      fun f_k ctrl ->
        let f_join_n =
          fun _ ->
            decr cpt;
            if !cpt = 0 then
              f_k unit_value
        in f_join_n

    let rml_par p_1 p_2 =
      fun f_k ctrl jp cd ->
        let cpt =
          match jp with
          | None -> ref 0
          | Some cpt -> cpt
        in
        let j = join_n cpt f_k ctrl in
        let f_1 = p_1 (Obj.magic j: 'a step) ctrl (Some cpt) cd in
        let f_2 = p_2 (Obj.magic j: 'b step) ctrl (Some cpt) cd in
        let f_par =
          fun _ ->
            cpt := !cpt + 2;
            add_current f_2 cd;
            f_1 unit_value
        in f_par

(**************************************)
(* merge                              *)
(**************************************)

    let rml_merge = rml_par
(*     let rml_merge p_1 p_2 = *)
(*       fun f_k ctrl jp cd -> *)
(*      fun _ -> raise RML *)


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

    let rml_signal p =
      fun f_k ctrl jp cd ->
        let f_signal =
          fun _ ->
            let evt = new_evt cd in
            let f = p evt f_k ctrl jp cd in
            f unit_value
        in f_signal

    let rml_signal_combine default comb p =
      fun f_k ctrl jp cd ->
        let f_signal =
          fun _ ->
            let evt = new_evt_combine cd (default()) (comb()) in
            let f = p evt f_k ctrl jp cd in
            f unit_value
        in f_signal

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k ctrl jp cd ->
        let f_def =
          fun _ ->
            let f = p (e()) f_k ctrl jp cd in
            f unit_value
        in f_def

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k ctrl jp cd ->
        let f_def =
          p1
            (fun v ->
              let f = p2 v f_k ctrl None cd in
              f unit_value)
            ctrl jp cd
        in f_def

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
      fun f_k ctrl jp cd ->
        let f_match =
          fun _ ->
            let f = p (e()) f_k ctrl jp cd in
            f unit_value
        in f_match


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k ctrl jp cd ->
        let f_run =
          fun _ ->
            let f_1 = (e ()) () f_k ctrl jp cd in
            f_1 unit_value
        in f_run


(**************************************)
(* until                              *)
(**************************************)
(* ---------- Misc functions for until, control and when ---------- *)
    let start_ctrl f_k ctrl f new_ctrl =
      let f_ctrl =
        fun _ ->
          if new_ctrl.alive
          then
            (ctrl.children <- new_ctrl :: ctrl.children)
          else
            (new_ctrl.alive <- true;
             new_ctrl.susp <- false;
             R.clear_next new_ctrl.next;
             R.clear_next new_ctrl.next_boi);
          f unit_value
      in f_ctrl

    let end_ctrl f_k new_ctrl =
      fun x ->
        set_kill new_ctrl;
        new_ctrl.alive <- false;
        f_k x
(* ---------------------------------------------------------------- *)

    let rml_until expr_evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_until =
          fun _ ->
            let (n,_,_) = expr_evt () in
            new_ctrl.cond <- (fun () -> R.Event.status n);
            start_ctrl f_k ctrl f new_ctrl unit_value
        in f_until

    let rml_until' (n,_,_) p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        new_ctrl.cond <- (fun () -> R.Event.status n);
        start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* until_conf                         *)
(**************************************)

    let rml_until_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_until =
          fun _ ->
            let cond, _ = expr_cfg true in
            new_ctrl.cond <- cond;
            start_ctrl f_k ctrl f new_ctrl unit_value
        in f_until



(**************************************)
(* until handler                      *)
(**************************************)

    let rml_until_handler_local
        (expr_evt: unit -> ('a, 'b) event) matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let evt = ref (Obj.magic() : ('a, 'b) event) in
        let handler =
          fun () ->
            let x =
              let n, _, _ = !evt in
              if R.Event.status n
              then R.Event.value n
              else raise R.RML
            in
            let f_handler = p_handler x f_k ctrl jp cd in
            f_handler
        in
        let new_ctrl = new_ctrl (Kill_handler handler) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_until =
          fun _ ->
            let (n, _, _) as e = expr_evt () in
            evt := e;
            begin match matching_opt with
            | None ->
                new_ctrl.cond <- (fun () -> R.Event.status n);
            | Some matching ->
                new_ctrl.cond <-
                  (fun () -> R.Event.status n && matching (R.Event.value n));
            end;
            start_ctrl f_k ctrl f new_ctrl unit_value
        in f_until

    let rml_until_handler_local' (n,_,_) matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let handler =
          fun () ->
            let x =
              if R.Event.status n
              then R.Event.value n
              else raise R.RML
            in
            let f_handler = p_handler x f_k ctrl jp cd in
            f_handler
        in
        let new_ctrl = new_ctrl (Kill_handler handler) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        begin match matching_opt with
        | None ->
            new_ctrl.cond <- (fun () -> R.Event.status n);
        | Some matching ->
            new_ctrl.cond <-
              (fun () -> R.Event.status n && matching (R.Event.value n));
        end;
        start_ctrl f_k ctrl f new_ctrl

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
    let rml_control expr_evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_control =
          fun _ ->
            let (n, _, _) = expr_evt () in
            new_ctrl.cond <- (fun () -> R.Event.status n);
            start_ctrl f_k ctrl f new_ctrl ()
        in f_control

    let rml_control' (n, _, _) p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        new_ctrl.cond <- (fun () -> R.Event.status n);
        start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* control_match                      *)
(**************************************)
    let rml_control_match expr_evt matching p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_control =
          fun _ ->
            let (n, _, _) = expr_evt () in
            new_ctrl.cond <-
              (fun () -> R.Event.status n && matching (R.Event.value n));
            start_ctrl f_k ctrl f new_ctrl ()
        in f_control

    let rml_control_match' (n, _, _) matching p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        new_ctrl.cond <- (fun () -> R.Event.status n && matching (R.Event.value n));
        start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* control_conf                       *)
(**************************************)

    let rml_control_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let f_control =
          fun _ ->
            let cond, _ = expr_cfg true in
            new_ctrl.cond <- cond;
            start_ctrl f_k ctrl f new_ctrl ()
        in f_control


(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl cd (n,wa,wp) f new_ctrl dummy =
      let w = if R.is_toplevel ctrl then wa else wp in
      new_ctrl.cond <- (fun () -> R.Event.status n);
      let rec f_when =
        fun _ ->
          if R.Event.status n
          then
            (new_ctrl.susp <- false;
             next_to_current cd new_ctrl)
          else
            if is_eoi cd
            then
              R.add_next f_when ctrl.next
            else
              (R.add_waiting f_when w;
               if not (R.is_toplevel ctrl) then add_weoi_waiting_list cd w)
      in
      let start_when =
        fun _ ->
          if new_ctrl.alive
          then
            (ctrl.children <- new_ctrl :: ctrl.children)
          else
            (new_ctrl.alive <- true;
             new_ctrl.susp <- false;
             R.clear_next new_ctrl.next;
             R.clear_next new_ctrl.next_boi);
          if R.Event.status n
          then
            (new_ctrl.susp <- false;
             R.clear_next new_ctrl.next;
             R.clear_next new_ctrl.next_boi;
             f unit_value)
          else
            (new_ctrl.susp <- true;
             R.clear_next new_ctrl.next;
             R.add_next f new_ctrl.next;
             R.clear_next new_ctrl.next_boi;
             R.add_waiting f_when w;
             if not (R.is_toplevel ctrl) then add_weoi_waiting_list cd w)
      in
      dummy := f_when;
      start_when

      let rml_when expr_evt p =
      fun f_k ctrl jp cd ->
        let dummy = ref dummy_step in
        let new_ctrl = new_ctrl (When dummy) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let start_when =
          fun _ ->
            let evt = expr_evt () in
            step_when f_k ctrl cd evt f new_ctrl dummy unit_value
        in
        start_when

    let rml_when' evt p =
      fun f_k ctrl jp cd ->
        let dummy = ref dummy_step in
        let new_ctrl = new_ctrl (When dummy) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        let start_when =
          step_when f_k ctrl cd evt f new_ctrl dummy
        in
        start_when

(**************************************)
(* when_conf                          *)
(**************************************)
    let rml_when_conf expr_cfg =
      fun f_k ctrl ->
        fun _ -> raise R.RML


(**************************************)
(* if                                 *)
(**************************************)

    let rml_if e p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        let f_if =
          fun _ ->
            if e() then
              f_1 unit_value
            else
              f_2 unit_value
        in f_if


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k ctrl jp cd ->
        let f_body = ref dummy_step in
        let f_while =
          fun _ ->
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
        let rec f_for i v2 =
          fun _ ->
            incr i;
            if cmp !i v2
            then p !i (f_for i v2) ctrl None cd unit_value
            else f_k unit_value
        in
        let f_for_init =
          fun _ ->
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
        let j = join_n cpt f_k ctrl in
        let f_fordopar =
          fun _ ->
            if dir then
              begin
                let min = e1() in
                let max = e2() in
                cpt := max - min + 1;
                if !cpt <= 0 then
                  f_k unit_value
                else
                  begin
                    for i = max downto min do
                      let f = p i j ctrl (Some cpt) cd in
                      R.add_current f cd
                    done
                  end
              end
            else
              begin
                let max = e1() in
                let min = e2() in
                cpt := max - min + 1;
                if !cpt <= 0 then
                  f_k unit_value
                else
                  begin
                    for i = min to max do
                      let f = p i j ctrl (Some cpt) cd in
                      R.add_current f cd
                    done
                  end
              end
        in
        f_fordopar


(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

    let rml_await expr_evt =
      fun f_k ctrl jp cd ->
        fun _ ->
          let evt = expr_evt () in
          rml_await_immediate' evt (rml_pause f_k ctrl jp cd) ctrl None cd unit_value

    let rml_await' evt =
      fun f_k ctrl jp cd ->
        rml_await_immediate' evt (rml_pause f_k ctrl jp cd) ctrl None cd

    let rml_await_all expr_evt p =
      fun f_k ctrl jp cd ->
        fun _ ->
          let evt = expr_evt () in
          rml_await_immediate' evt (rml_get' evt p f_k ctrl jp cd)
            ctrl None cd unit_value

    let rml_await_all' evt p =
      fun f_k ctrl jp cd ->
        rml_await_immediate' evt (rml_get' evt p f_k ctrl jp cd) ctrl None cd

    let rml_await_one expr_evt p =
      let pause_p x =
        fun f_k ctrl jp cd ->
          rml_pause (p x f_k ctrl jp cd) ctrl None cd
      in
      fun f_k ctrl jp cd ->
        fun _ ->
          let evt = expr_evt () in
          rml_await_immediate_one' evt pause_p f_k ctrl jp cd unit_value

    let rml_await_one' evt p =
      let pause_p x =
        fun f_k ctrl jp cd ->
          rml_pause (p x f_k ctrl jp cd) ctrl None cd
      in
      fun f_k ctrl jp cd ->
        rml_await_immediate_one' evt pause_p f_k ctrl jp cd

    let rml_await_conf expr_cfg =
      fun f_k ctrl jp cd ->
        rml_await_immediate_conf expr_cfg (rml_pause f_k ctrl jp cd) ctrl None cd

(* ------------------------------------------------------------------------ *)
(*
    let join_n cpt =
      fun f_k ctrl ->
        let f_join_n =
          fun() ->
            decr cpt;
            if !cpt = 0 then
              f_k()
            else
              sched()
        in f_join_n
*)
    let rml_par_n p_list =
      fun f_k ctrl jp cd ->
        let nb = List.length p_list in
        let cpt =
          match jp with
          | None -> ref 0
          | Some cpt -> cpt
        in
        let j = join_n cpt f_k ctrl in
        let f_list = List.rev_map (fun p -> p j ctrl (Some cpt) cd) p_list in
        let f_par_n =
          fun _ ->
            cpt := !cpt + nb;
            R.add_current_list f_list cd
        in f_par_n

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

    (* clock domain *)
    let rml_new_clock_domain () =
      R.mk_clock_domain ()

    let eoi_clock_domain cd () =
      R.eoi cd

    let step_clock_domain cd new_cd new_ctrl =
      let rec f_cd () =
        R.schedule new_cd;
        if R.macro_step_done new_cd then (
          R.add_weoi cd (eoi_clock_domain new_cd);
          R.add_next f_cd cd.cd_top.next;
        ) else (
          R.eoi new_cd;
          (* execute again in the same step but yield for now*)
          R.add_current f_cd cd
        )
      in
      f_cd

    let end_clock_domain f_k ctrl x =
      end_ctrl f_k ctrl x

    let rml_at_clock_domain new_cd p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_cd.cd_top in
        let f = p (end_clock_domain f_k ctrl) new_ctrl None new_cd in
        add_current f new_cd;
        step_clock_domain cd new_cd new_ctrl


(**************************************)
(* pauseclock                         *)
(**************************************)
    let rml_pauseclock' pause_cd =
      fun f_k ctrl jp cd ->
        let f_pause _ =
          R.set_pauseclock pause_cd;
          rml_pause_at' pause_cd f_k ctrl jp cd ()
        in f_pause

    let rml_pauseclock e =
      rml_pauseclock' (e ())


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
      p () (join_end()) cd.cd_top jp cd


end (* Module Rml_interpreter *)
