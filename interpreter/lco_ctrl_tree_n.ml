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
  functor (R : Runtime.CONTROL_TREE_R with type 'a Step.t = 'a -> unit) ->
  struct
    exception RML = R.RML

    type event_cfg = bool -> (unit -> bool) * (R.waiting_list * R.clock_domain) list

    type join_point = int ref option
    and 'a expr = 'a R.Step.t -> R.control_tree -> join_point -> R.clock_domain -> unit R.Step.t
    and 'a process = unit -> 'a expr

    let unit_value = ()
    let dummy_step _ = ()

    open R


    (** Executes 'f v' at the eoi of cd. *)
    let do_at_eoi cd f v =
      let rec f _ =
        if is_eoi cd then
          f v
        else
          R.add_weoi cd f
      in
      f()

    (** Executes 'f_w v_w' if w is emitted before the end of instant of cd.
        Otherwise, executes 'f_eoi v_eoi' during the end of this instant. *)
    let do_when_w_or_at_eoi cd w f_w v_w f_eoi v_eoi =
      let act _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          f_eoi v_eoi
        else
          (* signal activated *)
          f_w v_w
      in
      R.add_waiting act w;
      R.add_weoi_waiting_list cd w

    exception Wait_again
    (* executes 'f v' if ctrl becomes active in the current step.
       Waits for the next activation of w otherwise, or if the call
       raises Wait_again *)
    let rec do_when_ctrl_and_w cd ctrl w f v =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          (try
            f v
          with
            | Wait_again -> R.add_waiting self w)
        else ((*ctrl is not active, wait end of instant*)
          R.add_next eoi_await ctrl.next_tmp;
          R.add_weoi_next cd ctrl.next_tmp
        )
      and eoi_await _ =
        if is_eoi cd then
            (*ctrl was not activated, await the signal again *)
          R.add_waiting self w
        else
            (* ctrl was activated, signal is present*)
          (try
            f v
          with
            | Wait_again -> R.add_waiting self w)
      in
      R.add_waiting self w


   (* executes 'f v' during the eoi if ctrl becomes active in the current step.
       Waits for the next activation of w otherwise, or if the call
       raises Wait_again *)
    let rec do_when_ctrl_and_w_at_eoi cd ctrl w f v =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          R.add_weoi cd eoi_work
        else ((*ctrl is not active, wait end of instant*)
          R.add_next eoi_await ctrl.next_tmp;
          R.add_weoi_next cd ctrl.next_tmp
        )
      and eoi_await _ =
        if is_eoi cd then
            (*ctrl was not activated, await the signal again *)
          R.add_waiting self w
        else
            (* ctrl was activated, signal is present*)
          R.add_weoi cd eoi_work
      and eoi_work _ =
          (try
            f v
          with
            | Wait_again -> R.add_waiting self w)
      in
      R.add_waiting self w


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
    let cfg_present' (n,wa,wp) is_long_wait =
      (fun () -> Event.status n),
      [ (if is_long_wait then wa else wp), R.Event.clock_domain n ]

    let cfg_present evt_expr is_long_wait =
      let evt = evt_expr() in
      cfg_present' evt is_long_wait

    let cfg_and c1 c2 is_long_wait =
      let is_true1, evt_list1 = c1 is_long_wait in
      let is_true2, evt_list2 = c2 is_long_wait in
      (fun () -> is_true1() && is_true2()),
      List.rev_append evt_list1 evt_list2

    let cfg_or c1 c2 is_long_wait =
      let is_true1, evt_list1 = c1 is_long_wait in
      let is_true2, evt_list2 = c2 is_long_wait in
      (fun () -> is_true1() || is_true2()),
      List.rev_append evt_list1 evt_list2


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
        let v = e cd () in
        f_k v

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause_at' pause_cd =
      fun f_k ctrl jp cd _ ->
        R.add_weoi pause_cd (fun () -> R.add_next f_k ctrl.next)

    let rml_pause_at e =
      fun f_k ctrl jp cd _ ->
        rml_pause_at' (e ()) f_k ctrl jp cd unit_value

    let rml_pause_top f_k ctrl jp cd =
      rml_pause_at' R.top_clock_domain f_k ctrl jp cd

    let rml_pause =
      fun f_k ctrl jp cd _ ->
        (* Format.eprintf "Pause @."; *)
        R.add_next f_k ctrl.next

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k ctrl jp cd _ ->
        ()

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' (n,wa,wp) e =
      fun f_k ctrl jp cd _->
        R.Event.emit n (e());
        R.add_current_waiting_list wa cd;
        R.add_current_waiting_list wp cd;
        f_k unit_value

    let rml_emit_val expr_evt e =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_emit_val' evt e f_k ctrl jp cd unit_value

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

    let rml_await_immediate' (n,wa,_) =
      fun f_k ctrl jp cd _ ->
        if R.Event.status n then
          f_k unit_value
        else
          do_when_ctrl_and_w cd ctrl wa f_k unit_value

    let rml_await_immediate expr_evt =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_await_immediate' evt f_k ctrl jp cd unit_value

(**************************************)
(* await_immediate_conf               *)
(**************************************)

    let rml_await_immediate_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        let is_true, w_list = expr_cfg true in
        if is_true() then
          f_k unit_value
        else
          let is_fired = ref false in
          let f _ =
            if not !is_fired then
              (if is_true() then
                  (is_fired := true;
                   f_k unit_value)
               else
                  raise Wait_again)
          in
          List.iter (fun (w,_) -> do_when_ctrl_and_w cd ctrl w f unit_value) w_list

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' (n,_,_) p =
      fun f_k ctrl jp cd _ ->
        let f_get _ =
          let x =
            if R.Event.status n
            then R.Event.value n
            else R.Event.default n
          in
          let f_body = p x f_k ctrl jp cd in
          R.add_next f_body ctrl.next
        in
        do_at_eoi (R.Event.clock_domain n) f_get unit_value

    let rml_get expr_evt p =
      let evt = expr_evt() in
      rml_get' evt p

(**************************************)
(* await_immediate_one                *)
(**************************************)

    let rml_await_immediate_one' (n,wa,_) p =
      fun f_k ctrl jp cd _ ->
        let f _ =
          let x = R.Event.one n in
          p x f_k ctrl jp cd unit_value
        in
        if R.Event.status n then
          f unit_value
        else
          do_when_ctrl_and_w cd ctrl wa f unit_value

     let rml_await_immediate_one expr_evt p =
       fun f_k ctrl jp cd _ ->
         let evt = expr_evt() in
         rml_await_immediate_one' evt p f_k ctrl jp cd unit_value

(**************************************)
(* await_all_match                    *)
(**************************************)

     let step_await ctrl (n,wa,_) await_eoi =
       let sig_cd = R.Event.clock_domain n in
       if R.Event.status n then
         R.add_weoi sig_cd await_eoi
       else
         do_when_ctrl_and_w_at_eoi sig_cd ctrl wa await_eoi unit_value

     let rml_await_all_match' ((n,wa,_) as evt) matching p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = R.Event.value n in
           if matching v then
             R.add_next (p v f_k ctrl jp cd) ctrl.next
           else
             raise Wait_again
         in
         step_await ctrl evt await_eoi

    let rml_await_all_match expr_evt matching p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        rml_await_all_match' evt matching p f_k ctrl jp cd unit_value

(**************************************)
(* await                              *)
(**************************************)

    let rml_await' evt =
      fun f_k ctrl jp cd _ ->
        let await_eoi _ = R.add_next f_k ctrl.next in
        step_await ctrl evt await_eoi

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

    let rml_await_one' ((n,_,_) as evt) p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = R.Event.one n in
           R.add_next (p v f_k ctrl jp cd) ctrl.next
         in
         step_await ctrl evt await_eoi

    let rml_await_one expr_evt p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_await_one' evt p f_k ctrl jp cd unit_value

    let rml_await_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        let is_true, w_list = expr_cfg true in
        if is_true() then
          R.add_next f_k ctrl.next
        else
          let is_fired = ref false in
          let f _ =
            if not !is_fired then
              (if is_true() then
                  (is_fired := true;
                   R.add_next f_k ctrl.next)
               else
                  raise Wait_again)
          in
          List.iter (fun (w,sig_cd) ->
            do_when_ctrl_and_w_at_eoi sig_cd ctrl w f unit_value) w_list

(**************************************)
(* present                            *)
(**************************************)

    let rml_present' (n,_,wp) p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          if R.Event.status n then
            f_1 unit_value
          else
            do_when_w_or_at_eoi cd wp f_1 unit_value
              (fun () -> R.add_next f_2 ctrl.next) unit_value

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        rml_present' evt p_1 p_2 f_k ctrl jp cd unit_value

(**************************************)
(* present_conf                       *)
(**************************************)

    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl jp cd _ ->
          let f_1 = p_1 f_k ctrl jp cd in
          let f_2 = p_2 f_k ctrl jp cd in
          let is_true, w_list = expr_cfg false in
          if is_true () then
            f_1 unit_value
          else
            let is_fired = ref false in
            let f _ =
              if not !is_fired then
                (if is_true() then
                    (is_fired := true;
                     f_1 unit_value)
                 else
                    raise Wait_again)
            in
            let present_eoi _ =
              if not !is_fired then
                (is_fired := true;
                 R.add_next f_2 ctrl.next)
            in
            List.iter
              (fun (w,_) -> do_when_w_or_at_eoi cd w f unit_value
                present_eoi unit_value) w_list

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
        let cpt =
          match jp with
          | None -> ref 0
          | Some cpt -> cpt
        in
        let j = join_n cpt f_k ctrl jp cd in
        let f_1 = p_1 (Obj.magic j: 'a step) ctrl (Some cpt) cd in
        let f_2 = p_2 (Obj.magic j: 'b step) ctrl (Some cpt) cd in
        fun () ->
          cpt := !cpt + 2;
          add_current f_2 cd;
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

    let rml_signal p =
      fun f_k ctrl jp cd _ ->
        p (new_evt cd) f_k ctrl jp cd unit_value

    let rml_signal_combine default comb p =
      fun f_k ctrl jp cd _ ->
        let evt = new_evt_combine cd (default()) (comb()) in
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
                  R.add_current f cd
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
                  R.add_current f cd
                done
            end
        in
        f_fordopar

    let rml_par_n p_list =
      fun f_k ctrl jp cd ->
        let nb = List.length p_list in
        let cpt =
          match jp with
          | None -> ref 0
          | Some cpt -> cpt
        in
        let j = join_n cpt f_k ctrl jp cd in
        let f_list = List.rev_map (fun p -> p j ctrl (Some cpt) cd) p_list in
        fun _ ->
          cpt := !cpt + nb;
          R.add_current_list f_list cd

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
(* ---------- Misc functions for until, control and when ---------- *)
    let start_ctrl ctrl new_ctrl =
      if new_ctrl.alive then
        ctrl.children <- new_ctrl :: ctrl.children
      else (* reset new_ctrl *)
        (new_ctrl.alive <- true;
         new_ctrl.susp <- false;
         R.clear_next new_ctrl.next;
         R.clear_next new_ctrl.next_boi)

    let end_ctrl f_k new_ctrl x =
      set_kill new_ctrl;
      new_ctrl.alive <- false;
      f_k x
(* ---------------------------------------------------------------- *)

    let rml_until expr_evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        fun _ ->
          let (n,_,_) = expr_evt () in
          new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n));
          start_ctrl ctrl new_ctrl;
          f unit_value

    let rml_until' (n,_,_) p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl (Kill f_k) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        fun () ->
          new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n));
          start_ctrl ctrl new_ctrl;
          f unit_value

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
            start_ctrl ctrl new_ctrl;
            f unit_value
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
                new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n))
            | Some matching ->
                new_ctrl.cond <-
                  (fun () -> R.Event.status n && matching (R.Event.value n)
                    && is_eoi (R.Event.clock_domain n))
            end;
            start_ctrl ctrl new_ctrl;
            f unit_value
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
            new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n))
        | Some matching ->
            new_ctrl.cond <-
              (fun () -> R.Event.status n && matching (R.Event.value n)
                && is_eoi (R.Event.clock_domain n))
        end;
        fun () ->
          start_ctrl ctrl new_ctrl;
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
    let rml_control expr_evt p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        fun _ ->
          let (n, _, _) = expr_evt () in
          new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n));
          start_ctrl ctrl new_ctrl;
          f unit_value

    let rml_control' (n, _, _) p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        new_ctrl.cond <- (fun () -> R.Event.status n && is_eoi (R.Event.clock_domain n));
        fun () ->
          start_ctrl ctrl new_ctrl;
          f ()

(**************************************)
(* control_match                      *)
(**************************************)
    let rml_control_match expr_evt matching p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        fun _ ->
          let (n, _, _) = expr_evt () in
          new_ctrl.cond <-
            (fun () -> R.Event.status n && matching (R.Event.value n)
              && is_eoi (R.Event.clock_domain n));
          start_ctrl ctrl new_ctrl;
          f ()

    let rml_control_match' (n, _, _) matching p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        new_ctrl.cond <-
          (fun () -> R.Event.status n && matching (R.Event.value n)
            && is_eoi (R.Event.clock_domain n));
        fun () ->
          start_ctrl ctrl new_ctrl;
          f ()

(**************************************)
(* control_conf                       *)
(**************************************)

    let rml_control_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
        fun _ ->
          let cond, _ = expr_cfg true in
          new_ctrl.cond <- cond;
          start_ctrl ctrl new_ctrl;
          f unit_value

(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl jp cd p (n,wa,_) =
      let dummy = ref dummy_step in
      let new_ctrl = new_ctrl (When dummy) in
      let when_act _ =
        new_ctrl.susp <- false;
        next_to_current cd new_ctrl
      in
      let rec f_when _ =
        if R.Event.status n then
          when_act unit_value
        else
          do_when_ctrl_and_w cd ctrl wa when_act unit_value
      in
      let f = p (end_ctrl f_k new_ctrl) new_ctrl None cd in
      dummy := f_when;
      fun () ->
        start_ctrl ctrl new_ctrl;
        new_ctrl.cond <- (fun () -> R.Event.status n);
        new_ctrl.susp <- true;
        R.add_next f new_ctrl.next;
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
        fun _ -> raise R.RML

(**************************************)
(* clock domain                       *)
(**************************************)

    let rml_new_clock_domain () =
      R.mk_clock_domain ()

    let next_instant_clock_domain cd () =
      R.next_instant cd

    let step_clock_domain cd new_cd new_ctrl =
      let rec f_cd () =
        Format.printf "@.Step clock domain@.";
        R.schedule new_cd;
        R.eoi new_cd;
        if R.macro_step_done new_cd then (
          R.add_weoi cd (next_instant_clock_domain new_cd);
          R.add_next f_cd (control_tree cd).next;
        ) else (
          R.next_instant new_cd;
          (* execute again in the same step but yield for now*)
          R.add_current f_cd cd
        )
      in
      f_cd

    let end_clock_domain f_k ctrl x =
      end_ctrl f_k ctrl x

    let rml_at_clock_domain new_cd p =
      fun f_k ctrl jp cd ->
        let new_ctrl = control_tree new_cd in
        let f = p (end_clock_domain f_k ctrl) new_ctrl None new_cd in
        fun _ ->
          add_current f new_cd;
          start_ctrl new_ctrl ctrl;
          step_clock_domain cd new_cd new_ctrl unit_value


(**************************************)
(* pauseclock                         *)
(**************************************)
    let rml_pauseclock' pause_cd =
      fun f_k ctrl jp cd _ ->
        R.set_pauseclock pause_cd;
        rml_pause_at' pause_cd f_k ctrl jp cd ()

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

    module R = R

end (* Module Rml_interpreter *)


module Lco_ctrl_tree_seq_interpreter =
  Rml_interpreter(Seq_runtime.SeqListRuntime(Seq_runtime.SimpleStep)(Sig_env.Record))
