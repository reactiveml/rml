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

(* open Runtime_options *)
open Runtime

module Rml_interpreter =
  functor (R : Runtime.CONTROL_TREE_R) ->
  struct
    type 'a expr =
      'a step ->
      R.state_frame ->
      unit step
    and 'a process = unit -> 'a expr

    type ('a, 'b) event = ('a, 'b) R.event
    type 'a memory = ('a -> 'a, 'a) R.event
    type event_cfg_gen = unit -> R.event_cfg
    type clock_expr = R.clock Rml_types.clock
    type region_expr = clock_expr

    let unit_value = ()
    let dummy_step _ _ = ()

    open R
    open Rml_types

    let rml_top_clock = CkTop
    let rml_base_clock = CkLocal
    let toplevel_id = -1

    let eval_clock_expr current_cd ce = match ce with
      | CkLocal -> R.clock current_cd
      | CkTop -> R.top_clock ()
      | CkExpr e -> e

    let ensure_clock_expr ce = match ce with
      | CkExpr e -> e
      | CkTop -> R.top_clock ()
      | CkLocal -> (* should be rejected by compiler *)
          (* print_debug "Error: Unexpected local clock@."; *)
          raise Rml_types.RML

    let rec on_event_at_eoi evt ctrl f =
      let eoi_work id _ =
        try
          f id ()
        with
        | R.Wait_again -> on_event_at_eoi evt ctrl f id
      in
      R.on_event evt ctrl
        (fun id () -> R.on_eoi (R.Event.clock id evt) eoi_work id)

    let on_event_cfg_at_eoi evt_cfg ctrl f = ()

(* ------------------------------------------------------------------------ *)
    let rml_pre_status evt = R.Event.pre_status toplevel_id evt

    let rml_pre_value evt = R.Event.pre_value toplevel_id evt

    let rml_last evt = R.Event.last toplevel_id evt

    let rml_default evt = R.Event.default toplevel_id evt

    let rml_last_mem evt = R.Event.last toplevel_id evt

    let rml_clock evt = CkExpr (R.Event.clock toplevel_id evt)

(* ------------------------------------------------------------------------ *)

    let default_combine x y = x :: y
    let default_default = []

    let rml_global_signal_combine default combine =
      R.Event.new_evt_global toplevel_id Rml_types.Signal default combine

    let rml_global_signal_memory_combine default combine =
      R.Event.new_evt_global toplevel_id Rml_types.Memory default combine

    let rml_global_signal () =
      rml_global_signal_combine default_default default_combine

(* ------------------------------------------------------------------------ *)

    let cfg_present' evt _ =
      R.Event.cfg_present evt
    let cfg_present expr_evt _ =
      let evt = expr_evt () in
      R.Event.cfg_present evt
    let cfg_and ev1 ev2 _ =
      R.Event.cfg_and (ev1 ()) (ev2 ())
    let cfg_or ev1 ev2 _ =
      R.Event.cfg_or (ev1 ()) (ev2 ())

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k frame id _ ->
        f_k id unit_value


(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k frame id _ ->
        let v = e () in
        f_k id v

(**************************************)
(* pause                              *)
(**************************************)

    let rml_pause' pause_ce =
      fun f_k frame id _ ->
        let pause_ck = eval_clock_expr frame.st_cd pause_ce in
        R.on_eoi pause_ck
          (fun id () -> R.on_next_instant frame.st_ctrl f_k id)
          id

    (* let rml_pause e = *)
    (*   fun f_k ctrl jp cd _ -> *)
    (*     rml_pause' (e ()) f_k ctrl jp cd unit_value *)

    let rml_pause =
      fun f_k frame id _ ->
        rml_pause' rml_base_clock f_k frame id ()


(**************************************)
(* weak pause                              *)
(**************************************)

    let rml_weak_pause' pause_ce =
      fun f_k frame id _ ->
        let pause_ck = eval_clock_expr frame.st_cd pause_ce in
        R.on_eoi pause_ck
          (fun id () -> R.on_next_instant ~kind:Weak frame.st_ctrl f_k id)
          id

    let rml_weak_pause e =
      fun f_k frame id _ ->
        rml_weak_pause' (e ()) f_k frame id unit_value

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k frame id _ ->
        ()

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' evt e =
      fun f_k frame id _ ->
        R.Event.emit id evt (e());
        f_k id unit_value

    let rml_emit_val expr_evt e =
      fun f_k frame id _ ->
        let evt = expr_evt() in
        rml_emit_val' evt e f_k frame id unit_value

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val evt v =
      R.Event.emit toplevel_id evt v

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)

    let rml_await_immediate' evt =
      fun f_k frame id _ ->
        R.on_event evt frame.st_ctrl f_k id

    let rml_await_immediate expr_evt =
      fun f_k frame id _ ->
        let evt = expr_evt() in
        rml_await_immediate' evt f_k frame id unit_value

(**************************************)
(* await_immediate_conf               *)
(**************************************)

    let rml_await_immediate_conf expr_cfg =
      fun f_k frame id _ ->
        R.on_event_cfg (expr_cfg ()) frame.st_ctrl f_k id

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' evt p =
      fun f_k frame id _ ->
        let f_get id _ =
          let x =
            if R.Event.status id evt then
              R.Event.value id evt
            else
              R.Event.default id evt
          in
          let f_body = p x f_k frame in
          R.on_next_instant frame.st_ctrl f_body id
        in
        R.on_eoi (R.Event.clock id evt) f_get id

    let rml_get expr_evt p =
      let evt = expr_evt() in
      rml_get' evt p

(**************************************)
(* await_immediate_one                *)
(**************************************)

    let rml_await_immediate_one' evt p =
      fun f_k frame id _ ->
        let f id _ =
          let x = R.Event.one id evt in
          p x f_k frame id unit_value
        in
        R.on_event evt frame.st_ctrl f id

     let rml_await_immediate_one expr_evt p =
       fun f_k frame id _ ->
         let evt = expr_evt() in
         rml_await_immediate_one' evt p f_k frame id unit_value

(**************************************)
(* await_all_match                    *)
(**************************************)

     let rml_await_all_match' evt matching p =
       fun f_k frame id _ ->
         let await_eoi id _ =
           let v = R.Event.value id evt in
           if matching v then
             R.on_next_instant frame.st_ctrl (p v f_k frame) id
           else
             raise Wait_again
         in
         on_event_at_eoi evt frame.st_ctrl await_eoi id

    let rml_await_all_match expr_evt matching p =
      fun f_k frame id _ ->
        let evt = expr_evt() in
        rml_await_all_match' evt matching p f_k frame id unit_value

(**************************************)
(* await                              *)
(**************************************)

    let rml_await' evt =
      fun f_k frame id _ ->
        let await_eoi id _ = R.on_next_instant frame.st_ctrl f_k id in
        on_event_at_eoi evt frame.st_ctrl await_eoi id

    let rml_await expr_evt =
      fun f_k frame id _ ->
        let evt = expr_evt () in
        rml_await' evt f_k frame id unit_value

    let rml_await_all' evt p =
      rml_await_all_match' evt (fun _ -> true) p

    let rml_await_all expr_evt p =
      fun f_k frame id _ ->
        let evt = expr_evt () in
        rml_await_all_match' evt (fun _ -> true) p
          f_k frame id unit_value

    let rml_await_one' evt p =
       fun f_k frame id _ ->
         let await_eoi id _ =
           let v = R.Event.one id evt in
           R.on_next_instant frame.st_ctrl (p v f_k frame) id
         in
         on_event_at_eoi evt frame.st_ctrl await_eoi id

    let rml_await_one expr_evt p =
      fun f_k frame id _ ->
        let evt = expr_evt () in
        rml_await_one' evt p f_k frame id unit_value

    let rml_await_conf expr_cfg =
      fun f_k frame id _ ->
        on_event_cfg_at_eoi (expr_cfg ())
          frame.st_ctrl (fun id () -> R.on_next_instant frame.st_ctrl f_k id)

(**************************************)
(* present                            *)
(**************************************)

    let rml_present' evt p_1 p_2 =
      fun f_k frame ->
        let f_1 = p_1 f_k frame in
        let f_2 = p_2 f_k frame in
        fun id () ->
          R.on_event_or_next evt f_1 frame.st_cd frame.st_ctrl f_2 id

    let rml_present expr_evt p_1 p_2 =
      fun f_k frame id _ ->
        let evt = expr_evt () in
        rml_present' evt p_1 p_2 f_k frame id unit_value

(**************************************)
(* present_conf                       *)
(**************************************)

    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k frame ->
        let f_1 = p_1 f_k frame in
        let f_2 = p_2 f_k frame in
        fun id () ->
          R.on_event_cfg_or_next (expr_cfg ()) f_1 frame.st_cd frame.st_ctrl f_2 id

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k frame ->
        let f_2 = p_2 f_k frame in
        let f_1 = p_1 (fun id _ -> f_2 id ()) { frame with st_jp = None } in
        f_1

(**************************************)
(* par                                *)
(**************************************)
(* Utilisation de Obj.magic pour le pb de la generalisation des *)
(* applications partielles.                                     *)

    let join_n cpt =
      fun f_k frame id _ ->
        if R.Join.decr cpt then
          f_k id unit_value

    let rml_ignore f_k id _ =
      f_k id ()

    let rml_par p_1 p_2 =
      fun f_k frame ->
        let i = match frame.st_jp with None -> 2 | Some _ -> 1 in
        let cpt, j =
          match frame.st_jp with
            | None ->
                let cpt = R.Join.new_join_point 0 in
                cpt, join_n cpt f_k frame
            | Some cpt -> cpt, f_k
        in
        let f_1 = p_1 (rml_ignore j) { frame with st_jp = Some cpt } in
        let f_2 = p_2 (rml_ignore j) { frame with st_jp = Some cpt } in
        fun id () ->
          R.Join.incr cpt i;
          R.on_current_instant frame.st_cd f_2 id;
          f_1 id unit_value

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
      fun _ frame ->
        let f_1 = ref dummy_step in
        let f_loop =
          p (fun id _ -> !f_1 id unit_value) { frame with st_jp = None }
        in
        f_1 := f_loop;
        f_loop

(**************************************)
(* loop_n                             *)
(**************************************)

    let rml_loop_n e p =
      fun f_k frame ->
        let cpt = R.Join.new_join_point 0 in
        let f_1 = ref dummy_step in
        let f_loop =
          p
            (fun id _ ->
              if R.Join.decr cpt then
                f_k id unit_value
              else
                !f_1 id unit_value)
            { frame with st_jp = None }
        in
        f_1 := f_loop;
        fun id _ ->
          let n = e() in
          if n > 0 then
            (R.Join.incr cpt (n - 1);
             f_loop id unit_value)
          else
            f_k id unit_value


(**************************************)
(* signal                             *)
(**************************************)


    let rml_signal_ f_k frame kind ce re default combine reset p id =
      let ck = eval_clock_expr frame.st_cd ce in
      let r = R.Event.region_of_clock id (eval_clock_expr frame.st_cd re) in
      let reset =
        match reset with
          | None -> None
          | Some r -> Some (eval_clock_expr frame.st_cd r)
      in
      let k evt = p evt f_k frame in
      R.Event.new_evt id frame.st_cd ck r kind default combine reset k id ()

    let rml_signal_combine default combine p =
      fun f_k frame id _ ->
        rml_signal_ f_k frame
          Rml_types.Signal rml_base_clock rml_base_clock
          (default ()) (combine ()) None p
          id

    let rml_signal_memory_combine default combine p =
      fun f_k frame id _ ->
        rml_signal_ f_k frame
          Rml_types.Memory rml_base_clock rml_base_clock
          (default ()) (combine ()) None p
          id

    let rml_signal p =
      rml_signal_combine
        (fun () -> default_default) (fun () -> default_combine)
        p

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k frame id _ ->
        p (e()) f_k frame id unit_value

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k frame ->
        p1
          (fun id v -> p2 v f_k { frame with st_jp = None } id unit_value)
          frame

(**************************************)
(* def_and_dyn                        *)
(**************************************)
(*
    let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
        fun f_k frame ->
          fun x ->
            value_array.(i) <- x;
            decr cpt;
            if !cpt = 0 then
              let f = p3 value_array f_k frame in
              f unit_value
            else
              sched()
      in
      fun p_array p3 ->
        fun f_k frame ->
          let n = Array.length p_array in
          let cpt = ref n in
          let value_array = Array.make n (Obj.magic()) in
          let step_init =
            fun _ ->
              cpt := n;
              for i = 0 to n - 1 do
                let f =
                  p_array.(i)
                    (join_n cpt value_array p3 i f_k frame)
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
      fun f_k frame id _ ->
        p (e()) f_k frame id unit_value


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k frame id _ ->
        (e ()) () f_k frame id unit_value


(**************************************)
(* if                                 *)
(**************************************)

    let rml_if e p_1 p_2 =
      fun f_k frame ->
        let f_1 = p_1 f_k frame in
        let f_2 = p_2 f_k frame in
        fun id () ->
          if e() then
            f_1 id unit_value
          else
            f_2 id unit_value

(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k frame ->
        let f_body = ref dummy_step in
        let f_while id _ =
          if e()
          then !f_body id unit_value
          else f_k id unit_value
        in
        f_body := p f_while { frame with st_jp = None };
        f_while


(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k frame ->
        let rec f_for i v2 id _ =
          incr i;
          if cmp !i v2
          then p !i (f_for i v2) { frame with st_jp = None } id unit_value
          else f_k id unit_value
        in
        let f_for_init id _ =
          let i = ref (e1()) in
          let v2 = e2() in
          if cmp !i v2
          then p !i (f_for i v2) { frame with st_jp = None } id unit_value
          else f_k id unit_value
        in
        f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)

    let rml_fordopar e1 e2 dir p =
      fun f_k frame ->
        let cpt = R.Join.new_join_point 0 in
        let j = join_n cpt f_k frame in
        let f_fordopar id _ =
          if dir then
            begin
              let min = e1() in
              let max = e2() in
              let n = max - min + 1 in
              R.Join.incr cpt n;
              if n <= 0 then
                f_k id unit_value
              else
                for i = max downto min do
                  let f = p i j { frame with st_jp = Some cpt } in
                  R.on_current_instant frame.st_cd f id
                done
            end
          else
            begin
              let max = e1() in
              let min = e2() in
              let n = max - min + 1 in
              R.Join.incr cpt n;
              if n <= 0 then
                f_k id unit_value
              else
                for i = min to max do
                  let f = p i j { frame with st_jp = Some cpt } in
                  R.on_current_instant frame.st_cd f id
                done
            end
        in
        f_fordopar

    let rml_par_n p_list =
      fun f_k frame ->
        let nb = List.length p_list in
        let i = match frame.st_jp with None -> nb | Some _ -> nb - 1 in
        let cpt, j =
          match frame.st_jp with
            | None ->
                let cpt = R.Join.new_join_point 0 in
                cpt, join_n cpt f_k frame
            | Some cpt -> cpt, f_k
        in
        let f_list =
          List.rev_map (fun p -> p j { frame with st_jp = Some cpt }) p_list
        in
        fun id _ ->
          R.Join.incr cpt i;
          R.on_current_instant_list frame.st_cd f_list id

    let rml_seq_n =
      let rec fold p_list k frame =
        match p_list with
        | [] -> k
        | [ p ] -> p k frame
        | p::p_list -> p (fold p_list k frame) { frame with st_jp = None }
      in
      fun p_list ->
        fun f_k frame ->
          let f =
            (* List.fold_right (fun p -> fun k -> p k ctrl None) p_list f_k  *)
            fold p_list f_k frame
          in f



(**************************************)
(* until                              *)
(**************************************)

    let true_cond _ = true

    let rml_until expr_evt (p : _ expr)  : _ expr =
      let pause_kind = Rml_types.Strong in
      fun f_k frame ->
        let body f_k new_ctrl =
          p f_k { frame with st_ctrl = new_ctrl;
                             st_jp = None; }
        in
        let f =
          create_control (Kill (pause_kind, f_k))
            body f_k frame.st_ctrl frame.st_cd
        in
        fun id () ->
          let evt = expr_evt () in
          f id evt true_cond id ()

    let rml_until' evt p =
      rml_until (fun () -> evt) p

    let rml_until_conf expr_cfg p =
      let pause_kind = Rml_types.Strong in
      fun f_k frame ->
        let body f_k new_ctrl =
          p f_k { frame with st_ctrl = new_ctrl;
                             st_jp = None; }
        in
        let f =
          create_control_evt_conf (Kill (pause_kind, f_k))
            body f_k frame.st_ctrl frame.st_cd
        in
        fun id () ->
          let evt_cfg = expr_cfg () in
          f evt_cfg id ()

(**************************************)
(* until handler                      *)
(**************************************)

    let rml_until_handler_local expr_evt matching_opt p p_handler =
      p (* XXX TODO XXX *)
      (* let pause_kind = Rml_types.Strong in *)
      (* fun f_k frame -> *)
      (*   let evt = ref (Obj.magic() : ('a, 'b) event) in *)
      (*   let handler _ id x = *)
      (*     let x = R.Event.value id !evt in *)
      (*     p_handler x f_k frame id x *)
      (*   in *)
      (*   let body f_k new_ctrl = *)
      (*     p f_k { frame with st_ctrl = new_ctrl; *)
      (*                        st_jp = None; } *)
      (*   in *)
      (*   let f = *)
      (*     create_control (Kill_handler (pause_kind, handler)) *)
      (*       body f_k frame.st_ctrl frame.st_cd *)
      (*   in *)
      (*   let cond = *)
      (*     match matching_opt with *)
      (*       | None -> true_cond *)
      (*       | Some cond -> cond *)
      (*   in *)
      (*   fun id () -> *)
      (*     evt := expr_evt (); *)
      (*     f id !evt cond id () *)

    let rml_until_handler_local' evt matching_opt p p_handler =
      rml_until_handler_local (fun () -> evt) matching_opt p p_handler

    let rml_until_handler (* pause_kind *) expr_evt p p_handler =
      rml_until_handler_local (* pause_kind *) expr_evt None p p_handler

    let rml_until_handler' (* pause_kind *) evt p p_handler =
      rml_until_handler_local' (* pause_kind *) evt None p p_handler

    let rml_until_handler_match (* pause_kind *) expr_evt matching p p_handler =
      rml_until_handler_local (* pause_kind *) expr_evt (Some matching) p p_handler

    let rml_until_handler_match' (* pause_kind *) evt matching p p_handler =
      rml_until_handler_local' (* pause_kind *) evt (Some matching) p p_handler

    let rml_until_handler_match_conf expr_cfg matching p p_handler =
      let pause_kind = Rml_types.Strong in
      fun f_k frame ->
        let body f_k new_ctrl =
          p f_k { frame with st_ctrl = new_ctrl;
                             st_jp = None; }
        in
        let f =
          create_control_evt_conf (Kill (pause_kind, f_k))
            body f_k frame.st_ctrl frame.st_cd
        in
        fun id () ->
          let evt_cfg = expr_cfg () in
          f evt_cfg id ()



(**************************************)
(* control                            *)
(**************************************)

    let step_control_static evt cond p =
      p (* XXX TODO XXX *)
    (*   fun f_k frame -> *)
    (*     let body f_k new_ctrl = *)
    (*       p f_k { frame with st_ctrl = new_ctrl; *)
    (*                          st_jp = None; } *)
    (*     in *)
    (*     create_control Susp body f_k frame.st_ctrl frame.st_cd evt cond *)

    let step_control expr_evt cond p =
      p (* XXX TODO XXX *)
    (*   fun f_k frame -> *)
    (*     let body f_k new_ctrl = *)
    (*       p f_k { frame with st_ctrl = new_ctrl; *)
    (*                          st_jp = None; } *)
    (*     in *)
    (*     let f = create_control Susp body f_k frame.st_ctrl frame.st_cd in *)
    (*     fun id () -> *)
    (*       let evt = expr_evt () in *)
    (*       f id evt cond id () *)

    let rml_control' evt p = step_control_static evt true_cond p

    let rml_control expr_evt p = step_control expr_evt true_cond p

    let rml_control_match' evt matching p = step_control_static evt matching p

    let rml_control_match expr_evt matching p = step_control expr_evt matching p

    (* let rml_control_conf expr_cfg p = *)
    (*   fun f_k frame -> *)
    (*     let body f_k new_ctrl = *)
    (*       p f_k { frame with st_ctrl = new_ctrl; *)
    (*                          st_jp = None; } *)
    (*     in *)
    (*     let f = *)
    (*       create_control_evt_conf Susp body f_k frame.st_ctrl frame.st_cd *)
    (*     in *)
    (*     fun id () -> *)
    (*       let evt_cfg = expr_cfg () in *)
    (*       f evt_cfg id () *)

(**************************************)
(* when                               *)
(**************************************)

    (* let rml_when expr_evt p = *)
    (*   fun f_k frame -> *)
    (*    let body f_k new_ctrl = *)
    (*       p f_k { frame with st_ctrl = new_ctrl; *)
    (*                          st_jp = None; } *)
    (*    in *)
    (*     let f = create_control When body f_k frame.st_ctrl frame.st_cd in *)
    (*     fun id () -> *)
    (*       let evt = expr_evt () in *)
    (*       f evt true_cond id () *)

    (* let rml_when' evt p = *)
    (*   fun f_k frame -> *)
    (*     let body f_k new_ctrl = *)
    (*       p f_k { frame with st_ctrl = new_ctrl; *)
    (*                          st_jp = None; } *)
    (*     in *)
    (*     R.create_control When body f_k frame.st_ctrl frame.st_cd evt true_cond *)

    (* let rml_when_conf expr_cfg = *)
    (*   fun f_k ctrl -> *)
    (*     fun id _ -> *)
    (*       (\* print_debug "Unimplemented when_conf@."; *\) *)
    (*       raise Rml_types.RML *)

(**************************************)
(* clock domain                       *)
(**************************************)

    let rml_newclock sch period p =
      fun f_k frame ->
        R.new_clock_domain toplevel_id frame.st_cd frame.st_ctrl
          (fun cd ctrl f_k ->
             p (CkExpr (R.clock cd)) f_k { frame with st_ctrl = ctrl;
                                                      st_jp = None;
                                                      st_cd = cd; })
          sch period f_k

(**************************************)
(* rml_make                           *)
(**************************************)

    let rml_make cd result p =
     (* Function to create the last continuation of a toplevel process *)
      let jp, join_end =
        let term_cpt = R.Join.new_join_point 0 in
        Some term_cpt,
        fun () ->
          R.Join.incr term_cpt 1;
          let f id x =
            if R.Join.decr term_cpt then
              result := Some x
          in f
      in
      p () (join_end()) { st_ctrl = (control_tree cd);
                          st_jp = jp;
                          st_cd = cd;
                          st_prob = None; }

    let rml_make p =
      let cd = R.get_top_clock_domain () in
      let result = ref None in
      let step = rml_make cd result p in
      R.on_current_instant cd step toplevel_id;
      fun () ->
        R.react cd toplevel_id;
        !result


    let rml_make_n cd result pl =
      let jp, join_end =
        let term_cpt = R.Join.new_join_point 0 in
        Some term_cpt,
        fun () ->
          R.Join.incr term_cpt (List.length pl);
          let f x =
            if R.Join.decr term_cpt then
              result := Some x
          in f
      in
      List.map (fun p -> p () (join_end ()) (control_tree cd) jp cd) pl

    module R = R

  end  (* Module Rml_interpreter *)

(* This module declaration is only used to check that the module is compatible
   with the signature Lco_interpreter.S. It should not be used. *)
(* module Fake = *)
(*   (Rml_interpreter : *)
(*      (functor (R : Runtime.CONTROL_TREE_R) -> Lco_interpreter.S)) *)
