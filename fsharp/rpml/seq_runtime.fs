﻿#indent "off"

module Seq_runtime

module Step = 
  begin
    type 'a t = 'a -> unit
  end

module ListDataStruct =
 begin
  type next = unit Step.t list ref
  type current = unit Step.t list ref
  type waiting_list = unit Step.t list ref

  let mk_current () = ref ([] : unit Step.t list)
  let add_current p c =
    c := p :: !c
  let add_current_list pl c =
    c := List.rev_append pl !c
  let add_current_waiting_list w c =
    c := List.rev_append !w !c;
    w := []
  let add_current_next next c =
    c := List.rev_append !next !c;
    next := []

  exception Empty_current
  let take_current c = match !c with
    | f :: l -> c := l; f
    | [] -> raise Empty_current

  let mk_waiting_list () = ref ([]:unit Step.t list)
  let add_waiting p w =
    w := p :: ! w

  let mk_next () = ref ([]:unit Step.t list)
  let add_next p next =
    next := p :: !next
  let add_next_next n1 n2 =
    n2 := List.rev_append !n1 !n2;
    n1 := []
  let clear_next next =
    next := []
  let is_empty_next next = 
    match !next with 
    | [] -> true
    | _-> false
   (* !next = [] *)
 end

module SeqRuntime =
  begin
    module D = ListDataStruct
    module E = Event

    exception RML

    type 'a step = 'a -> unit

    type control_tree =
        { kind: control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (unit -> bool);
          mutable children: control_tree list;
          next: D.next;
          next_control : D.next; (* contains control processes that should not be
                                  taken into account to see if macro step is done *)
          next_boi: D.next; }
    and control_type =
      | Clock_domain (*of clock_domain*)
      | Kill of unit step
      | Kill_handler of (unit -> unit step)
      | Susp
      | When

    and clock_domain =
        { cd_current : D.current;
          cd_pause_clock: bool ref; (* end of macro instant *)
          cd_eoi : bool ref; (* is it the eoi of this clock *)
          cd_weoi : D.waiting_list; (* processes waiting for eoi *)
          mutable cd_wake_up : D.waiting_list list;
          mutable cd_wake_up_next : D.next list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : E.clock;
          mutable cd_top : control_tree;
        }

    type ('a, 'b) event = ('a,'b, clock_domain) E.t * D.waiting_list * D.waiting_list
    type event_cfg =
      | Cevent of (bool -> bool) * clock_domain * D.waiting_list * D.waiting_list
      (* status, cd, wa, wp*)
      | Cand of event_cfg * event_cfg
      | Cor of event_cfg * event_cfg

    let unit_value = ()


    module Event =
      begin
        let new_evt_combine cd _default combine =
          (E.create cd cd.cd_clock _default combine, D.mk_waiting_list (), D.mk_waiting_list ())

        let new_evt cd =
          new_evt_combine cd [] (fun x y -> x :: y)

        let status only_at_eoi (n,_,_) =
          E.status n && (not only_at_eoi || !((E.clock_domain n).cd_eoi))

        let value (n,_,_) =
          if E.status n then
            E.value n
          else
            raise RML

        let one (n,_,_) = E.one n
        let pre_status (n,_,_) = E.pre_status n
        let pre_value (n,_,_) = E.pre_value n
        let last (n,_,_) = E.last n
        let _default (n,_,_) = E._default n
        let clock_domain (n,_,_) = E.clock_domain n

        let emit (n,wa,wp) v =
          E.emit n v;
          let sig_cd = E.clock_domain n in
          D.add_current_waiting_list wa sig_cd.cd_current;
          D.add_current_waiting_list wp sig_cd.cd_current

        let cfg_present ((n,wa,wp) as evt) =
          Cevent ((fun eoi -> status eoi evt), E.clock_domain n, wa, wp)
        let cfg_or ev1 ev2 =
          Cor (ev1, ev2)
        let cfg_and ev1 ev2 =
          Cand (ev1, ev2)

        let cfg_status only_at_eoi evt_cfg =
          let rec status k = match k with
            | Cevent (c, _, _, _) -> c only_at_eoi
            | Cand (cfg1, cfg2) -> status cfg1 && status cfg2
            | Cor (cfg1, cfg2) -> status cfg1 || status cfg2
          in
          status evt_cfg

        let cfg_events evt_cfg long_wait =
          let rec events k = match k with
            | Cevent (_, cd, wa, wp) -> [(if long_wait then wa else wp), cd]
            | Cand (cfg1, cfg2) | Cor (cfg1, cfg2) ->
              List.rev_append (events cfg1) (events cfg2)
          in
          events evt_cfg
      end

(**************************************)
(* control tree                       *)
(**************************************)
    let new_ctrl cond kind =
      { kind = kind;
        alive = true;
        susp = false;
        children = [];
        cond = cond;
        next = D.mk_next ();
        next_control = D.mk_next ();
        next_boi = D.mk_next (); }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      D.clear_next p.next;
      D.clear_next p.next_boi;
      List.iter set_kill p.children;
      p.children <- []

    let start_ctrl ctrl new_ctrl =
      if new_ctrl.alive then
        ctrl.children <- new_ctrl :: ctrl.children
      else (* reset new_ctrl *)
        (new_ctrl.alive <- true;
         new_ctrl.susp <- false;
         D.clear_next new_ctrl.next;
         D.clear_next new_ctrl.next_boi)

    let end_ctrl new_ctrl f_k x =
      set_kill new_ctrl;
      new_ctrl.alive <- false;
      f_k x

(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current cd =
      let rec eval pere p active =
        if p.alive then
          match p.kind with
            | Clock_domain _ -> true
            | Kill f_k ->
              if p.cond()
              then
                (D.add_next f_k pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active;
                 if active then 
                   next_to_current cd p
                 else 
                   next_to_father pere p;
                 true)
          | Kill_handler handler ->
              if p.cond()
              then
                (D.add_next (handler()) pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active;
                 if active then 
                   next_to_current cd p
                 else 
                   next_to_father pere p;
                 true)
          | Susp ->
              let pre_susp = p.susp in
              if p.cond() then p.susp <- not pre_susp;
              let active = active && not p.susp in
              if pre_susp
              then
                (if active then 
                   next_to_current cd p;
                 true)
              else
                (p.children <- eval_children p p.children active;
                 if active then 
                   next_to_current cd p
                 else 
                   if not p.susp then 
                     next_to_father pere p;
                 true)
          | When ->
              if p.susp
              then true
              else
                (p.susp <- true;
                 p.children <- eval_children p p.children false;
                 true)
        else
          (set_kill p;
           false)

      and eval_children p nodes active =
        List.filter (fun node -> eval p node active) nodes

      and next_to_current ck node =
        D.add_current_next node.next ck.cd_current;
        D.add_current_next node.next_control ck.cd_current;
        D.add_current_next node.next_boi ck.cd_current;
      and next_to_father pere node =
        D.add_next_next node.next pere.next;
        D.add_next_next node.next_control pere.next_control;
        D.add_next_next node.next_boi pere.next_boi;
      in
        cd.cd_top.children <- eval_children cd.cd_top cd.cd_top.children true;
        next_to_current cd cd.cd_top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current cd p =
      if p.alive && not p.susp then
        (D.add_current_next p.next cd;
         D.add_current_next p.next_control cd;
         List.iter (next_to_current cd) p.children;
         D.add_current_next p.next_boi cd)

    let wake_up_ctrl new_ctrl cd =
      new_ctrl.susp <- false;
      next_to_current cd.cd_current new_ctrl

    let is_active ctrl =
      ctrl.alive && not ctrl.susp

    let set_suspended ctrl v =
      ctrl.susp <- v

    let set_condition ctrl c =
      ctrl.cond <- c

    let mk_clock_domain () =
      { cd_current = D.mk_current ();
        cd_pause_clock = ref false;
        cd_eoi = ref false;
        cd_weoi = D.mk_waiting_list ();
        cd_wake_up = [];
        cd_wake_up_next =  [];
        cd_clock = E.init_clock ();
        cd_top = new_ctrl (fun () -> false) Clock_domain;
      }


    let top_clock_domain = mk_clock_domain ()
    let is_eoi cd = !(cd.cd_eoi)
    let set_pauseclock cd =
      cd.cd_pause_clock := true
    let control_tree cd = cd.cd_top
    let add_weoi cd p =
      D.add_waiting p cd.cd_weoi
    let add_weoi_waiting_list cd w =
      cd.cd_wake_up <- w :: cd.cd_wake_up
    let add_weoi_next cd n =
      cd.cd_wake_up_next <- n :: cd.cd_wake_up_next

(* debloquer les processus en attent d'un evt *)
    let wake_up ck w =
      D.add_current_waiting_list w ck.cd_current

    let wake_up_all ck =
      List.iter (fun wp -> D.add_current_waiting_list wp ck.cd_current) ck.cd_wake_up;
      ck.cd_wake_up <- []

    let wake_up_all_next cd =
      List.iter (fun n -> D.add_current_next n cd.cd_current) cd.cd_wake_up_next;
      cd.cd_wake_up_next <- []

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f = D.add_current f cd.cd_current
    let on_current_instant_list cd fl = D.add_current_list fl cd.cd_current
    let on_next_instant ctrl f = D.add_next f ctrl.next

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
    let on_eoi cd f =
      if is_eoi cd then
        f unit_value
      else
        add_weoi cd f

  (** [on_event_or_next evt f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let _on_event_or_next (_,_,w) f_w v_w cd ctrl f_next =
      let act _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          D.add_next f_next ctrl.next
        else
          (* signal activated *)
          f_w v_w
      in
      D.add_waiting act w;
      add_weoi_waiting_list cd w

    let on_event_or_next evt f_w v_w cd ctrl f_next =
      if Event.status false evt then
        f_w v_w
      else
        _on_event_or_next evt f_w v_w cd ctrl f_next

  (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next =
      if Event.cfg_status false evt_cfg then
        f_w v_w
      else
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            if is_eoi cd then
              (is_fired := true;
               D.add_next f_next ctrl.next)
            else
              (if Event.cfg_status false evt_cfg then
                  (is_fired := true;
                   f_w v_w))
        in
        let w_list = Event.cfg_events evt_cfg false in
        List.iter
          (fun (w,_) -> D.add_waiting try_fire w; add_weoi_waiting_list cd w) w_list


    (** [on_event evt ctrl f v] executes 'f v' if evt is emitted and
        ctrl is active in the same step.
        It waits for the next activation of w otherwise,
        or if the call raises Wait_again *)
    let _on_event w sig_cd ctrl f v =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          (try
            f v
           with
            | Wait_again -> D.add_waiting self w)
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_weoi sig_cd (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           D.add_waiting self w)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (try
             is_fired := true; f v
           with
             | Wait_again -> D.add_waiting self w)
      in
      D.add_waiting self w

    let on_event (n,w,_) ctrl f v =
      if E.status n then
        (try
           f v
         with
           | Wait_again -> _on_event w (E.clock_domain n) ctrl f v)
      else
        _on_event w (E.clock_domain n) ctrl f v

    (** [on_event_cfg evt_cfg ctrl f v] executes 'f v' if evt_cfg is true and
        ctrl is active in the same step.
        It waits for the next activation of evt_cfg otherwise,
        or if the call raises Wait_again *)
    let on_event_cfg evt_cfg ctrl f v  =
      let wait_event_cfg () =
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            (if Event.cfg_status false evt_cfg then
                (is_fired := true;
                 f v)
             else
                raise Wait_again)
        in
        let w_list = Event.cfg_events evt_cfg true in
        List.iter (fun (w,cd) -> _on_event w cd ctrl try_fire unit_value) w_list
      in
      if Event.cfg_status false evt_cfg then
        (try
           f v
         with
           | Wait_again -> wait_event_cfg ())
      else
        wait_event_cfg ()

    (** [on_event_at_eoi evt ctrl f] executes 'f ()' during the eoi
        (of evt's clock domain) if ctrl is active in the same step.
        Waits for the next activation of evt otherwise, or if the call
        raises Wait_again *)
    let _on_event_at_eoi sig_cd ctrl w f =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          add_weoi sig_cd eoi_work
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_weoi sig_cd (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
          (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           D.add_waiting self w)
      and ctrl_await is_fired _ =
        if not !is_fired then
         (* ctrl was activated, signal is present*)
          (is_fired :=  true;
           add_weoi sig_cd eoi_work)
      and eoi_work _ =
          (try
            f unit_value
           with
            | Wait_again -> D.add_waiting self w)
      in
        D.add_waiting self w

     let on_event_at_eoi (n,wa,_) ctrl f =
       let sig_cd = E.clock_domain n in
       if E.status n then
         let eoi_work _ =
           (try
              f unit_value
            with
              | Wait_again -> _on_event_at_eoi sig_cd ctrl wa f)
         in
         add_weoi sig_cd eoi_work
       else
         _on_event_at_eoi sig_cd ctrl wa f

    (** [on_event_cfg_at_eoi evt ctrl f] executes 'f ()' during the eoi
        (of evt_cfg's clock domain) if ctrl is active in the same step.
        Waits for the next activation of evt otherwise. *)
     let on_event_cfg_at_eoi evt_cfg ctrl f =
       if Event.cfg_status false evt_cfg then
         (* TODO: trouver le bon cd sur lequel attendre *)
         let w_list = Event.cfg_events evt_cfg true in
         let sig_cd = snd (List.hd w_list) in
         add_weoi sig_cd f
       else
         let is_fired = ref false in
         let f _ =
           if not !is_fired then
             (if Event.cfg_status false evt_cfg then
                 (is_fired := true;
                  D.add_next f ctrl.next)
              else
                 raise Wait_again)
         in
         let w_list = Event.cfg_events evt_cfg true in
         List.iter (fun (w,sig_cd) -> _on_event_at_eoi sig_cd ctrl w f) w_list



    let schedule cd =
      let ssched () =
        try
          let f = D.take_current cd.cd_current in
          f (); false
        with
            Empty_current -> true
      in
      while not (ssched ()) do
        ()
      done

    let eoi cd =
      cd.cd_eoi := true;
      wake_up cd cd.cd_weoi;
      wake_up_all cd;
      schedule cd

    let next_instant cd =
      eval_control_and_next_to_current cd;
      E.next cd.cd_clock;
      cd.cd_eoi := false;
      cd.cd_pause_clock := false

    let rec has_next ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain -> has_next_children ctrl
          | Kill _ | Kill_handler _ ->
            ctrl.cond () || has_next_children ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond ()) || (not ctrl.susp && not (ctrl.cond ())) in
              active && has_next_children ctrl
          | When ->
            not ctrl.susp && has_next_children ctrl
    and has_next_children ctrl =
      not (D.is_empty_next ctrl.next) || List.exists has_next ctrl.children

    let macro_step_done cd =
      !(cd.cd_pause_clock) || not (has_next cd.cd_top)

    let step_clock_domain ctrl new_ctrl cd new_cd =
      let next_instant_clock_domain _ = next_instant new_cd in
      let rec f_cd () =
        schedule new_cd;
        eoi new_cd;
        if macro_step_done new_cd then (
          add_weoi cd next_instant_clock_domain;
          D.add_next f_cd ctrl.next_control;
        ) else (
          next_instant new_cd;
          (* execute again in the same step but yield for now*)
          D.add_current f_cd cd.cd_current
        )
      in
      f_cd

    (* the react function *)
    let react cd =
      schedule cd;
      eoi cd;
      next_instant cd
  end
