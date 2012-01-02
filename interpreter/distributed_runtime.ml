

module Make (D: Runtime.SEQ_DATA_STRUCT) (C : Communication.S) (E: Sig_env.S) =
struct
    module D = D(struct
      type 'a t = 'a -> unit
    end)

    type site_msg =
        (* scheduling *)
        | Mnew_cd
        | Mcd_created of C.gid
        | Mstep (* do one global step of the clock domain *)
        | Mstep_done of C.gid (* a clock domain has finished its step *)
        | Mdone of C.gid (* global step done *)
        | Mpauseclock of C.gid
        | Meoi (* End of instant of clock domain *)
        | Mnext_instant (* Go to next instant *)
        | Mreq_has_next of C.gid
        | Mhas_next of C.gid
            (* Whether there is processes to execute in the next local step *)
        (* signals *)
        | Memit of C.gid (* Emit a value *)
        | Mvalue of C.gid (* Send the value of the signal to child cd *)
        | Msignal of C.gid (* Send the whole signal *)
        | Mreq_signal of C.gid (* Request the value of the signal *)

    module Callbacks = Callbacks.Make (C) (struct
      type t = site_msg
      let compare = compare
    end)


    module GidHandle = Dhandle_typed.Make (struct
      type t = C.gid
      let compare = compare
    end) (C)

    exception RML

    type clock = {
      ck_gid : C.gid;
      ck_parent : clock option;
      ck_clock : E.clock GidHandle.handle;
    }
    (** Saving and searching clock state *)
    type clock_state = (clock * E.clock_index) list

    type 'a step = 'a -> unit
    let unit_value = ()
    let dummy_step () = ()

    type control_tree =
        { kind: control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (unit -> bool);
          mutable cond_v : bool;
          mutable children: control_tree list;
          next: D.next;
          next_control : D.next; (* contains control processes that should not be
                                  taken into account to see if macro step is done *)
          mutable last_activation : clock_state;
        }
    and control_type =
      | Clock_domain of clock
      | Kill of unit step
      | Kill_handler of (unit -> unit step)
      | Susp
      | When

    type waiting_kind =
        | Wbefore_eoi of C.gid (* before the eoi *)
        | Weoi of C.gid (* eoi of the current clock domain *)
        | Wnext_instant of C.gid (* next instant of the current clock domain, after eoi*)
        | Wsignal_wa of C.gid (* On emission of signal *)
        | Wsignal_wp of C.gid (* On emission of signal or end of instant of parent clock domain *)

    module WaitingMap = Map.Make (struct
      type t = waiting_kind
      let compare = compare
    end)

    module SignalHandle = Dhandle.Make (struct
      type key = C.gid
      type ('a, 'b) value = ('a, 'b) E.t
      let compare = compare
    end) (C)

    type ('a, 'b) event = {
      ev_handle : ('a,'b) SignalHandle.handle;
      ev_clock : clock;
      ev_gid : C.gid
    }
    type event_cfg = unit
   (*   | Cevent of (bool -> bool) * clock * gid list
      (* status, cd, wa, wp*)
      | Cand of event_cfg * event_cfg
      | Cor of event_cfg * event_cfg *)

    type clock_domain =
        { cd_current : D.current;
          mutable cd_pause_clock: bool; (* end of macro instant *)
          mutable cd_eoi : bool; (* is it the eoi of this clock *)
          mutable cd_wake_up : waiting_kind list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : clock;
          mutable cd_top : control_tree;
          cd_parent_ctrl : control_tree option;

          cd_site : site;
          mutable cd_children_have_next : bool;
          mutable cd_remaining_async : int ref;
          mutable cd_emitted_signals : C.GidSet.t; (* signals emitted during current step *)
          mutable cd_remotes : C.SiteSet.t; (* remotes with child clock domains*)
        }

    and site = {
      mutable s_clock_domains : clock_domain C.GidMap.t;
      s_msg_queue : Callbacks.msg_queue;
      s_callbacks : Callbacks.dispatcher;
      mutable s_msg_thread : Thread.t option;
      s_clock_cache : E.clock GidHandle.cache;
      mutable s_signal_cache : SignalHandle.cache;
      mutable s_waiting : D.waiting_list WaitingMap.t; (* waiting lists for parent cds and slow signals *)
    }

    let find_waiting site wk =
      try
        WaitingMap.find wk site.s_waiting
      with
        | Not_found ->
            let w = D.mk_waiting_list () in
            site.s_waiting <- WaitingMap.add wk w site.s_waiting;
            w
    let add_waiting site wk f =
      let w = find_waiting site wk in
        D.add_waiting f w
    let wake_up_now site wk =
      let w = find_waiting site wk in
      List.iter (fun f -> f ()) (D.take_all w)

    let add_callback site msg f =
      Callbacks.add_callback msg f site.s_callbacks

    let get_clock site ck =
      GidHandle.get site.s_clock_cache ck
    let get_clock_domain site ck =
      C.GidMap.find ck.ck_gid site.s_clock_domains
    let get_event site ev =
      SignalHandle.get site.s_signal_cache ev.ev_handle

    let process_msgs site =
      Callbacks.await_new_msg site.s_msg_queue;
      Callbacks.dispatch_all site.s_msg_queue site.s_callbacks


    module Clock = struct
      let equal_clock ck1 ck2 =
        ck1.ck_gid = ck2.ck_gid

      let mk_clock site parent_ck =
        let gid = C.fresh () in
        { ck_gid = gid;
          ck_parent = parent_ck;
          ck_clock = GidHandle.init site.s_clock_cache gid (E.init_clock ()) }

    let rec top_clock ck = match ck.ck_parent with
      | None -> ck
      | Some ck -> top_clock ck

      let get_clock_index site ck =
        E.get (GidHandle.get site.s_clock_cache ck.ck_clock)

      (** Returns the current index of ck and all its ancestors *)
      let rec save_clock_state site ck =
        let l = match ck.ck_parent with
          | None -> []
          | Some ck -> save_clock_state site ck
        in
          (ck, get_clock_index site ck)::l

      (** [check_clock_state st clock] checks that the index of [clock] saved in
          [st] is the same as its current index. *)
      let check_clock_state site st clock =
        let rec check_last_activation l = match l with
          | [] -> false
          | (ck, saved_idx)::l ->
              if equal_clock ck clock then
                E.equal saved_idx (get_clock_index site ck)
              else
                check_last_activation l
        in
          check_last_activation st
    end

    module Event =
      struct
        let lift_handle f cd ev =
          f (get_event cd.cd_site ev)

        let value cd ev =
          let n = get_event cd.cd_site ev in
          if E.status n then
            E.value n
          else
            raise RML

        let one cd ev = lift_handle E.one cd ev
        let pre_status cd ev = lift_handle E.pre_status cd ev
        let pre_value cd ev = lift_handle E.pre_value cd ev
        let last cd ev = lift_handle E.last cd ev
        let default cd ev = lift_handle E.default cd ev
        let clock _ ev = ev.ev_clock

        let send_value_to_remotes site ev n () =
          let cd = get_clock_domain site ev.ev_clock in
          C.SiteSet.iter
            (fun r -> C.send r (Mvalue ev.ev_gid) (E.value n))
            cd.cd_remotes

        let do_emit site ev v =
          let n = get_event site ev in
          let cd = get_clock_domain site ev.ev_clock in
          E.emit n v;
          (* wake up processes awaiting this signal *)
          wake_up_now site (Wsignal_wa ev.ev_gid);
          wake_up_now site (Wsignal_wp ev.ev_gid);
          (* if we have remote clock domains, we should send them the value later *)
          if not (C.SiteSet.is_empty cd.cd_remotes) &&
            not (C.GidSet.mem ev.ev_gid cd.cd_emitted_signals) then (
              add_waiting site (Wbefore_eoi ev.ev_clock.ck_gid) (send_value_to_remotes site ev n);
              cd.cd_emitted_signals <- C.GidSet.add ev.ev_gid cd.cd_emitted_signals
            )

        let emit cd ev v =
          if C.is_local ev.ev_gid then (
            do_emit cd.cd_site ev v
          ) else
            C.send_owner ev.ev_gid (Memit ev.ev_gid) v

        (* Called after receiving Mreq_signal id *)
        let receive_req ev msg =
          let id:C.gid = C.from_msg msg in
            C.send_owner id (Msignal id) ev

        (* Called after receiving Memit id *)
        let receive_emit site cd (ev:('a, 'b) event) msg =
          let v:'a = C.from_msg msg in
            do_emit site ev v

        (* Called after receiving Mvalue id *)
        let update_local_value site gid (n : ('a, 'b) E.t) msg =
          let v:'b = C.from_msg msg in
            E.set_value n v;
            wake_up_now site (Wsignal_wa gid)

        (* Called after receiving Msignal id *)
        let replace_local_value site (n : ('a, 'b) E.t) msg =
          let new_n:('a, 'b) E.t = C.from_msg msg in
            E.copy n new_n

        (* Called when a local process tries to access a remote signal for the first time.
           Creates local callbacks for this signal. *)
        let signal_local_value site gid n =
          add_callback site (Mvalue gid) (update_local_value site gid n);
          add_callback site (Msignal gid) (replace_local_value site n);
          n

        let new_evt_combine cd ck default combine =
          let n = E.create (get_clock cd.cd_site ck.ck_clock) default combine in
          let gid = C.fresh () in
          let ev  =
            { ev_handle = SignalHandle.init cd.cd_site.s_signal_cache gid n;
              ev_gid = gid;
              ev_clock = ck }
          in
          let cd = get_clock_domain cd.cd_site ck in
            (*WaitingMap.add (Wsignal_wa gid) (D.mk_waiting_list ()) s.s_waiting;
            WaitingMap.add (Wsignal_wp gid) (D.mk_waiting_list ()) s.s_waiting; *)
            add_callback cd.cd_site (Memit gid) (receive_emit cd.cd_site cd ev);
            add_callback cd.cd_site (Mreq_signal gid) (receive_req ev);
            ev

        let new_evt cd ck =
          new_evt_combine cd ck [] (fun x y -> x :: y)

        let status ?(only_at_eoi=false) cd ev =
          let n = get_event cd.cd_site ev in
          E.status n (* && (not only_at_eoi || !(sig_cd.cd_eoi)) *)
            (** TODO: remettre only_at_eoi pour les configs *)

        let cfg_present cd ev = ()
        let cfg_or cd c1 c2 = ()
        let cfg_and cd c1 c2 = ()
        let cfg_status ?(only_at_eoi=false) cd evt_cfg =
          false

        (* let cfg_present ((n,sig_cd,wa,wp) as evt) =
          Cevent ((fun eoi -> status ~only_at_eoi:eoi evt), sig_cd, wa, wp)
        let cfg_or ev1 ev2 =
          Cor (ev1, ev2)
        let cfg_and ev1 ev2 =
          Cand (ev1, ev2)

        let cfg_status ?(only_at_eoi=false) evt_cfg =
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
        *)
      end

(**************************************)
(* control tree                       *)
(**************************************)
    let new_ctrl ?(cond = (fun () -> false)) kind =
      { kind = kind;
        alive = true;
        susp = false;
        children = [];
        cond = cond;
        cond_v = false;
        last_activation = [];
        next = D.mk_next ();
        next_control = D.mk_next () }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      D.clear_next p.next;
      List.iter set_kill p.children;
      p.children <- []

    let start_ctrl ctrl new_ctrl =
      if new_ctrl.alive then
        ctrl.children <- new_ctrl :: ctrl.children
      else (* reset new_ctrl *)
        (new_ctrl.alive <- true;
         new_ctrl.susp <- false;
         D.clear_next new_ctrl.next)

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
            | Clock_domain ck ->
                if not (C.is_local ck.ck_gid) then (
                  incr cd.cd_remaining_async;
                  C.send_owner ck.ck_gid Mnext_instant
                    (cd.cd_clock, Clock.get_clock_index cd.cd_site cd.cd_clock)
                );
                true
            | Kill f_k ->
              if p.cond_v
              then
                (D.add_next f_k pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active;
                 if active then next_to_current cd p
                 else next_to_father pere p;
                 true)
          | Kill_handler handler ->
              if p.cond_v
              then
                (set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active;
                 if active then next_to_current cd p
                 else next_to_father pere p;
                 true)
          | Susp ->
              let pre_susp = p.susp in
              if p.cond_v then p.susp <- not pre_susp;
              let active = active && not p.susp in
              if pre_susp
              then
                (if active then next_to_current cd p;
                 true)
              else
                (p.children <- eval_children p p.children active;
                 if active then next_to_current cd p
                 else if not p.susp then next_to_father pere p;
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

      and next_to_current cd node =
        node.last_activation <- Clock.save_clock_state cd.cd_site cd.cd_clock;
        D.add_current_next node.next cd.cd_current;
        D.add_current_next node.next_control cd.cd_current;
      and next_to_father pere node = ()
       (* D.add_next_next node.next pere.next;
        D.add_next_next node.next_control pere.next_control
        *)
        (* TODO: ne marche plus si on veut que last_activation soit correct *)
      in
        cd.cd_top.children <- eval_children cd.cd_top cd.cd_top.children true;
        next_to_current cd cd.cd_top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current cd p =
      if p.alive && not p.susp then
        (D.add_current_next p.next cd.cd_current;
         D.add_current_next p.next_control cd.cd_current;
         p.last_activation <- Clock.save_clock_state cd.cd_site cd.cd_clock;
         List.iter (next_to_current cd) p.children)


    (** Evaluates the condition of control nodes. This can be called several
        times for a same control node, zhen doing the eoi of several clocks.
        We can keep the last condition (if it was true for the eoi of the fast clock,
        it is also true for the eoi of the slow clock), but we have to make sure
        to fire the handler only once. *)
    let eoi_control ctrl =
      let rec _eoi_control pere ctrl =
        if ctrl.alive then (
          (match ctrl.kind with
            | Clock_domain ck when not (C.is_local ck.ck_gid) -> ()
            | _ ->
                ctrl.cond_v <- ctrl.cond ();
                (match ctrl.kind with
                  | Kill_handler handler ->
                      if ctrl.cond_v then (
                        D.add_next (handler()) pere.next;
                        set_kill ctrl
                      )
                  | _ -> ());
                List.iter (_eoi_control ctrl) ctrl.children;
          )
        )
      in
        List.iter (_eoi_control ctrl) ctrl.children

    let rec has_next cd ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain ck ->
              if C.is_local ck.ck_gid then
                has_next_children cd ctrl
              else (
                (* waiting for a Has_next message from this clock domain *)
                incr cd.cd_remaining_async;
                C.send_owner ck.ck_gid (Mreq_has_next ck.ck_gid) ();
                false
              )
          | Kill _ | Kill_handler _ ->
            ctrl.cond () || has_next_children cd ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond ()) || (not ctrl.susp && not (ctrl.cond ())) in
              active && has_next_children cd ctrl
          | When ->
            not ctrl.susp && has_next_children cd ctrl
    and has_next_children cd ctrl =
      not (D.is_empty_next ctrl.next) || List.exists (has_next cd) ctrl.children

    let wake_up_ctrl new_ctrl cd =
      new_ctrl.susp <- false;
      next_to_current cd new_ctrl

    let is_active ctrl =
      ctrl.alive && not ctrl.susp

    let set_suspended ctrl v =
      ctrl.susp <- v

    let set_condition ctrl c =
      ctrl.cond <- c

    (** clock domains operations *)
    let mk_clock_domain site clock parent_ctrl =
      let cd = { cd_site = site;
        cd_current = D.mk_current ();
        cd_pause_clock = false;
        cd_eoi = false;
        cd_wake_up = [];
        cd_clock = clock;
        cd_top = new_ctrl (Clock_domain clock);
        cd_parent_ctrl = parent_ctrl;
        cd_children_have_next = false;
        cd_remaining_async = ref 0;
        cd_emitted_signals = C.GidSet.empty;
        cd_remotes = C.SiteSet.empty;
      } in
      site.s_clock_domains <- C.GidMap.add clock.ck_gid cd site.s_clock_domains;
      cd

    let is_eoi cd = cd.cd_eoi
    let control_tree cd = cd.cd_top
    let clock cd = cd.cd_clock
    let add_weoi_waiting_list cd wk =
      cd.cd_wake_up <- wk :: cd.cd_wake_up

    let top_clock cd =
      Clock.top_clock cd.cd_clock

(*
    let step_clock_domain ctrl new_ctrl cd new_cd =
      let next_instant_clock_domain _ = next_instant new_cd in
      let rec f_cd () =
        schedule new_cd;
        eoi new_cd;
        if macro_step_done new_cd then (
          add_waiting site (Wnext_instant ck) next_instant_clock_domain;
          D.add_next f_cd ctrl.next_control;
        ) else (
          next_instant new_cd;
          (* execute again in the same step but yield for now*)
          D.add_current f_cd cd.cd_current
        )
      in
      f_cd
        *)

    let await_all_children cd =
      while !(cd.cd_remaining_async) <> 0 do
        process_msgs cd.cd_site
      done

    let macro_step_done cd =
      cd.cd_children_have_next <- false;
      let has_next_ctrl = has_next cd cd.cd_top in
        (* Awaits Mhas_next from all remote clock domains *)
        await_all_children cd;
        cd.cd_pause_clock || not (has_next_ctrl || cd.cd_children_have_next)

    (* cd.cd_current_lock should be locked when calling schedule and is still locked
       when it returns.  *)
    let schedule cd =
      let ssched () =
        try
          let f = D.take_current cd.cd_current in
          f (); false
        with
            D.Empty_current -> true
      in
      while not (ssched ()) do
        ()
      done

    let eoi cd =
      wake_up_now cd.cd_site (Wbefore_eoi cd.cd_clock.ck_gid);
      cd.cd_eoi <- true;
      C.SiteSet.iter (fun r -> C.send r Meoi cd.cd_clock.ck_gid) cd.cd_remotes;
      eoi_control cd.cd_top;
      wake_up_now cd.cd_site (Weoi cd.cd_clock.ck_gid);
      List.iter (wake_up_now cd.cd_site) cd.cd_wake_up;
      cd.cd_wake_up <- []

    let next_instant cd =
      E.next (get_clock cd.cd_site cd.cd_clock.ck_clock);
      (* next instant of child clock domains *)
      wake_up_now cd.cd_site (Wnext_instant cd.cd_clock.ck_gid);
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd;
      (* reset the clock domain *)
      cd.cd_eoi <- false;
      cd.cd_pause_clock <- false;
      cd.cd_emitted_signals <- C.GidSet.empty;
      cd.cd_children_have_next <- false

    let rec exec_cd cd () =
      schedule cd;
      if !(cd.cd_remaining_async) = 0 then (
        eoi cd;
        match cd.cd_clock.ck_parent with
          | None -> (* top clock domain *)
              next_instant cd
          | Some ck ->
              if macro_step_done cd then (
                (* if the parent clock is not here, send Done message*)
                if not (C.is_local ck.ck_gid) then
                  C.send_owner ck.ck_gid (Mstep_done ck.ck_gid) ()
                else (
                  add_waiting cd.cd_site
                    (Wnext_instant ck.ck_gid) (fun () -> next_instant cd);
                  (match cd.cd_parent_ctrl with
                    | None -> assert false
                    | Some ctrl -> D.add_next (exec_cd cd) ctrl.next_control)
                )
              ) else (
                next_instant cd;
                (* do another step*)
                exec_cd cd ()
              )
      )



  let receive_req_has_next cd _ =
      let has_next = has_next cd cd.cd_top in
      match cd.cd_clock.ck_parent with
        | None -> assert false
        | Some ck -> C.send_owner ck.ck_gid (Mhas_next ck.ck_gid) has_next

    let gather_has_next cd msg =
      let has_next:bool = C.from_msg msg in
      decr cd.cd_remaining_async;
      cd.cd_children_have_next <- has_next or cd.cd_children_have_next

    let set_pauseclock cd ck =
      if C.is_local ck.ck_gid then
        let cd = get_clock_domain cd.cd_site ck in
          cd.cd_pause_clock <- true
      else
        C.send_owner ck.ck_gid (Mpauseclock ck.ck_gid) ()

    let gather_pauseclock cd _ =
      cd.cd_pause_clock <- true


    let step_remote_clock_domain ck_id () =
      C.send_owner ck_id Mstep ck_id

    let receive_step_done cd ctrl remote_ck_id _ =
      D.add_next (step_remote_clock_domain remote_ck_id) ctrl.next_control;
      decr cd.cd_remaining_async;
      (* wake up cd to emit the done message *)
      if !(cd.cd_remaining_async) = 0 then
        exec_cd cd ()

    let receive_done_cd cd f_k _ =
      decr cd.cd_remaining_async;
      (* wake up cd to emit the done message *)
      if !(cd.cd_remaining_async) = 0 then
        exec_cd cd ();
      f_k ()



    let init_clock_domain s clock parent_ctrl =
      let cd = mk_clock_domain s clock parent_ctrl in
        add_callback s (Mhas_next clock.ck_gid) (gather_has_next cd);
        add_callback s (Mreq_has_next clock.ck_gid) (receive_req_has_next cd);
        add_callback s (Mpauseclock clock.ck_gid) (gather_pauseclock cd);
        cd

    let end_clock_domain new_cd new_ctrl f_k () =
      new_cd.cd_site.s_clock_domains <-
        C.GidMap.remove new_cd.cd_clock.ck_gid new_cd.cd_site.s_clock_domains;
      match new_cd.cd_clock.ck_parent with
        | Some ck ->
            if not (C.is_local ck.ck_gid) then
              C.send_owner ck.ck_gid (Mdone new_cd.cd_clock.ck_gid) ()
            else
              end_ctrl new_ctrl f_k ()
        | None -> assert false

    let new_local_clock_domain cd ctrl p f_k =
      let new_ck = Clock.mk_clock cd.cd_site (Some cd.cd_clock) in
      let new_cd = init_clock_domain cd.cd_site new_ck (Some ctrl) in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl f_k) in
      fun _ ->
        D.add_current f new_cd.cd_current;
        start_ctrl ctrl new_ctrl;
        exec_cd new_cd ()

    let create_cd s msg =
      let (tmp_id:C.gid), (parent_ck:clock),
        (p:clock_domain -> control_tree -> unit step -> unit step) =
        C.from_msg msg
      in
      let new_ck = Clock.mk_clock s (Some parent_ck) in
      C.send_owner tmp_id (Mcd_created tmp_id) new_ck;
      let new_cd = init_clock_domain s new_ck None in
      s.s_clock_domains <- C.GidMap.add new_ck.ck_gid new_cd s.s_clock_domains;
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl dummy_step) in
      D.add_current f new_cd.cd_current;
      exec_cd new_cd ()

    let start_remote_clock_domain cd ctrl f_k msg =
      let new_ck:clock = C.from_msg msg in
      let new_ctrl = new_ctrl (Clock_domain new_ck) in
      Callbacks.add_callback ~kind:Callbacks.Once
        (Mdone new_ck.ck_gid) (receive_done_cd cd f_k) cd.cd_site.s_callbacks;
      add_callback cd.cd_site (Mstep_done new_ck.ck_gid) (receive_step_done cd ctrl new_ck.ck_gid);
      start_ctrl ctrl new_ctrl;
      step_remote_clock_domain new_ck.ck_gid ()

    let new_remote_clock_domain site remote_site cd ctrl p f_k _ =
      incr cd.cd_remaining_async;
      let tmp_id = C.fresh () in
      add_callback site (Mcd_created tmp_id) (start_remote_clock_domain cd ctrl f_k);
      C.send remote_site Mnew_cd (tmp_id, cd.cd_clock, p)

    let should_be_distributed site cd =
      false

    let find_available_site site =
      C.available_site ()

    let new_clock_domain cd ctrl p f_k =
      if should_be_distributed cd.cd_site cd then (
        let remote_site = find_available_site cd.cd_site in
          cd.cd_remotes <- C.SiteSet.add remote_site cd.cd_remotes;
          new_remote_clock_domain cd.cd_site remote_site cd ctrl p f_k
      ) else
        new_local_clock_domain cd ctrl p f_k



    (** Sites operations *)

  (*  let rec exec_cds stop_at_end_step s =
      while true do
        List.iter (exec_cd s) s.s_current;
        s.s_current := [];
        process_msgs s
      done
  *)

    let is_master () =
      C.is_master ()

    let rec react cd =
      exec_cd cd ();
      if !(cd.cd_remaining_async) <> 0 then (
        process_msgs cd.cd_site;
        react cd
      )

   let start_cd site msg =
      let cd_id:C.gid = C.from_msg msg in
        try
          exec_cd (C.GidMap.find cd_id site.s_clock_domains) ()
        with
          | Not_found -> Format.eprintf "Received Start for unknown clock domain.@."

    let receive_eoi site msg =
        let ck_id:C.gid = C.from_msg msg in
          wake_up_now site (Weoi ck_id);
          (* evaluate control tree conditions for all clock domains*)
          C.GidMap.iter (fun _ cd -> eoi_control cd.cd_top) site.s_clock_domains

    let receive_next_instant site msg =
      (* update the status of the clock *)
      let ck:clock = C.from_msg msg in
        E.next (get_clock site ck.ck_clock);
        (* do next instant for all clock domains *)
        C.GidMap.iter (fun _ cd -> next_instant cd) site.s_clock_domains

    let init_site () =
      let wrong_m = (module struct
        let local_value _ v = v
      end : SignalHandle.LOCAL_VALUE) in
      let rec s = {
        s_clock_domains = C.GidMap.empty;
        (*  s_top_clock_domains = clock_domain list; *)
        s_msg_queue = Callbacks.mk_queue ();
        s_callbacks = Callbacks.mk_dispatcher ();
        s_waiting = WaitingMap.empty;
        s_msg_thread = None;
        s_clock_cache = GidHandle.mk_cache (fun ck -> ck);
        s_signal_cache = SignalHandle.mk_cache wrong_m;
      } in
      let m = (module struct
        let local_value id ev = Event.signal_local_value s id ev
      end : SignalHandle.LOCAL_VALUE) in
      s.s_signal_cache <- SignalHandle.mk_cache m;
      add_callback s Mnew_cd (create_cd s);
      add_callback s Mstep (start_cd s);
      add_callback s Meoi (receive_eoi s);
      add_callback s Mnext_instant (receive_next_instant s);
      s.s_msg_thread <- Some (Thread.create Callbacks.receive s.s_msg_queue);
      s

    let start_slave () =
      let s = init_site () in
      while true do
        process_msgs s
      done

    let mk_top_clock_domain () =
      let site = init_site () in
      let ck = Clock.mk_clock site None in
      mk_clock_domain site ck None

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f = D.add_current f cd.cd_current
    let on_current_instant_list cd fl = D.add_current_list fl cd.cd_current
    let on_next_instant ctrl f = D.add_next f ctrl.next

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
    let on_eoi cd ck f =
      let cd = get_clock_domain cd.cd_site ck in
        if C.is_local ck.ck_gid && is_eoi cd then
          f unit_value
        else
          add_waiting cd.cd_site (Weoi ck.ck_gid) f

  (** [on_event_or_next evt f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let _on_event_or_next ev f_w v_w cd ctrl f_next =
      let act _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          D.add_next f_next ctrl.next
        else
          (* signal activated *)
          f_w v_w
      in
      add_waiting cd.cd_site (Wsignal_wp ev.ev_gid) act;
      add_weoi_waiting_list cd (Wsignal_wp ev.ev_gid)

    let on_event_or_next ev f_w v_w cd ctrl f_next =
      let n = get_event cd.cd_site ev in
      if E.status n then
        f_w v_w
      else
        _on_event_or_next ev f_w v_w cd ctrl f_next

    let on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next =
      ()

(*
  (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next =
      if Event.cfg_status evt_cfg then
        f_w v_w
      else
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            if is_eoi cd then
              (is_fired := true;
               D.add_next f_next ctrl.next)
            else
              (if Event.cfg_status evt_cfg then
                  (is_fired := true;
                   f_w v_w))
        in
        let w_list = Event.cfg_events evt_cfg false in
        List.iter
          (fun (w,_) -> D.add_waiting try_fire w; add_weoi_waiting_list cd w) w_list
*)

    (** [on_event evt ctrl f v] executes 'f v' if evt is emitted and
        ctrl is active in the same step.
        It waits for the next activation of w otherwise,
        or if the call raises Wait_again *)
    let _on_event site ev ctrl f v =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          (try
            f v
          with
            | Wait_again -> add_waiting site (Wsignal_wa ev.ev_gid) self)
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_waiting site (Weoi ev.ev_clock.ck_gid) (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           add_waiting site (Wsignal_wa ev.ev_gid) self)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (try
             is_fired := true; f v
           with
             | Wait_again -> add_waiting site (Wsignal_wa ev.ev_gid) self)
      in
      add_waiting site (Wsignal_wa ev.ev_gid) self

    let on_event cd ev ctrl f v =
      let n = get_event cd.cd_site ev in
      if E.status n then
        (try
           f v
         with
           | Wait_again -> _on_event cd.cd_site ev ctrl f v)
      else
        _on_event cd.cd_site ev ctrl f v

    let on_event_cfg cd evt_cfg ctrl f v  =
      ()

(*
    (** [on_event_cfg evt_cfg ctrl f v] executes 'f v' if evt_cfg is true and
        ctrl is active in the same step.
        It waits for the next activation of evt_cfg otherwise,
        or if the call raises Wait_again *)
    let on_event_cfg evt_cfg ctrl f v  =
      let wait_event_cfg () =
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            (if Event.cfg_status evt_cfg then
                (is_fired := true;
                 f v)
             else
                raise Wait_again)
        in
        let w_list = Event.cfg_events evt_cfg true in
        List.iter (fun (w,cd) -> _on_event w cd ctrl try_fire unit_value) w_list
      in
      if Event.cfg_status evt_cfg then
        (try
           f v
         with
           | Wait_again -> wait_event_cfg ())
      else
        wait_event_cfg ()
*)

    let has_been_active site ctrl ev =
      Clock.check_clock_state site ctrl.last_activation ev.ev_clock

    (** [on_event_at_eoi evt ctrl f] executes 'f ()' during the eoi
        (of evt's clock domain) if ctrl is active in the same step.
        Waits for the next activation of evt otherwise, or if the call
        raises Wait_again *)
    let _on_local_event_at_eoi site ev ctrl f =
      let rec self _ =
        if has_been_active site ctrl ev then
          (*ctrl is activated, run continuation*)
          add_waiting site (Weoi ev.ev_clock.ck_gid) eoi_work
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_waiting site (Weoi ev.ev_clock.ck_gid) (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
          (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           add_waiting site (Wsignal_wa ev.ev_gid) self)
      and ctrl_await is_fired _ =
        if not !is_fired then
         (* ctrl was activated, signal is present*)
          (is_fired :=  true;
           add_waiting site (Weoi ev.ev_clock.ck_gid) eoi_work)
      and eoi_work _ =
          (try
            f unit_value
          with
            | Wait_again -> add_waiting site (Wsignal_wa ev.ev_gid) self)
      in
      add_waiting site (Wsignal_wa ev.ev_gid) self

    let _on_remote_event_at_eoi site ev ctrl f =
      let rec self _ =
        if has_been_active site ctrl ev then
          add_waiting site (Weoi ev.ev_clock.ck_gid) eoi_work
        else
           add_waiting site (Wsignal_wa ev.ev_gid) self
      and eoi_work _ =
          (try
            f unit_value
          with
            | Wait_again -> add_waiting site (Wsignal_wa ev.ev_gid) self)
      in
        add_waiting site (Wsignal_wa ev.ev_gid) self

     let on_event_at_eoi cd ev ctrl f =
       let n = get_event cd.cd_site ev in
         if C.is_local ev.ev_gid then (
           if E.status n then
             let eoi_work _ =
               (try
                   f unit_value
                 with
                   | Wait_again -> _on_local_event_at_eoi cd.cd_site ev ctrl f)
             in
               add_waiting cd.cd_site (Weoi ev.ev_clock.ck_gid) eoi_work
           else
               _on_local_event_at_eoi cd.cd_site ev ctrl f
         ) else
             _on_remote_event_at_eoi cd.cd_site ev ctrl f

     let on_event_cfg_at_eoi cd evt_cfg ctrl f =
       ()

(*
    (** [on_event_cfg_at_eoi evt ctrl f] executes 'f ()' during the eoi
        (of evt_cfg's clock domain) if ctrl is active in the same step.
        Waits for the next activation of evt otherwise. *)
     let on_event_cfg_at_eoi evt_cfg ctrl f =
       if Event.cfg_status evt_cfg then
         (* TODO: trouver le bon cd sur lequel attendre *)
         let w_list = Event.cfg_events evt_cfg true in
         let sig_cd = snd (List.hd w_list) in
         add_weoi sig_cd f
       else
         let is_fired = ref false in
         let f _ =
           if not !is_fired then
             (if Event.cfg_status evt_cfg then
                 (is_fired := true;
                  D.add_next f ctrl.next)
              else
                 raise Wait_again)
         in
         let w_list = Event.cfg_events evt_cfg true in
         List.iter (fun (w,sig_cd) -> _on_event_at_eoi sig_cd ctrl w f) w_list
*)



end

module MpiRuntime = Make(Seq_runtime.ListDataStruct)(Mpi_communication)(Sig_env.Record)
