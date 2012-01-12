
module Make
  (D: Runtime.SEQ_DATA_STRUCT)
  (CF : functor (T : Communication.TAG_TYPE) -> Communication.S with type 'gid tag = 'gid T.t)
  (E: Sig_env.S) =
struct
    module D = D(struct
      type 'a t = 'a -> unit
    end)


    module SiteMsgs = struct
      type 'gid t =
          | Mfinalize
          | Mdummy
          (* scheduling *)
          | Mnew_cd
          | Mcd_created of 'gid
          | Mstep (* do one global step of the clock domain *)
          | Mstep_done of 'gid (* a clock domain has finished its step *)
          | Mdone of 'gid (* global step done *)
          | Mpauseclock of 'gid
          | Meoi (* End of instant of clock domain *)
          | Meoi_control of 'gid
          | Mnext_instant (* Go to next instant *)
          | Mhas_next of 'gid
              (* Whether there is processes to execute in the next local step *)
              (* signals *)
          | Memit of 'gid (* Emit a value *)
          | Mvalue of 'gid (* Send the value of the signal to child cd *)
          | Msignal of 'gid (* Send the whole signal *)
          | Mreq_signal of 'gid (* Request the value of the signal *)
          | Mcreate_signal
          | Msignal_created of 'gid

      open Format
      let print print_gid ff m = match m with
        | Mdummy -> fprintf ff "Mdummy"
        | Mfinalize -> fprintf ff "Mfinalize"
        | Mnew_cd -> fprintf ff "Mnew_cd"
        | Mcd_created gid -> fprintf ff "Mcd_created %a" print_gid gid
        | Mstep -> fprintf ff "Mstep"
        | Mstep_done gid -> fprintf ff "Mstep_done %a" print_gid gid
        | Mdone gid -> fprintf ff "Mdone %a" print_gid gid
        | Mpauseclock gid -> fprintf ff "Mpauseclock %a" print_gid gid
        | Meoi -> fprintf ff "Meoi"
        | Meoi_control gid -> fprintf ff "Meoi_control %a" print_gid gid
        | Mnext_instant -> fprintf ff "Mnext_instant"
        | Mhas_next gid -> fprintf ff "Mhas_next %a" print_gid gid
            (* Whether there is processes to execute in the next local step *)
        (* signals *)
        | Memit gid -> fprintf ff "Memit %a" print_gid gid
        | Mvalue gid -> fprintf ff "Mvalue %a" print_gid gid
        | Msignal gid -> fprintf ff "Msignal %a" print_gid gid
        | Mreq_signal gid -> fprintf ff "Mreq_signal %a" print_gid gid
        | Mcreate_signal -> fprintf ff "Mcreate_signal"
        | Msignal_created gid -> fprintf ff "Msignal_created %a" print_gid gid
    end
    open SiteMsgs

    module C = CF(SiteMsgs)

    module Callbacks = Callbacks.Make (C)
    module L = Load_balancer.Make (C)

    module GidHandle = Dhandle_typed.Make (struct
      type t = C.gid
      let compare = compare
    end) (C)

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
      type ('a, 'b) value = ('a, 'b) E.t * clock
      let compare = compare
    end) (C)

    type ('a, 'b) event = {
      ev_handle : ('a,'b) SignalHandle.handle;
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

          cd_load_balancer : L.load_balancer;
          mutable cd_children_have_next : bool;
          mutable cd_remaining_async : int ref;
          mutable cd_emitted_signals : C.GidSet.t; (* signals emitted during current step *)
          (* mutable cd_remotes : C.SiteSet.t; (* remotes with child clock domains*) *)
        }

    type site = {
      mutable s_clock_domains : clock_domain C.GidMap.t;
      s_msg_queue : Callbacks.msg_queue;
      s_callbacks : Callbacks.dispatcher;
      mutable s_msg_thread : Thread.t option;
      s_clock_cache : E.clock GidHandle.cache;
      mutable s_signal_cache : SignalHandle.cache;
      mutable s_waiting : D.waiting_list WaitingMap.t; (* waiting lists for parent cds and slow signals *)
      s_seed : C.seed;
      s_comm_site : C.site;
     (* mutable s_children : C.SiteSet.t; (* remotes with child clock domains*) *)
    }

    let print_clock ff ck =
      C.print_gid ff ck.ck_gid
    let print_cd ff cd =
      C.print_gid ff cd.cd_clock.ck_gid
    let print_signal ff ev =
      C.print_gid ff ev.ev_gid

    let get_site () =
      (Mpi.get_local_ref ():site)

    let find_waiting wk =
      let site = get_site () in
      try
        WaitingMap.find wk site.s_waiting
      with
        | Not_found ->
            let w = D.mk_waiting_list () in
            site.s_waiting <- WaitingMap.add wk w site.s_waiting;
            w
    let add_waiting wk f =
      let w = find_waiting wk in
        D.add_waiting f w
    let wake_up_now wk =
      let w = find_waiting wk in
      List.iter (fun f -> f ()) (D.take_all w)

    let add_callback msg f =
      let site = get_site () in
      Callbacks.add_callback msg f site.s_callbacks

    let get_clock ck =
      let site = get_site () in
      GidHandle.get site.s_clock_cache ck
    let get_clock_domain ck =
      try
        let site = get_site () in
        C.GidMap.find ck.ck_gid site.s_clock_domains
      with
        | Not_found -> Format.eprintf "Error: Cannot find clock domain %a@." C.print_gid ck.ck_gid; assert false
    let get_event ev =
      let site = get_site () in
      fst (SignalHandle.get site.s_signal_cache ev.ev_handle)
    let get_event_clock ev =
      let site = get_site () in
      snd (SignalHandle.get site.s_signal_cache ev.ev_handle)

    let process_msgs () =
      let site = get_site () in
      Callbacks.await_new_msg site.s_msg_queue;
      Callbacks.dispatch_all site.s_msg_queue site.s_callbacks

    module Msgs = struct
      let mk_send_recv tag =
        let send site (d:'a) = C.send site tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv

      let mk_broadcast_recv tag =
        let broadcast (d:'a) = C.broadcast tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        broadcast, recv

      let mk_send_owner_recv tag =
        let send gid (d:'a) = C.send_owner gid tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv

      let send_finalize, recv_finalize = mk_send_recv Mfinalize
      let send_dummy, recv_dummy = mk_send_recv Mdummy
      let send_new_cd, recv_new_cd = mk_send_recv Mnew_cd
      let send_cd_created, recv_cd_created =
        let send id (d:'a) = C.send_owner id (Mcd_created id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_step, recv_step = mk_send_owner_recv Mstep
      let send_step_done, recv_step_done =
        let send dest_id id (d:'a) = C.send_owner dest_id (Mstep_done id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_done, recv_done =
        let send dest_id id (d:'a) = C.send_owner dest_id (Mdone id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_pauseclock, recv_pauseclock =
        let send id (d:'a) = C.send_owner id (Mpauseclock id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let broadcast_eoi, recv_eoi = mk_broadcast_recv Meoi
      let send_eoi_control, recv_eoi_control =
        let send id (d:'a) = C.send_owner id (Meoi_control id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let broadcast_next_instant, recv_next_instant = mk_broadcast_recv Mnext_instant
      let send_has_next, recv_has_next =
        let send id (d:'a) = C.send_owner id (Mhas_next id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_req_signal, recv_req_signal =
        let send id (d:'a) = C.send_owner id (Mreq_signal id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_create_signal, recv_create_signal = mk_send_owner_recv Mcreate_signal
    end


    module Clock = struct
      let equal_clock ck1 ck2 =
        ck1.ck_gid = ck2.ck_gid

      let mk_clock parent_ck =
        let site = get_site () in
        let gid = C.fresh site.s_seed in
        { ck_gid = gid;
          ck_parent = parent_ck;
          ck_clock = GidHandle.init site.s_clock_cache gid (E.init_clock ()) }

      let rec top_clock ck = match ck.ck_parent with
        | None -> ck
        | Some ck -> top_clock ck

      let get_clock_index ck =
        let site = get_site () in
        E.get (GidHandle.get site.s_clock_cache ck.ck_clock)

      (** Returns the current index of ck and all its ancestors *)
      let rec save_clock_state ck =
        let l = match ck.ck_parent with
          | None -> []
          | Some ck -> save_clock_state ck
        in
          (ck, get_clock_index ck)::l

      (** [check_clock_state st clock] checks that the index of [clock] saved in
          [st] is the same as its current index. *)
      let check_clock_state st clock =
        let rec check_last_activation l = match l with
          | [] -> false
          | (ck, saved_idx)::l ->
              if equal_clock ck clock then
                E.equal saved_idx (get_clock_index ck)
              else
                check_last_activation l
        in
          check_last_activation st
    end

    module Event =
      struct
        let lift_handle f ev =
          f (get_event ev)

        let value ev =
          let n = get_event ev in
          if E.status n then
            E.value n
          else (
            Format.eprintf "Error: Reading the value of an absent signal %a@." C.print_gid ev.ev_gid;
            raise Types.RML
          )

        let one ev = lift_handle E.one ev
        let pre_status ev = lift_handle E.pre_status ev
        let pre_value ev = lift_handle E.pre_value ev
        let last ev = lift_handle E.last ev
        let default ev = lift_handle E.default ev
        let clock ev = get_event_clock ev

        let send_value_to_remotes cd ev n () =
          C.broadcast (Mvalue ev.ev_gid) (E.value n)

        let do_emit ev v =
          let n = get_event ev in
          let ev_ck = get_event_clock ev in
          if not (C.is_local ev_ck.ck_gid) then
            Format.eprintf "Error: do_emit on remote signal %a of clock %a@."
              C.print_gid ev.ev_gid  C.print_gid ev_ck.ck_gid;
          let cd = get_clock_domain ev_ck in
          E.emit n v;
          (* wake up processes awaiting this signal *)
          wake_up_now (Wsignal_wa ev.ev_gid);
          wake_up_now (Wsignal_wp ev.ev_gid);
          (* if we have remote clock domains, we should send them the value later *)
          if (*not (C.SiteSet.is_empty cd.cd_remotes) &&*)
            not (C.GidSet.mem ev.ev_gid cd.cd_emitted_signals) then (
              add_waiting (Wbefore_eoi ev_ck.ck_gid) (send_value_to_remotes cd ev n);
              cd.cd_emitted_signals <- C.GidSet.add ev.ev_gid cd.cd_emitted_signals
            )

        let emit ev v =
          if C.is_local ev.ev_gid then (
            do_emit ev v
          ) else
            C.send_owner ev.ev_gid (Memit ev.ev_gid) v

        (* Called after receiving Mreq_signal id *)
        let receive_req (n : ('a, 'b) E.t) gid msg =
          let req_id = Msgs.recv_req_signal msg in
          C.send_owner req_id (Msignal gid) n

        (* Called after receiving Memit id *)
        let receive_emit (ev:('a, 'b) event) msg =
          let v:'a = C.from_msg msg in
            do_emit ev v

        (* Called after receiving Mvalue id *)
        let update_local_value gid (n : ('a, 'b) E.t) msg =
          let v:'b = C.from_msg msg in
            E.set_value n v;
            wake_up_now (Wsignal_wa gid)

        let setup_local_copy gid n ck =
          add_callback (Mvalue gid) (update_local_value gid n);
          E.set_clock n (get_clock ck.ck_clock)

        (* Called when a local process tries to access a remote signal for the first time.
           Creates local callbacks for this signal. *)
        (** TODO: remove callbacks when signal is no longer needed *)
        let signal_local_value gid (n, ck) =
          let site = get_site () in
          (* request the current value of the signal *)
          Msgs.send_req_signal gid (C.fresh site.s_seed);
          setup_local_copy gid n ck;
          let msg = Callbacks.recv_given_msg site.s_msg_queue
            site.s_callbacks (Msignal gid) in
          (* set the local value to the one received *)
          let new_n = C.from_msg msg in
          E.copy n new_n;
          n, ck

        let new_local_evt_combine ck default combine =
          let site = get_site () in
          let n = E.create (get_clock ck.ck_clock) default combine in
          let gid = C.fresh site.s_seed in
          let ev  =
            { ev_handle = SignalHandle.init site.s_signal_cache gid (n, ck);
              ev_gid = gid }
          in
          add_callback (Memit gid) (receive_emit ev);
          add_callback (Mreq_signal gid) (receive_req n gid);
          ev

        let new_remote_evt_combine ck default (combine : 'a -> 'b -> 'b) =
          let site = get_site () in
          let tmp_id = C.fresh site.s_seed in
          let create_signal () =
            let ev = new_local_evt_combine ck default combine in
            C.send_owner tmp_id (Msignal_created tmp_id) ev
          in
          Msgs.send_create_signal ck.ck_gid (tmp_id, create_signal);
          let msg = Callbacks.recv_given_msg site.s_msg_queue
            site.s_callbacks (Msignal_created tmp_id) in
          (* setup the local copy of the signal  *)
          let ev:('a, 'b) event = C.from_msg msg in
          SignalHandle.set_valid ev.ev_handle;
          let n, _ = SignalHandle.get site.s_signal_cache ev.ev_handle in
          setup_local_copy ev.ev_gid n ck;
          ev

        let new_evt_combine ck default combine =
          if C.is_local ck.ck_gid then
            new_local_evt_combine ck default combine
          else
            new_remote_evt_combine ck default combine

        let new_evt ck =
          new_evt_combine ck [] (fun x y -> x :: y)

        let status ?(only_at_eoi=false) ev =
          let n = get_event ev in
          E.status n (* && (not only_at_eoi || !(sig_cd.cd_eoi)) *)
            (** TODO: remettre only_at_eoi pour les configs *)

        let cfg_present ev = ()
        let cfg_or c1 c2 = ()
        let cfg_and c1 c2 = ()
        let cfg_status ?(only_at_eoi=false) evt_cfg =
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
      p.alive <- false;
      p.susp <- false;
      D.clear_next p.next;
      List.iter set_kill p.children;
      p.children <- []

    let start_ctrl cd ctrl new_ctrl =
      new_ctrl.last_activation <- Clock.save_clock_state cd.cd_clock;
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
            | Clock_domain ck -> true
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
        node.last_activation <- Clock.save_clock_state cd.cd_clock;
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
         p.last_activation <- Clock.save_clock_state cd.cd_clock;
         List.iter (next_to_current cd) p.children)

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
    let mk_clock_domain clock balancer parent_ctrl =
      let site = get_site () in
      let cd = {
        cd_current = D.mk_current ();
        cd_pause_clock = false;
        cd_eoi = false;
        cd_wake_up = [];
        cd_clock = clock;
        cd_top = new_ctrl (Clock_domain clock);
        cd_parent_ctrl = parent_ctrl;
        cd_children_have_next = false;
        cd_load_balancer = balancer;
        cd_remaining_async = ref 0;
        cd_emitted_signals = C.GidSet.empty;
       (* cd_remotes = C.SiteSet.empty; *)
      } in
      site.s_clock_domains <- C.GidMap.add clock.ck_gid cd site.s_clock_domains;
      cd.cd_top.last_activation <- Clock.save_clock_state cd.cd_clock;
      cd

    let is_eoi cd = cd.cd_eoi
    let control_tree cd = cd.cd_top
    let clock cd = cd.cd_clock
    let add_weoi_waiting_list cd wk =
      cd.cd_wake_up <- wk :: cd.cd_wake_up

    let any_clock_ref = ref None

   (* let top_clock cd =
      Clock.top_clock cd.cd_clock *)
    let top_clock () = match !any_clock_ref with
      | None -> Format.eprintf "No top clock@."; raise Types.RML
      | Some ck -> Clock.top_clock ck
    let mk_clock parent_ck =
      let ck = Clock.mk_clock parent_ck in
      if !any_clock_ref = None then
        any_clock_ref := Some ck;
      ck

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
        process_msgs ()
      done

    (** Evaluates the condition of control nodes. This can be called several
        times for a same control node, zhen doing the eoi of several clocks.
        We can keep the last condition (if it was true for the eoi of the fast clock,
        it is also true for the eoi of the slow clock), but we have to make sure
        to fire the handler only once. *)
    let rec eoi_control cd req_has_next ctrl =
      let rec _eoi_control pere ctrl =
        if ctrl.alive then (
          (match ctrl.kind with
            | Clock_domain ck when not (C.is_local ck.ck_gid) ->
                (* waiting for a Mhas_next message from this clock domain if req_has_next is true *)
                if req_has_next then
                  incr cd.cd_remaining_async;
                Msgs.send_eoi_control ck.ck_gid req_has_next
            | Clock_domain ck -> (*local clock domain*)
                let cd = get_clock_domain ck in
                eoi_control cd req_has_next cd.cd_top
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
      if req_has_next then
        cd.cd_children_have_next <- false;
      List.iter (_eoi_control ctrl) ctrl.children

    let rec has_next cd ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain ck ->
              if C.is_local ck.ck_gid then
                let cd = get_clock_domain ck in
                has_next_cd cd
              else
                false
          | Kill _ | Kill_handler _ ->
            ctrl.cond () || has_next_children cd ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond ()) || (not ctrl.susp && not (ctrl.cond ())) in
              active && has_next_children cd ctrl
          | When ->
            not ctrl.susp && has_next_children cd ctrl
    and has_next_children cd ctrl =
      not (D.is_empty_next ctrl.next) || List.exists (has_next cd) ctrl.children
    (* Computes has_next, waiting for child clock domains if necessary *)
    and has_next_cd cd =
      let has_next_ctrl = has_next_children cd cd.cd_top in
      (* Awaits Mhas_next from all remote clock domains *)
      await_all_children cd;
      has_next_ctrl || cd.cd_children_have_next


    let macro_step_done cd =
      let has_next = has_next_cd cd in
      cd.cd_pause_clock || not has_next

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
      wake_up_now (Wbefore_eoi cd.cd_clock.ck_gid);
      cd.cd_eoi <- true;
      Msgs.broadcast_eoi cd.cd_clock.ck_gid;
      (* if this is not the top clock domain, request all remote child clock domains
         to send back Mhas_next *)
      let req_has_next = match cd.cd_clock.ck_parent with Some _ -> true | _ -> false in
      eoi_control cd req_has_next cd.cd_top;
      wake_up_now (Weoi cd.cd_clock.ck_gid);
      List.iter wake_up_now cd.cd_wake_up;
      cd.cd_wake_up <- []

    let next_instant cd =
      E.next (get_clock cd.cd_clock.ck_clock);
      (* next instant of child clock domains *)
      wake_up_now (Wnext_instant cd.cd_clock.ck_gid);
      Msgs.broadcast_next_instant cd.cd_clock;
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd;
      (* reset the clock domain *)
      cd.cd_eoi <- false;
      cd.cd_pause_clock <- false;
      cd.cd_emitted_signals <- C.GidSet.empty;
      cd.cd_children_have_next <- false

    let rec exec_cd ?(wait_for_msgs=false) cd () =
      (*Format.eprintf "Exec cd@.";*)
      schedule cd;
      if cd.cd_top.alive && !(cd.cd_remaining_async) = 0 then (
        eoi cd;
        match cd.cd_clock.ck_parent with
          | None -> (* top clock domain *)
              next_instant cd
              (*Format.eprintf "Top clock domain next_instnt done : %d@." !(cd.cd_remaining_async)*)
          | Some ck ->
              if macro_step_done cd then (
                add_waiting (Wnext_instant ck.ck_gid) (fun () -> next_instant cd);
                (* if the parent clock is not here, send Done message*)
                if not (C.is_local ck.ck_gid) then
                  Msgs.send_step_done ck.ck_gid cd.cd_clock.ck_gid ()
                else (
                  (match cd.cd_parent_ctrl with
                    | None -> assert false
                    | Some ctrl -> D.add_next (exec_cd ~wait_for_msgs:wait_for_msgs cd) ctrl.next_control)
                )
              ) else (
                next_instant cd;
                (* do another step*)
                exec_cd ~wait_for_msgs:wait_for_msgs cd ()
              )
      ) else if wait_for_msgs then (
        if cd.cd_top.alive && !(cd.cd_remaining_async) <> 0 then (
          process_msgs ();
          exec_cd ~wait_for_msgs:wait_for_msgs cd ()
        )
      )

(*
    let rec react cd =
      exec_cd cd ();
      Format.eprintf "Exec of cd %a done with rem: %d @." C.print_gid cd.cd_clock.ck_gid !(cd.cd_remaining_async);
      if cd.cd_top.alive && !(cd.cd_remaining_async) <> 0 then (
        Format.eprintf "Waitinf for children to finish@.";
        process_msgs ();
        react cd
      )
*)
    let react cd = exec_cd ~wait_for_msgs:true cd ()

    (* After receiving Meoi_control *)
    let receive_eoi_control cd msg =
      let req_has_next = Msgs.recv_eoi_control msg in
      (* req has_next from children because this is not the top clock domain *)
      eoi_control cd true cd.cd_top;
      if req_has_next then (
        let has_next = has_next_cd cd in
        match cd.cd_clock.ck_parent with
          | None -> assert false
          | Some ck -> Msgs.send_has_next ck.ck_gid has_next
      )

    (* After receiving Mhas_next *)
    let gather_has_next cd msg =
      let has_next = Msgs.recv_has_next msg in
      decr cd.cd_remaining_async;
      cd.cd_children_have_next <- has_next or cd.cd_children_have_next

    let set_pauseclock cd ck =
      if C.is_local ck.ck_gid then
        let cd = get_clock_domain ck in
          cd.cd_pause_clock <- true
      else
        Msgs.send_pauseclock ck.ck_gid ()

    (* After receiving Mpauseclock *)
    let gather_pauseclock cd _ =
      cd.cd_pause_clock <- true


    let step_remote_clock_domain cd ck_id () =
      incr cd.cd_remaining_async;
      Msgs.send_step ck_id ck_id

    let wake_up_cd_if_done cd =
      if !(cd.cd_remaining_async) = 0 then (
        match cd.cd_clock.ck_parent with
          | Some ck when not (C.is_local ck.ck_gid) ->
              (*Format.eprintf "Waking up cd because of message@.";*)
              exec_cd cd ()
          | _ -> () (* the cd is already executing, waiting for messages *)
      )

    (* After receving Mstep_done*)
    let receive_step_done cd ctrl remote_ck_id _ =
      D.add_next (step_remote_clock_domain cd remote_ck_id) ctrl.next_control;
      decr cd.cd_remaining_async;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done cd

    (* After receiving Mdone *)
    let receive_done_cd cd new_ctrl f_k _ =
      set_kill new_ctrl;
      decr cd.cd_remaining_async;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done cd;
      f_k ()



    let init_clock_domain clock balancer parent_ctrl =
      let cd = mk_clock_domain clock balancer parent_ctrl in
        add_callback (Mhas_next clock.ck_gid) (gather_has_next cd);
        add_callback (Meoi_control clock.ck_gid) (receive_eoi_control cd);
        add_callback (Mpauseclock clock.ck_gid) (gather_pauseclock cd);
        cd

    let end_clock_domain new_cd new_ctrl f_k () =
      (*Format.eprintf "Ending clock domain@.";*)
      let site = get_site () in
      site.s_clock_domains <- C.GidMap.remove new_cd.cd_clock.ck_gid site.s_clock_domains;
      match new_cd.cd_clock.ck_parent with
        | Some ck ->
            if not (C.is_local ck.ck_gid) then
              Msgs.send_done ck.ck_gid new_cd.cd_clock.ck_gid ();
            end_ctrl new_ctrl f_k ()
        | None -> assert false

    let new_local_clock_domain cd new_balancer ctrl p f_k =
      let new_ck = mk_clock (Some cd.cd_clock) in
      Format.eprintf "Creating local clock domain %a@." C.print_gid new_ck.ck_gid;
      let new_cd = init_clock_domain new_ck new_balancer (Some ctrl) in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl f_k) in
      fun _ ->
        D.add_current f new_cd.cd_current;
        start_ctrl new_cd ctrl new_ctrl;
        react new_cd

    (* After receving Mnew_cd *)
    let create_cd msg =
      let tmp_id, parent_ck, new_balancer, p = Msgs.recv_new_cd msg in
      let new_ck = mk_clock (Some parent_ck) in
      Msgs.send_cd_created tmp_id new_ck;
      let new_cd = init_clock_domain new_ck new_balancer None in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl dummy_step) in
      D.add_current f new_cd.cd_current;
      exec_cd new_cd ()

    (* After receiving Mcd_created *)
    let start_remote_clock_domain cd ctrl f_k msg =
      (*Format.eprintf "Starting remote clock domain@.";*)
      let site = get_site () in
      let new_ck = Msgs.recv_cd_created msg in
      let new_ctrl = new_ctrl (Clock_domain new_ck) in
      Callbacks.add_callback ~kind:Callbacks.Once
        (Mdone new_ck.ck_gid) (receive_done_cd cd new_ctrl f_k) site.s_callbacks;
      add_callback (Mstep_done new_ck.ck_gid) (receive_step_done cd ctrl new_ck.ck_gid);
      start_ctrl cd ctrl new_ctrl

    let new_remote_clock_domain remote_site new_balancer cd ctrl p f_k =
      let site = get_site () in
      let tmp_id = C.fresh site.s_seed in
      add_callback (Mcd_created tmp_id) (start_remote_clock_domain cd ctrl f_k);
      incr cd.cd_remaining_async;
      Msgs.send_new_cd remote_site (tmp_id, cd.cd_clock, new_balancer, p)

    let new_clock_domain cd ctrl p f_k _ =
      let remote_site, new_balancer = cd.cd_load_balancer#new_child ()  in
      if remote_site <> C.local_site () then (
        Format.eprintf "Distributing new cd to site %a@." C.print_site remote_site;
        (*cd.cd_remotes <- C.SiteSet.add remote_site cd.cd_remotes;*)
        (*site.s_children <- C.SiteSet.add remote_site site.s_children; *)
        new_remote_clock_domain remote_site new_balancer cd ctrl p f_k
      ) else (
        new_local_clock_domain cd new_balancer ctrl p f_k ()
      )



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

    (* After receiving Mstep *)
   let start_cd msg =
     let cd_id = Msgs.recv_step msg in
     try
       let site = get_site () in
       exec_cd (C.GidMap.find cd_id site.s_clock_domains) ()
     with
       | Not_found -> Format.eprintf "Received Start for unknown clock domain.@."

   (* After receiving Meoi *)
    let receive_eoi msg =
      let ck_id:C.gid = C.from_msg msg in
      (*Format.eprintf "Doing eoi of remote clock domain@.";*)
      wake_up_now (Weoi ck_id)

    (* After receving Mnext_instant *)
    let receive_next_instant msg =
      (* update the status of the clock *)
      let ck = Msgs.recv_next_instant msg in
      E.next (get_clock ck.ck_clock);
      wake_up_now (Wnext_instant ck.ck_gid)

    (* After receiving Mcreate_signal *)
    let receive_create_signal msg =
      let tmp_id, f = Msgs.recv_create_signal msg in
      (* create the signal and send the created value *)
      f ()

    (* After receiving Mfinalize *)
    let terminate_site _ =
      let site = get_site () in
      Callbacks.stop_receiving site.s_msg_queue;
      (* send dummy messsage to stop the receiving thread *)
      Msgs.send_dummy site.s_comm_site ();
      (match site.s_msg_thread with Some t -> Thread.join t | _ -> assert false);
      if not (C.is_master ()) then (
        Format.eprintf "Exiting slave@.";
        exit 0
      )

    let init_site () =
      Format.eprintf "Init site@.";
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
        s_seed = C.mk_seed ();
        s_comm_site = C.local_site ();
       (* s_children = C.SiteSet.empty; *)
      } in
      let m = (module struct
        let local_value = Event.signal_local_value
      end : SignalHandle.LOCAL_VALUE) in
      s.s_signal_cache <- SignalHandle.mk_cache m;
      Mpi.init_local_ref s;
      add_callback Mnew_cd create_cd;
      add_callback Mstep start_cd;
      add_callback Meoi receive_eoi;
      add_callback Mnext_instant receive_next_instant;
      add_callback Mcreate_signal receive_create_signal;
      add_callback Mfinalize terminate_site;
      s.s_msg_thread <- Some (Thread.create Callbacks.receive s.s_msg_queue)

    let _ = init_site ()

    let start_slave () =
      while true do
        (*  Format.eprintf "Waiting for messages@."; *)
        process_msgs ()
      done

    let finalize_top_clock_domain cd =
      for i = 0 to C.number_of_sites () - 1 do
        Msgs.send_finalize (C.nth_site i) ()
      done;
      terminate_site ()

    let mk_top_clock_domain () =
      let ck = mk_clock None in
      let balancer = L.mk_top_balancer () in
      let cd = mk_clock_domain ck balancer None in
      cd

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f = D.add_current f cd.cd_current
    let on_current_instant_list cd fl = D.add_current_list fl cd.cd_current
    let on_next_instant ctrl f = D.add_next f ctrl.next

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
    let on_eoi ck f =
     (* Format.eprintf "On eoi@."; *)
      if C.is_local ck.ck_gid && is_eoi (get_clock_domain ck) then
        f unit_value
      else
        add_waiting (Weoi ck.ck_gid) f

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
      add_waiting (Wsignal_wp ev.ev_gid) act;
      add_weoi_waiting_list cd (Wsignal_wp ev.ev_gid)

    let on_event_or_next ev f_w v_w cd ctrl f_next =
      let n = get_event ev in
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
    let _on_event ev ev_ck ctrl f v =
      let rec self _ =
        if is_active ctrl then
            (*ctrl is activated, run continuation*)
          (try
              f v
            with
              | Wait_again -> add_waiting (Wsignal_wa ev.ev_gid) self)
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_waiting (Weoi ev_ck.ck_gid) (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           add_waiting (Wsignal_wa ev.ev_gid) self)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (try
             is_fired := true; f v
           with
             | Wait_again -> add_waiting (Wsignal_wa ev.ev_gid) self)
      in
      add_waiting (Wsignal_wa ev.ev_gid) self

    let on_event ev ctrl f v =
      let n = get_event ev in
      let ev_ck = get_event_clock ev in
      if E.status n then
        (try
           f v
         with
           | Wait_again -> _on_event ev ev_ck ctrl f v)
      else
        _on_event ev ev_ck ctrl f v

    let on_event_cfg evt_cfg ctrl f v  =
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

    let has_been_active ctrl ev =
      let ev_ck = get_event_clock ev in
      Clock.check_clock_state ctrl.last_activation ev_ck

    (** [on_event_at_eoi evt ctrl f] executes 'f ()' during the eoi
        (of evt's clock domain) if ctrl is active in the same step.
        Waits for the next activation of evt otherwise, or if the call
        raises Wait_again *)
    let _on_local_event_at_eoi ev ev_ck ctrl f =
      let rec self _ =
        if has_been_active ctrl ev then
          (*ctrl is activated, run continuation*)
          add_waiting (Weoi ev_ck.ck_gid) eoi_work
        else ((*ctrl is not active, wait end of instant*)
          let is_fired = ref false in
          D.add_next (ctrl_await is_fired) ctrl.next_control;
          add_waiting (Weoi ev_ck.ck_gid) (eoi_await is_fired)
        )
      and eoi_await is_fired _ =
        if not !is_fired then
          (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           add_waiting (Wsignal_wa ev.ev_gid) self)
      and ctrl_await is_fired _ =
        if not !is_fired then
         (* ctrl was activated, signal is present*)
          (is_fired :=  true;
           add_waiting (Weoi ev_ck.ck_gid) eoi_work)
      and eoi_work _ =
          (try
            f unit_value
          with
            | Wait_again -> add_waiting (Wsignal_wa ev.ev_gid) self)
      in
      add_waiting (Wsignal_wa ev.ev_gid) self

    let _on_remote_event_at_eoi ev ev_ck ctrl f =
      let rec self _ =
        if has_been_active ctrl ev then
          add_waiting (Weoi ev_ck.ck_gid) eoi_work
        else
           add_waiting (Wsignal_wa ev.ev_gid) self
      and eoi_work _ =
          (try
            f unit_value
          with
            | Wait_again -> add_waiting (Wsignal_wa ev.ev_gid) self)
      in
        add_waiting (Wsignal_wa ev.ev_gid) self

     let on_event_at_eoi ev ctrl f =
       let n = get_event ev in
       let ev_ck = get_event_clock ev in
         if C.is_local ev.ev_gid then (
           if E.status n then
             let eoi_work _ =
               (try
                   f unit_value
                 with
                   | Wait_again -> _on_local_event_at_eoi ev ev_ck ctrl f)
             in
               add_waiting (Weoi ev_ck.ck_gid) eoi_work
           else
               _on_local_event_at_eoi ev ev_ck ctrl f
         ) else
             _on_remote_event_at_eoi ev ev_ck ctrl f

     let on_event_cfg_at_eoi evt_cfg ctrl f =
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

module MpiRuntime =
  Make
    (Seq_runtime.ListDataStruct)
    (Mpi_communication.Make)
    (Sig_env.Record)
