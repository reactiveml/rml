open Runtime
open Runtime_options
open Types

module Make
  (D: Runtime.SEQ_DATA_STRUCT)
  (CF : functor (T : Communication.TAG_TYPE) -> Communication.S with type 'gid tag = 'gid T.t)
  (CALL : functor (C: Communication.S) -> Callbacks.S with type msg = C.msg and type tag = C.gid C.tag)
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
          | Mbefore_eoi (* Before the end of instant of clock domain *)
          | Meoi (* End of instant of clock domain *)
          | Meoi_control of 'gid
          | Mnext_instant (* Go to next instant *)
          | Mreq_has_next of 'gid
          | Mhas_next of 'gid
              (* Whether there is processes to execute in the next local step *)
          | Mnew_remote of 'gid
              (* signals *)
          | Memit of 'gid (* Emit a value *)
          | Mvalue of 'gid (* Send the value of the signal to child cd *)
          | Msignal of 'gid (* Send the whole signal *)
          | Mreq_signal of 'gid (* Request the value of the signal *)
          | Mcreate_signal
          | Msignal_created of 'gid

      let dummy = Mdummy

      let flush_after tag = match tag with
         | Mfinalize | Mdummy | Mhas_next _ | Mvalue _ | Mnew_cd | Mcd_created _
         | Mreq_signal _ | Msignal_created _ | Mcreate_signal -> true
         | _ -> false

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
        | Meoi -> fprintf ff "Mbefore_eoi"
        | Mbefore_eoi -> fprintf ff "Meoi"
        | Meoi_control gid -> fprintf ff "Meoi_control %a" print_gid gid
        | Mnext_instant -> fprintf ff "Mnext_instant"
        | Mreq_has_next gid -> fprintf ff "Mreq_has_next %a" print_gid gid
        | Mhas_next gid -> fprintf ff "Mhas_next %a" print_gid gid
        | Mnew_remote gid -> fprintf ff "Mnew_remote %a" print_gid gid
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

    module Callbacks = CALL(C)
    module L = Load_balancer.Make (C)

    module GidHandle = Dhandle_typed.Make (struct
      type t = C.gid
      let compare = compare
    end)

    type clock = {
      ck_gid : C.gid;
      ck_parent : clock option;
      ck_clock : E.clock GidHandle.handle;
    }
    type region = clock
    (** Saving and searching clock state *)
    type clock_state = (clock * E.clock_index) list

    type 'a step = 'a -> unit
    let unit_value = ()
    let dummy_step () = ()

    type control_tree =
        { kind: (unit step, clock) control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (unit -> bool);
          mutable cond_v : bool;
          mutable children: control_tree list;
          next: D.next;
          next_control : D.next; (* contains control processes that should not be
                                  taken into account to see if macro step is done *)
          mutable last_activation : clock_state;
          mutable instance : int;
        }

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
      type ('a, 'b) value = ('a, 'b) E.t * clock * region * clock option
      let compare = compare
    end)

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
          cd_location : L.kind;
          mutable cd_children_have_next : bool;
          mutable cd_remaining_async : int ref;
          mutable cd_remotes : C.SiteSet.t; (* remotes with child clock domains*)

          mutable cd_counter : int;
          cd_period : int option;
        }

    type site = {
      mutable s_top_clock_domain : clock_domain option;
      mutable s_clock_domains : clock_domain C.GidMap.t;
      s_msg_queue : Callbacks.msg_queue;
      s_callbacks : Callbacks.dispatcher;
      s_clock_cache : E.clock GidHandle.cache;
      mutable s_signal_cache : SignalHandle.cache;
      mutable s_waiting : D.waiting_list WaitingMap.t; (* waiting lists for parent cds and slow signals *)
      s_seed : C.seed;
      s_comm_site : C.site;
      mutable s_signals_remotes : C.SiteSet.t C.GidMap.t;
     (* mutable s_children : C.SiteSet.t; (* remotes with child clock domains*) *)
    }

    let print_clock ff ck =
      C.print_gid ff ck.ck_gid
    let print_cd ff cd =
      C.print_gid ff cd.cd_clock.ck_gid
    let print_signal ff ev =
      C.print_gid ff ev.ev_gid

    let get_site () =
      (Local_ref.get 0:site)

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
        | Not_found ->
            print_debug "Error: Cannot find clock domain %a@." C.print_gid ck.ck_gid;
            assert false
    let get_event ev =
      let site = get_site () in
      let n, _, _, _ =  SignalHandle.get site.s_signal_cache ev.ev_handle in
      n
    let get_event_clock ev =
      let site = get_site () in
      let _, ck, _, _ = SignalHandle.get site.s_signal_cache ev.ev_handle in
      ck
    let get_event_region ev =
      let site = get_site () in
      let _, _, r, _ = SignalHandle.get site.s_signal_cache ev.ev_handle in
      r
    let get_event_whole ev =
      let site = get_site () in
      SignalHandle.get site.s_signal_cache ev.ev_handle

    let get_top_clock_domain () =
      let site = get_site () in
      match site.s_top_clock_domain with
        | None -> raise Types.RML
        | Some cd -> cd

    let process_msgs () =
      let site = get_site () in
      C.flush ();
      Callbacks.await_new_msg site.s_msg_queue;
      Callbacks.dispatch_all site.s_msg_queue site.s_callbacks

    module Msgs = struct
      let mk_send_recv tag =
        let send site (d:'a) = C.send site tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv

      let mk_broadcast_set_recv tag =
        let broadcast s (d:'a) = C.broadcast_set s tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        broadcast, recv

      let mk_send_owner_recv tag =
        let send gid (d:'a) = C.send_owner gid tag d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv

      let broadcast_finalize, recv_finalize = mk_broadcast_set_recv Mfinalize
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
      let broadcast_before_eoi, recv_before_eoi = mk_broadcast_set_recv Mbefore_eoi
      let broadcast_eoi, recv_eoi = mk_broadcast_set_recv Meoi
      let send_eoi_control, recv_eoi_control =
        let send id (d:'a) = C.send_owner id (Meoi_control id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let broadcast_next_instant, recv_next_instant = mk_broadcast_set_recv Mnext_instant
      let send_has_next, recv_has_next =
        let send id (d:'a) = C.send_owner id (Mhas_next id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_req_has_next, recv_req_has_next =
        let send id (d:'a) = C.send_owner id (Mreq_has_next id) d in
        let recv = (C.from_msg : C.msg -> 'a) in
        send, recv
      let send_new_remote, recv_new_remote =
        let send id (d:'a) = C.send_owner id (Mnew_remote id) d in
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
            print_debug "Error: Reading the value of an absent signal %a@." C.print_gid ev.ev_gid;
            raise Types.RML
          )

        let one ev = lift_handle E.one ev
        let pre_status ev = lift_handle E.pre_status ev
        let pre_value ev = lift_handle E.pre_value ev
        let last ev = lift_handle E.last ev
        let default ev = lift_handle E.default ev
        let clock ev = get_event_clock ev

        let region_of_clock ck = ck

        let get_signal_remotes gid =
          let site = get_site () in
          try
            C.GidMap.find gid site.s_signals_remotes
          with
            | Not_found -> C.SiteSet.empty

        let add_signal_remote s gid =
          let site = get_site () in
          let set =
            if C.GidMap.mem gid site.s_signals_remotes then
              let set = C.GidMap.find gid site.s_signals_remotes in
              C.SiteSet.add s set
            else
              C.SiteSet.singleton s
          in
          site.s_signals_remotes <- C.GidMap.add gid set site.s_signals_remotes

        let send_value_to_remotes cd ev n () =
          let remotes =
            if !Runtime_options.use_signals_users_set then
              get_signal_remotes ev.ev_gid
            else
              cd.cd_remotes
          in
          C.broadcast_set remotes (Mvalue ev.ev_gid) (E.value n)

        let do_emit ev v =
          print_debug "Emitting a value for %a@." print_signal ev;
          let n, ev_ck, ev_r, _ = get_event_whole ev in
          let already_present = E.status n in
          if not (C.is_local ev_r.ck_gid) then
            print_debug "Error: do_emit on remote signal %a at region %a of clock %a@."
              C.print_gid ev.ev_gid   print_clock ev_r  print_clock ev_ck;
          E.emit n v;
          (* wake up processes awaiting this signal *)
          wake_up_now (Wsignal_wa ev.ev_gid);
          wake_up_now (Wsignal_wp ev.ev_gid);
          (* if we have remote clock domains, we should send them the value later *)
          let cd = get_clock_domain ev_r in
          if not already_present && cd.cd_location = L.Lany then
            add_waiting (Wbefore_eoi ev_ck.ck_gid) (send_value_to_remotes cd ev n)

        let emit ev v =
          if C.is_local ev.ev_gid then (
            do_emit ev v
          ) else
            C.send_owner ev.ev_gid (Memit ev.ev_gid) v

        (* Called after receiving Mreq_signal id *)
        let receive_req (n : ('a, 'b) E.t) gid msg =
          let req_id = Msgs.recv_req_signal msg in
          if !Runtime_options.use_signals_users_set then
            add_signal_remote (C.site_of_gid req_id) gid;
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

        let add_reset n reset =
          (* create callback for resetting the signal *)
          (* TODO: use weak pointer *)
          match reset with
            | None -> ()
            | Some rck ->
              let rec reset_evt () =
                E.reset n;
                add_waiting (Weoi rck.ck_gid) reset_evt
              in
              add_waiting (Weoi rck.ck_gid) reset_evt

        let setup_local_copy gid n ck reset =
          add_callback (Mvalue gid) (update_local_value gid n);
          E.set_clock n (get_clock ck.ck_clock);
          add_reset n reset

        (* Called when a local process tries to access a remote signal for the first time.
           Creates local callbacks for this signal. *)
        (** TODO: remove callbacks when signal is no longer needed *)
        let signal_local_value gid (n, ck, r, reset) =
          let site = get_site () in
          (* request the current value of the signal *)
          Msgs.send_req_signal gid (C.fresh site.s_seed);
          setup_local_copy gid n ck reset;
          let msg = Callbacks.recv_given_msg site.s_msg_queue (Msignal gid) in
          (* set the local value to the one received *)
          let new_n = C.from_msg msg in
          E.copy n new_n;
          n, ck, r, reset

        let new_local_evt ck r is_memory default combine reset =
          let site = get_site () in
          let n = E.create (get_clock ck.ck_clock) is_memory default combine in
          let gid = C.fresh site.s_seed in
          let ev  =
            { ev_handle = SignalHandle.init site.s_signal_cache gid (n, ck, r, reset);
              ev_gid = gid }
          in
          print_debug "Created signal %a at region %a of clock %a@." print_signal ev
            print_clock r  print_clock ck;
          if (get_clock_domain r).cd_location = L.Lany then (
            add_callback (Memit gid) (receive_emit ev);
            add_callback (Mreq_signal gid) (receive_req n gid)
          );
          add_reset n reset;
          ev

        let new_remote_evt ck r is_memory default (combine : 'a -> 'b -> 'b) reset =
          let site = get_site () in
          let tmp_id = C.fresh site.s_seed in
          let create_signal () =
            let ev = new_local_evt ck r is_memory default combine reset in
            if !Runtime_options.use_signals_users_set then
              add_signal_remote (C.site_of_gid tmp_id) ev.ev_gid;
            print_debug "Created signal %a at region %a of clock %a from request by %a@." print_signal ev
              print_clock r   print_clock ck  C.print_gid tmp_id;
            C.send_owner tmp_id (Msignal_created tmp_id) ev
          in
          Msgs.send_create_signal r.ck_gid (tmp_id, create_signal);
          let msg = Callbacks.recv_given_msg site.s_msg_queue (Msignal_created tmp_id) in
          (* setup the local copy of the signal  *)
          let ev:('a, 'b) event = C.from_msg msg in
          SignalHandle.set_valid ev.ev_handle;
          let n = get_event ev in
          setup_local_copy ev.ev_gid n ck reset;
          ev

        let new_evt_any ck r is_memory default combine reset =
          let r = if !Runtime_options.use_local_slow_signals then r else ck in
          if C.is_local r.ck_gid then
            new_local_evt ck r is_memory default combine reset
          else
            new_remote_evt ck r is_memory default combine reset

        let new_evt_expr ck r is_memory default combine =
          new_evt_any ck r is_memory default combine None

        let new_evt _ ck r is_memory default combine reset k =
          let evt = new_evt_any ck r is_memory default combine reset in
          k evt

        let new_evt_global is_memory default (combine : 'a -> 'b -> 'b) =
          let site = get_site () in
          if C.is_master () then (
            (* create the signal *)
            let ck = (get_top_clock_domain ()).cd_clock in
            let ev = new_local_evt ck ck is_memory default combine None in
            (* send it to all other sites *)
            print_debug "Sending global signal %a@." print_signal ev;
            C.broadcast (Msignal ev.ev_gid) ev;
            if !Runtime_options.use_signals_users_set then
              C.SiteSet.iter
                (fun s -> if s <> C.local_site () then add_signal_remote s ev.ev_gid)
                (C.all_sites ());
            ev
          ) else (
            (* get the value sent by the master site *)
            let tmp_id = C.fresh site.s_seed in
            let ev_id = C.relocate_gid tmp_id (C.master_site ()) in
            print_debug "Waiting for global signal %a@." C.print_gid ev_id;
            let msg = Callbacks.recv_given_msg site.s_msg_queue (Msignal ev_id) in
            let ev : ('a, 'b) event = C.from_msg msg in
            print_debug "Received global signal %a@." C.print_gid ev_id;
            (* setup the local copy *)
            SignalHandle.set_valid ev.ev_handle;
            let n, ck, _, _ = get_event_whole ev in
            setup_local_copy ev.ev_gid n ck None;
            ev
          )

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
        instance = 0;
        next = D.mk_next ();
        next_control = D.mk_next () }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true; (* set to true, to show that the node is no longer attached to its parent
                       and needs to be reattaced if the node is reused *)
      p.susp <- false;
      D.clear_next p.next;
      List.iter set_kill p.children;
      p.children <- [];
      p.instance <- p.instance + 1

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
                false
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

    (** clock domains operations *)
    let mk_clock_domain clock balancer period location parent_ctrl =
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
        cd_location = location;
        cd_remaining_async = ref 0;
        cd_remotes = C.SiteSet.empty;
        cd_counter = 0;
        cd_period = period;
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
      | None -> print_debug "Error: No top clock@."; raise Types.RML
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

    (** Evaluates the condition of control nodes. This can be called several
        times for a same control node, zhen doing the eoi of several clocks.
        We can keep the last condition (if it was true for the eoi of the fast clock,
        it is also true for the eoi of the slow clock), but we have to make sure
        to fire the handler only once. *)
    let rec eoi_control cd ctrl =
      let rec _eoi_control pere ctrl =
        if ctrl.alive then (
          (match ctrl.kind with
            | Clock_domain ck when not (C.is_local ck.ck_gid) ->
                Msgs.send_eoi_control ck.ck_gid ()
            | Clock_domain ck -> (*local clock domain*)
                let cd = get_clock_domain ck in
                eoi_control cd cd.cd_top
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

    let rec has_next_cd cd =
      print_debug "Before Waiting: %d@." !(cd.cd_remaining_async);
      cd.cd_children_have_next <- false;
      let has_next_ctrl = has_next_children cd cd.cd_top in
      (* Awaits Mhas_next from all remote clock domains *)
      C.flush ();
      let site = get_site () in
      while !(cd.cd_remaining_async) > 0 do
        print_debug "Waiting for %d has_next msgs@." !(cd.cd_remaining_async);
        Callbacks.dispatch_given_msg site.s_msg_queue site.s_callbacks (Mhas_next cd.cd_clock.ck_gid)
      done;
      has_next_ctrl || cd.cd_children_have_next
    and has_next cd ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain ck ->
              if C.is_local ck.ck_gid then
                let cd = get_clock_domain ck in
                has_next_cd cd
              else (
                (* waiting for a Mhas_next message from this clock domain *)
                print_debug "++%a@." print_cd cd;
                incr cd.cd_remaining_async;
                Msgs.send_req_has_next ck.ck_gid ();
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
    (* Computes has_next, waiting for child clock domains if necessary *)

    let macro_step_done cd =
      let has_next = has_next_cd cd in
      print_debug "Macro step of clock domain %a: pauseclock = %b and has_next = %b@."
        print_cd cd  cd.cd_pause_clock  has_next;
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
      print_debug "Eoi of clock domain %a@." print_cd cd;
      wake_up_now (Wbefore_eoi cd.cd_clock.ck_gid);
      Msgs.broadcast_before_eoi cd.cd_remotes cd.cd_clock.ck_gid;
      cd.cd_eoi <- true;
      eoi_control cd cd.cd_top;
      wake_up_now (Weoi cd.cd_clock.ck_gid);
      Msgs.broadcast_eoi cd.cd_remotes cd.cd_clock.ck_gid;
      List.iter wake_up_now cd.cd_wake_up;
      cd.cd_wake_up <- []

    let next_instant cd =
      print_debug "Next instant of clock domain %a@." print_cd cd;
      (* next instant of child clock domains *)
      Msgs.broadcast_next_instant cd.cd_remotes cd.cd_clock;
      (*increment the clock after sending because the receiver will increment it*)
      E.next (get_clock cd.cd_clock.ck_clock);
      wake_up_now (Wnext_instant cd.cd_clock.ck_gid);
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd;
      (* reset the clock domain *)
      cd.cd_eoi <- false;
      cd.cd_pause_clock <- false;
      cd.cd_children_have_next <- false

    let rec exec_cd cd () =
      print_debug "Executing clock domain %a@." print_cd cd;
      schedule cd;
      cd.cd_counter <- cd.cd_counter + 1;
      if cd.cd_top.alive then (
        if !(cd.cd_remaining_async) = 0 then (
          eoi cd;
          match cd.cd_clock.ck_parent with
            | None -> (* top clock domain *)
                next_instant cd
                  (*Format.eprintf "Top clock domain next_instnt done : %d@." !(cd.cd_remaining_async)*)
            | Some ck ->
                let is_done = macro_step_done cd in
                let period_finished = match cd.cd_period with
                  | None -> false
                  | Some p -> cd.cd_counter >= p
                in
                if is_done || period_finished then (
                  cd.cd_counter <- 0;
                  print_debug "Macro step of clock domain %a is done@." print_cd cd;
                  add_waiting (Wnext_instant ck.ck_gid) (fun () -> next_instant cd);
                  (* if the parent clock is not here, send Done message*)
                  if not (C.is_local ck.ck_gid) then
                    Msgs.send_step_done ck.ck_gid cd.cd_clock.ck_gid ()
                  else (
                    (match cd.cd_parent_ctrl with
                      | None -> assert false
                      | Some ctrl -> D.add_next (exec_cd cd) ctrl.next_control)
                  )
                ) else (
                  next_instant cd;
                  (* do another step*)
                  exec_cd  cd ()
                )
        ) else
          match cd.cd_clock.ck_parent with
            | None -> (* top clock, wait for msgs *)
                print_debug "Top clock domain %a is waiting for %d children@."
                  print_cd cd  !(cd.cd_remaining_async);
                process_msgs ();
                exec_cd cd ()
            | Some ck ->
                (* parent is local, increment its async counter before returning *)
                if C.is_local ck.ck_gid then (
                  let parent_cd = get_clock_domain ck in
                  incr parent_cd.cd_remaining_async
                );
                print_debug "Execution of clock domain %a is suspended until a message is received@."
                  print_cd cd
      ) else
        print_debug "Clock domain %a is dead@." print_cd cd
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
    let react cd = exec_cd cd ()

    (* After receiving Meoi_control *)
    let receive_eoi_control cd _ =
      eoi_control cd cd.cd_top

    (* After receiving Mreq_has_next *)
    let receive_req_has_next cd _ =
      match cd.cd_clock.ck_parent with
        | None -> assert false
        | Some ck ->
            let has_next = has_next_cd cd in
            Msgs.send_has_next ck.ck_gid has_next

    (* After receiving Mhas_next *)
    let gather_has_next cd msg =
      let has_next = Msgs.recv_has_next msg in
      print_debug "--%a@." print_cd cd;
      decr cd.cd_remaining_async;
      if !debug_mode && !(cd.cd_remaining_async) < 0 then
        print_debug "Error: counter of %a is < 0@." print_cd cd;
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
      print_debug "++%a@." print_cd cd;
      incr cd.cd_remaining_async;
      Msgs.send_step ck_id ck_id

    let rec wake_up_cd_if_done cd =
      if !(cd.cd_remaining_async) = 0 then (
        match cd.cd_clock.ck_parent with
          | None -> () (* the cd is already executing, waiting for messages *)
          | Some ck ->
              print_debug "Waking up clock domain %a after receiving a message@." print_cd cd;
              if C.is_local ck.ck_gid then (
                let parent_cd = get_clock_domain ck in
                exec_cd cd ();
                print_debug "--%a@." print_cd parent_cd;
                decr parent_cd.cd_remaining_async;
                if !debug_mode && !(parent_cd.cd_remaining_async) < 0 then
                  print_debug "Error: counter of %a is < 0@." print_cd parent_cd;
                wake_up_cd_if_done parent_cd
              ) else
                exec_cd cd ()
      )

    (* After receving Mstep_done*)
    let receive_step_done cd ctrl remote_ck_id _ =
      D.add_next (step_remote_clock_domain cd remote_ck_id) ctrl.next_control;
      print_debug "--%a@." print_cd cd;
      decr cd.cd_remaining_async;
      if !debug_mode && !(cd.cd_remaining_async) < 0 then
        print_debug "Error: counter of %a is < 0@." print_cd cd;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done cd

    (* After receiving Mdone *)
    let receive_done_cd cd new_ctrl f_k _ =
      set_kill new_ctrl;
      print_debug "--%a@." print_cd cd;
      decr cd.cd_remaining_async;
      if !debug_mode && !(cd.cd_remaining_async) < 0 then
        print_debug "Error: counter of %a is < 0@." print_cd cd;
      D.add_current f_k cd.cd_current;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done cd

    let rec update_remote_set cd site =
      if not (C.SiteSet.mem site cd.cd_remotes) then (
        if C.SiteSet.is_empty cd.cd_remotes then (
          print_debug "Allocating callbacks for %a@." print_cd cd;
          register_cd_callbacks cd
        );
        print_debug "Adding site %a to remote sites of %a@." C.print_site site  print_cd cd;
        cd.cd_remotes <- C.SiteSet.add site cd.cd_remotes;
        (* propagate the information to the parent *)
        match cd.cd_clock.ck_parent with
          | None -> ()
          | Some pck when C.is_local pck.ck_gid ->
              let pcd = get_clock_domain pck in
              update_remote_set pcd site
          | Some pck -> Msgs.send_new_remote pck.ck_gid site
      )

    (* After receiving Mnew_remote *)
    and receive_new_remote cd msg =
      let site = Msgs.recv_new_remote msg in
      update_remote_set cd site

    (* Registers callbacks for messages sent by child cds *)
    and register_cd_callbacks cd =
      add_callback (Mhas_next cd.cd_clock.ck_gid) (gather_has_next cd);
      add_callback (Mpauseclock cd.cd_clock.ck_gid) (gather_pauseclock cd);
      add_callback (Mnew_remote cd.cd_clock.ck_gid) (receive_new_remote cd)

    (* Registers callbacks for messages sent by the parent cd *)
    let register_parent_callbacks cd =
      add_callback (Meoi_control cd.cd_clock.ck_gid) (receive_eoi_control cd);
      add_callback (Mreq_has_next cd.cd_clock.ck_gid) (receive_req_has_next cd)

    let unregister_cd_callbacks cd =
      let site = get_site () in
      Callbacks.remove_callback (Mhas_next cd.cd_clock.ck_gid) site.s_callbacks;
      Callbacks.remove_callback (Meoi_control cd.cd_clock.ck_gid) site.s_callbacks;
      Callbacks.remove_callback (Mreq_has_next cd.cd_clock.ck_gid) site.s_callbacks;
      Callbacks.remove_callback (Mpauseclock cd.cd_clock.ck_gid) site.s_callbacks;
      Callbacks.remove_callback (Mnew_remote cd.cd_clock.ck_gid) site.s_callbacks

    let end_clock_domain new_cd new_ctrl f_k () =
      print_debug "End of clock domain %a@." print_cd new_cd;
      let site = get_site () in
      site.s_clock_domains <- C.GidMap.remove new_cd.cd_clock.ck_gid site.s_clock_domains;
      unregister_cd_callbacks new_cd;
      match new_cd.cd_clock.ck_parent with
        | Some ck ->
            if not (C.is_local ck.ck_gid) then
              Msgs.send_done ck.ck_gid new_cd.cd_clock.ck_gid ();
            end_ctrl new_ctrl f_k ()
        | None -> assert false

    let new_local_clock_domain cd new_balancer period location ctrl p f_k =
      let new_ck = mk_clock (Some cd.cd_clock) in
      print_debug "Creating local clock domain %a@." C.print_gid new_ck.ck_gid;
      let new_cd = mk_clock_domain new_ck new_balancer period location (Some ctrl) in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl f_k) in
      fun _ ->
        D.add_current f new_cd.cd_current;
        start_ctrl new_cd ctrl new_ctrl;
        exec_cd new_cd ()

    (* After receving Mnew_cd *)
    let create_cd msg =
      print_debug "newcd@.";
      let tmp_id, parent_ck, new_balancer, period, location, p = Msgs.recv_new_cd msg in
      print_debug "newcd@.";
      let new_ck = mk_clock (Some parent_ck) in
      let new_cd = mk_clock_domain new_ck new_balancer period location None in
      register_parent_callbacks new_cd;
      print_debug "Created local clock domain %a after request by %a@."
        print_cd new_cd   print_clock parent_ck;
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_cd new_ctrl dummy_step) in
      D.add_current f new_cd.cd_current;
      (* we wait for the start of the clock domain *)
      Msgs.send_cd_created tmp_id new_ck;
      print_debug "newcd done@."

    (* After receiving Mcd_created *)
    let start_remote_clock_domain cd ctrl f_k msg =
      print_debug "Starting remote clock domain@.";
      let site = get_site () in
      let new_ck = Msgs.recv_cd_created msg in
      print_debug "--%a" print_cd cd;
      decr cd.cd_remaining_async;
      update_remote_set cd (C.site_of_gid new_ck.ck_gid);
      let new_ctrl = new_ctrl (Clock_domain new_ck) in
      (* add local callbacks *)
      Callbacks.add_callback ~kind:Callbacks.Once
        (Mdone new_ck.ck_gid) (receive_done_cd cd new_ctrl f_k) site.s_callbacks;
      add_callback (Mstep_done new_ck.ck_gid) (receive_step_done cd ctrl new_ck.ck_gid);
      start_ctrl cd ctrl new_ctrl;
      (* send a message to start the execution *)
      step_remote_clock_domain cd new_ck.ck_gid ()

    let new_remote_clock_domain remote_site new_balancer period location cd ctrl p f_k =
      let site = get_site () in
      let tmp_id = C.fresh site.s_seed in
      add_callback (Mcd_created tmp_id) (start_remote_clock_domain cd ctrl f_k);
      print_debug "++%a@." print_cd cd;
      incr cd.cd_remaining_async;
      Msgs.send_new_cd remote_site (tmp_id, cd.cd_clock, new_balancer, period, location, p)

    let new_clock_domain cd ctrl p user_balancer period f_k _ =
      if cd.cd_location = L.Lleaf then
        new_local_clock_domain cd cd.cd_load_balancer period L.Lleaf ctrl p f_k ()
      else (
        let remote_site, location, new_balancer = cd.cd_load_balancer#new_child user_balancer  in
        if remote_site <> C.local_site () then (
          print_debug "Distributing new cd to site %a@." C.print_site remote_site;
          new_remote_clock_domain remote_site new_balancer period location cd ctrl p f_k
        ) else
          new_local_clock_domain cd new_balancer period location ctrl p f_k ()
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
       print_debug "Starting local clock domain %a on request@." C.print_gid cd_id;
       let site = get_site () in
       exec_cd (C.GidMap.find cd_id site.s_clock_domains) ()
     with
       | Not_found -> print_debug "Error: Received Start for unknown clock domain.@."

   (* After receiving Mbefore_eoi *)
    let receive_before_eoi msg =
      let ck_id:C.gid = C.from_msg msg in
      wake_up_now (Wbefore_eoi ck_id)

   (* After receiving Meoi *)
    let receive_eoi msg =
      let ck_id:C.gid = C.from_msg msg in
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

    let terminate_site site =
      Callbacks.stop_receiving site.s_msg_queue;
      if not (C.is_master ()) then (
        print_debug "Exiting slave@.";
        exit 0
      )

    (* After receiving Mfinalize *)
    let receive_finalize_site msg =
      let site = get_site () in
      (* stop sending messages, but keep receiving from others *)
      let main_site = Msgs.recv_finalize msg in
      Msgs.send_dummy main_site ();
      (* wait for all the other sites to be done sending *)
      let _ = Callbacks.recv_given_msg site.s_msg_queue Mfinalize in
      (* really terminate*)
      terminate_site site

    let init_site () =
      print_debug "Init site@.";
      let s = {
        s_top_clock_domain = None;
        s_clock_domains = C.GidMap.empty;
        (*  s_top_clock_domains = clock_domain list; *)
        s_msg_queue = Callbacks.mk_queue ();
        s_callbacks = Callbacks.mk_dispatcher ();
        s_waiting = WaitingMap.empty;
        s_clock_cache = GidHandle.mk_cache (fun ck -> ck);
        s_signal_cache = SignalHandle.mk_cache { SignalHandle.c_local_value = Event.signal_local_value };
        s_seed = C.mk_seed ();
        s_comm_site = C.local_site ();
        s_signals_remotes = C.GidMap.empty;
       (* s_children = C.SiteSet.empty; *)
      } in
      Local_ref.init 0 s;
      add_callback Mnew_cd create_cd;
      add_callback Mstep start_cd;
      add_callback Meoi receive_eoi;
      add_callback Mnext_instant receive_next_instant;
      add_callback Mcreate_signal receive_create_signal;
      add_callback Mfinalize receive_finalize_site;
      Callbacks.start_receiving s.s_msg_queue

    let start_slave () =
      while true do
        process_msgs ()
      done

    let finalize_top_clock_domain cd =
      let site = get_site () in
      let sites = C.all_sites () in
      (* tell all sites to stop sending messages *)
      if not (C.SiteSet.is_empty sites) then (
        Msgs.broadcast_finalize sites site.s_comm_site;
        (* wait for all sites to be done *)
        Callbacks.recv_n_given_msg site.s_msg_queue Mdummy (C.SiteSet.cardinal sites);
        (* tell all sites to stop executing *)
        Msgs.broadcast_finalize sites site.s_comm_site
      ) else
        (* Hack: Make sure that the receiving thread is started before terminating *)
        Thread.delay 0.050;
      terminate_site site

    let init_done = ref false
    let init () =
      if not !init_done then (
        init_done := true;
        init_site ();
        let site = get_site () in
        if C.is_master () then (
          (* create top clock domain *)
          let ck = mk_clock None in
          let balancer = L.mk_top_balancer () in
          let cd = mk_clock_domain ck balancer None L.Lany None in
          site.s_top_clock_domain <- Some cd
        ) else (
          (* just create a fresh id to keep the counters of
             all sites synchronized with the master, so that
             they can receive global signals *)
          let _ = C.fresh site.s_seed in ()
        )
      )

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f = D.add_current f cd.cd_current
    let on_current_instant_list cd fl = D.add_current_list fl cd.cd_current
    let on_next_instant ?(kind=Strong) ctrl f =
      match kind with
        | Strong -> D.add_next f ctrl.next
        | Weak -> D.add_next f ctrl.next_control

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
      let instance = ctrl.instance in
      let rec self _ =
        if ctrl.instance = instance then (
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
      let instance = ctrl.instance in
      let rec self _ =
        if ctrl.instance = instance then (
          if has_been_active ctrl ev then
            (*ctrl is activated, run continuation*)
            add_waiting (Weoi ev_ck.ck_gid) eoi_work
          else ((*ctrl is not active, wait end of instant*)
            let is_fired = ref false in
            D.add_next (ctrl_await is_fired) ctrl.next_control;
            add_waiting (Weoi ev_ck.ck_gid) (eoi_await is_fired)
          )
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
      let instance = ctrl.instance in
      let rec self _ =
        if ctrl.instance = instance then (
          if has_been_active ctrl ev then
            add_waiting (Weoi ev_ck.ck_gid) eoi_work
          else (
            print_debug "Signal %a was emitted, but ctrl is not active so keep waiting@." print_signal ev;
            add_waiting (Wsignal_wa ev.ev_gid) self
          )
        )
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

    let create_control kind body f_k ctrl cd =
      let new_ctrl = new_ctrl kind in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      match kind with
        | When ->
            fun evt other_cond ->
              let rec when_act _ =
                wake_up_ctrl new_ctrl cd;
                on_next_instant ctrl f_when
              and f_when _ =
                on_event evt ctrl when_act unit_value
              in
              new_ctrl.cond <- (fun () -> Event.status evt);
              fun () ->
                start_ctrl cd ctrl new_ctrl;
                new_ctrl.susp <- true;
                on_next_instant new_ctrl f;
                f_when ()
        | _ ->
            fun evt other_cond ->
              new_ctrl.cond <-
                (fun () -> Event.status ~only_at_eoi:true evt && other_cond (Event.value evt));
              fun () ->
                start_ctrl cd ctrl new_ctrl;
                f ()

    let create_control_evt_conf kind body f_k ctrl cd =
      let new_ctrl = new_ctrl kind in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      fun evt_cfg ->
        new_ctrl.cond <- (fun () -> Event.cfg_status ~only_at_eoi:true evt_cfg);
        fun () ->
          start_ctrl cd ctrl new_ctrl;
          f ()


end

module MpiRuntime =
  Make
    (Seq_runtime.ListDataStruct)
    (Mpi_communication.Make)
    (Callbacks.Make)
    (Sig_env.Record)

module MpiCRuntime =
  Make
    (Seq_runtime.ListDataStruct)
    (Mpi_communication.Make)
    (Callbacks.MakeC)
    (Sig_env.Record)

module MpiBufferedRuntime =
  Make
    (Seq_runtime.ListDataStruct)
    (Mpi_buffer_communication.Make)
    (Callbacks.Make)
    (Sig_env.Record)

