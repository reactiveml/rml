open Runtime
open Runtime_options
open Types

let assert_some v = match v with
  | None -> assert false
  | Some v -> v

module E = Sig_env.Record
module D = Seq_runtime.ListListDataStruct

module Make
  (CF : functor (T : Communication.TAG_TYPE) -> Communication.S
   with type 'gid tag = 'gid T.t)
  (CALL : functor (C: Communication.S) -> Callbacks.S
   with type msg = C.msg and type tag = C.gid C.tag) =
struct
    module SiteMsgs = struct
      type 'gid t =
          | Mfinalize
          | Mfinalize_ack
          (* scheduling *)
          | Mnew_cd
          | Mstep (* do one global step of the clock domain *)
          | Mstep_done of 'gid (* a clock domain has finished its step *)
          | Mdone of 'gid (* global step done *)
          | Meoi (* End of instant of clock domain *)
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

      let dummy = Mfinalize_ack

      let flush_after tag = match tag with
         | Mfinalize | Mfinalize_ack | Mhas_next _ | Mvalue _ | Mnew_cd
         | Mreq_signal _ | Msignal_created _ | Mcreate_signal -> true
         | _ -> false

      open Format
      let print print_gid ff m = match m with
        | Mfinalize_ack -> fprintf ff "Mfinalize_ack"
        | Mfinalize -> fprintf ff "Mfinalize"
        | Mnew_cd -> fprintf ff "Mnew_cd"
        | Mstep -> fprintf ff "Mstep"
        | Mstep_done gid -> fprintf ff "Mstep_done %a" print_gid gid
        | Mdone gid -> fprintf ff "Mdone %a" print_gid gid
        | Meoi -> fprintf ff "Meoi"
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

    type control_type =
        | Clock_domain of clock option
        | Kill of unit step
        | Kill_handler of (unit -> unit step)
        | Susp
        | When

    let control_of_runtime_control k = match k with
      | Runtime.Kill (_, h) -> Kill h
      | Runtime.Kill_handler (_, h) -> Kill_handler h
      | Runtime.Susp -> Susp
      | Runtime.When -> When
      | _ -> assert false

    type control_tree =
        { kind: control_type;
          mutable alive: bool;
          mutable susp: bool;
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
        | Wafter_eoi of C.gid (* after the eoi *)
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
          mutable cd_eoi : bool; (* is it the eoi of this clock *)
          mutable cd_wake_up : waiting_kind list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : clock;
          mutable cd_top : control_tree;
          cd_parent_ctrl : control_tree option;
          cd_parent_self_id : C.gid option; (* id to send Mstep_done msgs to *)

          cd_load_balancer : L.load_balancer;
          cd_location : L.kind;
          mutable cd_children_have_next : bool;
          cd_active_children : int ref;
          mutable cd_last_activation : clock_state;
          cd_remaining_async : int ref;
          mutable cd_direct_remotes : C.SiteSet.t; (* remotes with direct child clock domains*)
          mutable cd_other_remotes : C.SiteSet.t; (* remotes with grand-child clock domains*)

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
      mutable s_remotes : C.SiteSet.t; (* remotes with child clock domains*)
    }

    let print_clock ff ck =
      C.print_gid ff ck.ck_gid
    let print_cd ff cd =
      C.print_gid ff cd.cd_clock.ck_gid
    let print_signal ff ev =
      C.print_gid ff ev.ev_gid

    let get_site () =
      (Local_ref.get 0:site)

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
    let add_callback_once site msg f =
      Callbacks.add_callback ~kind:Callbacks.Once msg f site.s_callbacks

    let get_clock site ck =
      GidHandle.get site.s_clock_cache ck
    let get_clock_domain site ck =
      try
        C.GidMap.find ck.ck_gid site.s_clock_domains
      with
        | Not_found ->
            IFDEF RML_DEBUG THEN
              print_debug "Error: Cannot find clock domain %a@." C.print_gid ck.ck_gid
            ELSE () END;
            assert false
    let get_event site ev =
      let n, _, _, _ =  SignalHandle.get site.s_signal_cache ev.ev_handle in
      n
    let get_event_clock site ev =
      let _, ck, _, _ = SignalHandle.get site.s_signal_cache ev.ev_handle in
      ck
    let get_event_region site ev =
      let _, _, r, _ = SignalHandle.get site.s_signal_cache ev.ev_handle in
      r
    let get_event_reset site ev =
      let _, _, _, reset = SignalHandle.get site.s_signal_cache ev.ev_handle in
      reset
    let get_event_whole site ev =
      SignalHandle.get site.s_signal_cache ev.ev_handle

    let process_msgs site () =
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
      let send_finalize_ack, recv_finalize_ack = mk_send_recv Mfinalize_ack
      let send_new_cd, recv_new_cd = mk_send_recv Mnew_cd
      let send_step, recv_step = mk_send_owner_recv Mstep
      let send_step_done, recv_step_done =
        (fun () ->
          let send dest_id id (d:'a) = C.send_owner dest_id (Mstep_done id) d in
          let recv = (C.from_msg : C.msg -> 'a) in
          send, recv) ()
      let send_done, recv_done =
        (fun () ->
          let send dest_id id (d:'a) = C.send_owner dest_id (Mdone id) d in
          let recv = (C.from_msg : C.msg -> 'a) in
          send, recv) ()
      let broadcast_eoi, recv_eoi = mk_broadcast_set_recv Meoi
      let send_has_next, recv_has_next =
        (fun () ->
          let send id (d:'a) = C.send_owner id (Mhas_next id) d in
          let recv = (C.from_msg : C.msg -> 'a) in
          send, recv) ()
      let send_new_remote, recv_new_remote =
        (fun () ->
          let send id (d:'a) = C.send_owner id (Mnew_remote id) d in
          let recv = (C.from_msg : C.msg -> 'a) in
          send, recv) ()
      let send_req_signal, recv_req_signal =
        (fun () ->
          let send id (d:'a) = C.send_owner id (Mreq_signal id) d in
          let recv = (C.from_msg : C.msg -> 'a) in
          send, recv) ()
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

      let get_clock_index site ck =
        E.get (GidHandle.get site.s_clock_cache ck.ck_clock)

      let empty_clock_state = []

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

      (** [update_clock ck] makes sure that the local copy of ck and
          all its ancestors in the clock tree are equal to the value stored in [ck]
          (which should have been received in a message). *)
      let update_clock site ck =
        let rec aux ck =
          let local_ck = GidHandle.get_local site.s_clock_cache ck.ck_clock in
          let stored_value = E.get (GidHandle.get_stored ck.ck_clock) in
          if not (E.equal stored_value (E.get local_ck)) then (
            IFDEF RML_DEBUG THEN
              print_debug "Updating value of clock %a from %a to %a@."
                print_clock ck  E.print_clock_index (E.get local_ck)
                E.print_clock_index stored_value
            ELSE () END;
            E.set local_ck stored_value;
            match ck.ck_parent with
              | None -> ()
              | Some pck -> aux pck
          )
        in
        aux ck
    end


(**************************************)
(* control tree                       *)
(**************************************)
    let new_ctrl kind =
      { kind = kind;
        alive = true;
        susp = false;
        children = [];
        cond_v = false;
        last_activation = Clock.empty_clock_state;
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
      new_ctrl.last_activation <- Clock.save_clock_state (get_site ()) cd.cd_clock;
      if new_ctrl.alive then (
        ctrl.children <- new_ctrl :: ctrl.children;
        new_ctrl.cond_v <- false
      ) else (* reset new_ctrl *)
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
    let eval_control_and_next_to_current site cd =
      let rec eval pere p active =
        if p.alive then
          match p.kind with
            | Clock_domain _ -> true
            | Kill f_k ->
              if p.cond_v
              then
                false
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
        node.last_activation <- Clock.save_clock_state site cd.cd_clock;
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
         p.last_activation <- Clock.save_clock_state (get_site ()) cd.cd_clock;
         List.iter (next_to_current cd) p.children)

    let wake_up_ctrl new_ctrl cd =
      new_ctrl.susp <- false;
      next_to_current cd new_ctrl

    let is_active ctrl =
      ctrl.alive && not ctrl.susp

    (** clock domains operations *)
    let mk_clock_domain site clock balancer period location parent_ctrl parent_self_id =
      let cd = {
        cd_current = D.mk_current ();
        cd_eoi = false;
        cd_wake_up = [];
        cd_clock = clock;
        cd_top = new_ctrl (Clock_domain (Some clock));
        cd_parent_ctrl = parent_ctrl;
        cd_parent_self_id = parent_self_id;
        cd_children_have_next = false;
        cd_active_children = ref 0;
        cd_last_activation = Clock.empty_clock_state;
        cd_load_balancer = balancer;
        cd_location = location;
        cd_remaining_async = ref 0;
        cd_other_remotes = C.SiteSet.empty;
        cd_direct_remotes = C.SiteSet.empty;
        cd_counter = 0;
        cd_period = period;
      } in
      site.s_clock_domains <- C.GidMap.add clock.ck_gid cd site.s_clock_domains;
      cd.cd_top.last_activation <- Clock.save_clock_state site cd.cd_clock;
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
      | None -> IFDEF RML_DEBUG THEN print_debug "No top clock@." ELSE () END; raise Types.RML
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

    (* Computes has_next, waiting for child clock domains if necessary *)
    let rec has_next_cd site cd =
      IFDEF RML_DEBUG THEN
        print_debug "Before Waiting: %d@." !(cd.cd_remaining_async)
      ELSE () END;
      let has_next_ctrl = has_next_children site cd cd.cd_top in
      (* Awaits Mhas_next from all remote clock domains *)
      C.flush ();
      while !(cd.cd_remaining_async) > 0 do
        IFDEF RML_DEBUG THEN
          print_debug "Waiting for %d has_next msgs@." !(cd.cd_remaining_async)
        ELSE () END;
        Callbacks.dispatch_given_msg site.s_msg_queue site.s_callbacks (Mhas_next cd.cd_clock.ck_gid)
      done;
      IFDEF RML_DEBUG THEN
        print_debug "has_next of %a: has_next_children=%b, self=%b@."
          print_cd cd cd.cd_children_have_next has_next_ctrl
      ELSE () END;
      has_next_ctrl || cd.cd_children_have_next
    and has_next site cd ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain None -> false (* waiting for a Mhas_next message from this clock domain *)
          | Clock_domain (Some ck) ->
              let cd = get_clock_domain site ck in
              has_next_cd site cd
          | Kill _ | Kill_handler _ ->
            ctrl.cond_v || has_next_children site cd ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond_v) || (not ctrl.susp && not (ctrl.cond_v)) in
              active && has_next_children site cd ctrl
          | When ->
            not ctrl.susp && has_next_children site cd ctrl
    and has_next_children site cd ctrl =
      not (D.is_empty_next ctrl.next) || List.exists (has_next site cd) ctrl.children

    let prepare_has_next cd _ =
      (* the top clock domain does not receive Mhas_next msgs*)
      if cd.cd_clock.ck_parent <> None then (
        IFDEF RML_DEBUG THEN
          if cd.cd_children_have_next = true then
            print_debug "have_next_children of %a is already true@." print_cd cd;
          print_debug "Waiting for %d Mhas_next msgs@." !(cd.cd_active_children)
        ELSE () END;
        cd.cd_children_have_next <- false;
        cd.cd_remaining_async := !(cd.cd_remaining_async) + !(cd.cd_active_children)
      )

    let send_has_next site cd _ =
      let pck = assert_some cd.cd_clock.ck_parent in
      let has_next = has_next_cd site cd in
      IFDEF RML_DEBUG THEN
        print_debug "send_has_next=%b from %a to %a@." has_next  print_cd cd  print_clock pck
      ELSE () END;
      Msgs.send_has_next pck.ck_gid has_next

    (* Register a handler to send Has_next msg to paremt clock for each
       ancestor clock that is not local*)
    let register_send_has_next site cd pck =
      let rec aux ck = match ck.ck_parent with
        | None -> () (* top ck, do nothing*)
        | Some pck ->
            (* make sure to register only once per instant of ck *)
            if not (C.is_local ck.ck_gid)
              && not (Clock.check_clock_state site cd.cd_last_activation ck) then
              add_waiting site (Wafter_eoi ck.ck_gid) (send_has_next site cd);
            aux pck
      in
      aux pck

    (* Register a handler that prepares for receiving has_next msgs from children
       for all ancestor clocks (except topck) *)
    let register_prepare_has_next site cd pck =
      let rec aux ck = match ck.ck_parent with
        | None -> () (* top ck, do nothing*)
        | Some pck ->
            (* make sure to register only once per instant of ck *)
            if not (Clock.check_clock_state site cd.cd_last_activation ck) then
              add_waiting site (Wbefore_eoi ck.ck_gid) (prepare_has_next cd);
            aux pck
      in
      aux pck

    let register_has_next_handlers site cd pck =
      (* if parent is not local, we will send has_next msgs *)
      if not (C.is_local pck.ck_gid) then
        register_send_has_next site cd pck;
      (* if we have remote cds, we will receive has_next msgs *)
      if !(cd.cd_active_children) > 0 then
        register_prepare_has_next site cd pck;
      (* remember when we last registered has_next handlers *)
      cd.cd_last_activation <- Clock.save_clock_state site cd.cd_clock

    let macro_step_done site cd =
      let has_next = has_next_cd site cd in
      IFDEF RML_DEBUG THEN
        print_debug "Macro step of clock domain %a: has_next = %b@."
          print_cd cd  has_next
      ELSE () END;
      not has_next

    let rec schedule cd =
      match D.take_current cd.cd_current with
        | Some f -> f (); schedule cd
        | None -> ()

    let eoi site cd =
      IFDEF RML_DEBUG THEN
        print_debug "Eoi of clock domain %a@." print_cd cd
      ELSE () END;
      wake_up_now site (Wbefore_eoi cd.cd_clock.ck_gid);
      cd.cd_eoi <- true;
      wake_up_now site (Weoi cd.cd_clock.ck_gid);
      prepare_has_next cd ();
      (* send Meoi only to direct children *)
      Msgs.broadcast_eoi cd.cd_direct_remotes cd.cd_clock.ck_gid;
      List.iter (wake_up_now site) cd.cd_wake_up;
      cd.cd_wake_up <- []

    let next_instant site cd =
      IFDEF RML_DEBUG THEN
        print_debug "Next instant of clock domain %a@." print_cd cd
      ELSE () END;
      E.next (get_clock site cd.cd_clock.ck_clock);
      wake_up_now site (Wnext_instant cd.cd_clock.ck_gid);
      (* next instant of this clock domain *)
      eval_control_and_next_to_current site cd;
      (* reset the clock domain *)
      cd.cd_eoi <- false;
      cd.cd_children_have_next <- false;
      cd.cd_active_children := 0

    let rec exec_cd site cd () =
      IFDEF RML_DEBUG THEN
        print_debug "Executing clock domain %a@." print_cd cd
      ELSE () END;
      schedule cd;
      cd.cd_counter <- cd.cd_counter + 1;
      if cd.cd_top.alive then (
        if !(cd.cd_remaining_async) = 0 then (
          eoi site cd;
          match cd.cd_clock.ck_parent with
            | None -> (* top clock domain *)
                next_instant site cd
                  (*Format.eprintf "Top clock domain next_instnt done : %d@." !(cd.cd_remaining_async)*)
            | Some ck ->
                let is_done = macro_step_done site cd in
                let period_finished = match cd.cd_period with
                  | None -> false
                  | Some p -> cd.cd_counter >= p
                in
                if is_done || period_finished then (
                  cd.cd_counter <- 0;
                  IFDEF RML_DEBUG THEN
                    print_debug "Macro step of clock domain %a is done@." print_cd cd
                  ELSE () END;
                  add_waiting site (Wnext_instant ck.ck_gid) (fun () -> next_instant site cd);
                  (* register handlers for sending and receiving Mhas_next msgs *)
                  register_has_next_handlers site cd ck;
                  (* if the parent clock is not here, send Done message*)
                  if not (C.is_local ck.ck_gid) then (
                    Msgs.send_step_done ck.ck_gid (assert_some cd.cd_parent_self_id) cd.cd_clock.ck_gid
                  ) else (
                    (match cd.cd_parent_ctrl with
                      | None -> assert false
                      | Some ctrl -> D.add_next (exec_cd site cd) ctrl.next_control)
                  )
                ) else (
                  next_instant site cd;
                  (* do another step*)
                  exec_cd site cd ()
                )
        ) else
          match cd.cd_clock.ck_parent with
            | None -> (* top clock, wait for msgs *)
                IFDEF RML_DEBUG THEN
                  print_debug "Top clock domain %a is waiting for %d children@."
                    print_cd cd  !(cd.cd_remaining_async)
                ELSE () END;
                process_msgs site ();
                exec_cd site cd ()
            | Some ck ->
                (* parent is local, increment its async counter before returning *)
                if C.is_local ck.ck_gid then (
                  let parent_cd = get_clock_domain site ck in
                  incr parent_cd.cd_remaining_async
                );
                IFDEF RML_DEBUG THEN
                  print_debug "Execution of clock domain %a is suspended until a message is received@."
                    print_cd cd
                ELSE () END
      ) else
        IFDEF RML_DEBUG THEN
          print_debug "Clock domain %a is dead@." print_cd cd
        ELSE () END
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
    let react cd = exec_cd (get_site ()) cd ()

    (* After receiving Mhas_next *)
    let gather_has_next cd msg =
      let has_next:bool = Msgs.recv_has_next msg in
      decr cd.cd_remaining_async;
      IFDEF RML_DEBUG THEN
        print_debug "--%a@." print_cd cd;
        if !(cd.cd_remaining_async) < 0 then
          print_debug "Error: counter of %a is < 0@." print_cd cd;
        print_debug "Received has_next=%b for %a@." has_next print_cd cd
      ELSE () END;
      cd.cd_children_have_next <- has_next or cd.cd_children_have_next

    let step_remote_clock_domain cd ck_id () =
      IFDEF RML_DEBUG THEN print_debug "++%a@." print_cd cd ELSE () END;
      incr cd.cd_remaining_async;
      Msgs.send_step ck_id (cd.cd_clock, ck_id)

    let rec wake_up_cd_if_done site cd =
      if !(cd.cd_remaining_async) = 0 then (
        match cd.cd_clock.ck_parent with
          | None -> () (* the cd is already executing, waiting for messages *)
          | Some ck ->
              IFDEF RML_DEBUG THEN
                print_debug "Waking up clock domain %a after receiving a message@." print_cd cd
              ELSE () END;
              if C.is_local ck.ck_gid then (
                let parent_cd = get_clock_domain site ck in
                exec_cd site cd ();
                IFDEF RML_DEBUG THEN print_debug "--%a@." print_cd parent_cd ELSE () END;
                decr parent_cd.cd_remaining_async;
                IFDEF RML_DEBUG THEN
                  if !(parent_cd.cd_remaining_async) < 0 then
                    print_debug "Error: counter of %a is < 0@." print_cd parent_cd
                ELSE () END;
                wake_up_cd_if_done site parent_cd
              ) else
                exec_cd site cd ()
      )

    (* After receving Mstep_done*)
    let receive_step_done site cd ctrl msg =
      let remote_ck_id = Msgs.recv_step_done msg in
      D.add_next (step_remote_clock_domain cd remote_ck_id) ctrl.next_control;
      IFDEF RML_DEBUG THEN print_debug "--%a@." print_cd cd ELSE () END;
      decr cd.cd_remaining_async;
      incr cd.cd_active_children;
      IFDEF RML_DEBUG THEN
        if !(cd.cd_remaining_async) < 0 then
          print_debug "Error: counter of %a is < 0@." print_cd cd
      ELSE () END;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done site cd

    (* After receiving Mdone *)
    let receive_done_cd site cd new_ctrl f_k _ =
      set_kill new_ctrl;
      IFDEF RML_DEBUG THEN print_debug "--%a@." print_cd cd ELSE () END;
      decr cd.cd_remaining_async;
      IFDEF RML_DEBUG THEN
        if !(cd.cd_remaining_async) < 0 then
          print_debug "Error: counter of %a is < 0@." print_cd cd
      ELSE () END;
      D.add_current f_k cd.cd_current;
      (* wake up cd to emit the done message *)
      wake_up_cd_if_done site cd

    let rec update_remote_set here cd is_direct site =
      let set =
        if is_direct then
          cd.cd_direct_remotes
        else
          cd.cd_other_remotes
      in
      if not (C.SiteSet.mem site set) then (
        if C.SiteSet.is_empty cd.cd_direct_remotes then (
          IFDEF RML_DEBUG THEN
            print_debug "Allocating callbacks for %a@." print_cd cd
          ELSE () END;
          register_cd_callbacks here cd
        );
        IFDEF RML_DEBUG THEN
          print_debug "Adding site %a to remote sites of %a@." C.print_site site  print_cd cd
        ELSE () END;
        if is_direct then
          cd.cd_direct_remotes <- C.SiteSet.add site cd.cd_direct_remotes
        else
          cd.cd_other_remotes <- C.SiteSet.add site cd.cd_other_remotes;
        (* add to the site site too *)
        if is_direct then (
          IFDEF RML_DEBUG THEN
            print_debug "Adding site %a to site remotes." C.print_site site
          ELSE () END;
          here.s_remotes <- C.SiteSet.add site here.s_remotes
        );
        (* propagate the information to the parent *)
        match cd.cd_clock.ck_parent with
          | None -> ()
          | Some pck when C.is_local pck.ck_gid ->
              let pcd = get_clock_domain here pck in
              update_remote_set here pcd is_direct site
          | Some pck -> Msgs.send_new_remote pck.ck_gid site
      )

    (* After receiving Mnew_remote *)
    and receive_new_remote here cd msg =
      let site = Msgs.recv_new_remote msg in
      update_remote_set here cd false site

    (* Registers callbacks for messages sent by child cds *)
    and register_cd_callbacks site cd =
      add_callback site (Mhas_next cd.cd_clock.ck_gid) (gather_has_next cd);
      add_callback site (Mnew_remote cd.cd_clock.ck_gid) (receive_new_remote site cd)

    let unregister_cd_callbacks site cd =
      Callbacks.remove_callback (Mhas_next cd.cd_clock.ck_gid) site.s_callbacks;
      Callbacks.remove_callback (Mnew_remote cd.cd_clock.ck_gid) site.s_callbacks

    let end_clock_domain site new_cd new_ctrl f_k () =
      IFDEF RML_DEBUG THEN print_debug "End of clock domain %a@." print_cd new_cd ELSE () END;
      site.s_clock_domains <- C.GidMap.remove new_cd.cd_clock.ck_gid site.s_clock_domains;
      unregister_cd_callbacks site new_cd;
      match new_cd.cd_clock.ck_parent with
        | Some ck ->
            if not (C.is_local ck.ck_gid) then
              Msgs.send_done ck.ck_gid (assert_some new_cd.cd_parent_self_id) ();
            end_ctrl new_ctrl f_k ()
        | None -> assert false

    let new_local_clock_domain site cd new_balancer period location ctrl p f_k =
      let new_ck = mk_clock (Some cd.cd_clock) in
      IFDEF RML_DEBUG THEN
        print_debug "Creating local clock domain %a@." C.print_gid new_ck.ck_gid
      ELSE () END;
      let new_cd = mk_clock_domain site new_ck new_balancer period location (Some ctrl) None in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain site new_cd new_ctrl f_k) in
      fun _ ->
        D.add_current f new_cd.cd_current;
        start_ctrl new_cd ctrl new_ctrl;
        exec_cd site new_cd ()

    (* After receving Mnew_cd *)
    let create_cd site msg =
      let tmp_id, parent_ck, new_balancer, period, location, p = Msgs.recv_new_cd msg in
      let new_ck = mk_clock (Some parent_ck) in
      let new_cd = mk_clock_domain site new_ck new_balancer period location None (Some tmp_id) in
      IFDEF RML_DEBUG THEN
        print_debug "Created local clock domain %a after request by %a@."
          print_cd new_cd   print_clock parent_ck
      ELSE () END;
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain site new_cd new_ctrl dummy_step) in
      D.add_current f new_cd.cd_current;
      (* execute the first step *)
      IFDEF RML_DEBUG THEN
        print_debug "Starting local clock domain %a on request@." C.print_gid new_ck.ck_gid
      ELSE () END;
      Clock.update_clock site parent_ck;
      exec_cd site new_cd ()

    let new_remote_clock_domain site remote_site new_balancer period location cd ctrl p f_k =
      let tmp_id = C.fresh site.s_seed in
      update_remote_set site cd true remote_site;
      let new_ctrl = new_ctrl (Clock_domain None) in
      (* add local callbacks *)
      add_callback_once site (Mdone tmp_id) (receive_done_cd site cd new_ctrl f_k);
      add_callback site (Mstep_done tmp_id) (receive_step_done site cd ctrl);
      start_ctrl cd ctrl new_ctrl;
      (* send a message to start the execution *)
      IFDEF RML_DEBUG THEN print_debug "Starting remote clock domain@." ELSE () END;
      IFDEF RML_DEBUG THEN print_debug "++%a@." print_cd cd ELSE () END;
      incr cd.cd_remaining_async;
      Msgs.send_new_cd remote_site (tmp_id, cd.cd_clock, new_balancer, period, location, p)

    let new_clock_domain cd ctrl p user_balancer period f_k _ =
      let site = get_site () in
      if cd.cd_location = L.Lleaf then
        new_local_clock_domain site cd cd.cd_load_balancer period L.Lleaf ctrl p f_k ()
      else (
        let remote_site, location, new_balancer = cd.cd_load_balancer#new_child user_balancer  in
        if remote_site <> C.local_site () then (
          IFDEF RML_DEBUG THEN
            print_debug "Distributing new cd to site %a@." C.print_site remote_site
          ELSE () END;
          new_remote_clock_domain site remote_site new_balancer period location cd ctrl p f_k
        ) else
          new_local_clock_domain site cd new_balancer period location ctrl p f_k ()
     )

   module Join = Seq_runtime.JoinRef
   type join_point = Seq_runtime.JoinRef.join_point

   module Event =
      struct
        let lift_handle f ev =
          f (get_event (get_site ()) ev)

        let value ev =
          let n = get_event (get_site ()) ev in
          if E.status n then
            E.value n
          else (
            IFDEF RML_DEBUG THEN
              print_debug "Error: Reading the value of an absent signal %a@." C.print_gid ev.ev_gid
            ELSE () END;
            raise Types.RML
          )

        let one ev = lift_handle E.one ev
        let pre_status ev = lift_handle E.pre_status ev
        let pre_value ev = lift_handle E.pre_value ev
        let last ev = lift_handle E.last ev
        let default ev = lift_handle E.default ev
        let clock ev = get_event_clock (get_site ()) ev

        let region_of_clock ck = ck

        let get_signal_remotes site gid =
          try
            C.GidMap.find gid site.s_signals_remotes
          with
            | Not_found -> C.SiteSet.empty

        let add_signal_remote site s gid =
          let set =
            if C.GidMap.mem gid site.s_signals_remotes then
              let set = C.GidMap.find gid site.s_signals_remotes in
              C.SiteSet.add s set
            else
              C.SiteSet.singleton s
          in
          site.s_signals_remotes <- C.GidMap.add gid set site.s_signals_remotes

        let send_value_to_remotes site cd ev n () =
          let remotes =
            if !Runtime_options.use_signals_users_set then
              get_signal_remotes site ev.ev_gid
            else
              C.SiteSet.union cd.cd_direct_remotes cd.cd_other_remotes
          in
          C.broadcast_set remotes (Mvalue ev.ev_gid) (E.value n)

        let do_emit site ev v =
          IFDEF RML_DEBUG THEN
            print_debug "Emitting a value for %a@." print_signal ev
          ELSE () END;
          let n, ev_ck, ev_r, _ = get_event_whole site ev in
          let already_present = E.status n in
          IFDEF RML_DEBUG THEN
            if not (C.is_local ev_r.ck_gid) then
              print_debug "Error: do_emit on remote signal %a at region %a of clock %a@."
                C.print_gid ev.ev_gid   print_clock ev_r  print_clock ev_ck
          ELSE () END;
          E.emit n v;
          (* wake up processes awaiting this signal *)
          wake_up_now site (Wsignal_wa ev.ev_gid);
          wake_up_now site (Wsignal_wp ev.ev_gid);
          (* if we have remote clock domains, we should send them the value later *)
          if not already_present then (
            let cd = get_clock_domain site ev_r in
            if cd.cd_location = L.Lany then
              add_waiting site (Wbefore_eoi ev_ck.ck_gid)
                (send_value_to_remotes site cd ev n)
          )

        let emit ev v =
          if C.is_local ev.ev_gid then (
            do_emit (get_site ()) ev v
          ) else
            C.send_owner ev.ev_gid (Memit ev.ev_gid) v

        (* Called after receiving Mreq_signal id *)
        let receive_req site (n : ('a, 'b) E.t) gid msg =
          let req_id = Msgs.recv_req_signal msg in
          if !Runtime_options.use_signals_users_set then
            add_signal_remote site (C.site_of_gid req_id) gid;
          C.send_owner req_id (Msignal gid) n

        (* Called after receiving Memit id *)
        let receive_emit site (ev:('a, 'b) event) msg =
          let v:'a = C.from_msg msg in
            do_emit site ev v

        (* Called after receiving Mvalue id *)
        let update_local_value site gid (n : ('a, 'b) E.t) msg =
          let v:'b = C.from_msg msg in
            E.set_value n v;
            wake_up_now site (Wsignal_wa gid)

        (* create callback for resetting the signal *)
        (* TODO: use weak pointer *)
        let add_reset site gid n ck reset =
          match reset with
            | None -> ()
            | Some rck ->
              let rec reset_evt () =
                IFDEF RML_DEBUG THEN
                  print_debug "Resetting signal %a of clock %a on reset clock %a@."
                  C.print_gid gid  print_clock ck  print_clock rck
                ELSE () END;
                E.reset n;
                add_waiting site (Weoi rck.ck_gid) reset_evt
              in
              add_waiting site (Weoi rck.ck_gid) reset_evt

        let setup_local_copy site gid n ck reset =
          add_callback site (Mvalue gid) (update_local_value site gid n);
          E.set_clock n (get_clock site ck.ck_clock);
          add_reset site gid n ck reset

        (* Called when a local process tries to access a remote signal for the first time.
           Creates local callbacks for this signal. *)
        (** TODO: remove callbacks when signal is no longer needed *)
        let signal_local_value gid (n, ck, r, reset) =
          let site = get_site () in
          (* request the current value of the signal *)
          Msgs.send_req_signal gid (C.fresh site.s_seed);
          setup_local_copy site gid n ck reset;
          let msg = Callbacks.recv_given_msg site.s_msg_queue (Msignal gid) in
          (* set the local value to the one received *)
          let new_n = C.from_msg msg in
          E.copy n new_n;
          n, ck, r, reset

        let new_local_evt site ck r is_memory default combine reset =
          let n = E.create (get_clock site ck.ck_clock) is_memory default combine in
          let gid = C.fresh site.s_seed in
          let ev  =
            { ev_handle = SignalHandle.init site.s_signal_cache gid (n, ck, r, reset);
              ev_gid = gid }
          in
          IFDEF RML_DEBUG THEN
            print_debug "Created signal %a at region %a of clock %a@." print_signal ev
              print_clock r  print_clock ck
          ELSE () END;
          if (get_clock_domain site r).cd_location = L.Lany then (
            add_callback site (Memit gid) (receive_emit site ev);
            add_callback site (Mreq_signal gid) (receive_req site n gid)
          );
          add_reset site gid n ck reset;
          ev

        let new_remote_evt site local_cd ck r is_memory
            default (combine : 'a -> 'b -> 'b) reset k =
          let tmp_id = C.fresh site.s_seed in
          let create_signal () =
            let site = get_site () in
            let ev = new_local_evt site ck r is_memory default combine reset in
            if !Runtime_options.use_signals_users_set then
              add_signal_remote site (C.site_of_gid tmp_id) ev.ev_gid;
            IFDEF RML_DEBUG THEN
              print_debug "Created signal %a at region %a of clock %a from request by %a@." print_signal ev
                print_clock r   print_clock ck  C.print_gid tmp_id
            ELSE () END;
            C.send_owner tmp_id (Msignal_created tmp_id) ev
          in
          let recv_signal_created msg =
            let ev:('a, 'b) event = C.from_msg msg in
            decr local_cd.cd_remaining_async;
            IFDEF RML_DEBUG THEN
              print_debug "Received created signal %a at %a@." print_signal ev print_cd local_cd;
              print_debug "--%a@." print_cd local_cd;
              if !(local_cd.cd_remaining_async) < 0 then
                print_debug "Error: counter of %a is < 0@." print_cd local_cd;
            ELSE () END;
            SignalHandle.set_valid ev.ev_handle;
            let n = get_event site ev in
            setup_local_copy site ev.ev_gid n ck reset;
            D.add_current (k ev) local_cd.cd_current;
            wake_up_cd_if_done site local_cd
          in
          add_callback_once site (Msignal_created tmp_id) recv_signal_created;
          IFDEF RML_DEBUG THEN print_debug "++%a@." print_cd local_cd ELSE () END;
          incr local_cd.cd_remaining_async;
          Msgs.send_create_signal r.ck_gid (tmp_id, create_signal)

        let new_evt local_cd ck r is_memory default combine reset k _ =
          let site = get_site () in
          let r = if !Runtime_options.use_local_slow_signals then r else ck in
          if C.is_local r.ck_gid then
            let evt = new_local_evt site ck r is_memory default combine reset in
            k evt ()
          else
            new_remote_evt site local_cd ck r is_memory default combine reset k

        let new_evt_expr ck r is_memory default combine =
          let site = get_site () in
          let r = if !Runtime_options.use_local_slow_signals then r else ck in
          if C.is_local r.ck_gid then (
            new_local_evt site ck r is_memory default combine None
          ) else (
            IFDEF RML_DEBUG THEN
              print_debug "Trying to create a remote signal inside an expression@.";
            ELSE () END;
            raise Types.RML
          )


        let new_evt_global is_memory default (combine : 'a -> 'b -> 'b) =
          let site = get_site () in
          if C.is_master () then (
            (* create the signal *)
            let ck = top_clock () in
            let ev = new_local_evt site ck ck is_memory default combine None in
            (* send it to all other sites *)
            print_debug "Sending global signal %a@." print_signal ev;
            C.broadcast (Msignal ev.ev_gid) ev;
            if !Runtime_options.use_signals_users_set then
              C.SiteSet.iter
                (fun s -> if s <> C.local_site () then add_signal_remote site s ev.ev_gid)
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
            let n, ck, _, _ = get_event_whole site ev in
            setup_local_copy site ev.ev_gid n ck None;
            ev
          )

        let status ?(only_at_eoi=false) ev =
          let n = get_event (get_site ()) ev in
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
   let receive_step site msg =
     let pck, cd_id = Msgs.recv_step msg in
     try
       let cd = C.GidMap.find cd_id site.s_clock_domains in
       (* first do the end of instant*)
       Clock.update_clock site pck;
       next_instant site cd;
       (* then do the new step *)
       IFDEF RML_DEBUG THEN
         print_debug "Starting local clock domain %a on request@." C.print_gid cd_id
       ELSE () END;
       exec_cd site cd ()
     with
       | Not_found -> print_debug "Error: Received Start for unknown clock domain.@."

   (* After receiving Meoi *)
    let receive_eoi site msg =
      let ck_id:C.gid = C.from_msg msg in
      (* first, emit the value of local slow signals to remotes *)
      wake_up_now site (Wbefore_eoi ck_id);
      (* then forward eoi to remotes *)
      Msgs.broadcast_eoi site.s_remotes ck_id;
      (* and do local eoi *)
      wake_up_now site (Weoi ck_id);
      (* wake up processes that send has_next msgs *)
      wake_up_now site (Wafter_eoi ck_id)

    (* After receiving Mcreate_signal *)
    let receive_create_signal msg =
      let tmp_id, f = Msgs.recv_create_signal msg in
      (* create the signal and send the created value *)
      f ()

    let terminate_site site =
      Callbacks.stop_receiving site.s_msg_queue;
      if not (C.is_master ()) then (
        IFDEF RML_DEBUG THEN print_debug "Exiting slave@." ELSE () END;
        exit 0
      )

    (* After receiving Mfinalize *)
    let receive_finalize_site site msg =
      (* stop sending messages, but keep receiving from others *)
      let main_site = Msgs.recv_finalize msg in
      Msgs.send_finalize_ack main_site ();
      (* wait for all the other sites to be done sending *)
      let _ = Callbacks.recv_given_msg site.s_msg_queue Mfinalize in
      (* really terminate*)
      terminate_site site

    let init_site () =
      IFDEF RML_DEBUG THEN print_debug "Init site@." ELSE () END;
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
        s_remotes = C.SiteSet.empty;
      } in
      Local_ref.init 0 s;
      add_callback s Mnew_cd (create_cd s);
      add_callback s Mstep (receive_step s);
      add_callback s Meoi (receive_eoi s);
      add_callback s Mcreate_signal receive_create_signal;
      add_callback s Mfinalize (receive_finalize_site s);
      Callbacks.start_receiving s.s_msg_queue

    let start_slave () =
      let site = get_site () in
      while true do
        process_msgs site ()
      done

    let finalize_top_clock_domain cd =
      let site = get_site () in
      let sites = C.all_sites () in
      (* tell all sites to stop sending messages *)
      if not (C.SiteSet.is_empty sites) then (
        Msgs.broadcast_finalize sites site.s_comm_site;
        (* wait for all sites to be done *)
        Callbacks.recv_n_given_msg site.s_msg_queue Mfinalize_ack (C.SiteSet.cardinal sites);
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
          let cd = mk_clock_domain (get_site ()) ck balancer None L.Lany None None in
          site.s_top_clock_domain <- Some cd
        ) else (
          (* just create a fresh id to keep the counters of
             all sites synchronized with the master, so that
             they can receive global signals *)
          let _ = C.fresh site.s_seed in ()
        )
      )

    let get_top_clock_domain () =
      let site = get_site () in
      match site.s_top_clock_domain with
        | None ->
            IFDEF RML_DEBUG THEN print_debug "No top clock domain@." ELSE () END;
            raise Types.RML
        | Some cd -> cd

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
      let site = get_site () in
      if C.is_local ck.ck_gid && is_eoi (get_clock_domain site ck) then
        f unit_value
      else
        add_waiting site (Weoi ck.ck_gid) f

  (** [on_event_or_next evt f_w cd ctrl f_next] executes 'f_w ()' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let _on_event_or_next site  ev f_w cd ctrl f_next =
      let act _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          D.add_next f_next ctrl.next
        else
          (* signal activated *)
          f_w ()
      in
      add_waiting site (Wsignal_wp ev.ev_gid) act;
      add_weoi_waiting_list cd (Wsignal_wp ev.ev_gid)

    let on_event_or_next ev f_w cd ctrl f_next =
      let site = get_site () in
      let n = get_event site ev in
      if E.status n then
        f_w ()
      else
        _on_event_or_next site ev f_w cd ctrl f_next

    let on_event_cfg_or_next evt_cfg f_w cd ctrl f_next =
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

    let has_been_active site ctrl ev =
      let ev_ck = get_event_clock site ev in
      Clock.check_clock_state site ctrl.last_activation ev_ck

    (** [on_event evt ctrl f] executes 'f ()' if evt is emitted and
        ctrl is active in the same step.
        It waits for the next activation of w otherwise,
        or if the call raises Wait_again *)
    let _on_local_event site ev ev_ck ctrl f =
      let n = get_event site ev in
      let instance = ctrl.instance in
      let rec try_launch _ =
        try
          f ()
        with
          | Wait_again ->
            on_eoi ev_ck (fun () -> add_waiting site (Wsignal_wa ev.ev_gid) self)
      and self _ =
        if ctrl.instance = instance then (
          if has_been_active site ctrl ev then
            (*ctrl is activated, run continuation*)
            try_launch ()
          else ((*ctrl is not active, wait end of instant*)
            let is_fired = ref false in
            D.add_next (ctrl_await is_fired) ctrl.next_control;
            add_waiting site (Weoi ev_ck.ck_gid) (eoi_await is_fired)
          )
        )
      and eoi_await is_fired _ =
        if not !is_fired then
          (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           add_waiting site (Wsignal_wa ev.ev_gid) self)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (is_fired := true; try_launch ())
      in
      if E.status n then
        try_launch ()
      else
        add_waiting site (Wsignal_wa ev.ev_gid) self

    let _on_remote_event site ev ev_ck ctrl f =
      let instance = ctrl.instance in
      let rec self _ =
        if ctrl.instance = instance then (
          if has_been_active site ctrl ev then
            (*ctrl is activated, run continuation*)
            (try
                f ()
              with
                | Wait_again -> add_waiting site (Wsignal_wa ev.ev_gid) self)
          else (
            IFDEF RML_DEBUG THEN
              print_debug "Signal %a was emitted, but ctrl is not active so keep waiting@." print_signal ev
            ELSE () END;
            add_waiting site (Wsignal_wa ev.ev_gid) self
          )
        )
      in
        add_waiting site (Wsignal_wa ev.ev_gid) self

    let on_event ev ctrl f =
      let site = get_site () in
      let ev_ck = get_event_clock site ev in
      if C.is_local ev.ev_gid then
        _on_local_event site ev ev_ck ctrl f
      else
        _on_remote_event site ev ev_ck ctrl f

    let on_event_at_eoi evt ctrl f =
      on_event evt ctrl (fun () -> on_eoi (Event.clock evt) f)


    let on_event_cfg evt_cfg ctrl f =
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

    let create_control kind body f_k ctrl cd =
      let kind = control_of_runtime_control kind in
      let new_ctrl = new_ctrl kind in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      match kind with
        | When ->
            (fun evt other_cond ->
              let rec when_act _ =
                wake_up_ctrl new_ctrl cd;
                on_next_instant ctrl f_when
              and f_when _ =
                on_event evt ctrl when_act
              in
              fun () ->
                start_ctrl cd ctrl new_ctrl;
                new_ctrl.susp <- true;
                on_next_instant new_ctrl f;
                f_when ())
        | Kill_handler f_handler ->
            (fun evt other_cond ->
              let rec handler () =
                if other_cond (Event.value evt) then (
                  new_ctrl.cond_v <- true;
                  D.add_next (f_handler ()) ctrl.next;
                  set_kill new_ctrl
                ) else
                  (* wait again *)
                  on_next_instant ctrl f_until
              and f_until () =
                on_event_at_eoi evt ctrl handler
              in
              fun () ->
                on_event_at_eoi evt ctrl handler;
                start_ctrl cd ctrl new_ctrl;
                f ())
        | Kill f_k ->
            (fun evt other_cond ->
              let rec handler () =
                if other_cond (Event.value evt) then (
                  new_ctrl.cond_v <- true;
                  D.add_next f_k ctrl.next;
                  set_kill new_ctrl
                ) else
                  on_next_instant ctrl f_until
              and f_until () =
                on_event_at_eoi evt ctrl handler
              in
              fun () ->
                on_event_at_eoi evt ctrl handler;
                start_ctrl cd ctrl new_ctrl;
                f ())
        | Susp ->
            (fun evt other_cond ->
              let rec handler () =
                if other_cond (Event.value evt) then
                  new_ctrl.cond_v <- true;
                  on_next_instant ctrl f_susp
              and f_susp () =
                on_event_at_eoi evt ctrl handler
              in
              fun () ->
                on_event_at_eoi evt ctrl handler;
                start_ctrl cd ctrl new_ctrl;
                f ())
        | Clock_domain _ -> assert false

    let create_control_evt_conf kind body f_k ctrl cd =
      let kind = control_of_runtime_control kind in
      let new_ctrl = new_ctrl kind in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      fun evt_cfg ->
        (*new_ctrl.cond <- (fun () -> Event.cfg_status ~only_at_eoi:true evt_cfg);*)
        fun () ->
          start_ctrl cd ctrl new_ctrl;
          f ()


end

module MpiRuntime =
  Make
    (Mpi_communication.Make)
    (Callbacks.Make)

module MpiCRuntime =
  Make
    (Mpi_communication.Make)
    (Callbacks.MakeC)

module MpiBufferedRuntime =
  Make
    (Mpi_buffer_communication.Make)
    (Callbacks.Make)

