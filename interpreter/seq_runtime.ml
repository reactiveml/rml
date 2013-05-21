open Runtime
open Types

module JoinRef = struct
  type join_point = int ref

  let new_join_point nb = ref nb
  let incr j nb =
    j := !j + nb
  let decr j =
    decr j; !j = 0
end

module Make (D: Runtime.SEQ_DATA_STRUCT) (E: Sig_env.S) =
struct
    module D = D(struct
      type 'a t = 'a -> unit
    end)

    type 'a step = 'a -> unit

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
          mutable last_activation : (clock_domain * E.clock_index) list;
          mutable instance : int;
        }

    and clock_domain =
        { cd_current : D.current;
          cd_eoi : bool ref; (* is it the eoi of this clock *)
          cd_weoi : D.waiting_list; (* processes waiting for eoi *)
          cd_next_instant : D.waiting_list; (* processes waiting for the move to next instant *)
          mutable cd_wake_up : D.waiting_list list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : E.clock;
          mutable cd_top : control_tree;
          mutable cd_parent : clock_domain option;
          mutable cd_counter : int;
        }

    and clock = clock_domain
    and region = clock

    type ('a, 'b) event = ('a,'b) E.t * clock_domain * D.waiting_list * D.waiting_list
    type event_cfg =
      | Cevent of (bool -> bool) * clock_domain * D.waiting_list * D.waiting_list
      (* status, cd, wa, wp*)
      | Cand of event_cfg * event_cfg
      | Cor of event_cfg * event_cfg

    let unit_value = ()

    let top_clock_ref = ref None
    let get_top_clock_domain () =
      match !top_clock_ref with
        | None -> raise Types.RML
        | Some cd -> cd

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
        next = D.mk_next ();
        next_control = D.mk_next ();
        last_activation = [];
        instance = 0; }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true; (* set to true, to show that the node is no longer attached to its parent
                       and needs to be reattaced if the node is reused *)
      p.susp <- false;
      D.clear_next p.next;
      List.iter set_kill p.children;
      p.children <- [];
      p.instance <- p.instance + 1

    let rec save_clock_state cd =
      let l = match cd.cd_parent with
        | None -> []
        | Some cd -> save_clock_state cd
      in
        (cd, E.get cd.cd_clock)::l

    let start_ctrl cd ctrl new_ctrl =
      new_ctrl.last_activation <- save_clock_state cd;
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
            | Clock_domain _ -> true
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

      and next_to_current ck node =
        node.last_activation <- save_clock_state ck;
        D.add_current_next node.next ck.cd_current;
        D.add_current_next node.next_control ck.cd_current;
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
         p.last_activation <- save_clock_state cd;
         List.iter (next_to_current cd) p.children)

    (** Evaluates the condition of control nodes. This can be called several
        times for a same control node, when doing the eoi of several clocks.
        We can keep the last condition (if it was true for the eoi of the fast clock,
        it is also true for the eoi of the slow clock), but we have to make sure
        to fire the handler only once. *)
    let eoi_control ctrl =
      let rec _eoi_control pere ctrl =
        if ctrl.alive then (
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
      in
        List.iter (_eoi_control ctrl) ctrl.children

    let wake_up_ctrl new_ctrl cd =
      new_ctrl.susp <- false;
      next_to_current cd new_ctrl

    let mk_clock_domain parent =
      let cd = { cd_current = D.mk_current ();
        cd_eoi = ref false;
        cd_weoi = D.mk_waiting_list ();
        cd_next_instant = D.mk_waiting_list ();
        cd_wake_up = [];
        cd_clock = E.init_clock ();
        cd_top = new_ctrl Susp;
        cd_parent = parent;
        cd_counter = 0;
      } in
      cd.cd_top <- new_ctrl (Clock_domain cd);
      cd.cd_top.last_activation <- save_clock_state cd;
      cd

    let finalize_top_clock_domain _ =
      ()
    let init () =
      match !top_clock_ref with
        | None ->
            (* create top clock domain *)
            let cd = mk_clock_domain None in
            top_clock_ref := Some cd
        | Some _ -> () (* init already done *)

    let is_eoi cd = !(cd.cd_eoi)
    let control_tree cd = cd.cd_top
    let clock cd = cd
   (* let rec top_clock cd = match cd.cd_parent with
      | None -> cd
      | Some cd -> top_clock cd *)
    let top_clock () = match !top_clock_ref with
      | None -> Format.eprintf "No top clock@."; raise Types.RML
      | Some ck -> ck

    let add_weoi cd p =
      D.add_waiting p cd.cd_weoi
    let add_weoi_waiting_list cd w =
      cd.cd_wake_up <- w :: cd.cd_wake_up

(* debloquer les processus en attent d'un evt *)
    let wake_up ck w =
      D.add_current_waiting_list w ck.cd_current

    let wake_up_all ck =
      List.iter (fun wp -> D.add_current_waiting_list wp ck.cd_current) ck.cd_wake_up;
      ck.cd_wake_up <- []

(* ------------------------------------------------------------------------ *)

    module Join = JoinRef
    type join_point = JoinRef.join_point

    module Event =
      struct
        let new_evt_expr cd _ kind default combine =
          (E.create cd.cd_clock kind default combine, cd,
           D.mk_waiting_list (), D.mk_waiting_list ())

        let new_evt _ cd r kind default combine reset k =
          let evt = new_evt_expr cd r kind default combine in
          let k =
         (* create a callback to reset the signal at each eoi of the reset *)
            match reset with
              | None -> k
              | Some rck ->
                fun evt _ ->
                  let (n, _, _, _) = evt in
                  let w = Weak.create 1 in
                  Weak.set w 0 (Some evt);
                  let rec reset_evt () =
                    (match Weak.get w 0 with
                      | None -> () (* signal is no longer used, terminate *)
                      | Some evt ->
                        E.reset n;
                        add_weoi rck reset_evt)
                  in
                  add_weoi rck reset_evt;
                  k evt ()
          in
          k evt

        let new_evt_global kind default combine =
          let cd = get_top_clock_domain () in
          new_evt_expr cd cd kind default combine

        let status ?(only_at_eoi=false) (n,sig_cd,_,_) =
          E.status n && (not only_at_eoi || !(sig_cd.cd_eoi))

        let value (n,_,_,_) =
          if E.status n then
            E.value n
          else
            raise Types.RML

        let one (n,_,_,_) = E.one n
        let pre_status (n,_,_,_) = E.pre_status n
        let pre_value (n,_,_,_) = E.pre_value n
        let last (n,_,_,_) = E.last n
        let default (n,_,_,_) = E.default n
        let clock (_,sig_cd,_,_) = sig_cd

        let region_of_clock cd = cd

        let emit (n,sig_cd,wa,wp) v =
          E.emit n v;
          D.add_current_waiting_list wa sig_cd.cd_current;
          D.add_current_waiting_list wp sig_cd.cd_current

        let cfg_present ((n,sig_cd,wa,wp) as evt) =
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
      end

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f = D.add_current f cd.cd_current
    let on_current_instant_list cd fl = D.add_current_list fl cd.cd_current
    let on_next_instant ?(kind=Strong) ctrl f =
      match kind with
        | Strong -> D.add_next f ctrl.next
        | Weak -> D.add_next f ctrl.next_control

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
    let on_eoi cd f =
      if is_eoi cd then
        f unit_value
      else
        add_weoi cd f

  (** [on_event_or_next evt f_w cd ctrl f_next] executes 'f_w ()' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let _on_event_or_next (_,_,_,w) f_w cd ctrl f_next =
      let act _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          D.add_next f_next ctrl.next
        else
          (* signal activated *)
          f_w ()
      in
      D.add_waiting act w;
      add_weoi_waiting_list cd w

    let on_event_or_next evt f_w cd ctrl f_next =
      if Event.status evt then
        f_w ()
      else
        _on_event_or_next evt f_w cd ctrl f_next

  (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w ()' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let on_event_cfg_or_next evt_cfg f_w cd ctrl f_next =
      if Event.cfg_status evt_cfg then
        f_w ()
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
                   f_w ()))
        in
        let w_list = Event.cfg_events evt_cfg false in
        List.iter
          (fun (w,_) -> D.add_waiting try_fire w; add_weoi_waiting_list cd w) w_list

    let has_been_active ctrl sig_cd =
      let rec check_last_activation l = match l with
        | [] -> Format.eprintf "id not find the signal clock@."; false
        | (cd, ck)::l ->
            if cd == sig_cd then (
              ck = (E.get sig_cd.cd_clock)
            ) else
              check_last_activation l
      in
        check_last_activation ctrl.last_activation

    (** [on_event evt ctrl f] executes 'f ()' if evt is emitted and
        ctrl is active in the same step.
        It waits for the next activation of w otherwise,
        or if the call raises Wait_again *)
    let _on_event w sig_cd ctrl f =
      let instance = ctrl.instance in
      let rec try_launch () =
        try
          f ()
        with
          | Wait_again -> on_eoi sig_cd (fun () -> D.add_waiting self w)
      and self _ =
        if ctrl.instance = instance then (
          if has_been_active ctrl sig_cd then
            (*ctrl is activated, run continuation*)
            try_launch ()
          else ((*ctrl is not active, wait end of instant*)
            let is_fired = ref false in
            D.add_next (ctrl_await is_fired) ctrl.next_control;
            add_weoi sig_cd (eoi_await is_fired)
          )
        )
      and eoi_await is_fired _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           D.add_waiting self w)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (is_fired := true; try_launch ())
      in
      D.add_waiting self w

    let on_event (n,sig_cd,w,_) ctrl f =
      if E.status n then
        (try
           f ()
         with
           | Wait_again -> _on_event w sig_cd ctrl f)
      else
        _on_event w sig_cd ctrl f

    (** [on_event_cfg evt_cfg ctrl f ()] executes 'f ()' if evt_cfg is true and
        ctrl is active in the same step.
        It waits for the next activation of evt_cfg otherwise,
        or if the call raises Wait_again *)
    let on_event_cfg evt_cfg ctrl f =
      let wait_event_cfg () =
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            (if Event.cfg_status evt_cfg then
                (is_fired := true;
                 f ())
             else
                raise Wait_again)
        in
        let w_list = Event.cfg_events evt_cfg true in
        List.iter (fun (w,cd) -> _on_event w cd ctrl try_fire) w_list
      in
      if Event.cfg_status evt_cfg then
        (try
           f ()
         with
           | Wait_again -> wait_event_cfg ())
      else
        wait_event_cfg ()

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

    (* Control structures *)
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
                on_event evt ctrl when_act
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


    let rec schedule cd =
      match D.take_current cd.cd_current with
        | Some f -> f (); schedule cd
        | None -> ()

    let eoi cd =
      cd.cd_eoi := true;
      eoi_control cd.cd_top;
      wake_up cd cd.cd_weoi;
      wake_up_all cd;
      schedule cd

    let next_instant cd =
      E.next cd.cd_clock;
      (* next instant of child clock domains *)
      wake_up cd cd.cd_next_instant;
      schedule cd;
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd;
      cd.cd_eoi := false

    let rec has_next ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain _ -> has_next_children ctrl
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
      not (has_next cd.cd_top)

    let step_clock_domain ctrl new_ctrl cd new_cd period =
      let next_instant_clock_domain _ = next_instant new_cd in
      let rec f_cd () =
        new_cd.cd_counter <- new_cd.cd_counter + 1;
        schedule new_cd;
        if new_cd.cd_top.alive then (
          eoi new_cd;
          let period_finished = match period with
            | None -> false
            | Some p -> new_cd.cd_counter >= p
          in
          if period_finished || macro_step_done new_cd then (
            new_cd.cd_counter <- 0;
            D.add_waiting next_instant_clock_domain cd.cd_next_instant;
            D.add_next f_cd ctrl.next_control;
          ) else (
            next_instant new_cd;
            (* execute again in the same step but yield for now*)
            D.add_current f_cd cd.cd_current
          )
        )
      in
      f_cd

    let end_clock_domain new_ctrl f_k x =
      end_ctrl new_ctrl f_k x

    let new_clock_domain cd ctrl p _ period f_k =
      let new_cd = mk_clock_domain (Some cd) in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_ctrl f_k) in
      fun _ ->
        on_current_instant new_cd f;
        start_ctrl new_cd ctrl new_ctrl;
        step_clock_domain ctrl new_ctrl cd new_cd period unit_value

    (* the react function *)
    let react cd =
      schedule cd;
      eoi cd;
      next_instant cd

    (* Fake functions *)
    let start_slave _ = ()
    let is_master _ = true
end

module ListDataStruct (S: Runtime.STEP) =
struct
  module Step = S

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

  let current_length c =
    List.length !c

  let take_current c = match !c with
    | f :: l -> c := l; Some f
    | [] -> None

  let mk_waiting_list () = ref ([]:unit Step.t list)
  let add_waiting p w =
    w := p :: ! w
  let take_all w =
    let l = !w in
    w := [];
    l
  let waiting_length w =
    List.length !w

  let mk_next () = ref ([]:unit Step.t list)
  let add_next p next =
    next := p :: !next
  let add_next_next n1 n2 =
    n2 := List.rev_append !n1 !n2;
    n1 := []
  let clear_next next =
    next := []
  let is_empty_next next =
    !next = []
end


module ListListDataStruct (S: Runtime.STEP) =
struct
  module Step = S

  type next = unit Step.t list ref
  type current = unit Step.t list list ref
  type waiting_list = unit Step.t list ref

  let mk_current () = ref ([] : unit Step.t list list)
  let add_current p c = match !c with
    | [] -> c := [[p]]
    | l::tl -> c := (p::l)::tl
  let add_current_list pl c = match pl with
    | [] -> ()
    | _ -> c := pl::!c
  let add_current_waiting_list w c =
    match !w with
      | [] -> ()
      | l -> c := l::!c; w := []
  let add_current_next next c =
    match !next with
      | [] -> ()
      | l -> c := l::!c; next := []

  let current_length c =
    List.fold_left (fun acc l -> (List.length l) + acc) 0 !c

  let take_current c = match !c with
    | [] -> None
    | [f] :: tl -> c := tl; Some f
    | (f::l) :: tl -> c := l::tl; Some f
    | _ -> assert false

  let mk_waiting_list () = ref ([]:unit Step.t list)
  let add_waiting p w =
    w := p :: ! w
  let take_all w =
    let l = !w in
    w := [];
    l
  let waiting_length w =
    List.length !w

  let mk_next () = ref ([]:unit Step.t list)
  let add_next p next =
    next := p :: !next
  let add_next_next n1 n2 =
    n2 := List.rev_append !n1 !n2;
    n1 := []
  let clear_next next =
    next := []
  let is_empty_next next =
    !next = []
end


module SeqRuntime = Make(ListDataStruct)(Sig_env.Record)
module SeqListRuntime = Make(ListListDataStruct)(Sig_env.Record)
