open Runtime
open Rml_types

module JoinRef = struct
  type join_point = int ref

  let new_join_point nb = ref nb
  let incr j nb =
    j := !j + nb
  let decr j =
    decr j; !j = 0
end

module ListDataStruct : SEQ_DATA_STRUCT =
struct
  (* XXX TODO: handle id properly XXX *)

  type next = unit step list ref
  type current = unit step list ref
  type waiting_list = unit step list ref

  let mk_current id = ref ([] : unit step list)
  let add_current id p c =
    c := p :: !c
  let add_current_list id pl c =
    c := List.rev_append pl !c
  let add_current_waiting_list id w c =
    c := List.rev_append !w !c;
    w := []
  let add_current_next id next c =
    c := List.rev_append !next !c;
    next := []

  let current_length c =
    List.length !c

  let rec exec_all_current id c =
    match !c with
    | f :: l ->
      c := l;
      f id ();
      exec_all_current id c
    | [] -> ()

  let mk_waiting_list id = ref ([]:unit step list)
  let add_waiting id p w =
    w := p :: ! w
  let take_all id w =
    let l = !w in
    w := [];
    l
  let waiting_length w =
    List.length !w

  let mk_next id = ref ([]:unit step list)
  let add_next id p next =
    next := p :: !next
  let add_next_next id n1 n2 =
    n2 := List.rev_append !n1 !n2;
    n1 := []
  let clear_next id next =
    next := []
  let is_empty_next id next =
    !next = []
end

module ListListDataStruct : SEQ_DATA_STRUCT =
struct
  (* XXX TODO: handle id properly XXX *)

  type next = (unit step) list ref
  type current = (unit step) list list ref
  type waiting_list = (unit step) list ref

  let mk_current id = ref ([] : (unit step) list list)
  let add_current id p c =
    match !c with
    | [] -> c := [[p]]
    | l::tl -> c := (p::l)::tl
  let add_current_list id pl c =
    match pl with
    | [] -> ()
    | _ -> c := pl::!c
  let add_current_waiting_list id w c =
    match !w with
      | [] -> ()
      | l -> c := l::!c; w := []
  let add_current_next id next c =
    match !next with
      | [] -> ()
      | l -> c := l::!c; next := []

  let current_length c =
    List.fold_left (fun acc l -> (List.length l) + acc) 0 !c

  let rec exec_all_current id c =
    match !c with
    | [] -> ()
    | [f] :: tl ->
      c := tl;
      f id ();
      exec_all_current id c
    | (f::l) :: tl ->
      c := l::tl;
      f id ();
      exec_all_current id c
    | _ -> assert false


  let mk_waiting_list id = ref ([]:(unit step) list)
  let add_waiting id p w =
    w := p :: ! w
  let take_all id w =
    let l = !w in
    w := [];
    l
  let waiting_length w =
    List.length !w

  let mk_next id = ref ([]:(unit step) list)
  let add_next id p next =
    next := p :: !next
  let add_next_next id n1 n2 =
    n2 := List.rev_append !n1 !n2;
    n1 := []
  let clear_next id next =
    next := []
  let is_empty_next id next =
    !next = []
end

module D = ListListDataStruct
module E = Sig_env_clock.Record


module SeqRuntime =
struct
    type 'a step = 'a Runtime.step

    type inference_state =
      { inf_samples : sample array;
        mutable inf_sample_index: int; }
    and sample =
      { sample_score: float;
        sample_continuation: unit step; }

    type control_tree =
        { kind: (unit step, clock) control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (id -> bool);
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
      | Cevent of (id -> bool -> bool) * clock_domain * D.waiting_list * D.waiting_list
      (* status, cd, wa, wp*)
      | Cand of event_cfg * event_cfg
      | Cor of event_cfg * event_cfg

    let unit_value = ()

    let top_clock_ref = ref None
    let get_top_clock_domain () =
      match !top_clock_ref with
        | None -> raise Rml_types.RML
        | Some cd -> cd

(**************************************)
(* control tree                       *)
(**************************************)
    let new_ctrl id ?(cond = (fun id -> false)) kind =
      { kind = kind;
        alive = true;
        susp = false;
        children = [];
        cond = cond;
        cond_v = false;
        next = D.mk_next id;
        next_control = D.mk_next id;
        last_activation = [];
        instance = 0; }

    (* tuer un arbre p *)
    let rec set_kill id p =
      p.alive <- true; (* set to true, to show that the node is no longer attached to its parent
                       and needs to be reattaced if the node is reused *)
      p.susp <- false;
      D.clear_next id p.next;
      List.iter (set_kill id) p.children;
      p.children <- [];
      p.instance <- p.instance + 1

    let rec save_clock_state cd =
      let l = match cd.cd_parent with
        | None -> []
        | Some cd -> save_clock_state cd
      in
        (cd, E.get cd.cd_clock)::l

    let start_ctrl cd ctrl new_ctrl id =
      new_ctrl.last_activation <- save_clock_state cd;
      if new_ctrl.alive then
        ctrl.children <- new_ctrl :: ctrl.children
      else (* reset new_ctrl *)
        (new_ctrl.alive <- true;
         new_ctrl.susp <- false;
         D.clear_next id new_ctrl.next)

    let end_ctrl new_ctrl f_k id x =
      set_kill id new_ctrl;
      new_ctrl.alive <- false;
      f_k id x

    let mk_clock_domain id parent =
      let cd = { cd_current = D.mk_current id;
        cd_eoi = ref false;
        cd_weoi = D.mk_waiting_list id;
        cd_next_instant = D.mk_waiting_list id;
        cd_wake_up = [];
        cd_clock = E.init_clock ();
        cd_top = new_ctrl id Susp;
        cd_parent = parent;
        cd_counter = 0;
      } in
      cd.cd_top <- new_ctrl id (Clock_domain cd);
      cd.cd_top.last_activation <- save_clock_state cd;
      cd

    let finalize_top_clock_domain _ =
      ()
    let init id =
      match !top_clock_ref with
        | None ->
            (* create top clock domain *)
            let cd = mk_clock_domain id None in
            top_clock_ref := Some cd
        | Some _ -> () (* init already done *)
    let () = init (-1)

    let is_eoi cd = !(cd.cd_eoi)
    let control_tree cd = cd.cd_top
    let clock cd = cd
   (* let rec top_clock cd = match cd.cd_parent with
      | None -> cd
      | Some cd -> top_clock cd *)
    let top_clock () = match !top_clock_ref with
      | None -> Format.eprintf "No top clock@."; raise Rml_types.RML
      | Some ck -> ck

    let add_weoi id cd p =
      D.add_waiting id p cd.cd_weoi
    let add_weoi_waiting_list id cd w =
      cd.cd_wake_up <- w :: cd.cd_wake_up

(* debloquer les processus en attent d'un evt *)
    let wake_up id ck w =
      D.add_current_waiting_list id w ck.cd_current

    let wake_up_all id ck =
      List.iter (fun wp -> D.add_current_waiting_list id wp ck.cd_current) ck.cd_wake_up;
      ck.cd_wake_up <- []

(* ------------------------------------------------------------------------ *)

    module Join = JoinRef
    type join_point = JoinRef.join_point

    type state_frame = {
      st_ctrl: control_tree;
      st_jp: join_point option;
      st_cd: clock_domain;
      st_prob: inference_state option;
    }

    module Event =
      struct
        let new_evt_expr id cd _ kind default combine =
          (E.create cd.cd_clock kind default combine, cd,
           D.mk_waiting_list id, D.mk_waiting_list id)

        let new_evt id _ cd r kind default combine reset k =
          let evt = new_evt_expr id cd r kind default combine in
          let k =
         (* create a callback to reset the signal at each eoi of the reset *)
            match reset with
              | None -> k
              | Some rck ->
                fun evt id _ ->
                  let (n, _, _, _) = evt in
                  let w = Weak.create 1 in
                  Weak.set w 0 (Some evt);
                  let rec reset_evt id () =
                    (match Weak.get w 0 with
                      | None -> () (* signal is no longer used, terminate *)
                      | Some evt ->
                        E.reset n;
                        add_weoi id rck reset_evt)
                  in
                  add_weoi id rck reset_evt;
                  k evt id ()
          in
          k evt

        let new_evt_global id kind default combine =
          let cd = get_top_clock_domain () in
          new_evt_expr id cd cd kind default combine

        let status id ?(only_at_eoi=false) (n,sig_cd,_,_) =
          E.status n && (not only_at_eoi || !(sig_cd.cd_eoi))

        let value id (n,_,_,_) =
          if E.status n then
            E.value n
          else
            raise Rml_types.RML

        let one id (n,_,_,_) = E.one n
        let pre_status id (n,_,_,_) = E.pre_status n
        let pre_value id (n,_,_,_) = E.pre_value n
        let last id (n,_,_,_) = E.last n
        let default id (n,_,_,_) = E.default n
        let clock id (_,sig_cd,_,_) = sig_cd

        let region_of_clock id cd = cd

        let emit id (n,sig_cd,wa,wp) v =
          E.emit n v;
          D.add_current_waiting_list id wa sig_cd.cd_current;
          D.add_current_waiting_list id wp sig_cd.cd_current

        let cfg_present id ((n,sig_cd,wa,wp) as evt) =
          Cevent ((fun id eoi -> status id ~only_at_eoi:eoi evt), sig_cd, wa, wp)
        let cfg_or id ev1 ev2 =
          Cor (ev1, ev2)
        let cfg_and id ev1 ev2 =
          Cand (ev1, ev2)

        let cfg_status id ?(only_at_eoi=false) evt_cfg =
          let rec status k = match k with
            | Cevent (c, _, _, _) -> c id only_at_eoi
            | Cand (cfg1, cfg2) -> status cfg1 && status cfg2
            | Cor (cfg1, cfg2) -> status cfg1 || status cfg2
          in
          status evt_cfg

        let cfg_events id evt_cfg long_wait =
          let rec events k = match k with
            | Cevent (_, cd, wa, wp) -> [(if long_wait then wa else wp), cd]
            | Cand (cfg1, cfg2) | Cor (cfg1, cfg2) ->
              List.rev_append (events cfg1) (events cfg2)
          in
          events evt_cfg
      end

(* ------------------------------------------------------------------------ *)

    exception Wait_again

    let on_current_instant cd f id = D.add_current id f cd.cd_current
    let on_current_instant_list cd fl id = D.add_current_list id fl cd.cd_current
    let on_next_instant ?(kind=Strong) ctrl f id =
      match kind with
        | Strong -> D.add_next id f ctrl.next
        | Weak -> D.add_next id f ctrl.next_control

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
    let on_eoi cd f id =
      if is_eoi cd then
        f id unit_value
      else
        add_weoi id cd f

  (** [on_event_or_next evt f_w cd ctrl f_next] executes 'f_w ()' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let _on_event_or_next (_,_,_,w) f_w cd ctrl f_next id =
      let act id _ =
        if is_eoi cd then
          (*eoi was reached, launch fallback*)
          D.add_next id f_next ctrl.next
        else
          (* signal activated *)
          f_w id ()
      in
      D.add_waiting id act w;
      add_weoi_waiting_list id cd w

    let on_event_or_next evt f_w cd ctrl f_next id =
      if Event.status id evt then
        f_w id ()
      else
        _on_event_or_next evt f_w cd ctrl f_next id

  (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w ()' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
    let on_event_cfg_or_next evt_cfg f_w cd ctrl f_next id =
      if Event.cfg_status id evt_cfg then
        f_w id ()
      else
        let is_fired = ref false in
        let try_fire id _ =
          if not !is_fired then
            if is_eoi cd then
              (is_fired := true;
               D.add_next id f_next ctrl.next)
            else
              (if Event.cfg_status id evt_cfg then
                  (is_fired := true;
                   f_w id ()))
        in
        let w_list = Event.cfg_events id evt_cfg false in
        List.iter
          (fun (w,_) ->
             D.add_waiting id try_fire w;
             add_weoi_waiting_list id cd w)
          w_list

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
    let _on_event w sig_cd ctrl f id =
      let instance = ctrl.instance in
      let rec try_launch id () =
        try
          f id ()
        with
          | Wait_again ->
            on_eoi sig_cd (fun id () -> D.add_waiting id self w) id
      and self id _ =
        if ctrl.instance = instance then (
          if has_been_active ctrl sig_cd then
            (*ctrl is activated, run continuation*)
            try_launch id ()
          else ((*ctrl is not active, wait end of instant*)
            let is_fired = ref false in
            D.add_next id (ctrl_await is_fired) ctrl.next_control;
            add_weoi id sig_cd (eoi_await is_fired)
          )
        )
      and eoi_await is_fired id _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           D.add_waiting id self w)
      and ctrl_await is_fired id _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (is_fired := true; try_launch id ())
      in
      D.add_waiting id self w

    let on_event (n,sig_cd,w,_) ctrl f id =
      if E.status n then
        (try
           f id ()
         with
           | Wait_again -> _on_event w sig_cd ctrl f id)
      else
        _on_event w sig_cd ctrl f id

    (** [on_event_cfg evt_cfg ctrl f ()] executes 'f ()' if evt_cfg is true and
        ctrl is active in the same step.
        It waits for the next activation of evt_cfg otherwise,
        or if the call raises Wait_again *)
    let on_event_cfg evt_cfg ctrl f id =
      let wait_event_cfg id () =
        let is_fired = ref false in
        let try_fire id _ =
          if not !is_fired then
            (if Event.cfg_status id evt_cfg then
                (is_fired := true;
                 f id ())
             else
                raise Wait_again)
        in
        let w_list = Event.cfg_events id evt_cfg true in
        List.iter
          (fun (w,cd) -> _on_event w cd ctrl try_fire id)
          w_list
      in
      if Event.cfg_status id evt_cfg then
        (try
           f id ()
         with
           | Wait_again -> wait_event_cfg id ())
      else
        wait_event_cfg id ()

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

(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current cd id =
      let rec eval pere p active id =
        if p.alive then
          match p.kind with
            | Clock_domain _ -> true
            | Kill (pause_kind, f_k) ->
              if p.cond_v
              then
                (on_next_instant ~kind:pause_kind pere f_k id;
                 set_kill id p;
                 false)
              else
                (p.children <- eval_children p p.children active id;
                 if active then next_to_current id cd p
                 else next_to_father id pere p;
                 true)
          | Kill_handler (_, handler) ->
              if p.cond_v
              then
                false
              else
                (p.children <- eval_children p p.children active id;
                 if active then next_to_current id cd p
                 else next_to_father id pere p;
                 true)
          | Susp ->
              let pre_susp = p.susp in
              if p.cond_v then p.susp <- not pre_susp;
              let active = active && not p.susp in
              if pre_susp
              then
                (if active then next_to_current id cd p;
                 true)
              else
                (p.children <- eval_children p p.children active id;
                 if active then next_to_current id cd p
                 else if not p.susp then next_to_father id pere p;
                 true)
          | When ->
              if p.susp
              then true
              else
                (p.susp <- true;
                 p.children <- eval_children p p.children false id;
                 true)
        else
          (set_kill id p;
           false)

      and eval_children p nodes active id =
        List.filter (fun node -> eval p node active id) nodes

      and next_to_current id ck node =
        node.last_activation <- save_clock_state ck;
        D.add_current_next id node.next ck.cd_current;
        D.add_current_next id node.next_control ck.cd_current;
      and next_to_father id pere node = ()
       (* D.add_next_next node.next pere.next;
        D.add_next_next node.next_control pere.next_control
        *)
        (* TODO: ne marche plus si on veut que last_activation soit correct *)
      in
      cd.cd_top.children <- eval_children cd.cd_top cd.cd_top.children true id;
      next_to_current id cd cd.cd_top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current id cd p =
      if p.alive && not p.susp then
        (D.add_current_next id p.next cd.cd_current;
         D.add_current_next id p.next_control cd.cd_current;
         p.last_activation <- save_clock_state cd;
         List.iter (next_to_current id cd) p.children)

    (** Evaluates the condition of control nodes. This can be called several
        times for a same control node, when doing the eoi of several clocks.
        We can keep the last condition (if it was true for the eoi of the fast clock,
        it is also true for the eoi of the slow clock), but we have to make sure
        to fire the handler only once. *)
    let eoi_control ctrl id =
      let rec _eoi_control pere ctrl id =
        if ctrl.alive then (
          ctrl.cond_v <- ctrl.cond id;
          (match ctrl.kind with
            | Kill_handler (pause_kind, handler) ->
                if ctrl.cond_v then (
                  on_next_instant ~kind:pause_kind pere (handler()) id;
                  set_kill id ctrl
                )
            | _ -> ());
          List.iter (fun c -> _eoi_control ctrl c id) ctrl.children;
        )
      in
        List.iter (fun c -> _eoi_control ctrl c id) ctrl.children

    let wake_up_ctrl new_ctrl cd id =
      new_ctrl.susp <- false;
      next_to_current id cd new_ctrl

    (* Control structures *)
    let create_control kind body f_k ctrl cd id =
      fun evt cond ->
        body f_k ctrl
      (* assert false *) (* XXX TODO XXX *)
      (* let new_ctrl = new_ctrl id kind in *)
      (* let f = body (end_ctrl new_ctrl f_k) new_ctrl id in *)
      (* match kind with *)
      (*   | When -> *)
      (*       fun evt other_cond -> *)
      (*         let rec when_act id _ = *)
      (*           wake_up_ctrl new_ctrl cd id; *)
      (*           on_next_instant ctrl f_when id *)
      (*         and f_when id _ = *)
      (*           on_event evt ctrl when_act id *)
      (*         in *)
      (*         new_ctrl.cond <- (fun id -> Event.status id evt); *)
      (*         fun id () -> *)
      (*           start_ctrl cd ctrl new_ctrl id; *)
      (*           new_ctrl.susp <- true; *)
      (*           on_next_instant new_ctrl f id; *)
      (*           f_when id () *)
      (*   | _ -> *)
      (*       fun evt other_cond -> *)
      (*         new_ctrl.cond <- *)
      (*           (fun id -> Event.status id ~only_at_eoi:true evt && other_cond (Event.value evt)); *)
      (*         fun id () -> *)
      (*           start_ctrl cd ctrl new_ctrl id; *)
      (*           f id () *)

    let create_control_evt_conf kind body f_k ctrl cd id =
      assert false (* XXX TODO XXX *)
      (* let new_ctrl = new_ctrl id kind in *)
      (* let f = body (end_ctrl new_ctrl f_k) new_ctrl in *)
      (* fun evt_cfg -> *)
      (*   new_ctrl.cond <- (fun id -> Event.cfg_status id ~only_at_eoi:true evt_cfg); *)
      (*   fun id () -> *)
      (*     start_ctrl cd ctrl new_ctrl id; *)
      (*     f id () *)


    let schedule cd id =
      D.exec_all_current id cd.cd_current

    let eoi cd id =
      cd.cd_eoi := true;
      eoi_control cd.cd_top id;
      wake_up id cd cd.cd_weoi;
      wake_up_all id cd;
      schedule cd id

    let next_instant cd id =
      E.next cd.cd_clock;
      (* next instant of child clock domains *)
      wake_up id cd cd.cd_next_instant;
      schedule cd id;
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd id;
      cd.cd_eoi := false

    let rec has_next id ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain _ -> has_next_children id ctrl
          | Kill (pause_kind, _) | Kill_handler (pause_kind, _) ->
            (pause_kind = Strong && ctrl.cond id) || has_next_children id ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond id) || (not ctrl.susp && not (ctrl.cond id)) in
              active && has_next_children id ctrl
          | When ->
            not ctrl.susp && has_next_children id ctrl
    and has_next_children id ctrl =
      not (D.is_empty_next id ctrl.next) ||
      List.exists (has_next id) ctrl.children

    let macro_step_done cd id =
      not (has_next id cd.cd_top)

    let step_clock_domain ctrl new_ctrl cd new_cd period =
      let next_instant_clock_domain id _ = next_instant new_cd id in
      let rec f_cd id () =
        new_cd.cd_counter <- new_cd.cd_counter + 1;
        schedule new_cd id;
        if new_cd.cd_top.alive then (
          eoi new_cd id;
          let period_finished = match period with
            | None -> false
            | Some p -> new_cd.cd_counter >= p
          in
          if period_finished || macro_step_done new_cd id then (
            new_cd.cd_counter <- 0;
            D.add_waiting id next_instant_clock_domain cd.cd_next_instant;
            D.add_next id f_cd ctrl.next_control
          ) else (
            next_instant new_cd id;
            (* execute again in the same step but yield for now*)
            D.add_current id f_cd cd.cd_current
          )
        )
      in
      f_cd

    let end_clock_domain new_ctrl f_k id x =
      end_ctrl new_ctrl f_k id x

    let new_clock_domain id cd ctrl p _ period f_k =
      let new_cd = mk_clock_domain id (Some cd) in
      let new_ctrl = control_tree new_cd in
      let f = p new_cd new_ctrl (end_clock_domain new_ctrl f_k) in
      fun id _ ->
        on_current_instant new_cd f id;
        start_ctrl new_cd ctrl new_ctrl id;
        step_clock_domain ctrl new_ctrl cd new_cd period id unit_value

    (* the react function *)
    let react cd id =
      schedule cd id;
      eoi cd id;
      next_instant cd id

    (* Fake functions *)
    let start_slave _ = ()
    let is_master _ = true
end
