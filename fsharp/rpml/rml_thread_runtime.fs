module RmlThreadRuntime

open System
open System.Threading
open System.Collections.Concurrent
open Types
open Runtime

let ensure b = if not b then raise Types.RML

type JoinThread(nb) =
  let j = ref nb
  interface Join with
    member this.incr nb =
      ignore (Interlocked.Add (j, nb))
    member this.decr () =
      (Interlocked.Decrement j) = 0

type ThreadNext() =
  inherit ConcurrentStack<unit Step.t>()
  member this.Add p = this.Push p
  member this.AddNext (n:Next) =
    let n = n :?> ThreadNext in 
    this.PushRange (n.ToArray ());
    n.Clear ()

  interface Next with
    member this.Add p = this.Add p
    member this.Clear () = this.Clear ()
    member this.AddNext n1 = this.AddNext n1
    member this.IsEmpty = this.IsEmpty

type ThreadWaitingList() =
  inherit ConcurrentStack<unit Step.t>()
  member this.Add p = this.Push p
  member this.TakeAll () =
    let a = this.ToArray () in
    this.Clear ();
    List.ofArray a
 
  interface WaitingList with       
    member this.Add p = this.Add p
    member this.TakeAll () = this.TakeAll ()
    member this.Length = this.Count

let tmp = new ThreadLocal<unit Step.t ref> (fun () -> ref (fun () -> ()))  

type ThreadCurrent() =
  inherit ConcurrentBag<unit Step.t>()
  member this.AddList pl =
    List.iter this.Add pl
  member this.AddWaitingList (w:WaitingList) =
    this.AddList (w.TakeAll ())
  member this.AddNext (n:Next) =
    let n = n :?> ThreadNext in 
    for p in n do this.Add p done;
    n.Clear ()
  member this.Take () =
    if this.TryTake tmp.Value then
      Some !tmp.Value
    else
      None

  interface Current with
    member this.Add p = this.Add p
    member this.AddList pl = this.AddList pl
    member this.AddWaitingList w = this.AddWaitingList w
    member this.AddNext next = this.AddNext next
    member this.Length = this.Count
    member this.Take () = this.Take ()
         
                                                                                 
type ListStruct() =
  interface SeqDataStruct with
    member this.mk_current () = new ThreadCurrent () :> Current
    member this.mk_waiting_list () = new ThreadWaitingList () :> WaitingList
    member this.mk_next () = new ThreadNext () :> Next


type ThreadControlTree(D:SeqDataStruct, kind, cond) =
  member val alive = true with get, set
  member val kind = kind
  member val susp = false with get, set
  member val children = ([]:ThreadControlTree list) with get, set
  member val cond = cond with get, set
  member val cond_v = false with get, set
  member val next = D.mk_next ();
  member val next_control = D.mk_next ()
  member val last_activation = [] with get, set
  member val instance = 0 with get, set
  new (D:SeqDataStruct, kind) =
    ThreadControlTree(D, kind, fun () -> false)
  member this.setKill () =
    this.alive <- true; (* set to true, to show that the node is no longer attached to its parent
                   and needs to be reattaced if the node is reused *)
    this.susp <- false;
    this.next.Clear ();
    this.children <- [];
    this.instance <- this.instance + 1;
    List.iter (fun (p:ThreadControlTree) -> p.setKill ()) this.children;

and SeqClockDomain(S:#SeqDataStruct, parent) =
  member val cd_current = S.mk_current ()
  member val cd_current_event = new AutoResetEvent(false)
  member val cd_eoi = ref false
  member val cd_weoi = S.mk_waiting_list ()
  member val cd_next_instant = S.mk_waiting_list ()
  member val cd_wake_up = new ConcurrentStack<WaitingList> ()
  member val cd_clock = Event.init_clock ()
  member val cd_top = new ThreadControlTree(S, Susp) with get, set
  member val cd_parent = parent
  member val cd_counter = 0 with get, set 

  member this.is_eoi () = !(this.cd_eoi)
  member this.control_tree = this.cd_top
  member this.clock = this

  interface ClockDomain<clock, ThreadControlTree> with
    member this.is_eoi () = this.is_eoi ()
    member this.control_tree = this.control_tree
    member this.clock = this.clock
    
and clock = SeqClockDomain
and region = clock

let unit_value = ()

let top_clock_ref = ref None
let get_top_clock_domain () =
  match !top_clock_ref with
  | None -> raise Types.RML
  | Some cd -> cd

(**************************************)
(* control tree                       *)
(**************************************)

let rec save_clock_state (cd:SeqClockDomain) =
  let l = match cd.cd_parent with
    | None -> []
    | Some cd -> save_clock_state cd
  in
    (cd, Event.get cd.cd_clock)::l

let start_ctrl cd (ctrl:ThreadControlTree) (new_ctrl:ThreadControlTree) =
  new_ctrl.last_activation <- save_clock_state cd;
  if new_ctrl.alive then (
    Monitor.Enter ctrl;
    ctrl.children <- new_ctrl :: ctrl.children;
    Monitor.Exit ctrl
  ) else (* reset new_ctrl *)
    (new_ctrl.alive <- true;
     new_ctrl.susp <- false;
     new_ctrl.next.Clear ())

let end_ctrl (new_ctrl:ThreadControlTree) f_k x =
  Monitor.Enter new_ctrl;
  new_ctrl.setKill ();
  new_ctrl.alive <- false;
  Monitor.Exit new_ctrl;
  f_k x


(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
let eval_control_and_next_to_current cd =
  let rec eval (pere:ThreadControlTree) (p:ThreadControlTree) active =
    if p.alive then
      match p.kind with
      | Clock_domain _ -> true
      | Kill f_k ->
          if p.cond_v
          then
            (pere.next.Add f_k;
             p.setKill ();
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
      (p.setKill ();
       false)

  and eval_children p nodes active =
    List.filter (fun node -> eval p node active) nodes

  and next_to_current ck node =
    node.last_activation <- save_clock_state ck;
    ck.cd_current.AddNext node.next;
    ck.cd_current.AddNext node.next_control;
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
let rec next_to_current (cd:SeqClockDomain) (p:ThreadControlTree) =
  if p.alive && not p.susp then
    (cd.cd_current.AddNext p.next;
     cd.cd_current.AddNext p.next_control;
     p.last_activation <- save_clock_state cd;
     List.iter (next_to_current cd) p.children)

(** Evaluates the condition of control nodes. This can be called several
    times for a same control node, when doing the eoi of several clocks.
    We can keep the last condition (if it was true for the eoi of the fast clock,
    it is also true for the eoi of the slow clock), but we have to make sure
    to fire the handler only once. *)
let eoi_control (ctrl:ThreadControlTree) =
  let rec _eoi_control (pere:ThreadControlTree) (ctrl:ThreadControlTree) =
    if ctrl.alive then (
      ctrl.cond_v <- ctrl.cond ();
      (match ctrl.kind with
        | Kill_handler handler ->
            if ctrl.cond_v then (
              pere.next.Add (handler());
              ctrl.setKill ()
            )
        | _ -> ());
      List.iter (_eoi_control ctrl) ctrl.children;
    )
  in
    List.iter (_eoi_control ctrl) ctrl.children

let wake_up_ctrl (new_ctrl:ThreadControlTree) cd =
  Monitor.Enter new_ctrl;
  new_ctrl.susp <- false;
  next_to_current cd new_ctrl;
  Monitor.Exit new_ctrl


(* let rec top_clock cd = match cd.cd_parent with
  | None -> cd
  | Some cd -> top_clock cd *)
let top_clock () = match !top_clock_ref with
  | None -> raise Types.RML
  | Some ck -> ck

let add_weoi (cd:SeqClockDomain) p =
  cd.cd_weoi.Add p
let add_weoi_waiting_list (cd:SeqClockDomain) w =
  cd.cd_wake_up.Push w

(* debloquer les processus en attent d'un evt *)
let wake_up (ck:SeqClockDomain) w =
  ck.cd_current.AddWaitingList w;
  ensure (ck.cd_current_event.Set ())

let wake_up_all (ck:SeqClockDomain) =
  for w in ck.cd_wake_up do ck.cd_current.AddWaitingList w done
  ck.cd_wake_up.Clear ()


(* ------------------------------------------------------------------------ *)

type ThreadEvent<'a, 'b>(S:SeqDataStruct, ck:clock, r, kind, def, combine:'a -> 'b -> 'b) =
  member val n = Event.create ck.cd_clock kind def combine
  member val clock = ck
  member val wa = S.mk_waiting_list ()
  member val wp = S.mk_waiting_list ()

  (* Called during eoi. No locks *)
  member this.status only_at_eoi =
    Event.status this.n && (not only_at_eoi || !(this.clock.cd_eoi))
  member this.value with get () =
      if Event.status this.n then
        Event.value this.n
      else
        raise Types.RML

  (* no locks because there is no writers during the instant *)
  member this.pre_status with get () = Event.pre_status this.n
  member this.pre_value with get () = Event.pre_value this.n
  member this.last with get () = Event.last this.n
  member this._default with get () = Event._default this.n
  member this.one () = Event.one this.n
        
  member this.emit v =
    Monitor.Enter this;
    Event.emit this.n v;
    this.clock.cd_current.AddWaitingList this.wa;
    this.clock.cd_current.AddWaitingList this.wp;
    ensure (this.clock.cd_current_event.Set ())
    Monitor.Exit this

  interface REvent<'a, 'b, clock> with
    member this.status only_at_eoi = this.status only_at_eoi
    member this.value = this.value
    member this.pre_status = this.pre_status
    member this.pre_value = this.pre_value
    member this.last = this.last
    member this.one () = this.one ()      
    member this._default = this._default
    member this.clock = this.clock
    member this.emit v = this.emit v

type ThreadEventCfg(k) =
  member this.kind = k
  member this.cfg_events long_wait = 
    match this.kind with
    | Cevent (_, cd, wa, wp) -> [(if long_wait then wa else wp), cd]
    | Cand (cfg1, cfg2) | Cor (cfg1, cfg2) ->
       List.rev_append (cfg1.cfg_events long_wait) (cfg2.cfg_events long_wait)
  member this.cfg_status only_at_eoi =
      match this.kind with
      | Cevent (c, _, _, _) -> c only_at_eoi
      | Cand (cfg1, cfg2) -> cfg1.cfg_status only_at_eoi && cfg2.cfg_status only_at_eoi
      | Cor (cfg1, cfg2) -> cfg1.cfg_status only_at_eoi || cfg2.cfg_status only_at_eoi
  interface REventCfg with
    member this.cfg_status only_at_eoi = this.cfg_status only_at_eoi 

and event_cfg =
  | Cevent of (bool -> bool) * SeqClockDomain * WaitingList * WaitingList
  (* status, cd, wa, wp*)
  | Cand of ThreadEventCfg * ThreadEventCfg
  | Cor of ThreadEventCfg * ThreadEventCfg    


(* ------------------------------------------------------------------------ *)

type ThreadRuntime(D:#SeqDataStruct) =

     (** [on_event_or_next evt f_w cd ctrl f_next] executes 'f_w ()' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  let on_event_or_next (evt:ThreadEvent<'a, 'b>) f_w (cd:SeqClockDomain) (ctrl:ThreadControlTree) f_next =
      let act _ =
        if cd.is_eoi () then
          (*eoi was reached, launch fallback*)
          ctrl.next.Add f_next
        else
          (* signal activated *)
          f_w ()
      in
      Monitor.Enter evt;
      if Event.status evt.n then
        Monitor.Exit evt; 
        f_w ()
      else
        evt.wp.Add act;
        Monitor.Exit evt;
        add_weoi_waiting_list cd evt.wp        

    (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w ()' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  let on_event_cfg_or_next (evt_cfg:REventCfg) f_w (cd:SeqClockDomain) (ctrl:ThreadControlTree) f_next =
      let evt_cfg = evt_cfg :?> ThreadEventCfg in
      if evt_cfg.cfg_status false then
        f_w ()
      else
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            if cd.is_eoi () then
              (is_fired := true;
               ctrl.next.Add f_next)
            else
              (if evt_cfg.cfg_status false then
                  (is_fired := true;
                   f_w ()))
        in
        let w_list = evt_cfg.cfg_events false in
        List.iter
          (fun (w:#WaitingList,_) -> w.Add try_fire; add_weoi_waiting_list cd w) w_list

  let has_been_active (ctrl:ThreadControlTree) (sig_cd:SeqClockDomain) =
      let rec check_last_activation l = match l with
        | [] -> false
        | (cd, ck)::l ->
            if cd == sig_cd then (
              ck = (Event.get sig_cd.cd_clock)
            ) else
              check_last_activation l
      in
      check_last_activation ctrl.last_activation

    (** [on_event evt ctrl f] executes 'f ()' if evt is emitted and
        ctrl is active in the same step.
        It waits for the next activation of w otherwise,
        or if the call raises Wait_again *)
  let on_event (evt:ThreadEvent<'a, 'b>) (ctrl:ThreadControlTree) f =
      let instance = ctrl.instance in
      let rec try_launch () =
        try 
          f ()
        with
        | Wait_again -> evt.clock.cd_weoi.Add (fun () -> evt.wa.Add self)
      and self _ =
        Monitor.Enter ctrl;
        if ctrl.instance = instance then (
          if has_been_active ctrl evt.clock then
            (*ctrl is activated, run continuation*)
            Monitor.Exit ctrl;
            try_launch ()
          else ((*ctrl is not active, wait end of instant*)
            let is_fired = ref false in
            ctrl.next_control.Add (ctrl_await is_fired);
            add_weoi evt.clock (eoi_await is_fired);
            Monitor.Exit ctrl
          )
        ) else
          Monitor.Exit ctrl
      and eoi_await is_fired _ =
        if not !is_fired then
            (*ctrl was not activated, await the signal again *)
          (is_fired := true;
           evt.wa.Add self)
      and ctrl_await is_fired _ =
        if not !is_fired then
          (* ctrl was activated, signal is present*)
          (is_fired := true; try_launch ())
      in
      Monitor.Enter evt;
      if Event.status evt.n then
        (try
           Monitor.Exit evt;
           f ()
         with
           | Wait_again -> evt.wa.Add self)
      else
        evt.wa.Add self;
        Monitor.Exit evt

    (** [on_event_cfg evt_cfg ctrl f ()] executes 'f ()' if evt_cfg is true and
        ctrl is active in the same step.
        It waits for the next activation of evt_cfg otherwise,
        or if the call raises Wait_again *)
  let on_event_cfg (evt_cfg:ThreadEventCfg) ctrl f = () (* TODO *)
(*
      let wait_event_cfg () =
        let is_fired = ref false in
        let try_fire _ =
          if not !is_fired then
            (if evt_cfg.cfg_status false then
                (is_fired := true;
                 f ())
             else
                raise Wait_again)
        in
        let w_list = evt_cfg.cfg_events true in
        List.iter (fun (w,cd) -> _on_event w cd ctrl try_fire) w_list
      in
      if evt_cfg.cfg_status false then
        (try
           f ()
         with
           | Wait_again -> wait_event_cfg ())
      else
        wait_event_cfg ()
*)

  let current_step = ref true

  let done_threads1 = new CountdownEvent(!Runtime_options.nb_threads)
  let done_threads2 = new CountdownEvent(!Runtime_options.nb_threads)
  let current_done_threads () = 
      if !current_step then done_threads1 else done_threads2

  let next_step1 = new ManualResetEvent false
  let next_step2 = new ManualResetEvent false
  let current_next_step () = 
      if !current_step then next_step1 else next_step2

  let rec schedule (cd:SeqClockDomain) =
      match cd.cd_current.Take () with
      | Some f -> f (); schedule cd
      | None -> ()

  let rec do_one_step cd (done_threads:CountdownEvent) = 
    schedule cd; 
    (*wait for the current list to be filled or the end of step *)
    Monitor.Enter done_threads
    if not (done_threads.Signal()) then
      Monitor.Exit done_threads
      ignore (WaitHandle.WaitAny (([|cd.cd_current_event; done_threads.WaitHandle|]:WaitHandle[]), 50))
      Monitor.Enter done_threads
    
    if not done_threads.IsSet then (*back to work*)
      done_threads.AddCount();
      Monitor.Exit done_threads;
      do_one_step cd done_threads
    else (*end of step*)
      Monitor.Exit done_threads

  let thread_fun i =
    try 
      let cd = get_top_clock_domain () in
      let ev = ref next_step1 in
      while true do
        (*attend le debut de l'instant suivant*)
        ensure ((!ev).WaitOne ());
        ev := current_next_step ();
        do_one_step cd (current_done_threads ())
      done
    with | :? ThreadInterruptedException -> ()


  let exec_sched (cd:SeqClockDomain) = 
      let ev = 
        if !current_step then 
          (ensure (next_step2.Reset ()); next_step1) 
        else 
          (ensure (next_step1.Reset ()); next_step2) 
      in
      let done_threads = if !current_step then done_threads2 else done_threads1 in
      done_threads.Reset !Runtime_options.nb_threads;
      current_step := not !current_step;
      ensure (ev.Set ());
      do_one_step cd done_threads


  let eoi (cd:SeqClockDomain) =
      cd.cd_eoi := true;
      eoi_control cd.cd_top;
      wake_up cd cd.cd_weoi;
      wake_up_all cd;
      schedule cd

  let next_instant (cd:SeqClockDomain) =
      Event.next cd.cd_clock;
      (* next instant of this clock domain *)
      eval_control_and_next_to_current cd;
      cd.cd_eoi := false

  member this.new_evt _ cd r kind _default combine reset k =
      let evt = new ThreadEvent<_,_>(D, cd, cd, kind, _default, combine) in
      k (evt :> REvent<'a, 'b, clock>)
  member this.new_evt_global kind _default combine =
      let cd = get_top_clock_domain () in
      new ThreadEvent<_,_>(D, cd, cd, kind, _default, combine) :> REvent<'a, 'b, clock>
  
  member this.cfg_present (evt:REvent<'a,'b,clock>) =
      let evt = evt :?> ThreadEvent<'a, 'b> in
      new ThreadEventCfg (Cevent ((fun eoi -> evt.status eoi), evt.clock, evt.wa, evt.wp)) :> REventCfg
  member this.cfg_or (ev1:REventCfg) (ev2:REventCfg) =
      new ThreadEventCfg (Cor (ev1 :?> ThreadEventCfg, ev2 :?> ThreadEventCfg)) :> REventCfg
  member this.cfg_and (ev1:REventCfg) (ev2:REventCfg) =
      new ThreadEventCfg (Cand (ev1 :?> ThreadEventCfg, ev2 :?> ThreadEventCfg)) :> REventCfg


  member this.new_join_point nb = 
    new JoinThread(nb) :> Join  


  member this.mk_clock_domain parent =
    let cd = new SeqClockDomain(D, parent) in
    cd.cd_top <- new ThreadControlTree (D, Clock_domain cd);
    cd.cd_top.last_activation <- save_clock_state cd;
    cd

  member val thread_list = (Array.empty:Thread array) with get, set
  member this.finalize_top_clock_domain _ =
    Array.iter (fun (t:Thread) -> t.Interrupt ()) this.thread_list
  member this.init () =
      match !top_clock_ref with
      | None ->
        (* create top clock domain *)
        let cd = this.mk_clock_domain None in
        top_clock_ref := Some cd;
        this.thread_list <- Array.init (!Runtime_options.nb_threads-1) (fun _ -> new Thread(ThreadStart(thread_fun)));
        Array.iter (fun (t:Thread) -> t.Start()) this.thread_list
      | Some _ -> () (* init already done *)

  member this.on_current_instant (cd:SeqClockDomain) f = 
    cd.cd_current.Add f
    ensure (cd.cd_current_event.Set ())
  member this.on_current_instant_list (cd:SeqClockDomain) fl = 
    cd.cd_current.AddList fl
    ensure (cd.cd_current_event.Set ())
  member this.on_next_instant kind (ctrl:ThreadControlTree) f =
      match kind with
        | Strong -> ctrl.next.Add f
        | Weak -> ctrl.next_control.Add f

    (** [on_eoi cd f] executes 'f ()' during the eoi of cd. *)
  member this.on_eoi (cd:SeqClockDomain) f =
      if cd.is_eoi () then
        f unit_value
      else
        add_weoi cd f


 
    (* Control structures *)
  member this.create_control (kind:control_type<clock>) body f_k (ctrl:ThreadControlTree) cd =
      let new_ctrl = new ThreadControlTree(D, kind) in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      match kind with
        | When ->
            fun (evt:REvent<'a,'b,clock>) other_cond ->
              let evt = evt :?> ThreadEvent<'a, 'b> in 
              let rec when_act _ =
                wake_up_ctrl new_ctrl cd;
                this.on_next_instant Strong ctrl f_when
              and f_when _ =
                on_event evt ctrl when_act
              in
              new_ctrl.cond <- (fun () -> evt.status false);
              fun () ->
                start_ctrl cd ctrl new_ctrl;
                new_ctrl.susp <- true;
                this.on_next_instant Strong new_ctrl f;
                f_when ()
        | _ ->
            fun (evt:REvent<'a,'b,clock>) other_cond ->
              let evt = evt :?> ThreadEvent<'a, 'b> in 
              new_ctrl.cond <-
                (fun () -> evt.status true && other_cond evt.value);
              fun () ->
                start_ctrl cd ctrl new_ctrl;
                f ()

  member this.create_control_evt_conf kind body f_k ctrl cd =
      let new_ctrl = new ThreadControlTree (D, kind) in
      let f = body (end_ctrl new_ctrl f_k) new_ctrl in
      fun (evt_cfg:REventCfg) ->
        new_ctrl.cond <- (fun () -> evt_cfg.cfg_status true);
        fun () ->
          start_ctrl cd ctrl new_ctrl;
          f ()

  member this.new_clock_domain cd ctrl p _ period f_k =
    printf "Unsupported construct\n"; 
    raise Types.RML

    (* the react function *)
  member this.react cd =
      exec_sched cd;
      eoi cd;
      next_instant cd


  interface Runtime<clock, ThreadControlTree> with        
    member this.create_control (kind:control_type<clock>) body f_k ctrl (cd:ClockDomain<clock, ThreadControlTree>) = 
      this.create_control kind body f_k ctrl (cd :?> SeqClockDomain)
    member this.create_control_evt_conf kind body f_k ctrl cd = 
      this.create_control_evt_conf kind body f_k ctrl (cd :?> SeqClockDomain) 

    member this.on_current_instant cd f =
      this.on_current_instant (cd :?> SeqClockDomain) f
    member this.on_current_instant_list cd fl = 
      this.on_current_instant_list (cd :?> SeqClockDomain) fl
    member this.on_next_instant kind ctrl f = this.on_next_instant kind ctrl f 
    member this.on_eoi (cd:clock) f = 
      this.on_eoi cd f

    member this.on_event_or_next (evt:REvent<'a,'b,clock>) f_w cd ctrl f_next =
      on_event_or_next (evt :?> ThreadEvent<'a, 'b>) f_w (cd :?> SeqClockDomain) ctrl f_next
    member this.on_event_cfg_or_next evt_cfg f_w cd ctrl f_next =
      on_event_cfg_or_next (evt_cfg :?> ThreadEventCfg) f_w (cd :?> SeqClockDomain) ctrl f_next
    member this.on_event (evt:REvent<'a,'b,clock>) ctrl f =
      on_event (evt :?> ThreadEvent<'a, 'b>) ctrl f
    member this.on_event_cfg evt_cfg ctrl f =
      on_event_cfg (evt_cfg :?> ThreadEventCfg) ctrl f
    
    member this.new_clock_domain cd ctrl p sch period f_k = 
      this.new_clock_domain (cd :?> SeqClockDomain) ctrl p sch period f_k
    member this.react cd = 
      this.react (cd :?> SeqClockDomain)
    
    member this.init () = this.init ()
    member this.get_top_clock_domain () = get_top_clock_domain () :> ClockDomain<_,_>
    member this.top_clock () = top_clock () 
    member this.finalize_top_clock_domain cd = 
      this.finalize_top_clock_domain (cd :?> SeqClockDomain)

    member this.new_evt_global kind _default combine = this.new_evt_global kind _default combine
    member this.new_evt cd ck r kind _default combine reset k = 
      this.new_evt (cd :?> SeqClockDomain) ck r kind _default combine reset k
    member this.cfg_present evt = this.cfg_present evt
    member this.cfg_or cfg1 cfg2 = this.cfg_or cfg1 cfg2
    member this.cfg_and cfg1 cfg2 = this.cfg_and cfg1 cfg2
    member this.region_of_clock cd = cd

    member this.new_join_point nb = this.new_join_point nb 

let R = new ThreadRuntime(new ListStruct ()) :> Runtime<_,_>

