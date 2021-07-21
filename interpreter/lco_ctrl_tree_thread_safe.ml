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

(* file: lco_ctrl_tree_thread_safe *)
(* author: Louis Mandel *)
(* created: 2006-04-14  *)
(* Remark: taken from lco_ctrl_tree.ml *)

(* Description :                                                      *)
(*   Gestion de plusieur scheduler executé dans des threads           *)
(*   differents.                                                      *)
(*                                                                    *)
(*  A FAIRE:                                                          *)
(*  - Vérifier qu'un processus ne peut pas changer de ctrl en cours   *)
(*    d'exécution (cf pb de pause par exemple).                       *)


module Rml_interpreter (* : Lco_interpreter.S *) =
  functor (Event: Sig_env.S) ->
  struct

    (* hack to avoid the error message: "Thread.self: not initialized" *)
    Thread.delay 0.0;;


    exception RML

    type ('a, 'b) event =
	scheduler_id *
	  ('a,'b) Event.t * unit step list ref * unit step list ref
    and event_cfg = bool -> (scheduler -> bool) * unit step list ref list

    and control_tree =
	{ kind: contol_type;
	  mutable alive: bool;
	  mutable susp: bool;
	  mutable cond: (scheduler -> bool);
	  mutable children: control_tree list;
	  mutable next: next; }
    and contol_type =
	Top
      | Kill of unit step
      | Kill_handler of (scheduler -> unit step)
      | Susp
      | When of unit step ref

    and 'a step = scheduler -> 'a -> unit
    and next = unit step list
    and 'a expr = 'a step -> control_tree ->  unit step
    and 'a process = unit -> 'a expr
    and scheduler_id = (* Thread.t *) int
    and scheduler =
	{ id: scheduler_id;
	  mutable current: unit step list;
	  mutable toWakeUp:  unit step list ref list;
	  top: control_tree;
	  mutable eoi: bool;
	  weoi: unit step list ref; }

    exception Wrong_scheduler

(* gestion des schedulers *)
    let schedulers = ref ([]: (scheduler_id * scheduler) list)
    let m_sched = Mutex.create()

(* creation d'un scheduler *)
    let new_scheduler () =
      let id = Thread.id (Thread.self()) in
      let top =
	{ kind = Top;
	  alive = true;
	  susp = false;
	  children = [];
	  cond = (fun sched -> false);
	  next = []; }
      in
      let sched =
	{ id = id;
	  current = [];
	  toWakeUp = [];
	  top = top;
	  eoi = false;
	  weoi = ref []; }
      in
      Mutex.lock m_sched;
      schedulers := (id,sched) :: !schedulers;
      Mutex.unlock m_sched;
      sched


(* récuperer le scheduler courrant *)
    let get_current_sched () =
      let self = Thread.id (Thread.self()) in
      Mutex.lock m_sched;
      let sched =  List.assoc self !schedulers in
      Mutex.unlock m_sched;
      sched

    let get_current_sched_id () =
      let sched = get_current_sched () in
      sched.id

(* liste des listes de processus a revillier a la fin d'instant *)
    let wakeUpAll sched =
      List.iter
	(fun wp ->
	  sched.current <- List.rev_append !wp sched.current;
	  wp := [])
	sched.toWakeUp;
      sched.toWakeUp <- []

(* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      p.next <- [];
      List.iter set_kill p.children;
      p.children <- []

    let rec rev_app x1 x2 =
      match x1 with
      | [] -> x2
      | f :: x1' -> rev_app x1' (f::x2)


(* calculer le nouvel etat de l'arbre de control *)
    let eval_control =
      let rec eval pere p sched =
	if p.alive then
	  match p.kind with
	  | Top -> raise RML
	  | Kill f_k ->
	      if p.cond sched
	      then
		(pere.next <- f_k :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.children <-
		  List.fold_left
		    (fun acc node ->
		      if eval p node sched
		      then node :: acc
		      else acc)
		    [] p.children;
		 true)
	  | Kill_handler handler ->
	      if p.cond sched
	      then
		(pere.next <- (handler sched) :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.children <-
		  List.fold_left
		    (fun acc node ->
		      if eval p node sched
		      then node :: acc
		      else acc)
		    [] p.children;
		 true)
	  | Susp -> (
	      let pre_susp = p.susp in
	      if p.cond sched then p.susp <- not pre_susp;
	      if pre_susp
	      then true
	      else
		(p.children <-
		  List.fold_left
		    (fun acc node ->
		      if eval p node sched
		      then node :: acc
		      else acc)
		    [] p.children;
		 true))
	  | When f_when ->
	      if p.susp
	      then true
	      else
		(p.susp <- true;
		 pere.next <- !f_when :: pere.next;
		 p.children <-
		   List.fold_left
		     (fun acc node ->
		       if eval p node sched
		       then node :: acc
		       else acc)
		     [] p.children;
		 true)
	else
	  (set_kill p;
	   false)
      in
      fun sched ->
	sched.top.children <-
	  (List.fold_left
	     (fun acc node ->
	       if eval sched.top node sched
	       then node :: acc
	       else acc)
	     [] sched.top.children)

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let next_to_current sched =
      let rec next_to_current p =
	if p.alive
	then
	  if p.susp
	  then ()
	  else
	    (sched.current <- List.rev_append p.next sched.current;
	     p.next <- [];
	     List.iter next_to_current p.children)
	else ()
      in next_to_current

(* debloquer les processus en attent d'un evt *)
    let wakeUp sched w =
      sched.current <- List.rev_append !w sched.current;
      w := []


(* creation d'evenements *)
    let new_evt_combine sched_id default combine =
      (sched_id, Event.create default combine, ref [], ref [])

    let new_evt sched_id =
      new_evt_combine sched_id [] (fun x y -> x :: y)

    let unit_value = ()
    let dummy_step _ _ = ()


(**************************************************)
(* sched                                          *)
(**************************************************)
    let rec schedule sched =
	match sched.current with
	| f :: c ->
	    sched.current <- c;
	    f sched unit_value
	| [] -> ()

(* ------------------------------------------------------------------------ *)
    let rml_pre_status (sid, n, _, _) =
      if sid = get_current_sched_id() then
	Event.pre_status n
      else
	raise Wrong_scheduler

    let rml_pre_value (sid, n, _, _) =
      if sid = get_current_sched_id() then
	Event.pre_value n
      else
	raise Wrong_scheduler

    let rml_last (sid, n, _, _) =
      if sid = get_current_sched_id() then
	Event.last n
      else
	raise Wrong_scheduler

    let rml_default (sid, n, _, _) =
      if sid = get_current_sched_id() then
	Event.default n
      else
	raise Wrong_scheduler


(* ------------------------------------------------------------------------ *)
    let rml_global_signal () = new_evt (get_current_sched_id())

    let rml_global_signal_combine default combine =
      new_evt_combine (get_current_sched_id()) default combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' (sid, n,wa,wp) =
      fun is_long_wait ->
	(fun sched ->
	  if sid = sched.id then
	    Event.status n
	  else
	    raise Wrong_scheduler),
	[ if is_long_wait then wa else wp ]

    let cfg_present evt_expr =
      fun is_long_wait ->
	let evt = evt_expr() in
	cfg_present' evt is_long_wait

    let cfg_and c1 c2 =
      fun is_long_wait ->
	let is_true1, evt_list1 = c1 is_long_wait in
	let is_true2, evt_list2 = c2 is_long_wait in
	(fun sched -> is_true1 sched && is_true2 sched),
	List.rev_append evt_list1 evt_list2

    let cfg_or c1 c2 =
      fun is_long_wait ->
	let is_true1, evt_list1 = c1 is_long_wait in
	let is_true2, evt_list2 = c2 is_long_wait in
	(fun sched -> is_true1 sched || is_true2 sched),
	List.rev_append evt_list1 evt_list2



(* ------------------------------------------------------------------------ *)

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k ctrl ->
	let f_nothing =
	  fun sched _ ->
	    f_k sched unit_value
	in f_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k ctrl ->
	let f_compute =
	  fun sched _ ->
	    let v = e() in
	    f_k sched v
	in f_compute

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun f_k ctrl ->
	let f_pause =
	  fun sched _ ->
	    ctrl.next <- f_k :: ctrl.next;
	    schedule sched
	in f_pause

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k ctrl ->
	let f_halt =
	  fun sched _ ->
	    schedule sched
	in f_halt

(**************************************)
(* emit                               *)
(**************************************)
    let step_emit f_k ctrl (sid,n,wa,wp) e sched _ =
      if sid = sched.id then
	(Event.emit n (e());
	 wakeUp sched wa;
	 wakeUp sched wp;
	 f_k sched unit_value)
      else
	raise Wrong_scheduler

    let rml_emit_val expr_evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  fun sched _ ->
	    let evt = expr_evt() in
	    step_emit f_k ctrl evt e sched unit_value
	in f_emit_val

    let rml_emit_val' evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  step_emit f_k ctrl evt e
	in f_emit_val

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val (sid,n,wa,wp) v =
      let sched = get_current_sched() in
      if sid = sched.id then
	(Event.emit n v;
	 wakeUp sched wa;
	 wakeUp sched wp)
      else
	raise Wrong_scheduler

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate f_k ctrl (sid,n,wa,wp) =
      let w = if ctrl.kind = Top then wa else wp in
      if ctrl.kind = Top then
	let rec f_await_top =
	  fun sched _ ->
	    if sid = sched.id then
	      if Event.status n
	      then
		f_k sched unit_value
	      else
		(w := f_await_top :: !w;
		 schedule sched)
	    else
	      raise Wrong_scheduler
	in f_await_top
      else
	let rec f_await_not_top =
	  fun sched _ ->
	    if sid = sched.id then
	      if Event.status n
	      then
		f_k sched unit_value
	      else
		if sched.eoi
		then
		  (ctrl.next <- f_await_not_top :: ctrl.next;
		   schedule sched)
		else
		  (w := f_await_not_top :: !w;
		   sched.toWakeUp <- w :: sched.toWakeUp;
		   schedule sched)
	    else
	      raise Wrong_scheduler
	in f_await_not_top

    let rml_await_immediate expr_evt =
      fun f_k ctrl ->
	let f_await =
	  fun sched _ ->
	    let evt = expr_evt() in
	    step_await_immediate f_k ctrl evt sched unit_value
	in f_await

    let rml_await_immediate' evt =
      fun f_k ctrl ->
	let f_await =
	  step_await_immediate f_k ctrl evt
	in f_await

(**************************************)
(* await_immediate_conf               *)
(**************************************)
    let rml_await_immediate_conf expr_cfg =
      fun f_k ctrl ->
	if ctrl.kind = Top then
	  let f_await_top =
	    fun sched _ ->
	      let is_true, w_list = expr_cfg true in
	      if is_true sched then
		f_k sched unit_value
	      else
		let ref_f = ref None in
		let f w step_wake_up sched _=
		  if is_true sched then
		    (ref_f := None;
		     f_k sched unit_value)
		  else
		    (w := step_wake_up :: !w;
		     schedule sched)
		in
		let gen_step w =
		  let rec step_wake_up sched x =
		    match !ref_f with
		    | None -> schedule sched
		    | Some f -> f w step_wake_up sched x
		  in step_wake_up
		in
		ref_f := Some f;
		List.iter
		  (fun w -> w := (gen_step w) :: !w)
		  w_list;
		schedule sched
	  in f_await_top
	else
	  let f_await_not_top =
	    fun sched _ ->
	      let is_true, w_list = expr_cfg false in
	      if is_true sched then
		f_k sched unit_value
	      else
		let ref_f = ref None in
		let rec f w step_wake_up sched _ =
		  if is_true sched then
		    (ref_f := None;
		     f_k sched unit_value)
		  else
		    if sched.eoi
		    then
		      (ctrl.next <- step_wake_up :: ctrl.next;
		       schedule sched)
		    else
		      (w := step_wake_up :: !w;
		       sched.toWakeUp <- w :: sched.toWakeUp;
		       schedule sched)
		in
		let gen_step w =
		  let rec step_wake_up sched x =
		    match !ref_f with
		    | None -> schedule sched
		    | Some f -> f w step_wake_up sched x
		  in step_wake_up
		in
		ref_f := Some f;
		List.iter
		  (fun w ->
		    w := (gen_step w) :: !w;
		    sched.toWakeUp <- w :: sched.toWakeUp)
		  w_list;
		schedule sched
	  in f_await_not_top

(**************************************)
(* get                                *)
(**************************************)
    let step_get f_k ctrl (sid,n,_,_) p =
      let rec f_get =
	fun sched _ ->
	  if sid = sched.id then
	    if sched.eoi
	    then
	      let x =
		if Event.status n
		then Event.value n
		else Event.default n
	      in
	      let f_body = p x f_k ctrl in
	      ctrl.next <- f_body :: ctrl.next;
	      schedule sched
	    else
	      (sched.weoi := f_get :: !(sched.weoi);
	       schedule sched)
	  else
	    raise Wrong_scheduler
      in f_get

    let rml_get expr_evt p =
      fun f_k ctrl ->
	let f_get =
	  fun sched _ ->
	    let evt = expr_evt() in
	    step_get f_k ctrl evt p sched unit_value
	in f_get

    let rml_get' evt p =
      fun f_k ctrl ->
	let f_get =
	    step_get f_k ctrl evt p
	in f_get




(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one f_k ctrl (sid,n,wa,wp) p =
      let w = if ctrl.kind = Top then wa else wp in
      let f_await_one =
	if ctrl.kind = Top then
	  let rec f_await_one_top =
	    fun sched _ ->
	      if sid = sched.id then
		if Event.status n
		then
		  let x = Event.one n in
		  p x f_k ctrl sched unit_value
		else
		  (w := f_await_one_top :: !w;
		   schedule sched)
	      else
		raise Wrong_scheduler
	  in f_await_one_top
	else
	  let rec f_await_one_not_top =
	    fun sched _ ->
	      if sid = sched.id then
		if Event.status n
		then
		  let x = Event.one n in
		  p x f_k ctrl sched unit_value
		else
		  if sched.eoi
		  then
		    (ctrl.next <- f_await_one_not_top :: ctrl.next;
		     schedule sched)
		  else
		    (w := f_await_one_not_top :: !w;
		     sched.toWakeUp <- w :: sched.toWakeUp;
		     schedule sched)
	      else raise Wrong_scheduler
	  in f_await_one_not_top
      in f_await_one

     let rml_await_immediate_one expr_evt p =
      fun f_k ctrl ->
      let f_await_one =
	fun sched _ ->
	  let evt = expr_evt() in
	  step_await_immediate_one f_k ctrl evt p sched unit_value
      in f_await_one

    let rml_await_immediate_one' evt p =
      fun f_k ctrl ->
 	step_await_immediate_one f_k ctrl evt p


(**************************************)
(* await_all_match                    *)
(**************************************)
    let step_await_all_match f_k ctrl (sid,n,wa,wp) matching p =
      let w = if ctrl.kind = Top then wa else wp in
      let f_await_all_match =
	if ctrl.kind = Top then
	  let rec f_await_top =
	    fun sched _ ->
	      if sid = sched.id then
		let v = Event.value n in
		if sched.eoi
		then
		  if Event.status n && matching v
		  then
		    let x = v in
		    let f_body = p x f_k ctrl in
		    ctrl.next <- f_body :: ctrl.next;
		    schedule sched
		  else
		    (w := f_await_top :: !w;
		     schedule sched)
		else
		  if Event.status n
		  then
		    (sched.weoi := f_await_top :: !(sched.weoi);
		     schedule sched)
		  else
		    (w := f_await_top :: !w;
		     schedule sched)
	      else
		raise Wrong_scheduler
	  in f_await_top
	else
	  let rec f_await_not_top =
	    fun sched _ ->
	      if sid = sched.id then
		if sched.eoi
		then
		  let v = Event.value n in
		  if Event.status n && matching v
		  then
		    let x = v in
		    let f_body = p x f_k ctrl in
		    ctrl.next <- f_body :: ctrl.next;
		    schedule sched
		  else
		    (ctrl.next <- f_await_not_top :: ctrl.next;
		     schedule sched)
		else
		  (w := f_await_not_top :: !w;
		   sched.toWakeUp <- w :: sched.toWakeUp;
		   schedule sched)
	      else
		raise Wrong_scheduler
	  in f_await_not_top
      in f_await_all_match


    let rml_await_all_match expr_evt matching p =
      fun f_k ctrl ->
	let f_await_all_match =
	  fun sched _ ->
	    let evt = expr_evt() in
	    step_await_all_match f_k ctrl evt matching p sched unit_value
	in f_await_all_match

    let rml_await_all_match' evt matching p =
      fun f_k ctrl ->
 	step_await_all_match f_k ctrl evt matching p

(**************************************)
(* present                            *)
(**************************************)

    let step_present f_k ctrl (sid,n,_,wp) f_1 f_2 =
      let rec f_present =
	fun sched _ ->
	  if sid = sched.id then
	    if Event.status n
	    then
	      f_1 sched unit_value
	    else
	      if sched.eoi
	      then
		(ctrl.next <- f_2 :: ctrl.next;
		 schedule sched)
	      else
		(wp := f_present :: !wp;
		 sched.toWakeUp <- wp :: sched.toWakeUp;
		 schedule sched)
      in f_present

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let rec f_present =
	  fun sched _ ->
	    let evt = expr_evt () in
	    step_present f_k ctrl evt f_1 f_2 sched unit_value
	in f_present

    let rml_present' evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	step_present f_k ctrl evt f_1 f_2

(**************************************)
(* present_conf                       *)
(**************************************)
    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl ->
	fun sched _ ->
	  let f_1 = p_1 f_k ctrl in
	  let f_2 = p_2 f_k ctrl in
	  let is_true, w_list = expr_cfg false in
	  if is_true sched
	  then
	    f_1 sched unit_value
	  else
	    let ref_f = ref None in
	    let f w step_wake_up sched _ =
	      if is_true sched then
		(ref_f := None;
		 f_1 sched unit_value)
	      else
		if sched.eoi
		then
		  (ctrl.next <- f_2 :: ctrl.next;
		   schedule sched)
		else
		  (w := step_wake_up :: !w;
		   sched.toWakeUp <- w :: sched.toWakeUp;
		   schedule sched)
	    in
	    let gen_step w =
	      let rec step_wake_up sched x =
	      match !ref_f with
	      | None -> schedule sched
	      | Some f -> f w step_wake_up sched x
	      in step_wake_up
	    in
	    ref_f := Some f;
	    List.iter
	      (fun w ->
		w := (gen_step w) :: !w;
		sched.toWakeUp <- w :: sched.toWakeUp)
	      w_list;
	    schedule sched

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k ctrl ->
	let f_2 = p_2 f_k ctrl in
	let f_1 = p_1 (fun sched x -> f_2 sched ()) ctrl in
	f_1

(**************************************)
(* par                                *)
(**************************************)
(* Utilisation de Obj.magic pour le pb de la generalisation des *)
(* applications partielles.                                     *)

    let join cpt =
      fun f_k ctrl ->
	let f_join =
	  fun sched _ ->
	    incr cpt;
	    if !cpt = 2
	    then (
	      (* cpt := 0; *)
	      f_k sched unit_value
	     )
	    else
	      schedule sched
	in f_join

    let rml_par p_1 p_2 =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let j = join cpt f_k ctrl in
	let f_1 = p_1 (Obj.magic j: 'a step) ctrl in
	let f_2 = p_2 (Obj.magic j: 'b step) ctrl in
	let f_par =
	  fun sched _ ->
	    cpt := 0;
	    sched.current <- f_2 :: sched.current;
	    f_1 sched unit_value
	in f_par

(**************************************)
(* merge                              *)
(**************************************)

    let rml_merge p_1 p_2 =
      fun f_k ctrl ->
	fun sched _ -> raise RML


(**************************************)
(* loop                               *)
(**************************************)
(*
let rec rml_loop p f_k ctrl sched _ =
  p (rml_loop p f_k ctrl) ctrl sched unit_value
*)

(*
let rml_loop p =
  fun f_k ctrl ->
    let rec f_1 = lazy (p f ctrl)
    and f =
      fun sched _ ->
	Lazy.force f_1 sched unit_value
    in
    f
*)

    let rml_loop p =
      fun f_k ctrl ->
	let f_1 = ref dummy_step in
	let f_loop = p (fun sched _ -> !f_1 sched unit_value) ctrl in
	f_1 := f_loop;
	f_loop


(**************************************)
(* loop_n                             *)
(**************************************)

    let rml_loop_n e p =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let f_1 = ref dummy_step in
	let f_loop =
	  p
	    (fun sched _ ->
	      if !cpt > 0 then
		(decr cpt; !f_1 sched unit_value)
	      else
		f_k sched unit_value)
	    ctrl
	in
	f_1 := f_loop;
	fun sched _ ->
	  cpt := e();
	  f_loop sched unit_value


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p =
      fun f_k ctrl ->
	let f_signal =
	  fun sched _ ->
	    let evt = new_evt sched.id in
	    let f = p evt f_k ctrl in
	    f sched unit_value
	in f_signal

    let rml_signal_combine default comb p =
      fun f_k ctrl ->
	let f_signal =
	  fun sched _ ->
	    let evt = new_evt_combine sched.id (default()) (comb()) in
	    let f = p evt f_k ctrl in
	    f sched unit_value
	in f_signal

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k ctrl ->
	let f_def =
	  fun sched _ ->
	    let f = p (e()) f_k ctrl in
	    f sched unit_value
	in f_def

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k ctrl ->
	let f_def =
	  p1
	    (fun sched v ->
	      let f = p2 v f_k ctrl in
	      f sched unit_value)
	    ctrl
	in f_def

(**************************************)
(* def_and_dyn                        *)
(**************************************)

    let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
	fun f_k ctrl ->
	  fun sched x ->
	    value_array.(i) <- x;
	    decr cpt;
	    if !cpt = 0 then
	      let f = p3 value_array f_k ctrl in
	      f sched unit_value
	    else
	      schedule sched
      in
      fun p_array p3 ->
	fun f_k ctrl ->
	  let n = Array.length p_array in
	  let cpt = ref n in
	  let value_array = Array.make n (Obj.magic()) in
	  let step_init =
	    fun sched _ ->
	      cpt := n;
	      for i = 0 to n - 1 do
		let f =
		  p_array.(i) (join_n cpt value_array p3 i f_k ctrl) ctrl
		in
		sched.current <- f :: sched.current
	      done;
	      schedule sched
	  in step_init

(**************************************)
(* match                              *)
(**************************************)

    let rml_match e p =
      fun f_k ctrl ->
	let f_match =
	  fun sched _ ->
	    let f = p (e()) f_k ctrl in
	    f sched unit_value
	in f_match


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k ctrl ->
	let f_run =
	  fun sched _ ->
	    let f_1 = (e ()) () f_k ctrl in
	    f_1 sched unit_value
	in f_run


(**************************************)
(* until                              *)
(**************************************)
(* ---------- Misc functions for until, control and when ---------- *)
    let new_ctrl kind =
      { kind = kind;
	alive = true;
	susp = false;
	children = [];
	cond = (fun sched -> false);
	next = [] }

    let start_ctrl f_k ctrl f new_ctrl =
      let f_ctrl =
	fun sched _ ->
	  if new_ctrl.alive
	  then
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f sched unit_value
      in f_ctrl

    let end_ctrl f_k new_ctrl =
      fun sched x ->
	set_kill new_ctrl;
	new_ctrl.alive <- false;
	f_k sched x
(* ---------------------------------------------------------------- *)

    let rml_until expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun sched _ ->
	    let (sid,n,_,_) = expr_evt () in
	    new_ctrl.cond <-
	      (fun sched ->
		if sid = sched.id then
		  Event.status n
		else
		  raise Wrong_scheduler);
	    start_ctrl f_k ctrl f new_ctrl sched unit_value
	in f_until

    let rml_until' (sid,n,_,_) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	new_ctrl.cond <-
	  (fun sched ->
	    if sid = sched.id then
	      Event.status n
	    else
	      raise Wrong_scheduler);
	start_ctrl f_k ctrl f new_ctrl


(**************************************)
(* until_conf                         *)
(**************************************)

    let rml_until_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun sched _ ->
	    let cond, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl sched unit_value
	in f_until



(**************************************)
(* until handler                      *)
(**************************************)

    let rml_until_handler_local expr_evt matching_opt p p_handler =
      fun f_k ctrl ->
	let evt = ref (Obj.magic()) in
	let handler =
	  fun sched ->
	    let x =
	      let sid,n, _, _ = !evt in
	      if sid = sched.id then
		if Event.status n
		then Event.value n
		else raise RML
	      else raise Wrong_scheduler
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun sched _ ->
	    let (sid, n, _, _) as e = expr_evt () in
	    evt := e;
	    begin match matching_opt with
	    | None ->
		new_ctrl.cond <-
		  (fun sched ->
		    if sid = sched.id then
		      Event.status n
		    else raise Wrong_scheduler);
	    | Some matching ->
		new_ctrl.cond <-
		  (fun sched ->
		    if sid = sched.id then
		      Event.status n && matching (Event.value n)
		    else raise Wrong_scheduler);
	    end;
	    start_ctrl f_k ctrl f new_ctrl sched unit_value
	in f_until

    let rml_until_handler_local' (sid,n,_,_) matching_opt p p_handler =
      fun f_k ctrl ->
	let handler =
	  fun sched ->
	    let x =
	      if sid = sched.id then
		if Event.status n
		then Event.value n
		else raise RML
	      else raise Wrong_scheduler
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	begin match matching_opt with
	| None ->
	    new_ctrl.cond <-
	      (fun sched ->
		if sid = sched.id then
		  Event.status n
		else raise Wrong_scheduler);
	| Some matching ->
	    new_ctrl.cond <-
	      (fun sched ->
		if sid = sched.id then
		  Event.status n && matching (Event.value n)
		else raise Wrong_scheduler);
	end;
	start_ctrl f_k ctrl f new_ctrl

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
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun sched _ ->
	    let (sid, n, _, _) = expr_evt () in
	    new_ctrl.cond <-
	      (fun sched ->
		if sid = sched.id then
		  Event.status n
		else raise Wrong_scheduler);
	    start_ctrl f_k ctrl f new_ctrl sched unit_value
	in f_control

    let rml_control' (sid, n, _, _) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	new_ctrl.cond <-
	  (fun sched ->
	    if sid = sched.id then
	      Event.status n
	    else raise Wrong_scheduler);
	start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* control_conf                       *)
(**************************************)

    let rml_control_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun sched _ ->
	    let cond, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl sched ()
	in f_control


(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl (sid,n,wa,wp) f new_ctrl dummy =
      let w = if ctrl.kind = Top then wa else wp in
      new_ctrl.cond <-
	(fun sched ->
	  if sid = sched.id then
	    Event.status n
	  else raise Wrong_scheduler);
      let rec f_when =
	fun sched _ ->
	  if sid = sched.id then
	    if Event.status n
	    then
	      (new_ctrl.susp <- false;
	       next_to_current sched new_ctrl;
	       schedule sched)
	    else
	      if sched.eoi
	      then
		(ctrl.next <- f_when :: ctrl.next;
		 schedule sched)
	      else
		(w := f_when :: !w;
		 if ctrl.kind <> Top then
		   sched.toWakeUp <- w :: sched.toWakeUp;
		 schedule sched)
      in
      let start_when =
	fun sched _ ->
	  if new_ctrl.alive
	  then
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  if sid = sched.id then
	    if Event.status n
	    then
	      (new_ctrl.susp <- false;
	       new_ctrl.next <- [];
	       f sched unit_value)
	    else
	      (new_ctrl.susp <- true;
	       new_ctrl.next <- [f];
	       w := f_when :: !w;
	       if ctrl.kind <> Top then sched.toWakeUp <- w :: sched.toWakeUp;
	       schedule sched)
	  else
	    raise Wrong_scheduler
      in
      dummy := f_when;
      start_when

      let rml_when expr_evt p =
      fun f_k ctrl ->
	let dummy = ref dummy_step in
	let new_ctrl = new_ctrl (When dummy) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let start_when =
	  fun sched _ ->
	    let evt = expr_evt () in
	    step_when f_k ctrl evt f new_ctrl dummy sched unit_value
	in
	start_when

    let rml_when' evt p =
      fun f_k ctrl ->
	let dummy = ref dummy_step in
	let new_ctrl = new_ctrl (When dummy) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let start_when =
	  step_when f_k ctrl evt f new_ctrl dummy
	in
	start_when

(**************************************)
(* when_conf                          *)
(**************************************)
    let rml_when_conf expr_cfg =
      fun f_k ctrl ->
	fun sched _ -> raise RML


(**************************************)
(* if                                 *)
(**************************************)

    let rml_if e p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let f_if =
	  fun sched _ ->
	    if e() then
	      f_1 sched unit_value
	    else
	      f_2 sched unit_value
	in f_if


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k ctrl ->
	let f_body = ref dummy_step in
	let f_while =
	  fun sched _ ->
	    if e()
	    then !f_body sched unit_value
	    else f_k sched unit_value
	in
	f_body := p f_while ctrl;
	f_while


(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k ctrl ->
	let rec f_for i v2 =
	  fun sched _ ->
	    incr i;
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl sched unit_value
	    else f_k sched unit_value
	in
	let f_for_init =
	  fun sched _ ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl sched unit_value
	    else f_k sched unit_value
	in
	f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)
    let join_n cpt =
      fun f_k ctrl ->
	let f_join_n =
	  fun sched _ ->
	    decr cpt;
	    if !cpt = 0 then
	      f_k sched unit_value
	    else
	      schedule sched
	in f_join_n

    let rml_fordopar e1 e2 dir p =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let j = join_n cpt f_k ctrl in
	let f_fordopar =
	  fun sched _ ->
	    if dir then
	      begin
		let min = e1() in
		let max = e2() in
		cpt := max - min + 1;
		if !cpt <= 0 then
		  f_k sched unit_value
		else
		  begin
		    for i = max downto min do
		      let f = p i j ctrl in
		      sched.current <- f :: sched.current
		    done;
		    schedule sched
		  end
	      end
	    else
	      begin
		let max = e1() in
		let min = e2() in
		cpt := max - min + 1;
		if !cpt <= 0 then
		  f_k sched unit_value
		else
		  begin
		    for i = min to max do
		      let f = p i j ctrl in
		      sched.current <- f :: sched.current
		    done;
		    schedule sched
		  end
	      end
	in
	f_fordopar


(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

    let rml_await expr_evt =
      fun f_k ctrl ->
	fun sched _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_pause f_k ctrl) ctrl sched unit_value

    let rml_await' evt =
      fun f_k ctrl ->
	rml_await_immediate' evt (rml_pause f_k ctrl) ctrl

    let rml_await_all expr_evt p =
      fun f_k ctrl ->
	fun sched _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_get' evt p f_k ctrl) ctrl
	    sched unit_value

    let rml_await_all' evt p =
      fun f_k ctrl ->
	rml_await_immediate' evt (rml_get' evt p f_k ctrl) ctrl

    let rml_await_one expr_evt p =
      let pause_p x =
	fun f_k ctrl ->
	  rml_pause (p x f_k ctrl) ctrl
      in
      fun f_k ctrl ->
	fun sched _ ->
	  let evt = expr_evt () in
	  rml_await_immediate_one' evt pause_p f_k ctrl sched unit_value

    let rml_await_one' evt p =
      let pause_p x =
	fun f_k ctrl ->
	  rml_pause (p x f_k ctrl) ctrl
      in
      fun f_k ctrl ->
	rml_await_immediate_one' evt pause_p f_k ctrl

    let rml_await_conf expr_cfg =
      fun f_k ctrl ->
	rml_await_immediate_conf expr_cfg (rml_pause f_k ctrl) ctrl

(* ------------------------------------------------------------------------ *)
(*
    let join_n cpt =
      fun f_k ctrl ->
	let f_join_n =
	  fun() ->
	    decr cpt;
	    if !cpt = 0 then
	      f_k()
	    else
	      sched()
	in f_join_n
*)
    let rml_par_n p_list =
      fun f_k ctrl ->
	let nb = List.length p_list in
	let cpt = ref nb in
	let j = join_n cpt f_k ctrl in
	let f_list = List.rev_map (fun p -> p j ctrl) p_list in
	let f_par_n =
	  fun sched _ ->
	    cpt := nb;
	    sched.current <- List.rev_append f_list sched.current;
	    schedule sched
	in f_par_n

    let rml_seq_n p_list =
      fun f_k ctrl ->
	let f =
	  List.fold_right (fun p -> fun k -> p k ctrl) p_list f_k
	in f

(* ------------------------------------------------------------------------ *)
    exception End


(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make p =
      let result = ref None in
      (* creates the scheduler *)
      let sched = new_scheduler () in
      (* the main step function *)
      let f = p () (fun _ x -> result := Some x; raise End) sched.top in
      sched.current <- [f];
      (* the react function *)
      let rml_react () =
	try
	  schedule sched;
	  sched.eoi <- true;
	  wakeUp sched sched.weoi;
	  wakeUpAll sched;
	  schedule sched;
	  eval_control sched;
	  next_to_current sched sched.top;
	  Event.next ();
	  sched.eoi <- false;
	  None
	with
	| End -> !result
      in
      rml_react


(**************************************************)
(* rml_make_unit                                  *)
(**************************************************)

    let rml_make_unit (p: unit process) =
      (* creates the scheduler *)
      let sched = new_scheduler () in

      (* Function to create the last continuation of a toplevel process *)
      let join_end =
	let term_cpt = ref 0 in
	fun () ->
	  incr term_cpt;
	  let f sched x =
	    decr term_cpt;
	    if !term_cpt > 0 then
	      schedule sched
	    else
	      raise End
	  in f
      in

      (* the main step function *)
      let f = p () (join_end ()) sched.top in
      sched.current <- [f];

      (* the react function *)
      let rml_react () =
	try
	  schedule sched;
	  sched.eoi <- true;
	  wakeUp sched sched.weoi;
	  wakeUpAll sched;
	  schedule sched;
	  eval_control sched;
	  next_to_current sched sched.top;
	  Event.next ();
	  sched.eoi <- false;
	  None
	with
	| End -> Some ()
      in

      (* the add_process function*)
      let add_process p =
	let f =  p () (join_end()) sched.top in
	sched.current <- f :: sched.current
      in

      rml_react, add_process

(**************************************************)
(* rml_make_exec_process                          *)
(**************************************************)

    let rml_make_exec_process (p: unit process) =

      (* creates the scheduler *)
      let sched = new_scheduler () in

      (* Function to create the last continuation of a toplevel process *)
      let join_end =
	let term_cpt = ref 0 in
	fun () ->
	  incr term_cpt;
	  let f sched x =
	    decr term_cpt;
	    if !term_cpt > 0 then
	      schedule sched
	    else
	      raise End
	  in f
      in

      (* the add_process function*)
      let add_process p =
	let f =  p () (join_end()) sched.top in
	sched.current <- f :: sched.current
      in

      (* the main step function *)
      let f = p () (join_end ()) sched.top in
      sched.current <- [f];

      (* the react function *)
      let rml_react proc_list =
	try
	  List.iter add_process proc_list;
	  schedule sched;
	  sched.eoi <- true;
	  wakeUp sched sched.weoi;
	  wakeUpAll sched;
	  schedule sched;
	  eval_control sched;
	  next_to_current sched sched.top;
	  Event.next ();
	  sched.eoi <- false;
	  ()
	with
	| End -> assert false
      in

      rml_react


  end (* Module Rml_interpreter *)
