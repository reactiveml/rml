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
(* created: 2007-02-11  *)
(* file: lk_threaded.ml *)


let print_DEBUG s =
  print_string s;
  print_newline()

let nb_threads = 4


module Lk_interpreter: Lk_interpreter.S =
  functor (Event: Sig_env.S) ->
  struct

    exception RML

    type ('a, 'b) event =
	('a,'b) Event.t * unit step list ref * unit step list ref  * event_ctrl

    and event_ctrl = Mutex.t

    and event_cfg = bool -> (unit -> bool) * unit step list ref list

    and control_tree =
	{ kind: control_type;
	  mutable alive: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable children: control_tree list;
	  mutable next: next;
	  lock: Mutex.t }
    and control_type =
	Top
      | Kill of (unit -> unit step)
      | Susp
      | When of unit step ref

    and 'a step = 'a -> unit
    and next = unit step list
    and current = unit step list
    and waiting = unit step list
    and 'a process = 'a step -> control_tree -> unit step
    and join_point = int ref * Mutex.t


    let new_event_ctrl = Mutex.create

(* ------------------------------------------------------------------------ *)

(* liste des processus a executer dans l'instant *)
    let current_lock = Mutex.create ()
    let current = ref ([]: current)
    let running = ref 0

    let sched_condition = Condition.create ()

    let rec spawn_threads = function
      | [] -> current := []
      | (f::c) as curr ->
	  if !running < nb_threads then
	    begin
	      incr running;
	      ignore (Thread.create f ());
	      spawn_threads c
	    end
	  else
	    current := curr

    let add_current =
      let rec aux l1 l2 =
	match l1 with
	| [] -> l2
	| [f] -> f::l2
	| f::l1' -> aux l1' (f::l2)
      in
      fun c ->
	Mutex.lock current_lock;
	match !current with
	| [] ->
	  if !running < nb_threads then
	    begin
	      spawn_threads c;
	      Mutex.unlock current_lock;
	    end
	  else
	    begin
	      current := c;
	      Mutex.unlock current_lock
	    end
	| curr ->
	    current := aux c curr;
	    Mutex.unlock current_lock

    let add_one_current f =
      Mutex.lock current_lock;
      match !current with
      | [] ->
	  if !running < nb_threads then
	    begin
	      incr running;
	      Mutex.unlock current_lock;
	      ignore (Thread.create f ())
	    end
	  else
	    begin
	      current := [f];
	      Mutex.unlock current_lock
	    end
      | curr ->
	  current := f :: !current;
	  Mutex.unlock current_lock


    let sched () =
      Mutex.lock current_lock;
      match !current with
      | f :: c ->
	  current := c;
	  Mutex.unlock current_lock;
	  f ()
      | [] ->
	  decr running;
	  if !running > 0 then
	    Mutex.unlock current_lock
	  else
	    begin
	      Condition.signal sched_condition;
	      Mutex.unlock current_lock
	    end

    let exec_sched () =
      match !current with
      | [] -> ()
      | c ->
	  Mutex.lock current_lock;
	  current := [];
	  spawn_threads c;
	  while !running > 0 do
	    Condition.wait sched_condition current_lock
	  done;
	  Mutex.unlock current_lock





(* ------------------------------------------------------------------------ *)


(* liste des listes de processus a revillier a la fin d'instant *)
    let toWakeUp_lock = Mutex.create()
    let toWakeUp = ref []
    let wakeUpAll () =
      List.iter
	(fun wp ->
	  current := List.rev_append !wp !current;
	  wp := [])
	!toWakeUp;
      toWakeUp := []


(* debloquer les processus en attent d'un evt *)
    let wakeUp w =
      add_current !w;
      w := []


(* racine de l'arbre de control *)
    let top =
      { kind = Top;
	alive = true;
	susp = false;
	children = [];
	cond = (fun () -> false);
	next = [];
	lock = Mutex.create(); }

    let rec rev_app x1 x2 =
      match x1 with
      | [] -> x2
      | f :: x1' -> rev_app x1' (f::x2)

(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current =
      let rec eval pere p active =
	if p.alive then
	  match p.kind with
	  | Top -> raise RML
	  | Kill handler ->
	      if p.cond()
	      then
		(pere.next <- (handler()) :: pere.next;
		 false)
	      else
		(p.children <- eval_children p p.children active [];
		 if active then next_to_current p
		 else next_to_father pere p;
		 true)
	  | Susp ->
	      let pre_susp = p.susp in
	      if p.cond() then p.susp <- not pre_susp;
	      let active = active && not p.susp in
	      if pre_susp
	      then
		(if active then next_to_current p;
		 true)
	      else
		(p.children <- eval_children p p.children active [];
		 if active then next_to_current p
		 else if not p.susp then next_to_father pere p;
		 true)
	  | When f_when ->
	      if p.susp
	      then true
	      else
		(p.susp <- true;
		 pere.next <- !f_when :: pere.next;
		 p.children <- eval_children p p.children false [];
		 true)
	else
	  false

      and eval_children p nodes active acc =
	match nodes with
	| [] -> acc
	| node :: nodes ->
	    if eval p node active
	    then eval_children p nodes active (node :: acc)
	    else eval_children p nodes active acc

      and next_to_current node =
	current := rev_app node.next !current;
	node.next <- []
      and next_to_father pere node =
	pere.next <- rev_app node.next pere.next;
	node.next <- []
      in
      fun () ->
	top.children <- eval_children top top.children true [];
	next_to_current top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current p =
      if p.alive && not p.susp then
	(Mutex.lock p.lock;
	 add_current p.next;
	 p.next <- [];
	 Mutex.unlock p.lock;
	 List.iter next_to_current p.children)
      else ()

(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create default combine, ref [], ref [], new_event_ctrl())

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)

    let eoi = ref false
    let weoi_lock = Mutex.create()
    let weoi = ref ([]: waiting)

    let dummy_step _ = ()

(* ------------------------------------------------------------------------ *)

    let rml_pre_status (n, _, _, _) = Event.pre_status n

    let rml_pre_value (n, _, _, _) = Event.pre_value n

    let rml_last (n, _, _, _) = Event.last n

    let rml_default (n, _, _, _) = Event.default n


(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine =  new_evt_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* compute                            *)
(**************************************)

    let rml_compute_v v k _ =
      k v

    let rml_compute e k _ =
      k (e())

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause k ctrl _ =
      Mutex.lock ctrl.lock;
      ctrl.next <- k :: ctrl.next;
      Mutex.unlock ctrl.lock;
      sched ()

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi k ctrl _ =
      raise RML

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt _ =
      sched ()

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rml_halt_kboi _ =
      sched ()

(**************************************)
(* emit                               *)
(**************************************)
    let set_emit (n,wa,wp,evt_lock) v =
      Mutex.lock evt_lock;
      Event.emit n v;
      wakeUp wa;
      wakeUp wp;
      Mutex.unlock evt_lock

    let rml_emit_v_v v1 v2 k _ =
      set_emit v1 v2;
      k ()

    let rml_emit_v_e v1 e2 k _ =
      rml_emit_v_v v1 (e2()) k ()

    let rml_emit_e_v e1 v2 k _ =
      rml_emit_v_v (e1()) v2 k ()

    let rml_emit_e_e e1 e2 k _ =
      rml_emit_v_v (e1()) (e2()) k ()

    let rml_emit = rml_emit_e_e

    let rml_emit_pure_v v1 =
      rml_emit_v_v v1 ()

    let rml_emit_pure_e e1 =
      rml_emit_v_v (e1()) ()

    let rml_emit_pure = rml_emit_pure_e

    let rml_expr_emit = set_emit

    let rml_expr_emit_pure evt = rml_expr_emit evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate_top (n,wa,_,evt_lock) k =
      let rec self _ =
	Mutex.lock evt_lock;
	if Event.status n
	then
	  (Mutex.unlock evt_lock;
	   k ())
	else
	  (wa := k :: !wa;
	   Mutex.unlock evt_lock;
	   sched ())
      in self

    let step_await_immediate (n,_,wp,evt_lock) k ctrl =
      let rec self _ =
	Mutex.lock evt_lock;
	if Event.status n
	then
	  (Mutex.unlock evt_lock;
	   k ())
	else
	  if !eoi
	  then
	    (Mutex.unlock evt_lock;
	     Mutex.lock ctrl.lock;
	     ctrl.next <- self :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched())
	  else
	    (wp := self :: !wp;
	     Mutex.unlock evt_lock;
	     Mutex.lock toWakeUp_lock;
	     toWakeUp := wp :: !toWakeUp;
	     Mutex.unlock toWakeUp_lock;
	     sched())
      in self

    let rml_await_immediate_v evt k ctrl _ =
      if ctrl.kind = Top then
	step_await_immediate_top evt k ()
      else
	step_await_immediate evt k ctrl ()

    let rml_await_immediate expr_evt k ctrl _ =
      let evt = expr_evt() in
      rml_await_immediate_v evt k ctrl ()

(**************************************)
(* get                                *)
(**************************************)
    let step_get_eoi n f ctrl _ =
      let v =
	if Event.status n
	then Event.value n
	else Event.default n
      in
      let step = f v in
      Mutex.lock ctrl.lock;
      ctrl.next <- step :: ctrl.next;
      Mutex.unlock ctrl.lock;
      sched()

    let step_get (n,_,_,_) f ctrl _ =
      let step = step_get_eoi n f ctrl in
      Mutex.lock weoi_lock;
      weoi := step :: !weoi;
      Mutex.unlock weoi_lock;
      sched ()

    let rml_get_v = step_get

    let rml_get expr_evt f ctrl _ =
      step_get (expr_evt()) f ctrl ()

(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one_top (n, wa, _, evt_lock) f =
      let rec self _ =
	Mutex.lock evt_lock;
	if Event.status n
	then
	  (Mutex.unlock evt_lock;
	   let v = Event.one n in
	   f v ())
	else
	  (wa := self :: !wa;
	   Mutex.unlock evt_lock;
	   sched ())
      in self

    let step_await_immediate_one (n, _, wp, evt_lock) f ctrl =
      let rec self _ =
	Mutex.lock evt_lock;
	if Event.status n
	then
	  (Mutex.unlock evt_lock;
	   let v = Event.one n in
	   f v ())
	else
	  if !eoi
	  then
	    (Mutex.unlock evt_lock;
	     Mutex.lock ctrl.lock;
	     ctrl.next <- self :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched())
	  else
	    (wp := self :: !wp;
	     Mutex.unlock evt_lock;
	     Mutex.lock toWakeUp_lock;
	     toWakeUp := wp :: !toWakeUp;
	     Mutex.unlock toWakeUp_lock;
	     sched())
      in self

    let rml_await_immediate_one expr_evt f ctrl _ =
      if ctrl.kind = Top then
	step_await_immediate_one_top (expr_evt()) f ()
      else
	step_await_immediate_one (expr_evt()) f ctrl ()

    let rml_await_immediate_one_v evt f ctrl _ =
      if ctrl.kind = Top then
	step_await_immediate_one_top evt f ()
      else
	step_await_immediate_one evt f ctrl ()

(**************************************)
(* present                            *)
(**************************************)
    let step_present ctrl (n,_,wp,evt_lock) k_1 k_2 =
      let rec self _ =
	Mutex.lock evt_lock;
	if Event.status n
	then
	  (Mutex.unlock evt_lock;
	   k_1 ())
	else
	  if !eoi
	  then
	    (Mutex.unlock evt_lock;
	     Mutex.lock ctrl.lock;
	     ctrl.next <- k_2 :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched ())
	  else
	    (wp := self :: !wp;
	     Mutex.unlock evt_lock;
	     Mutex.lock toWakeUp_lock;
	     toWakeUp := wp :: !toWakeUp;
	     Mutex.unlock toWakeUp_lock;
	     sched ())
      in self

    let rml_present_v ctrl evt k_1 k_2 _ = step_present ctrl evt k_1 k_2 ()

    let rml_present ctrl expr_evt k_1 k_2 _ =
      let evt = expr_evt () in
      step_present ctrl evt k_1 k_2 ()


(**************************************)
(* await_all_match                    *)
(**************************************)

    let step_await_all_match_top (n, wa, _, evt_lock) matching f ctrl =
      let rec self _ =
	Mutex.lock evt_lock;
	if !eoi then
	  let v = Event.value n in
	  if Event.status n && matching v
	  then
	    (Mutex.unlock evt_lock;
	     let f_body = f v in
	     Mutex.lock ctrl.lock;
	     ctrl.next <- f_body :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched())
	  else
	    (wa := self :: !wa;
	     Mutex.unlock evt_lock;
	     sched ())
	else
	  if Event.status n
	  then
	    (Mutex.unlock evt_lock;
	     Mutex.lock weoi_lock;
	     weoi := self :: !weoi;
	     Mutex.unlock weoi_lock;
	     sched ())
	  else
	    (wa := self :: !wa;
	     Mutex.unlock evt_lock;
	     sched ())
      in self


    let step_await_all_match (n,_,wp,evt_lock) matching f ctrl =
      let rec self _ =
	Mutex.lock evt_lock;
	if !eoi then
	  let v = Event.value n in
	  if Event.status n && matching v
	  then
	    (Mutex.unlock evt_lock;
	     let f_body = f v in
	     Mutex.lock ctrl.lock;
	     ctrl.next <- f_body :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched())
	  else
	    (Mutex.unlock evt_lock;
	     Mutex.lock ctrl.lock;
	     ctrl.next <- self :: ctrl.next;
	     Mutex.unlock ctrl.lock;
	     sched())
	else
	  if Event.status n
	  then
	    (Mutex.unlock evt_lock;
	     Mutex.lock weoi_lock;
	     weoi := self :: !weoi;
	     Mutex.unlock weoi_lock;
	     sched ())
	  else
	    (wp := self :: !wp;
	     Mutex.unlock evt_lock;
	     Mutex.lock toWakeUp_lock;
	     toWakeUp := wp :: !toWakeUp;
	     Mutex.unlock toWakeUp_lock;
	     sched())
      in self

    let rml_await_all_match_v evt matching k ctrl _ =
      if ctrl.kind = Top then
	step_await_all_match_top evt matching k ctrl ()
      else
	step_await_all_match evt matching k ctrl ()

    let rml_await_all_match expr_evt matching k ctrl _ =
      let evt = expr_evt () in
      if ctrl.kind = Top then
	step_await_all_match_top evt matching k ctrl ()
      else
	step_await_all_match evt matching k ctrl ()



(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p _ =
      let evt = new_evt() in
      let f = p evt in
      f ()

    let rml_signal_combine_v_v default comb p _ =
      let evt = new_evt_combine default comb in
      let f = p evt in
      f ()

    let rml_signal_combine_v_e default comb p _ =
      rml_signal_combine_v_v default (comb()) p ()

    let rml_signal_combine_e_v default comb p _ =
      rml_signal_combine_v_v (default()) comb p ()

    let rml_signal_combine default comb p _ =
      rml_signal_combine_v_v (default()) (comb()) p ()

(**************************************)
(* par                                *)
(**************************************)
    let rml_split_par n f _ =
      let j = (ref n, Mutex.create()) in
      let k_list = f j in
      add_current k_list;
      sched()

    let rml_join_par (j, j_lock) k _ =
      Mutex.lock j_lock;
      decr j;
      if !j > 0 then
	(Mutex.unlock j_lock;
	 sched ())
      else
	(Mutex.unlock j_lock;
	 k ())

(**************************************)
(* join_def                           *)
(**************************************)
    let rml_join_def (j, j_lock) v_ref get_values k v =
      Mutex.lock j_lock;
      decr j;
      let n = !j in
      Mutex.unlock j_lock;
      v_ref := v;
      if n > 0 then
	sched ()
      else
        k (get_values())

(**************************************)
(* loop                               *)
(**************************************)
    let rml_loop p =
      let f_1 = ref dummy_step in
      let f_loop = p (fun _ -> !f_1 ()) in
      f_1 := f_loop;
      f_loop

(**************************************)
(* loop_n                             *)
(**************************************)
    let rml_loop_n_v n p k =
      let cpt = ref 0 in
      let f_1 = ref dummy_step in
      let f_loop =
	p
	  (fun _ ->
	    if !cpt > 0 then
	      (decr cpt; !f_1 ())
	    else
	      k ())
	in
	f_1 := f_loop;
	fun _ ->
	  if n > 0 then
	    (cpt := n - 1;
	     f_loop ())
	  else
	    k ()

    let rml_loop_n e p k _ =
      let n = e() in
      rml_loop_n_v n p k ()

(**************************************)
(* match                              *)
(**************************************)

    let rml_match_v e f _ =
      f e ()

    let rml_match e f _ =
      let k = f (e()) in
      k ()


(**************************************)
(* run                                *)
(**************************************)

    let rml_run_v p k ctrl _ =
      p k ctrl ()

    let rml_run e k ctrl _ =
      let body = e () k ctrl in
      body ()

(**************************************)
(* if                                 *)
(**************************************)

    let rml_if_v e k_1 k_2 _ =
      if e then
	k_1 ()
      else
	k_2 ()

    let rml_if e k_1 k_2 _ =
      rml_if_v (e()) k_1 k_2 ()


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p k =
      let f_body = ref dummy_step in
      let f_while _ =
	if e()
	then !f_body ()
	else k ()
      in
      f_body := p f_while;
      f_while

(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun k ->
	let rec f_for i v2 =
	  fun _ ->
	    incr i;
	    if cmp !i v2
	    then p !i (f_for i v2) ()
	    else k ()
	in
	let f_for_init =
	  fun _ ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then p !i (f_for i v2) ()
	    else k ()
	in
	f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)

    let rml_fordopar e1 e2 dir p k _ =
      if dir then
	begin
	  let min = e1() in
	  let max = e2() in
	  let j = ref (max - min + 1) in
	  let j_lock = Mutex.create() in
	  if !j <= 0 then
	    k ()
	  else
	    begin
	      for i = max downto min do
		let f = p (j, j_lock) i in
		add_one_current f
	      done;
	      sched()
	    end
	end
      else
	begin
	  let max = e1() in
	  let min = e2() in
	  let j = ref (max - min + 1) in
	  let j_lock = Mutex.create() in
	  if !j <= 0 then
	    k ()
	  else
	    begin
	      for i = min to max do
		let f = p (j, j_lock) i in
		add_one_current f
	      done;
	      sched ()
	    end
	end


(* ---------- Misc functions for until, control and when ---------- *)
    let new_ctrl kind cond =
      { kind = kind;
	alive = true;
	susp = false;
	children = [];
	cond = cond;
	next = [];
	lock = Mutex.create(); }


(**************************************)
(* until                              *)
(**************************************)
    let rml_start_until_v ctrl evt p k _ =
      let (n,_,_,evt_lock) = evt in
      let new_ctrl =
	new_ctrl
	  (Kill (fun () -> let v = Event.value n in k v))
	  (fun () -> Event.status n)
          (* Il n'est pas necessaite de proteger cette lecture
             car cette condition est evaluee a la fin d'instant *)
      in
      Mutex.lock ctrl.lock;
      ctrl.children <- new_ctrl :: ctrl.children;
      Mutex.unlock ctrl.lock;
      p new_ctrl ()

    let rml_start_until ctrl expr_evt p k _ =
      rml_start_until_v ctrl (expr_evt ()) p k ()

    let rml_end_until new_ctrl k x =
      new_ctrl.alive <- false;
      k x

(**************************************)
(* control                            *)
(**************************************)
   let rml_start_control_v ctrl evt p _ =
      let (n,_,_,evt_lock) = evt in
      let new_ctrl =
	new_ctrl
	  Susp
	  (fun () -> Event.status n)
      in
      Mutex.lock ctrl.lock;
      ctrl.children <- new_ctrl :: ctrl.children;
      Mutex.unlock ctrl.lock;
      p new_ctrl ()

   let rml_start_control ctrl expr_evt p _ =
     rml_start_control_v ctrl (expr_evt()) p ()

    let rml_end_control new_ctrl k x =
      new_ctrl.alive <- false;
      k x


(**************************************)
(* when                               *)
(**************************************)
    let step_when ctrl new_ctrl n w evt_lock =
      let rec f_when =
	fun _ ->
	  Mutex.lock evt_lock;
	  if Event.status n
	  then
	    (Mutex.unlock evt_lock;
	     new_ctrl.susp <- false;
	     next_to_current new_ctrl;
	     sched())
	  else
	    if !eoi
	    then
	      (Mutex.unlock evt_lock;
	       Mutex.lock ctrl.lock;
	       ctrl.next <- f_when :: ctrl.next;
	       Mutex.unlock ctrl.lock;
	       sched())
	    else
	      (w := f_when :: !w;
	       Mutex.unlock evt_lock;
	       if ctrl.kind <> Top then
		 (Mutex.lock toWakeUp_lock;
		  toWakeUp := w :: !toWakeUp;
		  Mutex.unlock toWakeUp_lock);
	       sched())
      in f_when

    let rml_start_when_v ctrl evt p _ =
      let (n,wa,wp,evt_lock) = evt in
      let dummy = ref (fun _ -> assert false) in
      let new_ctrl =
	new_ctrl
	  (When dummy)
	  (fun () -> Event.status n)
      in
      let f_when =
	step_when ctrl new_ctrl n (if ctrl.kind = Top then wa else wp) evt_lock
      in
      dummy := f_when;
      new_ctrl.next <- p new_ctrl :: new_ctrl.next;
      Mutex.lock ctrl.lock;
      ctrl.children <- new_ctrl :: ctrl.children;
      Mutex.unlock ctrl.lock;
      f_when ()

    let rml_start_when ctrl expr_evt p _ =
      rml_start_when_v ctrl (expr_evt()) p ()

    let rml_end_when new_ctrl k x =
      new_ctrl.alive <- false;
      k x



(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

     let rml_await expr_evt k ctrl _ =
      rml_await_immediate expr_evt (rml_pause k ctrl) ctrl ()

    let rml_await_v evt k ctrl _ =
      rml_await_immediate_v evt (rml_pause k ctrl) ctrl ()

    let rml_await_all expr_evt p ctrl _ =
      let evt = expr_evt () in
      rml_await_immediate_v evt (step_get evt p ctrl) ctrl ()

    let rml_await_all_v evt p ctrl _ =
      rml_await_immediate_v evt (step_get evt p ctrl) ctrl ()

    let rml_await_one expr_evt p ctrl _ =
      let pause_p x =
	rml_pause (fun () -> p x ()) ctrl
      in
      rml_await_immediate_one expr_evt pause_p ctrl ()

    let rml_await_one_v evt p ctrl _ =
      let pause_p x =
	rml_pause (fun () -> p x ()) ctrl
      in
      rml_await_immediate_one_v evt pause_p ctrl ()

    let rml_await_one_match expr_evt matching p =
      rml_await_all_match expr_evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)

    let rml_await_one_match_v evt matching p =
      rml_await_all_match_v evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)

(* ------------------------------------------------------------------------ *)
    exception End

(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make p =
      let result = ref None in
      (* the main step function *)
      let f = p (fun x ->
        result := Some x;
        sched () (*schedule to see that current is empty*) ) top in
      current := [f];
      (* the react function *)
      let rml_react () =
	  exec_sched ();
	  eoi := true;
          (* -- wakeUp weoi; -- *)
	  current := !weoi;
	  weoi := [];
	  (* -- *)
	  wakeUpAll ();
	  exec_sched ();
	  eval_control_and_next_to_current ();
	  Event.next ();
	  eoi := false;
	  !result
      in
      rml_react

  end
