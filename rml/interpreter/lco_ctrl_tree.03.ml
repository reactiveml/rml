(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 04/06/2004                                      *)
(* Fichier : lco_ctrl_tree                                            *)
(* Remarque : taken from                                              *)
(*            interpreter_without_scope_extrusion_control.ml          *)
(* Description :                                                      *)
(*   On a une liste next associee a chaque noeud de l'arbre de        *)
(*   control.                                                         *)
(*   On utilise un entier pour coder le status d'un signal pour ne    *)
(*   pas avoir a faire de reste a la fin d'instant.                   *) 
(*   Marche avec Scope Extrusion                                      *)
(*   Ajout des valeurs pour traiter def_dyn et def_and_dyn            *)
(*   Ajout de until_match et await_match                              *)
(*   Ajout des configurations evenementielles                         *)
(*   Parametrisation par le foncteur "Event"                          *)
(*   Suppression du type "value" et des "Obj.magic"                   *)
(*   Suppression de exec                                              *)
(*                                                                    *)
(**********************************************************************)

module Rml_interpreter (*: Lco_interpreter.S*) =
  functor (Event: Sig_env.S) ->
  struct
   
    exception RML

    type ('a, 'b) event = 
	('a,'b) Event.t * unit step list ref * unit step list ref  
    and event_cfg = bool -> (unit -> bool) * unit step list ref list

    and contol_tree = 
	{ kind: contol_type;
	  mutable alive: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable children: contol_tree list;
	  mutable next: next; }
    and contol_type = 
	Top 
      | Kill of unit step 
      | Kill_handler of (unit -> unit step)
      | Susp 
      | When of unit step ref

    and 'a step = 'a -> unit
    and next = unit step list
    and current = unit step list
    and 'a expr = 'a step -> contol_tree -> unit step
    and 'a process = unit -> 'a expr


	
(* liste des processus a executer dans l'instant *)
    let current = ref []
    
(* liste des listes de processus a revillier a la fin d'instant *)
    let toWakeUp = ref []
    let wakeUpAll () = 
      List.iter
	(fun wp ->
	  current := List.rev_append !wp !current;
	  wp := [])
	!toWakeUp;
      toWakeUp := []
	  
(* racine de l'arbre de control *)
    let top =
      { kind = Top;
	alive = true;
	susp = false;
	children = [];
	cond = (fun () -> false);
	next = []; }

(* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      p.next <- [];
      List.iter set_kill p.children;
      p.children <- []
	
(* calculer le nouvel etat de l'arbre de control *)
    let eval_control =
      let rec eval pere p =
	if p.alive then
	  match p.kind with
	  | Top -> raise RML
	  | Kill f_k -> 
	      if p.cond() 
	      then 
		(pere.next <- f_k :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.children <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
		      then node :: acc
		      else acc) 
		    [] p.children;
		 true)
	  | Kill_handler handler -> 
	      if p.cond() 
	      then 
		(pere.next <- (handler()) :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.children <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
		      then node :: acc
		      else acc) 
		    [] p.children;
		 true)
	  | Susp -> (
	      let pre_susp = p.susp in
	      if p.cond() then p.susp <- not pre_susp;
	      if pre_susp
	      then true
	      else 
		(p.children <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
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
		       if eval p node 
		       then node :: acc
		       else acc) 
		     [] p.children;
		 true)
	else 
	  (set_kill p;
	   false)
      in
      fun () ->
	top.children <-
	  (List.fold_left 
	     (fun acc node -> 
	       if eval top node 
	       then node :: acc
	       else acc) 
	     [] top.children)

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current p =
      if p.alive
      then
	if p.susp 
	then ()
	else
	  (current := List.rev_append p.next !current;
	   p.next <- [];
	   List.iter next_to_current p.children)
      else ()

(* debloquer les processus en attent d'un evt *)
    let wakeUp w =
      current := List.rev_append !w !current;
      w := []


(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create default combine, ref [], ref [])

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)

    let eoi = ref false
    let weoi = ref []

    let unit_value = () 
    let dummy_step _ = ()


(**************************************************)
(* sched                                          *)
(**************************************************)
    let rec sched =
      fun () ->
	match !current with
	| f :: c -> 
	    current := c;
	    f unit_value
	| [] -> ()

(* ------------------------------------------------------------------------ *)
    let rml_pre_status (n, _, _) = Event.pre_status n 
	  
    let rml_pre_value (n, _, _) = Event.pre_value n
 
(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine =  new_evt_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' (n,wa,wp) =
      fun is_long_wait ->
	(fun () -> Event.status n),
	[ if is_long_wait then wa else wp ]

    let cfg_present evt_expr =
      fun is_long_wait ->
	let evt = evt_expr() in
	cfg_present' evt is_long_wait

    let cfg_and c1 c2 =
      fun is_long_wait ->
	let is_true1, evt_list1 = c1 is_long_wait in
	let is_true2, evt_list2 = c2 is_long_wait in
	(fun () -> is_true1() && is_true2()),
	List.rev_append evt_list1 evt_list2

    let cfg_or c1 c2 =
      fun is_long_wait ->
	let is_true1, evt_list1 = c1 is_long_wait in
	let is_true2, evt_list2 = c2 is_long_wait in
	(fun () -> is_true1() or is_true2()),
	List.rev_append evt_list1 evt_list2



(* ------------------------------------------------------------------------ *)

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k ctrl ->
	let f_nothing =
	  fun _ ->
	    f_k unit_value
	in f_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k ctrl ->
	let f_compute =
	  fun _ ->
	    let v = e() in
	    f_k v
	in f_compute
      
(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun f_k ctrl ->
	let f_pause =
	  fun _ ->
	    ctrl.next <- f_k :: ctrl.next;
	    sched ()
	in f_pause

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun f_k ctrl ->
	let f_halt =
	  fun _ ->
	    sched ()
	in f_halt

(**************************************)
(* emit                               *)
(**************************************)
    let step_emit f_k ctrl (n,wa,wp) e _ =
      Event.emit n (e());
      wakeUp wa;
      wakeUp wp;
      f_k unit_value
 
    let rml_emit_val expr_evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  fun _ ->
	    let evt = expr_evt() in
	    step_emit f_k ctrl evt e unit_value
	in f_emit_val
	  
    let rml_emit_val' evt e =
      fun f_k ctrl ->
	let f_emit_val = 
	  step_emit f_k ctrl evt e
	in f_emit_val

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val (n,wa,wp) v =
      Event.emit n v;
      wakeUp wa;
      wakeUp wp

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate f_k ctrl (n,wa,wp) =
      let w = if ctrl.kind = Top then wa else wp in
      if ctrl.kind = Top then
	let rec f_await_top =
	  fun _ ->
	    if Event.status n
	    then
	      f_k unit_value
	    else
	      (w := f_await_top :: !w;
	       sched ())
	in f_await_top
      else
	let rec f_await_not_top =
	  fun _ -> 
	    if Event.status n
	    then
	      f_k unit_value
	    else
	      if !eoi
	      then
		(ctrl.next <- f_await_not_top :: ctrl.next;
		 sched())
	      else
		(w := f_await_not_top :: !w;
		 toWakeUp := w :: !toWakeUp;
		 sched())
	in f_await_not_top

    let rml_await_immediate expr_evt =
      fun f_k ctrl ->
	let f_await =
	  fun _ ->
	    let evt = expr_evt() in
	    step_await_immediate f_k ctrl evt unit_value
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
	    fun _ ->
	      let is_true, w_list = expr_cfg true in
	      if is_true() then 
		f_k unit_value
	      else
		let ref_f = ref None in
		let f w step_wake_up = 
		  if is_true() then
		    (ref_f := None;
		     f_k unit_value)
		  else
		    (w := step_wake_up :: !w;
		     sched ())
		in
		let gen_step w =
		  let rec step_wake_up _ =
		    match !ref_f with
		    | None -> sched ()
		    | Some f -> f w step_wake_up
		  in step_wake_up
		in
		ref_f := Some f;
		List.iter 
		  (fun w -> w := (gen_step w) :: !w)
		  w_list;
		sched()
	  in f_await_top
	else
	  let f_await_not_top = 
	    fun _ ->
	      let is_true, w_list = expr_cfg false in
	      if is_true() then 
		f_k unit_value
	      else
		let ref_f = ref None in
		let rec f w step_wake_up = 
		  if is_true() then
		    (ref_f := None;
		     f_k unit_value)
		  else
		    if !eoi
		    then
		      (ctrl.next <- step_wake_up :: ctrl.next;
		       sched())
		    else
		      (w := step_wake_up :: !w;
		       toWakeUp := w :: !toWakeUp;
		       sched ())
		in
		let gen_step w =
		  let rec step_wake_up _ =
		    match !ref_f with
		    | None -> sched ()
		    | Some f -> f w step_wake_up
		  in step_wake_up
		in
		ref_f := Some f;
		List.iter 
		  (fun w ->
		    w := (gen_step w) :: !w;
		    toWakeUp := w :: !toWakeUp)
		  w_list;
		sched()
	  in f_await_not_top

(**************************************)
(* get                                *)
(**************************************)
    let step_get f_k ctrl (n,_,_) p =
      let rec f_get =
	fun _ ->
	  if !eoi 
	  then 
	    let x =
	      if Event.status n
	      then Event.value n
	      else Event.default n
	    in
	    let f_body = p x f_k ctrl in
	    ctrl.next <- f_body :: ctrl.next;
	    sched()
	  else 
	    (weoi := f_get :: !weoi;
	     sched ())
      in f_get

    let rml_get expr_evt p =
      fun f_k ctrl ->
	let f_get =
	  fun _ ->
	    let evt = expr_evt() in
	    step_get f_k ctrl evt p unit_value
	in f_get

    let rml_get' evt p =
      fun f_k ctrl ->
	let f_get =
	    step_get f_k ctrl evt p
	in f_get




(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one f_k ctrl (n,wa,wp) p =
      let w = if ctrl.kind = Top then wa else wp in
      let f_await_one =
	if ctrl.kind = Top then 
	  let rec f_await_one_top =
	    fun _ ->
	      if Event.status n 
	      then 
		let x = Event.one n in
		p x f_k ctrl unit_value
	      else
		(w := f_await_one_top :: !w;
		 sched ())
	  in f_await_one_top
	else
	  let rec f_await_one_not_top =
	    fun _ ->
	      if Event.status n
	      then 
		let x = Event.one n in
		p x f_k ctrl unit_value
	      else
		if !eoi 
		then
		  (ctrl.next <- f_await_one_not_top :: ctrl.next;
		   sched())
		else
		  (w := f_await_one_not_top :: !w;
		   toWakeUp := w :: !toWakeUp;
		   sched())
	  in f_await_one_not_top
      in f_await_one
    
     let rml_await_immediate_one expr_evt p =
      fun f_k ctrl ->
      let f_await_one =
	fun _ ->
	  let evt = expr_evt() in
	  step_await_immediate_one f_k ctrl evt p unit_value
      in f_await_one
	
    let rml_await_immediate_one' evt p =
      fun f_k ctrl ->
 	step_await_immediate_one f_k ctrl evt p 
	 

(**************************************)
(* await_all_match                    *)
(**************************************)
    let step_await_all_match f_k ctrl (n,wa,wp) matching p =
      let w = if ctrl.kind = Top then wa else wp in
      let f_await_all_match =
	if ctrl.kind = Top then 
	  let rec f_await_top =
	    fun _ ->
	      let v = Event.value n in
	      if !eoi
	      then
		if Event.status n & matching v
		then
		  let x = v in
		  let f_body = p x f_k ctrl in
		  ctrl.next <- f_body :: ctrl.next;
		  sched()
		else
		  (w := f_await_top :: !w;
		   sched ())
	      else
		if Event.status n
		then
		  (weoi := f_await_top :: !weoi;
		   sched ())
		else
		  (w := f_await_top :: !w;
		   sched ())
	  in f_await_top
	else
	  let rec f_await_not_top =
	    fun _ ->
	      if !eoi
	      then
		let v = Event.value n in
		if Event.status n & matching v
		then
		  let x = v in
		  let f_body = p x f_k ctrl in
		  ctrl.next <- f_body :: ctrl.next;
		  sched()
		else
		    (ctrl.next <- f_await_not_top :: ctrl.next;
		     sched ())
	      else
		(w := f_await_not_top :: !w;
		 toWakeUp := w :: !toWakeUp;
		 sched ())
	  in f_await_not_top
      in f_await_all_match
    

    let rml_await_all_match expr_evt matching p = 
      fun f_k ctrl ->
	let f_await_all_match =
	  fun _ ->
	    let evt = expr_evt() in
	    step_await_all_match f_k ctrl evt matching p unit_value
	in f_await_all_match

    let rml_await_all_match' evt matching p =
      fun f_k ctrl ->
 	step_await_all_match f_k ctrl evt matching p 

(**************************************)
(* present                            *)
(**************************************)
    
    let step_present f_k ctrl (n,_,wp) f_1 f_2 =
      let rec f_present =
	fun _ ->
	  if Event.status n
	  then 
	    f_1 unit_value
	  else
	    if !eoi
	    then 
	      (ctrl.next <- f_2 :: ctrl.next;
	       sched ())
	    else 
	      (wp := f_present :: !wp;
	       toWakeUp := wp :: !toWakeUp;
	       sched ())
      in f_present

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let rec f_present =
	  fun _ ->
	    let evt = expr_evt () in
	    step_present f_k ctrl evt f_1 f_2 unit_value
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
	fun _ -> 
	  let f_1 = p_1 f_k ctrl in
	  let f_2 = p_2 f_k ctrl in
	  let is_true, w_list = expr_cfg false in
	  if is_true ()
	  then 
	    f_1 unit_value
	  else
	    let ref_f = ref None in
	    let f w step_wake_up = 
	      if is_true() then
		(ref_f := None;
		 f_1 unit_value)
	      else
		if !eoi
		then
		  (ctrl.next <- f_2 :: ctrl.next;
		   sched())
		else
		  (w := step_wake_up :: !w;
		   toWakeUp := w :: !toWakeUp;
		   sched ())
	    in
	    let gen_step w =
	      let rec step_wake_up _ =
	      match !ref_f with
	      | None -> sched ()
	      | Some f -> f w step_wake_up
	      in step_wake_up
	    in
	    ref_f := Some f;
	    List.iter 
	      (fun w ->
		w := (gen_step w) :: !w;
		toWakeUp := w :: !toWakeUp)
	      w_list;
	    sched()

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k ctrl ->
	let f_2 = p_2 f_k ctrl in
	let f_1 = p_1 (fun x -> f_2 ()) ctrl in
	f_1

(**************************************)
(* par                                *)
(**************************************)
(* Utilisation de Obj.magic pour le pb de la generalisation des *)
(* applications partielles.                                     *)

    let join cpt =
      fun f_k ctrl ->
	let f_join =
	  fun _ ->
	    incr cpt;
	    if !cpt = 2 
	    then (
	      (* cpt := 0; *)
	      f_k unit_value
	     )
	    else
	      sched ()
	in f_join
      
    let rml_par p_1 p_2 =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let j = join cpt f_k ctrl in
	let f_1 = p_1 (Obj.magic j: 'a step) ctrl in 
	let f_2 = p_2 (Obj.magic j: 'b step) ctrl in
	let f_par =
	  fun _ -> 
	    cpt := 0;
	    current := f_2 :: !current;
	    f_1 unit_value
	in f_par

(**************************************)
(* merge                              *)
(**************************************)

    let rml_merge p_1 p_2 =
      fun f_k ctrl ->
	fun _ -> raise RML


(**************************************)
(* loop                               *)
(**************************************)
(*
let rec rml_loop p f_k ctrl _ =
  p (rml_loop p f_k ctrl) ctrl unit_value
*)

(*
let rml_loop p =
  fun f_k ctrl ->
    let rec f_1 = lazy (p f ctrl) 
    and f =
      fun _ ->
	Lazy.force f_1 unit_value
    in
    f
*)

    let rml_loop p =
      fun f_k ctrl ->
	let f_1 = ref dummy_step in
	let f_loop = p (fun _ -> !f_1 unit_value) ctrl in
	f_1 := f_loop;
	f_loop


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p = 
      fun f_k ctrl ->
	let f_signal =
	  fun _ ->
	    let evt = new_evt() in
	    let f = p evt f_k ctrl in
	    f unit_value
	in f_signal

    let rml_signal_combine default comb p = 
      fun f_k ctrl ->
	let f_signal =
	  fun _ ->
	    let evt = new_evt_combine (default()) (comb()) in
	    let f = p evt f_k ctrl in
	    f unit_value
	in f_signal

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k ctrl ->
	let f_def =
	  fun _ ->
	    let f = p (e()) f_k ctrl in
	    f unit_value
	in f_def

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k ctrl ->
	let f_def =
	  p1 
	    (fun v ->
	      let f = p2 v f_k ctrl in
	      f unit_value)
	    ctrl
	in f_def

(**************************************)
(* def_and_dyn                        *)
(**************************************)

    let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
	fun f_k ctrl ->
	  fun x ->
	    value_array.(i) <- x;
	    decr cpt;
	    if !cpt = 0 then
	      let f = p3 value_array f_k ctrl in
	      f unit_value
	    else
	      sched()
      in
      fun p_array p3 ->
	fun f_k ctrl ->
	  let n = Array.length p_array in
	  let cpt = ref n in 
	  let value_array = Array.make n (Obj.magic()) in
	  let step_init = 
	    fun _ ->
	      cpt := n;
	      for i = 0 to n - 1 do
		let f = 
		  p_array.(i) (join_n cpt value_array p3 i f_k ctrl) ctrl
		in
		current := f :: !current
	      done;
	      sched()
	  in step_init

(**************************************)
(* match                              *)
(**************************************)

    let rml_match e p =
      fun f_k ctrl ->
	let f_match =
	  fun _ ->
	    let f = p (e()) f_k ctrl in
	    f unit_value
	in f_match


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k ctrl ->
	let f_run =
	  fun _ ->
	    let f_1 = (e ()) () f_k ctrl in
	    f_1 unit_value
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
	cond = (fun () -> false);
	next = [] }

    let start_ctrl f_k ctrl f new_ctrl =
      let f_ctrl =
	fun _ ->
	  if new_ctrl.alive
	  then 
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f unit_value
      in f_ctrl

    let end_ctrl f_k new_ctrl =
      fun x ->
	set_kill new_ctrl;
	new_ctrl.alive <- false;
	f_k x
(* ---------------------------------------------------------------- *)

    let rml_until expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let (n,_,_) = expr_evt () in
	    new_ctrl.cond <- (fun () -> Event.status n);
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until

    let rml_until' (n,_,_) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	new_ctrl.cond <- (fun () -> Event.status n);
	start_ctrl f_k ctrl f new_ctrl


(**************************************)
(* until_conf                         *)
(**************************************)

    let rml_until_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let cond, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until



(**************************************)
(* until handler                      *)
(**************************************)
	
    let rml_until_handler_local expr_evt matching_opt p p_handler =
      fun f_k ctrl ->
	let evt = ref (Obj.magic()) in
	let handler =
	  fun () ->
	    let x =
	      let n, _, _ = !evt in
	      if Event.status n
	      then Event.value n
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let (n, _, _) as e = expr_evt () in
	    evt := e;
	    begin match matching_opt with
	    | None ->
		new_ctrl.cond <- (fun () -> Event.status n);
	    | Some matching ->
		new_ctrl.cond <- 
		  (fun () -> Event.status n & matching (Event.value n));
	    end;
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until

    let rml_until_handler_local' (n,_,_) matching_opt p p_handler =
      fun f_k ctrl ->
	let handler =
	  fun () ->
	    let x =
	      if Event.status n
	      then Event.value n
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	begin match matching_opt with
	| None ->
	    new_ctrl.cond <- (fun () -> Event.status n);
	| Some matching ->
	    new_ctrl.cond <- 
	      (fun () -> Event.status n & matching (Event.value n));
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
	  fun _ ->
	    let (n, _, _) = expr_evt () in
	    new_ctrl.cond <- (fun () -> Event.status n);
	    start_ctrl f_k ctrl f new_ctrl ()
	in f_control
	  
    let rml_control' (n, _, _) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	new_ctrl.cond <- (fun () -> Event.status n);
	start_ctrl f_k ctrl f new_ctrl 

(**************************************)
(* control_conf                       *)
(**************************************)

    let rml_control_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun _ ->
	    let cond, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl ()
	in f_control


(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl (n,wa,wp) f new_ctrl dummy =
      let w = if ctrl.kind = Top then wa else wp in
      new_ctrl.cond <- (fun () -> Event.status n);
      let rec f_when =
	fun _ ->
	  if Event.status n
	  then
	    (new_ctrl.susp <- false;
	     next_to_current new_ctrl;
	     sched())
	  else 
	    if !eoi
	    then
	      (ctrl.next <- f_when :: ctrl.next;
	       sched())
	    else
	      (w := f_when :: !w;
	       if ctrl.kind <> Top then toWakeUp := w :: !toWakeUp;
	       sched())
      in
      let start_when =
	fun _ ->
	  if new_ctrl.alive
	  then 
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  if Event.status n
	  then 
	    (new_ctrl.susp <- false;
	     new_ctrl.next <- [];
	     f unit_value)
	  else 
	    (new_ctrl.susp <- true;
	     new_ctrl.next <- [f];
	     w := f_when :: !w;
	     if ctrl.kind <> Top then toWakeUp := w :: !toWakeUp;
	     sched())
      in
      dummy := f_when;
      start_when

      let rml_when expr_evt p =
      fun f_k ctrl ->
	let dummy = ref dummy_step in
	let new_ctrl = new_ctrl (When dummy) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let start_when =
	  fun _ ->
	    let evt = expr_evt () in
	    step_when f_k ctrl evt f new_ctrl dummy unit_value
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
	fun _ -> raise RML


(**************************************)
(* if                                 *)
(**************************************)
	    
    let rml_if e p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let f_if =
	  fun _ ->
	    if e() then
	      f_1 unit_value
	    else
	      f_2 unit_value
	in f_if


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k ctrl ->
	let f_body = ref dummy_step in
	let f_while =
	  fun _ ->
	    if e()
	    then !f_body unit_value
	    else f_k unit_value
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
	  fun _ ->
	    incr i;
	    if cmp !i v2 
	    then p !i (f_for i v2) ctrl unit_value
	    else f_k unit_value
	in
	let f_for_init =
	  fun _ ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl unit_value
	    else f_k unit_value
	in
	f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)
    let join_n cpt =
      fun f_k ctrl ->
	let f_join_n =
	  fun _ ->
	    decr cpt;
	    if !cpt = 0 then
	      f_k unit_value
	    else
	      sched ()
	in f_join_n

    let rml_fordopar e1 e2 dir p =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let j = join_n cpt f_k ctrl in
	let f_fordopar =
	  fun _ ->
	    if dir then
	      begin
		let min = e1() in
		let max = e2() in
		cpt := max - min + 1;
		if !cpt <= 0 then
		  f_k unit_value
		else
		  begin
		    for i = max downto min do
		      let f = p i j ctrl in
		      current := f :: !current
		    done;
		    sched()
		  end
	      end
	    else
	      begin
		let max = e1() in
		let min = e2() in
		cpt := max - min + 1;
		if !cpt <= 0 then
		  f_k unit_value
		else
		  begin
		    for i = min to max do
		      let f = p i j ctrl in
		      current := f :: !current
		    done;
		    sched ()
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
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_pause f_k ctrl) ctrl unit_value

    let rml_await' evt =
      fun f_k ctrl ->
	rml_await_immediate' evt (rml_pause f_k ctrl) ctrl 

    let rml_await_all expr_evt p =
      fun f_k ctrl ->
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_get' evt p f_k ctrl) ctrl unit_value

    let rml_await_all' evt p =
      fun f_k ctrl ->
	rml_await_immediate evt (rml_get evt p f_k ctrl) ctrl

    let rml_await_one expr_evt p =
      let pause_p x =
	fun f_k ctrl ->
	  rml_pause (p x f_k ctrl) ctrl
      in
      fun f_k ctrl ->
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate_one' evt pause_p f_k ctrl unit_value

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
	  fun _ ->
	    cpt := nb;
	    current := List.rev_append f_list !current;
	    sched()
	in f_par_n

    let rml_seq_n p_list =
      fun f_k ctrl ->
	let f = 
	  List.fold_right (fun p -> fun k -> p k ctrl) p_list f_k 
	in f

(* ------------------------------------------------------------------------ *)
    type value
    exception End of value


(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make p = 
      (* the main step function *)
      let f = p () (fun x -> raise (End (Obj.magic x: value))) top in
      current := [f];
      (* the react function *)
      let rml_react () =
	try 
	  sched ();
	  eoi := true;
	  wakeUp weoi;
	  wakeUpAll ();
	  sched ();
	  eval_control();
	  next_to_current top;
	  Event.next ();
	  eoi := false;
	  None
	with 
	| End v -> Some (Obj.magic v)
      in 
      rml_react


(**************************************************)
(* rml_make_unit                                  *)
(**************************************************)

    let rml_make_unit (p: unit process) = 

      (* Function to create the last continuation of a toplevel process *)
      let join_end =
	let term_cpt = ref 0 in
	fun () ->
	  incr term_cpt;
	  let f x =
	    decr term_cpt;
	    if !term_cpt > 0 then
	      sched()
	    else
	      raise (End (Obj.magic (): value))
	  in f
      in

      (* the main step function *)
      let f = p () (join_end()) top in
      current := [f];

      (* the react function *)
      let rml_react () =
	try 
	  sched ();
	  eoi := true;
	  wakeUp weoi;
	  wakeUpAll ();
	  sched ();
	  eval_control();
	  next_to_current top;
	  Event.next ();
	  eoi := false;
	  None
	with 
	| End v -> Some ()
      in 

      (* the add_process function*)
      let add_process p =
	let f =  p () (join_end()) top in
	current := f :: !current
      in

      rml_react, add_process



  end (* Module Rml_interpreter *)
