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
(*                                                                    *)
(**********************************************************************)

module Rml_interpreter: Lco_interpreter.S =
  struct
    exception RML
    exception End
   
    type value = { v : 'a.'a }
    type ('a, 'b) event = 
	{ mutable status: int;
	  mutable value: 'b;
	  mutable pre_status: int;
	  mutable pre_value: 'b;
	  mutable default: 'b;
	  combine: ('a -> 'b -> 'b);
	  wa : step list ref;
	  wp : step list ref; }

    and contol_tree = 
	{ kind: contol_type;
	  mutable active: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable fils: contol_tree list;
	  mutable next: next; }
    and contol_type = 
	Top 
      | Kill of step 
      | Kill_handler of (unit -> step)
      | Susp 
      | When of step ref

    and step = value -> unit
    and next = step list
    and current = step list
    and proc = step -> contol_tree -> step
    and event_cfg = bool -> (unit -> bool) * step list ref list

(* le numero d'instant *)
    let instant = ref 0
	
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
	active = true;
	susp = false;
	fils = [];
	cond = (fun () -> false);
	next = []; }

(* tuer un arbre p *)
    let rec set_kill p =
      p.active <- true;
      p.susp <- false;
      p.next <- [];
      List.iter set_kill p.fils;
      p.fils <- []
	
(* calculer le nouvel etat de l'arbre de control *)
    let eval_control =
      let rec eval pere p =
	if p.active then
	  match p.kind with
	  | Top -> raise RML
	  | Kill f_k -> 
	      if p.cond() 
	      then 
		(pere.next <- f_k :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.fils <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
		      then node :: acc
		      else acc) 
		    [] p.fils;
		 true)
	  | Kill_handler handler -> 
	      if p.cond() 
	      then 
		(pere.next <- (handler()) :: pere.next;
		 set_kill p;
		 false)
	      else
		(p.fils <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
		      then node :: acc
		      else acc) 
		    [] p.fils;
		 true)
	  | Susp -> (
	      let pre_susp = p.susp in
	      if p.cond() then p.susp <- not pre_susp;
	      if pre_susp
	      then true
	      else 
		(p.fils <-
		  List.fold_left 
		    (fun acc node -> 
		      if eval p node 
		      then node :: acc
		      else acc) 
		    [] p.fils;
		 true))
	  | When f_when ->
	      if p.susp 
	      then true
	      else
		(p.susp <- true;
		 pere.next <- !f_when :: pere.next;
		 p.fils <-
		   List.fold_left 
		     (fun acc node -> 
		       if eval p node 
		       then node :: acc
		       else acc) 
		     [] p.fils;
		 true)
	else 
	  (set_kill p;
	   false)
      in
      fun () ->
	top.fils <-
	  (List.fold_left 
	     (fun acc node -> 
	       if eval top node 
	       then node :: acc
	       else acc) 
	     [] top.fils)

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current p =
      if p.active
      then
	if p.susp 
	then ()
	else
	  (current := List.rev_append p.next !current;
	   p.next <- [];
	   List.iter next_to_current p.fils)
      else ()

(* debloquer les processus en attent d'un evt *)
    let wakeUp evt =
      current := List.rev_append !(evt.wa) !current;
      current := List.rev_append !(evt.wp) !current;
      evt.wa := [];
      evt.wp := []


(* le code pour absent *)
    let absent = -2

(* creation d'evenements *)
    let new_evt() =
      { status = absent; 
	value = [];
	pre_status = absent; 
	pre_value = [];
	default = [];
	combine = (fun x y -> x :: y);
	wa = ref [];
	wp = ref []; } 

    let new_evt_combine default combine =
      { status = absent; 
	value = default;
	pre_status = absent; 
	pre_value = default;
	default = default;
	combine = combine;
	wa = ref [];
	wp = ref []; } 

    let eoi = new_evt_combine () (fun () () -> ())

    let unit_value = { v = Obj.magic() }

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
    let rml_pre_status expr_evt =
      let evt = expr_evt () in 
      if (evt.status = !instant)
      then
	evt.pre_status = (!instant-1)
      else
	(evt.status = !instant-1)
	  
    let rml_pre_value expr_evt = 
      let evt = expr_evt () in 
      if (evt.status = !instant)
      then
	evt.pre_value
      else
	evt.value
 
(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine =  new_evt_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' evt =
      fun is_long_wait ->
	(fun () -> evt.status = !instant),
	[ if is_long_wait then evt.wa else evt.wp ]

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


(* !!!!!!!!!!!!!!!!!!!!!!!! NE MARCHE PAS  !!!!!!!!!!!!!!!!!!!!!!!! *)
(* Fonction enregistrement de cfg dans les listes d'attentes. 
   La fonction f_wake_up est ajoutee a Current quand la config 
   est satisfaite. *)
(*
    let cfg_record is_true w_list f_wake_up  =
      let ref_cfg = ref None in
      let step_wake_up w =
	(fun _ -> 
	  match !ref_cfg with
	  | None -> ()
	  | Some f -> f w)
      in
      let wake_up w =
	if is_true() then
	  (ref_cfg := None;
	   current := f_wake_up :: !current)
	else
	  w := (step_wake_up w) :: !w
      in 
      ref_cfg := Some wake_up;
      List.iter
	(fun w -> w := (step_wake_up w) :: !w)
	w_list
*)


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
	    f_k { v = Obj.magic v }
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
    let step_emit f_k ctrl evt e _ =
      if evt.status <> !instant 
      then 
	(evt.pre_status <- evt.status;
	 evt.pre_value <- evt.value;
	 evt.status <- !instant;
	 evt.value <- evt.combine (e()) evt.default;
	 wakeUp evt;
	 f_k unit_value)
      else
	(evt.value <- evt.combine (e()) evt.value;
    	 f_k unit_value)
 
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
(*
    let rml_emit evt =
      fun f_k ctrl ->
	let f_emit =
	  fun () ->
	    evt.status <- !instant;
	    wakeUp evt;
	    f_k ()
	in f_emit 
*)
    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val expr_evt e =
      let evt = expr_evt () in
      if evt.status <> !instant 
      then 
	(evt.pre_status <- evt.status;
	 evt.pre_value <- evt.value;
	 evt.status <- !instant;
	 evt.value <- evt.combine (e()) evt.default;
	 wakeUp evt)
      else
	evt.value <- evt.combine (e()) evt.value
      
    let rml_expr_emit expr_evt =
      rml_expr_emit_val expr_evt (fun() -> ())

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate f_k ctrl evt =
      let w = if ctrl.kind = Top then evt.wa else evt.wp in
      if ctrl.kind = Top then
	let rec f_await_top =
	  fun _ ->
	    if evt.status = !instant
	    then
	      f_k unit_value
	    else
	      (w := f_await_top :: !w;
	       sched ())
	in f_await_top
      else
	let rec f_await_not_top =
	  fun _ -> 
	    if evt.status = !instant
	    then
	      f_k unit_value
	    else
	      if eoi.status = !instant
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
	fun _ -> raise RML


(**************************************)
(* get                                *)
(**************************************)
    let step_get f_k ctrl evt p =
      let rec f_get =
	fun _ ->
	  if eoi.status = !instant 
	  then 
	    let x =
	      if evt.status = !instant
	      then evt.value
	      else evt.default
	    in
	    let f_body = p x f_k ctrl in
	    ctrl.next <- f_body :: ctrl.next;
	    sched()
	  else 
	    (eoi.wa := f_get :: !(eoi.wa);
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
    let step_await_immediate_one f_k ctrl evt p =
      let w = if ctrl.kind = Top then evt.wa else evt.wp in
      let f_await_one =
	if ctrl.kind = Top then 
	  let rec f_await_one_top =
	    fun _ ->
	      if evt.status = !instant 
	      then 
		let x = List.hd evt.value in
		p x f_k ctrl unit_value
	      else
		(w := f_await_one_top :: !w;
		 sched ())
	  in f_await_one_top
	else
	  let rec f_await_one_not_top =
	    fun _ ->
	      if evt.status = !instant 
	      then 
		let x = List.hd evt.value in
		p x f_k ctrl unit_value
	      else
		if eoi.status = !instant 
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
(* present                            *)
(**************************************)
    
    let step_present f_k ctrl evt f_1 f_2 =
      let rec f_present =
	fun _ ->
	  if evt.status = !instant
	  then 
	    f_1 unit_value
	  else
	    if eoi.status = !instant 
	    then 
	      (ctrl.next <- f_2 :: ctrl.next;
	       sched ())
	    else 
	      (evt.wp := f_present :: !(evt.wp);
	       toWakeUp := evt.wp :: !toWakeUp;
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
    let rml_present_conf expr_cfg =
      fun f_k ctrl ->
	fun _ -> raise RML

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k ctrl ->
	let f_2 = p_2 f_k ctrl in
	let f_1 = p_1 f_2 ctrl in
	f_1

(**************************************)
(* par                                *)
(**************************************)

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
	let f_1 = p_1 j ctrl in
	let f_2 = p_2 j ctrl in
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
let loop p =
  fun f_k ->
    let rec f_1 = lazy (p f) 
    and f =
      fun () ->
	Lazy.force f_1 ()
    in
    f
*)

    let rml_loop p =
      fun f_k ctrl ->
	let f_1 = ref (Obj.magic()) in
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
	    (fun x ->
	      let f = p2 x.v f_k ctrl in
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
	  let value_array = Array.make n ((Obj.magic()): value) in
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

    let step_until f_k ctrl evt f new_ctrl =
      new_ctrl.cond <- (fun () -> evt.status = !instant);
      let f_until =
	fun _ ->
	  if new_ctrl.active
	  then 
	    (ctrl.fils <- new_ctrl :: ctrl.fils)
	  else
	    (new_ctrl.active <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f unit_value
      in f_until
	
    let rml_until expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Kill f_k;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_until =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_until new_ctrl in
	let f_until =
	  fun _ ->
	    let evt = expr_evt () in
	    step_until f_k ctrl evt f new_ctrl unit_value
	in f_until

    let rml_until' evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Kill f_k;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_until =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_until new_ctrl in
	step_until f_k ctrl evt f new_ctrl


(**************************************)
(* until_conf                         *)
(**************************************)

    let rml_until_conf expr_cfg =
      fun f_k ctrl ->
	fun _ -> raise RML

(**************************************)
(* until handler                      *)
(**************************************)

    let step_until_handler f_k ctrl evt f new_ctrl =
      new_ctrl.cond <- (fun () -> evt.status = !instant);
      let f_until =
	fun _ ->
	  if new_ctrl.active
	  then 
	    (ctrl.fils <- new_ctrl :: ctrl.fils)
	  else
	    (new_ctrl.active <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f unit_value
      in f_until
	
    let rml_until_handler expr_evt p p_handler =
      fun f_k ctrl ->
	let evt = ref (Obj.magic()) in
	let handler =
	  fun () ->
	    let x =
	      if !evt.status = !instant
	      then !evt.value
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = 
	  { kind = Kill_handler handler;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_until =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_until new_ctrl in
	let f_until =
	  fun _ ->
	    evt := expr_evt ();
	    step_until f_k ctrl !evt f new_ctrl unit_value
	in f_until

    let rml_until_handler' evt p p_handler =
      fun f_k ctrl ->
	let handler =
	  fun () ->
	    let x =
	      if evt.status = !instant
	      then evt.value
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = 
	  { kind = Kill_handler handler;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_until =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_until new_ctrl in
	step_until f_k ctrl evt f new_ctrl

(**************************************)
(* control                            *)
(**************************************)
    let step_control f_k ctrl evt f new_ctrl =
      new_ctrl.cond <- (fun () -> evt.status = !instant);
      let f_control =
	fun _ ->
	  if new_ctrl.active
	  then 
	    (ctrl.fils <- new_ctrl :: ctrl.fils)
	  else
	    (new_ctrl.active <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f unit_value
      in f_control
	
    let rml_control expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Susp;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_control =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_control new_ctrl in
	let f_control =
	  fun _ ->
	    let evt = expr_evt () in
	    step_control f_k ctrl evt f new_ctrl ()
	in f_control
	  
    let rml_control' evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Susp;
	    active = true;
	    susp = false;
	    fils = [];
	    cond = (fun () -> false);
	    next = [] }
	in
	let end_control =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_control new_ctrl in
	let f_control =
	  fun _ ->
	    step_control f_k ctrl evt f new_ctrl ()
	in f_control

(**************************************)
(* control                            *)
(**************************************)

    let rml_control_conf expr_cfg =
      fun f_k ctrl ->
	fun _ -> raise RML

(**************************************)
(* when                               *)
(**************************************)

    let step_when f_k ctrl evt f new_ctrl dummy =
      let w = if ctrl.kind = Top then evt.wa else evt.wp in
      new_ctrl.cond <- (fun () -> evt.status = !instant);
      let rec f_when =
	fun _ ->
	  if evt.status = !instant
	  then
	    (new_ctrl.susp <- false;
	     next_to_current new_ctrl;
	     sched())
	  else 
	    if eoi.status = !instant
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
	  if new_ctrl.active
	  then 
	    (ctrl.fils <- new_ctrl :: ctrl.fils)
	  else
	    (new_ctrl.active <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  if evt.status = !instant
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
	let dummy = ref (Obj.magic()) in
	let new_ctrl = 
	  {kind = When dummy;
	   active = true;
	   susp = false;
	   fils = [];
	   cond = (fun () -> false);
	   next = []}
	in
	let end_when =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_when new_ctrl in
	let start_when =
	  fun _ ->
	    let evt = expr_evt () in
	    step_when f_k ctrl evt f new_ctrl dummy unit_value
	in
	start_when

    let rml_when' evt p =
      fun f_k ctrl ->
	let dummy = ref (Obj.magic()) in
	let new_ctrl = 
	  {kind = When dummy;
	   active = true;
	   susp = false;
	   fils = [];
	   cond = (fun () -> false);
	   next = []}
	in
	let end_when =
	  fun x ->
	    set_kill new_ctrl;
	    new_ctrl.active <- false;
	    f_k x
	in
	let f = p end_when new_ctrl in
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
	let f_body = ref (Obj.magic()) in
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
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
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
	fun _ ->
	  raise RML

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
(**************************************************)
(* rml_react                                      *)
(**************************************************)
    let rml_react () =
      incr instant;
      sched ();
      eoi.status <- !instant;
      wakeUp eoi;
      wakeUpAll ();
      sched ();
      eval_control();
      next_to_current top


(**************************************************)
(* rml_exec                                       *)
(**************************************************)

    let rml_exec p =
      let f = p () (fun x -> raise End) top in
      let rec exec () =
	rml_react();
	exec ()
      in
      current := [f];
      try 
	exec ()
      with
      | End -> ()


(**************************************************)
(* rml_exec_n                                     *)
(**************************************************)
    let rml_exec_n p nb =
      let f = p () (fun x -> raise End) top in
      let rec exec () =
	if !instant < nb then
	  begin
(*
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
*)
	    rml_react ();
	    exec ()
	  end
	else
	  ()
      in
      current := [f];
      try
	exec ()
      with
      | End -> ()

(**************************************************)
(* rml_exec_sampling                              *)
(**************************************************)
    let rml_exec_sampling p min =
      let _ = 
	Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ()))
      in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in

      let f = p () (fun x -> raise End) top in
      let rec exec () =
(* -------------------------------------------------------------- *)
	debut := Sys.time();
(* -------------------------------------------------------------- *)
	rml_react ();
(* -------------------------------------------------------------- *)
	fin := Sys.time();
	diff := min -. (!fin -. !debut);
	if !diff > 0.001 then (
(*
	  ignore (Unix.select [] [] [] !diff)
*)
	  ignore (Unix.setitimer 
		    Unix.ITIMER_REAL 
		    {Unix.it_interval = 0.0; Unix.it_value = !diff});
	  Unix.pause()
	 )  
	else ();
(* -------------------------------------------------------------- *)
	exec ()
      in
      current := [f];
      try
	exec ()
      with
      | End -> ()

(**************************************************)
(* rml_exec_n_sampling                            *)
(**************************************************)
    let rml_exec_n_sampling p nb min =
      let _ = 
	Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ()))
      in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in

      let f = p () (fun x -> raise End) top in
      let rec exec () =
	if !instant < nb then
	  begin
(* -------------------------------------------------------------- *)
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
	    debut := Sys.time();
(* -------------------------------------------------------------- *)
	    rml_react ();
(* -------------------------------------------------------------- *)
	    fin := Sys.time();
	    diff := min -. (!fin -. !debut);
	    if !diff > 0.001 then (
(*
	      ignore (Unix.select [] [] [] !diff)
*)
	      ignore (Unix.setitimer 
			Unix.ITIMER_REAL 
			{Unix.it_interval = 0.0; Unix.it_value = !diff});
	      Unix.pause()
	     )  
	    else (
	      print_string "Instant ";
	      print_int !instant;
	      print_string " : depassement = ";
	      print_float (-. !diff);
	      print_newline());
(* -------------------------------------------------------------- *)
	    exec ()
	  end
	else
	  ()
      in
      current := [f];
      try
	exec ()
      with
      | End -> ()

  end (* Module Rml_interpreter *)
