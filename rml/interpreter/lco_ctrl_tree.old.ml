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
(*                                                                    *)
(**********************************************************************)

module Rml_interpreter: Lco_interpreter.S =
  struct
    exception RML
    exception End
   
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
	  cond: (unit -> bool);
	  mutable fils: contol_tree list;
	  mutable next: next; }
    and contol_type = Top | Kill of step | Susp | When of step ref

    and step = unit -> unit
    and next = step list
    and current = step list
    and proc = step -> contol_tree -> step


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
      p.active <- false;
      p.next <- [];
      List.iter set_kill p.fils
	
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
		 set_kill p)
	      else
		(List.iter (eval p) p.fils)
	  | Susp -> (
	      let pre_susp = p.susp in
	      if p.cond() then p.susp <- not pre_susp;
	      if pre_susp
	      then ()
	      else List.iter (eval p) p.fils)
	  | When f_when ->
	      if p.susp 
	      then ()
	      else
		(p.susp <- true;
		 pere.next <- !f_when :: pere.next;
		 List.iter (eval p) p.fils)
	else ()
      in
      fun () ->
	List.iter (eval top) top.fils

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
    let absent = -1

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

(**************************************************)
(* sched                                          *)
(**************************************************)
    let rec sched =
      fun () ->
	match !current with
	| f :: c -> 
	    current := c;
	    f ()
	| [] -> ()

(* ------------------------------------------------------------------------ *)
let rml_pre_status evt = 
  if (evt.status = !instant)
  then
    evt.pre_status = (!instant-1)
  else
    (evt.status = !instant-1)

let rml_pre_value evt = 
  if (evt.status = !instant)
  then
    evt.pre_value
  else
    if (evt.status = !instant-1)
    then evt.value
    else evt.default 
 
(* ------------------------------------------------------------------------ *)

    let rml_nothing =
      fun f_k ctrl ->
	let f_nothing =
	  fun () ->
	    f_k ()
	in f_nothing

    let rml_compute e =
      fun f_k ctrl ->
	let f_compute =
	  fun () ->
	    e();
	    f_k()
	in f_compute
      
    let rml_pause =
      fun f_k ctrl ->
	let f_pause =
	  fun () ->
	    ctrl.next <- f_k :: ctrl.next;
	    sched ()
	in f_pause

    let rml_emit_val evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  fun () ->
	    if evt.status <> !instant 
	    then 
	      (evt.pre_status <- evt.status;
	       evt.pre_value <- evt.value;
	       evt.status <- !instant;
	       evt.value <- evt.combine (e()) evt.default;
	       wakeUp evt;
	       f_k())
	    else
	      (evt.value <- evt.combine (e()) evt.value;
    	       f_k ())
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
    let rml_emit evt = rml_emit_val evt (fun() -> ())

    let rml_await_immediate evt =
      fun f_k ctrl ->
	let w = if ctrl.kind = Top then evt.wa else evt.wp in
	let f_await =
	  if ctrl.kind = Top then
	    let rec f_await_top =
	      fun () ->
		if evt.status = !instant
		then
		  f_k ()
		else
		  (w := f_await_top :: !w;
		   sched ())
	    in f_await_top
	  else
	    let rec f_await_not_top =
	      fun () -> 
		if evt.status = !instant
		then
		  f_k ()
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
	in f_await

    let rml_values evt p =
      fun f_k ctrl ->
	let w = if ctrl.kind = Top then evt.wa else evt.wp in
	let rec f_values =
	  fun () ->
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
	      (eoi.wa := f_values :: !(eoi.wa);
	       sched ())
	in f_values

    let rml_await_one evt p =
      fun f_k ctrl ->
	let w = if ctrl.kind = Top then evt.wa else evt.wp in
	let f_await_one =
	  if ctrl.kind = Top then 
	    let rec f_await_one_top =
	      fun () ->
		if evt.status = !instant 
		then 
		  let x = List.hd evt.value in
		  p x f_k ctrl ()
		else
		  (w := f_await_one_top :: !w;
		   sched ())
	    in f_await_one_top
	  else
	    let rec f_await_one_not_top =
	      fun () ->
		if evt.status = !instant 
		then 
		  let x = List.hd evt.value in
		  p x f_k ctrl ()
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

    let rml_present evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let rec f_present =
	  fun () ->
	    if evt.status = !instant
	    then 
	      f_1 ()
	    else
	      if eoi.status = !instant 
	      then 
		(ctrl.next <- f_2 :: ctrl.next;
		 sched ())
	      else 
		(evt.wp := f_present :: !(evt.wp);
		 toWakeUp :=evt.wp :: !toWakeUp;
		 sched ())
	in f_present

    let rml_seq p_1 p_2 =
      fun f_k ctrl ->
	let f_2 = p_2 f_k ctrl in
	let f_1 = p_1 f_2 ctrl in
	f_1

    let join cpt =
      fun f_k ctrl ->
	let f_join =
	  fun () ->
	    incr cpt;
	    if !cpt = 2 
	    then (
	      (* cpt := 0; *)
	      f_k()
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
	  fun () -> 
	    cpt := 0;
	    current := f_2 :: !current;
	    f_1 ()
	in f_par

    let rml_merge p_1 p_2 =
      fun f_k ctrl ->
	fun () -> raise RML

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
	let f_loop = p (fun () -> !f_1 ()) ctrl in
	f_1 := f_loop;
	f_loop

    let rml_signal p = 
      fun f_k ctrl ->
	let f_signal =
	  fun () ->
	    let evt = new_evt() in
	    let f = p evt f_k ctrl in
	    f ()
	in f_signal

    let rml_signal_combine default comb p = 
      fun f_k ctrl ->
	let f_signal =
	  fun () ->
	    let evt = new_evt_combine (default()) (comb()) in
	    let f = p evt f_k ctrl in
	    f ()
	in f_signal
	
    let rml_def e p =
      fun f_k ctrl ->
	let f_def =
	  fun () ->
	    let f = p (e()) f_k ctrl in
	    f()
	in f_def

    let rml_match e p =
      fun f_k ctrl ->
	let f_match =
	  fun () ->
	    let f = p (e()) f_k ctrl in
	    f()
	in f_match

    let rml_run e evts =
      fun f_k ctrl ->
	let f_run =
	  fun () ->
	    let f_1 = (e ()) evts f_k ctrl in
	    f_1 ()
	in f_run

    let rml_until evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Kill f_k;
	    active = false;
	    susp = false;
	    fils = [];
	    cond = (fun () -> evt.status = !instant);
	    next = [] }
	in
	let _ = ctrl.fils <- new_ctrl :: ctrl.fils in
	let end_until =
	  fun () ->
	    new_ctrl.active <- false;
	    f_k ()
	in
	let f = p end_until new_ctrl in
	let f_until =
	  fun () ->
	    new_ctrl.active <- true;
	    new_ctrl.susp <- false;
	    new_ctrl.next <- [];
	    f()
	in f_until

    let rml_control evt p =
      fun f_k ctrl ->
	let new_ctrl = 
	  { kind = Susp;
	    active = false;
	    susp = false;
	    fils = [];
	    cond = (fun () -> evt.status = !instant);
	    next = [] }
	in
	let _ = ctrl.fils <- new_ctrl :: ctrl.fils in
	let end_control =
	  fun () ->
	    new_ctrl.active <- false;
	    f_k ()
	in
	let f = p end_control new_ctrl in
	let f_control =
	  fun () ->
	    new_ctrl.active <- true;
	    new_ctrl.susp <- false;
	    new_ctrl.next <- [];
	    f()
	in f_control

    let rml_when evt p =
      fun f_k ctrl ->
	let w = if ctrl.kind = Top then evt.wa else evt.wp in
	let dummy = ref (Obj.magic()) in
	let new_ctrl = 
	  {kind = When dummy;
	   active = false;
	   susp = false;
	   fils = [];
	   cond = (fun () -> false);
	   next = []}
	in
	let _ = ctrl.fils <- new_ctrl :: ctrl.fils in
	let end_when =
	  fun () ->
	    new_ctrl.active <- false;
	    f_k ()
	in
	let f = p end_when new_ctrl in
	let rec f_when =
	  fun () ->
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
	  fun () ->
	    new_ctrl.active <- true;
	    if evt.status = !instant
	    then 
	      (new_ctrl.susp <- false;
	       new_ctrl.next <- [];
	       f())
	    else 
	      (new_ctrl.susp <- true;
	       new_ctrl.next <- [f];
	       w := f_when :: !w;
	       if ctrl.kind <> Top then toWakeUp := w :: !toWakeUp;
	       sched())
	in
	dummy := f_when;
	start_when
	    
    let rml_if e p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let f_if =
	  fun () ->
	    if e() then
	      f_1 ()
	    else
	      f_2 ()
	in f_if

    let rml_while e p =
      fun f_k ctrl ->
	let f_body = ref (Obj.magic()) in
	let f_while =
	  fun () ->
	    if e()
	    then !f_body ()
	    else f_k ()
	in
	f_body := p f_while ctrl;
	f_while

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k ctrl ->
	let rec f_for i v2 =
	  fun () ->
	    incr i;
	    if cmp !i v2 
	    then p !i (f_for i v2) ctrl ()
	    else f_k ()
	in
	let f_for_init =
	  fun () ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl ()
	    else f_k ()
	in
	f_for_init


(* ------------------------------------------------------------------------ *)

    let rml_await evt =
      fun f_k ctrl ->
	rml_pause (rml_await_immediate evt f_k ctrl) ctrl

    let rml_await_all evt p =
      fun f_k ctrl ->
	rml_await_immediate evt (rml_values evt p f_k ctrl) ctrl


(* ------------------------------------------------------------------------ *)
    
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

    let rml_par_n p_list =
      fun f_k ctrl ->
	let nb = List.length p_list in
	let cpt = ref nb in
	let j = join_n cpt f_k ctrl in
	let f_list = List.rev_map (fun p -> p j ctrl) p_list in
	let f_par_n =
	  fun () ->
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
(* rml_exec                                       *)
(**************************************************)
    let rml_exec p =
      let f = p () (fun x -> raise End) top in
      let rec exec () =
	incr instant;
	sched ();
	eoi.status <- !instant;
	wakeUp eoi;
	wakeUpAll ();
	sched ();
	eval_control();
	next_to_current top;
	exec ()
      in
      current := [f];
      exec ()

(**************************************************)
(* rml_exec_n                                     *)
(**************************************************)
    let rml_exec_n p nb =
      let f = p () (fun x -> raise End) top in
      let rec exec () =
	if !instant < nb then
	  begin
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
	    incr instant;
	    sched ();
	    eoi.status <- !instant;
	    wakeUp eoi;
	    wakeUpAll ();
	    sched ();
	    eval_control();
	    next_to_current top;
	    exec ()
	  end
	else
	  ()
      in
      current := [f];
      exec ()

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
	incr instant;
	sched ();
	eoi.status <- !instant;
	wakeUp eoi;
	wakeUpAll ();
	sched ();
	eval_control();
	next_to_current top;
(* -------------------------------------------------------------- *)
	fin := Sys.time();
	diff := min -. (!fin -. !debut);
	if !diff > 0.001 then (
	  ignore (Unix.setitimer 
		    Unix.ITIMER_REAL 
		    {Unix.it_interval = 0.0; Unix.it_value = !diff});
	  Unix.pause())  
	else (
	  print_string "Instant ";
	  print_int !instant;
	  print_string " : depassement = ";
	  print_float (-. !diff);
	  print_newline());
(* -------------------------------------------------------------- *)
	exec ()
      in
      current := [f];
      exec ()

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
	    incr instant;
	    sched ();
	    eoi.status <- !instant;
	    wakeUp eoi;
	    wakeUpAll ();
	    sched ();
	    eval_control();
	    next_to_current top;
(* -------------------------------------------------------------- *)
	    fin := Sys.time();
	    diff := min -. (!fin -. !debut);
	    if !diff > 0.001 then (
	      ignore (Unix.setitimer 
			Unix.ITIMER_REAL 
			{Unix.it_interval = 0.0; Unix.it_value = !diff});
	      Unix.pause())  
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
      exec ()


  end (* Module Rml_interpreter *)
