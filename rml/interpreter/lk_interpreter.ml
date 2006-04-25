(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 31/08/2005                                      *)
(* Fichier : lk_interpreter.ml                                        *)
(*                                                                    *)
(**********************************************************************)

module type Lk_interpreter =
  functor (Event : Sig_env.S) ->
    sig
      type ('a, 'b) event 
      and event_cfg 
      and 'a step = 'a -> unit
      and control_tree
      and 'a process = 'a step -> control_tree -> unit step

      val rml_global_signal: unit -> ('a, 'a list) event
      val rml_global_signal_combine: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event 
      val rml_expr_emit: (unit, 'b) event -> unit 
      val rml_expr_emit_val: ('a, 'b) event -> 'a -> unit
      val rml_pre_status: ('a, 'b) event -> bool
      val rml_pre_value: ('a, 'b) event -> 'b


      val rml_compute: (unit -> 'a) -> 'a step -> 'b step
      val rml_pause: unit step -> control_tree -> 'a step
      val rml_halt: unit step -> 'a step
      val rml_emit_val:
        (unit -> ('a, 'b) event) -> (unit -> 'a) -> unit step -> 'c step
      val rml_emit:
        (unit -> (unit, 'a) event) -> unit step -> 'b step
      val rml_await_immediate:
        (unit -> ('a, 'b) event) -> unit step -> control_tree -> 'c step
      val rml_await:
        (unit -> ('a, 'b) event) -> unit step -> control_tree -> 'c step

      val rml_get:
        (unit -> ('a, 'b) event) -> ('b -> unit step) -> control_tree -> 
	  'c step
      val rml_await_immediate_one:
        (unit -> ('a, 'a list) event) -> ('a -> unit step) -> control_tree ->
	  'b step
      val rml_await_all:
        (unit -> ('a, 'b) event) -> ('b -> unit step) -> control_tree ->
	  'c step

      val rml_present:
	  control_tree -> (unit -> ('a, 'b) event) ->
	    unit step -> unit step -> 'c step

      val rml_signal: (('a, 'a list) event -> unit step) -> 'b step 
      val rml_signal_combine: 
	  (unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
	    (('a, 'b) event -> unit step) -> 'c step 
		
      val rml_split_par:
	  int -> (int ref -> (unit step) list) -> 'a step
      val rml_join_par:
	  int ref -> unit step -> 'a step
      val rml_join_def:
	  int ref -> 'a ref -> (unit -> 'b) -> 'b step -> 'a step

      val rml_loop: ('a step -> unit step) ->  unit step
      val rml_loop_n: 
	  (unit -> int) -> ('a step -> unit step) -> unit step ->  unit step

      val rml_match: (unit -> 'a) -> ('a -> unit step) -> 'b step

      val rml_run: (unit -> 'a process) -> 'a step -> control_tree -> 'b step

      val rml_if: (unit -> bool) -> unit step -> unit step -> 'a step

      val rml_while: 
	  (unit -> bool) -> ('a step -> unit step) -> unit step -> 'b step

      val rml_for: 
	  (unit -> int) -> (unit -> int) -> bool -> 
	    (int -> 'a step -> unit step) -> unit step -> 'b step
      val rml_fordopar: 
	  (unit -> int) -> (unit -> int) -> bool -> 
	    (int ref -> int -> unit step) -> unit step -> 'b step
	    

      val rml_make: 'a process -> (unit -> 'a option)
    end

module Lk_interpreter: Lk_interpreter =
  functor (Event: Sig_env.S) ->
  struct
   
    exception RML
 
    type ('a, 'b) event = 
	('a,'b) Event.t * unit step list ref * unit step list ref  
    and event_cfg = bool -> (unit -> bool) * unit step list ref list

    and control_tree = 
	{ kind: control_type;
	  mutable alive: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable children: control_tree list;
	  mutable next: next; }
    and control_type = 
	Top 
      | Kill of unit step 
      | Kill_handler of (unit -> unit step)
      | Susp 
      | When of unit step ref

    and 'a step = 'a -> unit
    and next = unit step list
    and current = unit step list
    and waiting = unit step list
    and 'a process = 'a step -> control_tree -> unit step


	
(* liste des processus a executer dans l'instant *)
    let current = ref ([]: current)
    
(* liste des listes de processus a revillier a la fin d'instant *)
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
      current := List.rev_append !w !current;
      w := []


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

(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create default combine, ref [], ref [])

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)

    let eoi = ref false
    let weoi = ref ([]: waiting)

    let dummy_step _ = ()

(* ------------------------------------------------------------------------ *)
    let sched () = 
      match !current with
      | f :: c -> 
	  current := c;
	  f ()
      | [] -> ()

(* ------------------------------------------------------------------------ *)
    let rml_pre_status (n, _, _) = Event.pre_status n 
	  
    let rml_pre_value (n, _, _) = Event.pre_value n
 
(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine =  new_evt_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e k _ =
      k (e())

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause k ctrl _ =
      ctrl.next <- k :: ctrl.next;
      sched ()

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt k _ =
      sched ()

(**************************************)
(* emit                               *)
(**************************************)
    let set_emit (n,wa,wp) v =
      Event.emit n v;
      wakeUp wa;
      wakeUp wp


    let rml_emit_val expr_evt e k _ =
      set_emit (expr_evt()) (e());
      k ()

    let rml_emit expr_evt k x =
      set_emit (expr_evt()) ();
      k ()

    let rml_expr_emit_val = set_emit

    let rml_expr_emit evt = rml_expr_emit_val evt ()


(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate_top (n,wa,_) k =
      let rec self _ =
	if Event.status n
	then
	  k ()
	else
	  (wa := (Obj.magic self: unit step) :: !wa;
	   sched ())
      in self

    let step_await_immediate (n,_,wp) k ctrl =
      let rec self _ =
	if Event.status n
	then
	  k ()
	else
	  if !eoi
	  then
	    (ctrl.next <- self :: ctrl.next;
	     sched())
	  else
	    (wp := self :: !wp;
	     toWakeUp := wp :: !toWakeUp;
	     sched())
      in self


    let rml_await_immediate' evt k ctrl _ =
      if ctrl.kind = Top then 
	step_await_immediate_top evt k ()
      else
	step_await_immediate evt k ctrl ()

    let rml_await_immediate expr_evt k ctrl _ =
      let evt = expr_evt() in 
      rml_await_immediate' evt k ctrl ()

(**************************************)
(* get                                *)
(**************************************)
    let step_get_eoi n f ctrl _ =
      let v =
	if Event.status n
	then Event.value n
	else Event.default n
      in
      ctrl.next <- (f v) :: ctrl.next;
      sched()
	
    let step_get (n,_,_) f ctrl _ =
      weoi := (step_get_eoi n f ctrl) :: !weoi;
      sched ()
      
    let rml_get expr_evt f ctrl _ =
      step_get (expr_evt()) f ctrl ()

(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one_top (n, wa, _) f =
      let rec self _ =
	if Event.status n 
	then 
	  let v = Event.one n in
	  f v ()
	else
	  (wa := self :: !wa;
	   sched ())
      in self

    let step_await_immediate_one (n, _, wp) f ctrl =
      let rec self _ =
	if Event.status n
	then
	  let v = Event.one n in
	  f v ()
	else
	  if !eoi
	  then
	    (ctrl.next <- self :: ctrl.next;
	     sched())
	  else
	    (wp := self :: !wp;
	     toWakeUp := wp :: !toWakeUp;
	     sched())
      in self

    let rml_await_immediate_one expr_evt f ctrl _ =
      if ctrl.kind = Top then 
	step_await_immediate_one_top (expr_evt()) f ()
      else
	step_await_immediate_one (expr_evt()) f ctrl ()


(**************************************)
(* present                            *)
(**************************************)    
    let step_present ctrl (n,_,wp) k_1 k_2 =
      let rec self _ =
	if Event.status n
	then 
	  k_1 ()
	else
	  if !eoi
	  then 
	    (ctrl.next <- k_2 :: ctrl.next;
	     sched ())
	  else 
	    (wp := self :: !wp;
	     toWakeUp := wp :: !toWakeUp;
	     sched ())
      in self

    let rml_present ctrl expr_evt k_1 k_2 _ =
      let evt = expr_evt () in
      step_present ctrl evt k_1 k_2 ()


(**************************************)
(* await_all_match                    *)
(**************************************)
    let await_all_match _ = failwith "Not yet implemented"


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p _ = 
      let evt = new_evt() in
      let f = p evt in
      f ()

    let rml_signal_combine default comb p _ = 
      let evt = new_evt_combine (default()) (comb()) in
      let f = p evt in
      f ()


(**************************************)
(* par                                *)
(**************************************)
    let rml_split_par n f _ =
      let j = ref n in
      let k_list = f j in
      current := List.rev_append k_list !current;
      sched()

    let rml_join_par j k _ =
      decr j;
      if !j > 0 then
	sched ()
      else
	k ()

(**************************************)
(* join_def                           *)
(**************************************)
    let rml_join_def j v_ref get_values k v =
      decr j;
      v_ref := v;
      if !j > 0 then
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
    let rml_loop_n e p k =
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
	  let n = e() in
	  if n > 0 then
	    (cpt := n - 1;
	     f_loop ())
	  else
	    k ()


(**************************************)
(* match                              *)
(**************************************)

    let rml_match e f _ =
      let k = f (e()) in
      k ()


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e k ctrl _ =
      let body = e () k ctrl in
      body ()

(**************************************)
(* if                                 *)
(**************************************)
	    
    let rml_if e k_1 k_2 _ =
      if e() then
	k_1 ()
      else
	k_2 ()


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
	  if !j <= 0 then
	    k ()
	  else
	    begin
	      for i = max downto min do
		let f = p j i in
		current := f :: !current
	      done;
	      sched()
	    end
	end
      else
	begin
	  let max = e1() in
	  let min = e2() in
	  let j = ref (max - min + 1) in
	  if !j <= 0 then
	    k ()
	  else
	    begin
	      for i = min to max do
		let f = p j i in
		current := f :: !current
	      done;
	      sched ()
	    end
	end


(* ---------- Misc functions for until, control and when ---------- *)
    let new_ctrl kind =
      { kind = kind;
	alive = true;
	susp = false;
	children = [];
	cond = (fun () -> false);
	next = [] }

    let start_ctrl f ctrl new_ctrl =
      let f_ctrl =
	fun _ ->
	  if new_ctrl.alive
	  then 
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     new_ctrl.next <- []);
	  f ()
      in f_ctrl

    let end_ctrl k new_ctrl =
      fun x ->
	set_kill new_ctrl;
	new_ctrl.alive <- false;
	k x

(**************************************)
(* until                              *)
(**************************************)
(*
    let rml_start_until expr_evt p ctrl =
	let new_ctrl = new_ctrl (Kill k) in
	let f = p new_ctrl in
	let f_until =
	  fun _ ->
	    let (n,_,_) = expr_evt () in
	    new_ctrl.cond <- (fun () -> Event.status n);
	    start_ctrl f ctrl new_ctrl ()
	in f_until
*)

(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

    let rml_await expr_evt k ctrl _ =
      rml_await_immediate expr_evt (rml_pause k ctrl) ctrl ()

    let rml_await_all expr_evt p ctrl _ =
      let evt = expr_evt () in
      rml_await_immediate' evt (step_get evt p ctrl) ctrl ()


(* ------------------------------------------------------------------------ *)
    type value
    exception End of value

(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make p = 
      (* the main step function *)
      let f = p (fun x -> raise (End (Obj.magic x: value))) top in
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

  end
