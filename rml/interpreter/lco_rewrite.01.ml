(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 29/04/2005                                      *)
(* Fichier : lco_rewite                                               *)
(* Description :                                                      *)
(*   Implantation à la Rewrite de Junior pour lco.                    *)
(**********************************************************************)
 
(* $Id: lco_rewrite.ml,v 1.1 2005/04/30 16:49:15 mandel Exp $ *)


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
	  combine: ('a -> 'b -> 'b); }

    and status = SUSP | STOP | TERM

    and proc = unit -> status * proc

(* le numero d'instant *)
    let instant = ref 0

(* Flag pour le calcul de point fix *)
    let move = ref false
    let eoi = ref false

(* le code pour absent *)
    let absent = -1

(* creation d'evenements *)
    let new_evt() =
      { status = absent; 
	value = [];
	pre_status = absent; 
	pre_value = [];
	default = [];
	combine = (fun x y -> x :: y); } 

    let new_evt_combine default combine =
      { status = absent; 
	value = default;
	pre_status = absent; 
	pre_value = default;
	default = default;
	combine = combine; } 



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

(**************************************)
(* nothing                            *)
(**************************************)
    let rec rml_nothing =
      fun () -> TERM, rml_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun () ->
	e();
	(TERM, rml_nothing)

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun () ->
	STOP, rml_nothing

(**************************************)
(* halt                               *)
(**************************************)
    let rec rml_halt =
      fun () ->
	STOP, rml_halt

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' evt e =
      fun () -> 
	move := true;
	if evt.status <> !instant 
	then 
	  (evt.pre_status <- evt.status;
	   evt.pre_value <- evt.value;
	   evt.status <- !instant;
	   evt.value <- evt.combine (e()) evt.default)
	else
	  evt.value <- evt.combine (e()) evt.value;
	(TERM, rml_nothing)
	  
    let rml_emit_val expr_evt e =
      fun () ->
	let evt = expr_evt () in
	rml_emit_val' evt e ()

    let rml_emit' evt =
      rml_emit_val' evt (fun () -> ()) 
	
    let rml_emit expr_evt =
      fun () ->
	let evt = expr_evt () in
	rml_emit_val' evt (fun () -> ()) ()
	  
    let rml_expr_emit_val expr_evt e =
      let evt = expr_evt () in
      move := true;
      if evt.status <> !instant 
      then 
	(evt.pre_status <- evt.status;
	 evt.pre_value <- evt.value;
	 evt.status <- !instant;
	 evt.value <- evt.combine (e()) evt.default)
      else
	evt.value <- evt.combine (e()) evt.value
      
    let rml_expr_emit expr_evt =
      rml_expr_emit_val expr_evt (fun() -> ())

(**************************************)
(* await_immediate                    *)
(**************************************)
    let rml_await_immediate' evt =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if evt.status = !instant
	    then
	      (TERM, rml_nothing)
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate expr_evt =
      fun () ->
	let evt = expr_evt () in
	rml_await_immediate' evt ()

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' evt p =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    let x =
	      if evt.status = !instant
	      then evt.value
	      else evt.default
	    in
	    let f_body = p x in
	    (STOP, f_body)
	  else
	    (SUSP, self)
      in self

    let rml_get expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_get' evt p ()

(**************************************)
(* await_immediate_one                *)
(**************************************)
    let rml_await_immediate_one' evt p =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if evt.status = !instant
	    then
	      let x = List.hd evt.value in
	      let f_body = p x in
	      f_body ()
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate_one expr_evt p =
      fun () ->
	let  evt = expr_evt () in
	rml_await_immediate_one' evt p ()

(**************************************)
(* present                            *)
(**************************************)
    let rml_present' evt p1 p2 =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, p2)
	  else
	    if evt.status = !instant
	    then
	      p1 ()
	    else
	      (SUSP, self)
      in self

    let rml_present expr_evt p1 p2 =
      fun () ->
	let evt = expr_evt() in
	rml_present' evt p1 p2 ()

(**************************************)
(* seq                                *)
(**************************************)
    let rec rml_seq p1 p2 =
      fun () ->
	match p1 () with
	| TERM, _ -> p2 ()
	| alpha, p1' -> (alpha, rml_seq p1' p2)


(**************************************)
(* par                                *)
(**************************************)
    let rml_par =
      let gamma alpha beta =
	match alpha, beta with
	| SUSP, _ -> SUSP
	| _, SUSP -> SUSP
	| STOP, _ -> STOP
	| _, STOP -> STOP
	| TERM, TERM -> TERM
      in
      let delta_1 alpha beta = 
	match alpha, beta with
	| STOP, (STOP | TERM) -> SUSP
	| _ -> alpha
      in
      let delta_2 alpha beta = 
	match alpha, beta with
	| (STOP | TERM), STOP -> SUSP
	| _ -> beta
      in
      let rec par alpha beta p1 p2 =
	match alpha, beta with
	| SUSP, SUSP -> par_SUSP_SUSP p1 p2
	| SUSP, beta -> par_SUSP_beta beta p1 p2
	| _, SUSP -> par_alpha_SUSP alpha p1 p2
	| TERM, TERM -> rml_nothing
	| _ ->  raise RML
      and par_SUSP_SUSP p1 p2 =
	fun () ->
	  let alpha, p1' = p1 () in
	  let beta, p2' = p2 () in
	  (gamma alpha beta, 
	   par (delta_1 alpha beta) (delta_2 alpha beta) p1' p2')

      and par_SUSP_beta beta p1 p2 =
	fun () ->
	  let alpha, p1' = p1 () in
	  (gamma alpha beta, 
	   par (delta_1 alpha beta) (delta_2 alpha beta) p1' p2)

      and par_alpha_SUSP alpha p1 p2 =
	fun () ->
	  let beta, p2' = p2 () in
	  (gamma alpha beta, 
	   par (delta_1 alpha beta) (delta_2 alpha beta) p1 p2')
      in par_SUSP_SUSP

(**************************************)
(* merge                              *)
(**************************************)
    let rml_merge = rml_par

(**************************************)
(* loop                               *)
(**************************************)
    let rml_loop p =
      let rec self =
	fun () ->
	  let alpha, p' = p () in
	  (alpha, rml_seq p' self)
      in self

(**************************************)
(* signal                             *)
(**************************************)
    let rml_signal p = 
      fun () ->
	let evt = new_evt() in
	let f = p evt  in
	f ()

    let rml_signal_combine default comb p = 
      fun () ->
	let evt = new_evt_combine (default()) (comb()) in
	let f = p evt in
	f ()

(**************************************)
(* def                                *)
(**************************************)
    let rml_def e p =
      fun () ->
	let f = p (e()) in
	f()

(**************************************)
(* def_dyn                            *)
(**************************************)
    let rml_def_dyn p1 p2 =
      raise RML

(**************************************)
(* def_and_dyn                        *)
(**************************************)
    let rml_def_and_dyn p1 p2 =
      raise RML

(**************************************)
(* match                              *)
(**************************************)
    let rml_match e p =
      fun () ->
	let f = p (e()) in
	f()

(**************************************)
(* run                                *)
(**************************************)
    let rml_run e =
      fun () ->
	let f = e () () in
	f ()

(**************************************)
(* until                              *)
(**************************************)
    let rml_until' =
      let rec until evt p =
	fun () ->
	  match p() with
	  | TERM, _ -> TERM, rml_nothing
	  | SUSP, p' -> SUSP, until evt p'
	  | STOP, p' -> until_star evt p' () 
      and until_star evt p =
	fun () ->
	  if !eoi
	  then
	    if evt.status = !instant 
	    then
	      (STOP, rml_nothing)
	    else
	      (STOP, until evt p)
	  else
	    (SUSP, until_star evt p)
      in until		  

    let rml_until expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_until' evt p ()

(**************************************)
(* until handler                      *)
(**************************************)
    let rml_until_handler' =
      let rec until evt p hdl =
	fun () ->
	  match p() with
	  | TERM, _ -> TERM, rml_nothing
	  | SUSP, p' -> SUSP, until evt p' hdl
	  | STOP, p' -> until_star evt p' hdl () 
      and until_star evt p hdl =
	fun () ->
	  if !eoi
	  then
	    if evt.status = !instant 
	    then
	      let f = hdl evt.value in
	      (STOP, f)
	    else
	      (STOP, until evt p hdl)
	  else
	    (STOP, until_star evt p hdl)
      in until 		  

    let rml_until_handler expr_evt p hdl =
      fun () ->
	let evt = expr_evt () in
	rml_until_handler' evt p hdl ()

(**************************************)
(* control                            *)
(**************************************)
    let rml_control' =
      let rec active evt p =
	fun () ->
	  match p () with
	  | TERM, _ -> TERM, rml_nothing
	  | alpha, p' ->
	      if !eoi && (evt.status = !instant)
	      then
		(alpha, suspended evt p')
	      else
		(alpha, active evt p')
      and suspended evt p =
	fun () ->
	  if !eoi && (evt.status = !instant)
	  then
	    (STOP, active evt p)
	  else
	    (STOP, suspended evt p)
      in active

    let rml_control expr_evt p =
      fun () -> 
	let evt = expr_evt () in
	rml_control' evt p ()

(**************************************)
(* when                               *)
(**************************************)
    let rec rml_when' evt p =
      let rec self =
	fun () ->
	  if evt.status = !instant 
	  then	
	    match p() with
	    | TERM, _ -> TERM, rml_nothing
	    | alpha, p' -> alpha, rml_when' evt p'
	  else
	    if !eoi
	  then
	      (STOP, self)
	    else
	      (SUSP, self)
      in self
	
    let rml_when expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_when' evt p ()

(**************************************)
(* if                                 *)
(**************************************)
    let rml_if e p1 p2 =
      fun () ->
	if e() then
	  p1 ()
	else 
	  p2 ()
  
(**************************************)
(* while                              *)
(**************************************)
    let rec rml_while e p =
      fun () ->
	if e() then
	  rml_seq p (rml_while e p) ()
	else
	  TERM, rml_nothing


(**************************************)
(* for                                *)
(**************************************)
    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      let rec f_for i v2 =
	fun () ->
	  incr i;
	  if cmp !i v2 
	  then rml_seq (p !i) (f_for i v2) ()
	  else TERM, rml_nothing
	in
	let f_for_init =
	  fun () ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then rml_seq (p !i) (f_for i v2) ()
	    else TERM, rml_nothing
	in
	f_for_init

(**************************************)
(* for_dopar                          *)
(**************************************)
    let rml_fordopar =
      let gamma par_status alpha =
	match par_status, alpha with
	| SUSP, _ -> SUSP
	| _, SUSP -> SUSP
	| TERM, TERM -> TERM
	| _ -> STOP
      in 
      let f body_array =
	let rec self =
	  fun () ->
	    let par_status = ref TERM in
	    for i = 0 to Array.length body_array - 1 do
	      let status, p = body_array.(i) in
	      match status with
	      | SUSP -> 
		  let (alpha, p') as body = p() in
		  body_array.(i) <- body;
		  par_status := gamma !par_status alpha
	      | _ -> 
		  par_status := gamma !par_status status
	    done;
	    match !par_status with
	    | TERM -> TERM, rml_nothing
	    | SUSP -> SUSP, self
	    | STOP -> 
		for i = 0 to Array.length body_array - 1 do
		  match body_array.(i) with
		  | SUSP, _ -> assert false
		  | STOP, p ->  body_array.(i) <- SUSP, p
		  | TERM, _ -> ()
		done;
		STOP, self
	in self
      in
      fun e1 e2 dir p ->
	let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
	fun () ->
	  let i = ref (e1()) in
	  let v2 = e2() in
	  let tab = 
	    Array.make (if dir then v2 - !i + 1 else !i - v2 + 1) (Obj.magic())
	  in
	  let j = ref 0 in
	  while (cmp !i v2) do
	    tab.(!j) <- (SUSP, (p !i));
	    incr i;
	    j := !j + 1
	  done;
	  f tab ()
	      
(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)
    let rml_await expr_evt =
      fun () ->
	let evt = expr_evt () in
	rml_seq rml_pause (rml_await_immediate' evt) ()

    let rml_await' evt =
      rml_seq rml_pause (rml_await_immediate' evt)

    let rml_await_all expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_seq (rml_await_immediate' evt) (rml_get' evt p) ()

    let rml_await_all' evt p =
      rml_seq (rml_await_immediate evt) (rml_get evt p)

    let rml_await_one expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_seq rml_pause (rml_await_immediate_one' evt p) ()

    let rml_await_one' evt p =
      rml_seq rml_pause (rml_await_immediate_one' evt p)

(* ------------------------------------------------------------------------ *)
(**************************************)
(* sched                              *)
(**************************************)
    let rec sched p =
      match p () with
      | SUSP, p' ->
	  if !move then
	    begin
	      move := false;
	      sched p'
	    end
	  else
	    begin
	      eoi := true;
	      sched p'
	    end
      | res -> res


(**************************************************)
(* rml_exec                                       *)
(**************************************************)
    let rml_exec p =
      let rec exec p =
	incr instant;
	eoi := false;
	move := false;
	match sched p with
	| STOP, p' -> exec p'
	| TERM, _ -> ()
	| SUSP, _ -> assert false
      in exec (p ())


(**************************************************)
(* rml_exec_n                                     *)
(**************************************************)
    let rml_exec_n p nb =
      let rec exec p nb =
	incr instant;
	eoi := false;
	move := false;
	match sched p with
	| STOP, p' -> if nb > 0 then exec p' (nb-1)
	| TERM, _ -> ()
	| SUSP, _ -> assert false
      in exec (p ()) nb

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

      let rec exec p =
(* -------------------------------------------------------------- *)
	debut := Sys.time();
(* -------------------------------------------------------------- *)
	incr instant;
	eoi := false;
	move := false;
	match sched p with
	| STOP, p' -> 
(* -------------------------------------------------------------- *)
	    fin := Sys.time();
	    diff := min -. (!fin -. !debut);
	    if !diff > 0.001 then (
	      ignore (Unix.setitimer 
			Unix.ITIMER_REAL 
			{Unix.it_interval = 0.0; Unix.it_value = !diff});
	      Unix.pause()
	     )  
	    else (
(*	  
          print_string "Instant ";
	  print_int !instant;
	  print_string " : depassement = ";
	  print_float (-. !diff);
	  print_newline();
*)
	      ());
(* -------------------------------------------------------------- *)
	    exec p'
	| TERM, _ -> ()
	| SUSP, _ -> assert false
      in exec (p ())

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

      let rec exec p n =
(* -------------------------------------------------------------- *)
	print_string ("************ Instant "^
		      (string_of_int !instant)^
		      " ************");
	print_newline();
	debut := Sys.time();
(* -------------------------------------------------------------- *)
	incr instant;
	eoi := false;
	move := false;
	match sched p with
	| STOP, p' -> 
(* -------------------------------------------------------------- *)
	    fin := Sys.time();
	    diff := min -. (!fin -. !debut);
	    if !diff > 0.001 then (
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
	      print_newline();
	      ());
(* -------------------------------------------------------------- *)
	    if n > 0 then exec p' (n-1)
	| TERM, _ -> ()
	| SUSP, _ -> assert false
 
      in exec (p()) nb 

  end (* Module Rml_interpreter *)
