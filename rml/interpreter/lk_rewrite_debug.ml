(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 31/03/2004                                      *)
(* Fichier : lk_rewrite.ml                                            *)
(* Description :                                                      *)
(*   Implantation a la rewrite du langage Lk.                         *)
(*   Un instruction est traduite en 2 fonctions step*step_eoi.        *)
(*                                                                    *)
(**********************************************************************)

module Rml_interpreter: Lk_interpreter.S =
  struct

    let print_debug s =
      print_string s;
      print_newline()

    exception RML
    exception End

    type ('a, 'b) event = 
	{ mutable status: bool;
	  mutable value: 'b;
	  mutable pre_status: bool;
	  mutable pre_value: 'b;
	  mutable default: 'b;
	  combine: 'a -> 'b -> 'b; }

    type status = TERM | SUSP 

    type step = env -> status * step * step_eoi
    and step_eoi = env -> step

    and env = (reset_env * move) 
    and reset_env = (unit -> unit) ref
    and move = bool ref
	  
    and proc = step
	  
    let global_env = (ref (fun() -> ()), ref false) 

    let new_evt (reset_env, _) =
      let evt = { status = false; 
		  value = [];
		  pre_status = false; 
		  pre_value = [];
		  default = [];
		  combine = fun x y -> x :: y; }
      in
      let r = !reset_env in
      reset_env := 
	(fun () -> 
	  evt.pre_status <- evt.status;
	  evt.pre_value <- evt.value;
	  evt.status <- false; 
	  evt.value <- []; 
	  r());
      evt
	
    let new_evt_combine (reset_env, _) default combine =
      let evt = { status = false; 
		  value = default;
		  pre_status = false; 
		  pre_value = default;
		  default = default;
		  combine = combine; }
      in
      let r = !reset_env in
      reset_env := 
	(fun () -> 
	  evt.pre_status <- evt.status;
	  evt.pre_value <- evt.value;
	  evt.status <- false; 
	  evt.value <- evt.default; 
	  r());
  evt
	
(* ------------------------------------------------------------------------ *)
    let rec sched ((_,move) as env) (p:step) =
      match p env with
      | TERM, f, f_eoi -> true, f
      | SUSP, f, f_eoi -> 
	  if !move then 
	    (move := false;
	     sched env f)
	  else
	    ( print_string ("--- eoi ");
	      print_newline();
	      (false, f_eoi env))
	
(* ------------------------------------------------------------------------ *)
    let rml_pre_status evt = evt.pre_status
    let rml_pre_value evt = evt.pre_value
      
(* ------------------------------------------------------------------------ *)
    let rml_global_signal () = new_evt global_env
    let rml_global_signal_combine default combine = 
      new_evt_combine global_env (default()) (combine())

(* ------------------------------------------------------------------------ *)
    let rec (rml_term: step) =
      let eoi_term = fun env -> rml_term in
      fun env -> (TERM, rml_term, eoi_term)
	  
(* ------------------------------------------------------------------------ *)
    let rml_compute e k =
      let f_compute =
	fun env -> 
	  print_debug "compute";
	  e();
	  k env
      in
      f_compute
	
    let rml_emit_val evt e k =
      let f_emit_val =
	fun ((_,move) as env) ->
	  print_debug "emit";
	  move := true;
	  evt.status <- true;
	  evt.value <- evt.combine (e()) evt.value;
	  k env
      in
      f_emit_val
	
	
    let rml_present evt k1 k2 =
      let rec f_present =
	fun env ->
	  print_debug "present";
	  if evt.status
	  then k1 env
	  else (SUSP, f_present, eoi_present)
      and
	  eoi_present = 
	fun env ->
	  print_debug "present";
	  if evt.status
	  then raise RML
	  else k2
      in
      f_present
	
(* let seq k1 k2 *)
	
    let rml_par k1 k2 k =
      let rec par_susp_susp f1 f2 =
	fun env ->
	  print_debug "par_susp_susp";
	  let status1, f1', eoi1 = f1 env in
	  let status2, f2', eoi2 = f2 env in
	  match status1, status2 with
	  | TERM, TERM -> k env
	  | SUSP, SUSP -> 
	      (SUSP, (par_susp_susp f1' f2'), (eoi_susp_susp eoi1 eoi2))
	  | TERM, SUSP -> 
	      (SUSP, (par_term_susp eoi1 f2'), (eoi_term_susp eoi1 eoi2))
	  | SUSP, TERM -> 
	      (SUSP, (par_susp_term f1' eoi2), (eoi_susp_term eoi1 eoi2))
      and
	  par_susp_term f1 eoi2 =
	fun env ->
	  print_debug "par_susp_term";
	  let status1, f1', eoi1 = f1 env in
	  if status1 = TERM
	  then k env
	  else (SUSP, (par_susp_term f1' eoi2), (eoi_susp_term eoi1 eoi2))
      and
	  par_term_susp eoi1 f2 =
	fun env ->
	  print_debug "par_term_susp";
	  let status2, f2', eoi2 = f2 env in
	  if status2 = TERM
	  then k env
	  else (SUSP, (par_term_susp eoi1 f2'), (eoi_term_susp eoi1 eoi2))
      and 
	  eoi_susp_susp eoi1 eoi2 =
	fun env ->
	  print_debug "par_susp_susp";
	  par_susp_susp (eoi1 env) (eoi2 env)
      and 
	  eoi_susp_term eoi1 (eoi2:step_eoi) =
	fun env ->
	  print_debug "par_susp_term";
	  par_susp_term (eoi1 env) eoi2
      and 
	  eoi_term_susp eoi1 eoi2 =
	fun env ->
	  print_debug "par_term_susp";
	  par_term_susp eoi1 (eoi2 env)
      in
      par_susp_susp k1 k2
	
    let rml_merge = rml_par
	
    let rml_loop k_init =
      let rec f_loop f_body =
	fun env ->
	  print_debug "loop";
	  match f_body env with 
	  | TERM, _, _ -> (f_loop k_init) env
	  | SUSP, f', eoi' -> (SUSP, f_loop f', eoi_loop eoi')
      and 
	  eoi_loop f_eoi =
	fun env ->
	  print_debug "loop";
	  f_loop (f_eoi env)
      in
      f_loop k_init
	
    let rml_while e k_init k =
      let rec f_while f_body =
	fun env ->
	  print_debug "while";
	  match f_body env with 
	  | TERM, _, _ -> 
	      if e() 
	      then (f_while k_init) env
	      else k env
	  | SUSP, f', eoi' -> (SUSP, f_while f', eoi_while eoi')
      and
	  eoi_while f_eoi =
	fun env ->
	  print_debug "loop";
	  f_while (f_eoi env)
      in
      let f_while_init =
	fun env ->
	  if e() 
	  then (f_while k_init) env
	  else k env
      in
      f_while_init 
	
    let rml_for e1 e2 dir k_init k =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      let f i v2 =
	let rec f_for f_body =
	  fun env ->
	    print_debug "for";
	    match f_body env with 
	    | TERM, _, _ -> 
		incr i;
		if cmp !i v2 
		then (f_for (k_init !i)) env
		else k env
	    | SUSP, f', eoi' -> (SUSP, f_for f', eoi_for eoi')
	and
	    eoi_for f_eoi =
	  fun env ->
	    f_for (f_eoi env)
	in
	f_for
      in
      let f_for_init =
	fun env ->
	  print_debug "for";
	  let i = ref (e1()) in
	  let v2 = e2() in
	  if cmp !i v2
	  then (f i v2 (k_init !i)) env
	  else k env
      in
      f_for_init
	
    let rml_signal k =
      let f_signal =
	fun env ->
	  print_debug "signal";
	  let evt = new_evt env in
	  let f = k evt in
	  f env
      in 
      f_signal
	
    let rml_signal_combine default combine k =
      let f_signal_combine =
	fun env ->
	  print_debug "signal";
	  let evt = new_evt_combine env (default()) (combine()) in
	  let f = k evt in
	  f env
      in 
      f_signal_combine
	
    let rml_def e k =
      let f_def =
	fun env ->
	  print_debug "def";
	  let x = e() in
	  let f = k x in
	  f env
      in
      f_def
	
    let rml_match e k =
      let f_match =
	fun env ->
	  print_debug "match";
	  let x = e() in
	  let f = k x in
	  f env
      in
      f_match

    let rml_values evt k =
      let rec f_values =
	fun env ->
	  print_debug "get";
	  (SUSP, f_values, eoi_values)
      and
	  eoi_values =
	fun env ->
	  let x = evt.value in
	  let f = k x in
	  f
      in
      f_values
	
    let rml_await_immediate_one evt k =
      let rec f_await_one =
	fun env ->
	  print_debug "await one";
	  if evt.status 
	  then 
	    let x = List.hd evt.value in
	    let f = k x in
	    f env
	  else
	    (SUSP, f_await_one, eoi_await_one)
      and
	  eoi_await_one =
	fun env ->
	  print_debug "await one";
	  f_await_one
      in
      f_await_one

    let rml_run e evts k =
      let f_run =
	fun env ->
	  print_debug "run";
	  let f = (e ()) evts k in
	  f env
      in
      f_run
	
    let rml_when evt k_body k =
      let rec f_when_reac f_body eoi_body =
	fun env ->
	  print_debug "when";
	  if evt.status then (
	    let s, f', f_eoi' = f_body env in
	    if s = TERM 
	    then k env
	    else (SUSP, f_when_reac f' f_eoi', eoi_when_reac f' f_eoi'))
	  else
	    (SUSP, f_when_reac f_body eoi_body, eoi_when_reac f_body eoi_body)
      and
	  eoi_when_reac f_body eoi_body =
	fun env ->
	  print_debug "when";
	  if evt.status 
	  then f_when_reac (eoi_body env) eoi_body
	  else f_when_reac f_body eoi_body
      in
      f_when_reac k_body (fun env -> k_body)
	
    let rml_until evt k_body k =
      let rec f_until f_body =
	fun env ->
	  print_debug "until";
	  let s, f', eoi' = f_body env in
	  if s = TERM 
	  then k env
	  else (SUSP, (f_until f'), (eoi_until eoi'))
      and
	  eoi_until eoi =
	fun env ->
	  print_debug "until";
	  if evt.status
	  then k
	  else f_until (eoi env)
      in
      f_until k_body
	
    let rml_control evt k_body k =
      let rec f_control_actif f_body eoi =
	fun env ->
	  print_debug "control";
	  let s, f', f_eoi' = f_body env in
	  if s = TERM 
	  then k env
	  else (SUSP, f_control_actif f' f_eoi', eoi_control_actif f' f_eoi')
      and
	  f_control_susp f_body eoi =
	fun env ->
	  print_debug "control";
	  (SUSP, (f_control_susp f_body eoi), (eoi_control_susp f_body eoi))
      and 
	  eoi_control_actif f_body eoi =
	fun env ->
	  print_debug "control";
	  let f' = eoi env in
	  if evt.status 
	  then f_control_susp f' eoi
	  else f_control_actif f' eoi
      and	  
	  eoi_control_susp f_body eoi =
	fun env ->
	  print_debug "control";
	  if evt.status 
	  then f_control_actif f_body eoi
	  else f_control_susp f_body eoi
      in
      f_control_actif k_body (fun env -> k_body) 
	
    let rml_if e k1 k2 =
      let f_if =
	fun env ->
	  print_debug "if";
	  if e() 
	  then k1 env 
	  else k2 env
      in
      f_if
	
    let rml_emit evt k = rml_emit_val evt (fun () -> ()) k
	
    let rml_pause k =  
      rml_signal (fun s -> rml_values s (fun x -> k))  
	
    let rml_await_immediate evt = rml_when evt rml_term
	
    let rml_await_all evt p =
      rml_await_immediate evt
	(rml_values evt p)
	
    let rml_await evt k = rml_pause (rml_await_immediate evt k)
	
    let rml_await_one evt k = rml_pause (rml_await_immediate_one evt k)

(**************************************************)
(* exec                                           *)
(**************************************************)
    let rml_exec =
      let rec exec ((reset_env, move) as env) p =
	let b, f = sched env p in
	if b then ()
	else (
      move := false;
	  !reset_env ();
	  exec env f)
      in
      fun p ->
	let f = 
	  p () 
	    (fun env -> (TERM,(fun env -> raise End),(fun env -> raise End)))
	in
	exec global_env f
	  
(**************************************************)
(* exec_n                                          *)
(**************************************************)
    let rml_exec_n p nb =
      let rec exec ((reset_env, move) as env) p cpt =
	if cpt < nb then
	  begin
	    print_string ("************ Instant "^
			  (string_of_int cpt)^
			  " ************");
	    print_newline();
	    let b, f = sched env p in
	    if b then ()
	    else (
	      move := false;
	      !reset_env ();
	      exec env f (cpt + 1))
	  end
      in
      let f = 
	p () (fun env -> (TERM,(fun env -> raise End),(fun env -> raise End)))
      in
      exec global_env f 0
	
(**************************************************)
(* exec_sampling                                  *)
(**************************************************)
    let rml_exec_sampling p min =
      let _ = 
	Sys.signal Sys.sigalrm 
	  (Sys.Signal_handle 
	     (fun x -> ())
(*	   print_string "ALARM !!";
   print_newline())
 *)
	  )
      in
      let instant = ref 0 in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      
      let rec exec ((reset_env, move) as env) p =
(* -------------------------------------------------------------- *)
	incr instant;
	debut := Sys.time();
(* -------------------------------------------------------------- *)
	let b, f = sched env p in
	if b then ()
	else (
	  move := false;
	  !reset_env ();
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
	  exec env f)
      in
      let f = 
	p () (fun env -> (TERM,(fun env -> raise End),(fun env -> raise End)))
      in
      exec global_env f
	
(**************************************************)
(* exec_n_sampling                                *)
(**************************************************)
    let rml_exec_n_sampling p nb min =
      let _ = 
	Sys.signal Sys.sigalrm 
	  (Sys.Signal_handle 
	     (fun x -> ())
(*	   print_string "ALARM !!";
   print_newline())
 *)
	  )
      in
      let instant = ref 0 in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      
      let rec exec ((reset_env, move) as env) p cpt =
	if cpt < nb then
	  begin
(* -------------------------------------------------------------- *)
	    incr instant;
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
	    debut := Sys.time();
(* -------------------------------------------------------------- *)
	    let b, f = sched env p in
	    if b then ()
	    else (
	      move := false;
	      !reset_env ();
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
	      exec env f (cpt + 1))
	  end
      in
      let f = 
	p () (fun env -> (TERM,(fun env -> raise End),(fun env -> raise End)))
      in
      exec global_env f 0

  end
