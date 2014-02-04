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

(* file: lco_rewite *)
(* author: Louis Mandel *)
(* created: 2005-04-29  *)

(* Description :                                                      *)
(*   Implantation à la Rewrite de Junior pour lco.                    *)
(*   Ajout des valeurs pour traiter def_dyn et def_and_dyn            *)

(* $Id: lco_rewrite.ml,v 1.1 2005/04/30 16:49:15 mandel Exp $ *)


module Rml_interpreter : Lco_interpreter.S =
  functor (Event: Sig_env.S) ->
  struct
    exception RML

    type ('a, 'b) event = ('a, 'b) Event.t

    and 'a event_cfg = unit -> (unit -> bool) * (unit -> 'a)

    and 'a status = SUSP | STOP | TERM of 'a

    and 'a expr = unit -> 'a status * 'a expr

    and 'a process = unit -> 'a expr


(* Flag pour le calcul de point fix *)
    let move = ref false
    let eoi = ref false


(* creation d'evenements *)
    let new_evt_combine = Event.create

    let new_evt_memory_combine = Event.create_memory

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)



(* ------------------------------------------------------------------------ *)
    let rml_pre_status = Event.pre_status

    let rml_pre_value = Event.pre_value

    let rml_last = Event.last

    let rml_default = Event.default

(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine = new_evt_combine

    let rml_global_signal_memory_combine = new_evt_memory_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' evt =
      fun () ->
        (fun () -> Event.status evt),
        (fun () -> Event.value evt)

    let cfg_present evt_expr =
      fun () ->
	let evt = evt_expr() in
	cfg_present' evt ()

    let cfg_and c1 c2 =
      fun () ->
	let is_true1, get1 = c1 () in
	let is_true2, get2 = c2 () in
	(fun () -> is_true1() && is_true2()),
        (fun () -> get1(), get2())

    let cfg_or c1 c2 =
      fun () ->
	let is_true1, get1 = c1 () in
	let is_true2, get2 = c2 () in
	(fun () -> is_true1() || is_true2()),
        (fun () -> if is_true1() then get1() else get2())

    let cfg_or_option c1 c2 =
      fun () ->
        let is_true1, get1 = c1 () in
        let is_true2, get2 = c2 () in
        (fun () -> is_true1() || is_true2()),
        (fun () ->
          (if is_true1() then Some (get1()) else None),
          (if is_true2() then Some (get2()) else None))

(* ------------------------------------------------------------------------ *)

(**************************************)
(* nothing                            *)
(**************************************)
    let rec rml_nothing =
      fun () -> TERM (), rml_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rec rml_compute e =
      fun () ->
	let v = e() in
	(TERM v, rml_compute (fun () -> v))

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun () ->
	STOP, rml_nothing

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi = rml_pause

(**************************************)
(* halt                               *)
(**************************************)
    let rec rml_halt =
      fun () ->
	STOP, rml_halt

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rec rml_halt_kboi = rml_halt

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' evt e =
      fun () ->
	move := true;
	Event.emit evt (e());
	(TERM (), rml_nothing)

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

    let rml_expr_emit_val evt v =
      move := true;
      Event.emit evt v

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

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
	    if Event.status evt
	    then
	      (TERM (), rml_nothing)
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate expr_evt =
      fun () ->
	let evt = expr_evt () in
	rml_await_immediate' evt ()

(**************************************)
(* await_immediate_conf               *)
(**************************************)
    let rml_await_immediate_conf' (cfg_status, _) =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if cfg_status()
	    then
	      (TERM (), rml_nothing)
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate_conf expr_cfg =
      fun () ->
	let cfg = expr_cfg () in
	rml_await_immediate_conf' cfg ()

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' evt p =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    let x =
	      if Event.status evt
	      then Event.value evt
	      else Event.default evt
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
	    if Event.status evt
	    then
	      let x = Event.one evt in
	      let f_body = p x in
	      f_body ()
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate_one expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_await_immediate_one' evt p ()

(**************************************)
(* await_all_match                    *)
(**************************************)
    let rml_await_all_match' evt matching p =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    let v = Event.value evt in
	    if (Event.status evt) && (matching v)
	    then
	      let x = v in
	      let f_body = p x in
	      (STOP, f_body)
	    else
	      (STOP, self)
	  else
	    (SUSP, self)
      in self

    let rml_await_all_match expr_evt matching p =
      fun () ->
	let evt = expr_evt () in
	rml_await_all_match' evt matching p ()

(**************************************)
(* await_all_match_conf               *)
(**************************************)
    let rml_await_all_match_conf' (cfg_status, cfg_value) matching p =
      let rec self =
        fun () ->
          if !eoi
          then
            let v = cfg_value () in
            if (cfg_status ()) && (matching v)
            then
              let x = v in
              let f_body = p x in
              (STOP, f_body)
            else
              (STOP, self)
          else
            (SUSP, self)
      in self

    let rml_await_all_match_conf expr_cfg matching p =
      fun () ->
        let cfg = expr_cfg () in
        rml_await_all_match_conf' cfg matching p ()

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
	    if Event.status evt
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
(* present_conf                       *)
(**************************************)
    let rml_present_conf' (cfg_status, _) p1 p2 =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, p2)
	  else
	    if cfg_status()
	    then
	      p1 ()
	    else
	      (SUSP, self)
      in self

    let rml_present_conf expr_cfg p1 p2 =
      fun () ->
	let cfg = expr_cfg() in
	rml_present_conf' cfg p1 p2 ()

(**************************************)
(* seq                                *)
(**************************************)
    let rec rml_seq p1 p2 =
      fun () ->
	match p1 () with
	| TERM _, _ -> p2 ()
	| (SUSP | STOP) as alpha, p1' -> (alpha, rml_seq p1' p2)


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
	| TERM _, TERM _ -> TERM ()
      in
      let delta_1 alpha beta =
	match alpha, beta with
	| STOP, (STOP | TERM _) -> SUSP
	| _ -> alpha
      in
      let delta_2 alpha beta =
	match alpha, beta with
	| (STOP | TERM _), STOP -> SUSP
	| _ -> beta
      in
      let rec par alpha beta p1 p2 =
	match alpha, beta with
	| SUSP, SUSP -> par_SUSP_SUSP p1 p2
	| SUSP, beta -> par_SUSP_beta beta p1 p2
	| _, SUSP -> par_alpha_SUSP alpha p1 p2
	| TERM _, TERM _ -> rml_nothing
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
	  match p () with
	  | (SUSP | STOP) as alpha, p' ->
	      (alpha, rml_seq p' self)
	  | _ -> failwith "Instantaneous loop !"
      in self

(**************************************)
(* loop_n                             *)
(**************************************)
    let rml_loop_n e p =
      let rec self n =
	fun () ->
	  if n > 0 then
	    match p () with
	    | (SUSP | STOP) as alpha, p' ->
		(alpha, rml_seq p' (self (n-1)))
	    | _ -> failwith "Instantaneous loop !"
	  else
	    (TERM (), rml_nothing)
      in
      fun () ->
	self (e()) ()

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

    let rml_signal_memory_combine default comb p =
      fun () ->
        let evt = new_evt_memory_combine (default()) (comb()) in
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
    let rec rml_def_dyn p1 p2 =
      fun () ->
	match p1 () with
	| TERM v, _ -> p2 v ()
	| (SUSP | STOP) as alpha, p1' -> alpha, rml_def_dyn p1' p2


(**************************************)
(* def_and_dyn                        *)
(**************************************)
    let rml_def_and_dyn =
      let gamma par_status alpha =
	match par_status, alpha with
	| SUSP, _ -> SUSP
	| _, SUSP -> SUSP
	| TERM _, TERM _ -> TERM ()
	| _ -> STOP
      in
      let f body_array p3 =
	let rec self =
	  fun () ->
	    let par_status = ref (TERM ()) in
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
	    | TERM _ ->
		let value_array =
		  Array.make (Array.length body_array) ()
		in
		for i = 0  to Array.length body_array - 1 do
		  match body_array.(i) with
		  | TERM v, _ -> value_array.(i) <- v
		  | _ -> assert false
		done;
		let f = p3 value_array in
		f ()
	    | SUSP -> SUSP, self
	    | STOP ->
		for i = 0 to Array.length body_array - 1 do
		  match body_array.(i) with
		  | SUSP, _ -> assert false
		  | STOP, p ->  body_array.(i) <- SUSP, p
		  | TERM _, _ -> ()
		done;
		STOP, self
	in self
      in
      fun p_array p ->
	let tab =
	  Array.make (Array.length p_array) (Obj.magic())
	in
	fun () ->
	  for i = 0 to Array.length p_array - 1 do
	    tab.(i) <- (SUSP, p_array.(i))
	  done;
	  f tab p ()



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
	  | TERM v, _ -> TERM v, rml_nothing
	  | SUSP, p' -> SUSP, until evt p'
	  | STOP, p' -> until_star evt p' ()
      and until_star evt p =
	fun () ->
	  if !eoi
	  then
	    if Event.status evt
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
(* until_conf                         *)
(**************************************)
    let rml_until_conf' =
      let rec until cfg p =
	fun () ->
	  match p() with
	  | TERM v, _ -> TERM v, rml_nothing
	  | SUSP, p' -> SUSP, until cfg p'
	  | STOP, p' -> until_star cfg p' ()
      and until_star ((cfg_status, _) as cfg) p =
	fun () ->
	  if !eoi
	  then
	    if cfg_status()
	    then
	      (STOP, rml_nothing)
	    else
	      (STOP, until cfg p)
	  else
	    (SUSP, until_star cfg p)
      in until

    let rml_until_conf expr_cfg p =
      fun () ->
	let cfg = expr_cfg () in
	rml_until_conf' cfg p ()

(**************************************)
(* until handler                      *)
(**************************************)
    let rml_until_handler' =
      let rec until evt p hdl =
	fun () ->
	  match p() with
	  | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
	  | SUSP, p' -> SUSP, until evt p' hdl
	  | STOP, p' -> until_star evt p' hdl ()
      and until_star evt p hdl =
	fun () ->
	  if !eoi
	  then
	    if Event.status evt
	    then
	      let f = hdl (Event.value evt) in
	      (STOP, f)
	    else
	      (STOP, until evt p hdl)
	  else
	    (SUSP, until_star evt p hdl)
      in until

    let rml_until_handler expr_evt p hdl =
      fun () ->
	let evt = expr_evt () in
	rml_until_handler' evt p hdl ()

(**************************************)
(* until handler match                *)
(**************************************)
    let rml_until_handler_match' =
      let rec until evt matching p hdl =
	fun () ->
	  match p() with
	  | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
	  | SUSP, p' -> SUSP, until evt matching p' hdl
	  | STOP, p' -> until_star evt matching p' hdl ()
      and until_star evt matching p hdl =
	fun () ->
	  if !eoi
	  then
	    let v = Event.value evt in
	    if Event.status evt && matching v
	    then
	      let f = hdl v in
	      (STOP, f)
	    else
	      (STOP, until evt matching p hdl)
	  else
	    (SUSP, until_star evt matching p hdl)
      in until

    let rml_until_handler_match expr_evt matching p hdl =
      fun () ->
	let evt = expr_evt () in
	rml_until_handler_match' evt matching p hdl ()


    let rml_until_handler_match_conf' =
      let rec until cfg matching p hdl =
        fun () ->
          match p() with
          | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
          | SUSP, p' -> SUSP, until cfg matching p' hdl
          | STOP, p' -> until_star cfg matching p' hdl ()
      and until_star ((cfg_status, cfg_value) as cfg) matching p hdl =
        fun () ->
          if !eoi
          then
            let v = cfg_value () in
            if cfg_status () && matching v
            then
              let f = hdl v in
              (STOP, f)
            else
              (STOP, until cfg matching p hdl)
          else
            (SUSP, until_star cfg matching p hdl)
      in until

    let rml_until_handler_conf expr_cfg p hdl =
      fun () ->
        let cfg = expr_cfg () in
        rml_until_handler_match_conf' cfg (fun _ -> true) p hdl ()

    let rml_until_handler_match_conf expr_cfg matching p hdl =
      fun () ->
        let cfg = expr_cfg () in
        rml_until_handler_match_conf' cfg matching p hdl ()

    let rml_until_match expr_evt matching p =
      rml_until_handler_match expr_evt matching p (fun _ -> rml_nothing)

    let rml_until_match' evt matching p =
      rml_until_handler_match' evt matching p (fun _ -> rml_nothing)

    let rml_until_match_conf expr_cfg matching p =
      rml_until_handler_match_conf expr_cfg matching p (fun _ -> rml_nothing)


(**************************************)
(* control                            *)
(**************************************)
    let rml_control_aux cond =
      let rec active evt p =
	fun () ->
	  match p () with
	  | TERM v, _ -> TERM v, rml_compute (fun () -> v)
	  | SUSP, p' ->
	      if !eoi then
		if cond evt
		then
		  (STOP, suspended evt p')
		else
		  (STOP, active evt p')
	      else
		(SUSP, active evt p')
	  | STOP, p' ->
	      if !eoi then
		if cond evt
		then
		  (STOP, suspended evt p')
		else
		  (STOP, active evt p')
	      else
		(SUSP, active_await evt p')
      and active_await evt p =
	fun () ->
	  if !eoi then
	    if cond evt
	    then
	      (STOP, suspended evt p)
	    else
	      (STOP, active evt p)
	  else (SUSP, active_await evt p)
      and suspended evt p =
	fun () ->
	  if !eoi then
	    if cond evt
	    then
	      (STOP, active evt p)
	    else
	      (STOP, suspended evt p)
	  else (SUSP, suspended evt p)
      in active

    let rml_control' evt p =
      rml_control_aux (fun evt -> Event.status evt) evt p

    let rml_control expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_control' evt p ()


(**************************************)
(* control_match                      *)
(**************************************)
    let rml_control_match' evt matching p =
      rml_control_aux
	(fun evt -> Event.status evt && matching (Event.value evt)) evt p

    let rml_control_match expr_evt matching p =
      fun () ->
	let evt = expr_evt () in
	rml_control_match' evt matching p ()

    let rml_control_match_conf' cfg matching p =
      rml_control_aux
        (fun (cfg_status, cfg_value) ->
          cfg_status () && matching (cfg_value ()))
        cfg p

    let rml_control_match_conf expr_cfg matching p =
      fun () ->
        let cfg = expr_cfg () in
        rml_control_match_conf' cfg matching p ()

(**************************************)
(* control_conf                       *)
(**************************************)
    let rml_control_conf' =
      let rec active ((cfg_status, _) as cfg) p =
	fun () ->
	  match p () with
	  | TERM v, _ -> TERM v, rml_compute (fun () -> v)
	  | SUSP, p' ->
	      if !eoi then
		if cfg_status()
		then
		  (STOP, suspended cfg p')
		else
		  (STOP, active cfg p')
	      else
		(SUSP, active cfg p')
	  | STOP, p' ->
	      if !eoi then
		if cfg_status()
		then
		  (STOP, suspended cfg p')
		else
		  (STOP, active cfg p')
	      else
		(SUSP, active_await cfg p')
      and active_await ((cfg_status, _) as cfg) p =
	fun () ->
	  if !eoi then
	    if cfg_status ()
	    then
	      (STOP, suspended cfg p)
	    else
	      (STOP, active cfg p)
	  else (SUSP, active_await cfg p)
      and suspended ((cfg_status, _) as cfg) p =
	fun () ->
	  if !eoi then
	    if cfg_status ()
	    then
	      (STOP, active cfg p)
	    else
	      (STOP, suspended cfg p)
	  else (SUSP, suspended cfg p)
      in active


    let rml_control_conf expr_cfg p =
      fun () ->
	let cfg = expr_cfg () in
	rml_control_conf' cfg p ()

(**************************************)
(* when                               *)
(**************************************)
    let rec rml_when' evt p =
      let rec self =
	fun () ->
	  if Event.status evt
	  then
	    match p() with
	    | TERM v, _ -> TERM v, rml_compute (fun () -> v)
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
(* when_conf                          *)
(**************************************)
    let rec rml_when_conf' ((cfg_status, _) as cfg) p =
      let rec self =
	fun () ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if cfg_status()
	    then
	      match p() with
	      | TERM v, _ -> TERM v, rml_compute (fun () -> v)
	      | alpha, p' -> alpha, rml_when_conf' cfg p'
	    else
	      (SUSP, self)
      in self

    let rml_when_conf expr_cfg p =
      fun () ->
	let cfg = expr_cfg () in
	rml_when_conf' cfg p ()

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
	  TERM (), rml_nothing


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
	  else TERM (), rml_nothing
      in
      let f_for_init =
	fun () ->
	  let i = ref (e1()) in
	  let v2 = e2() in
	  if cmp !i v2
	  then rml_seq (p !i) (f_for i v2) ()
	  else TERM (), rml_nothing
      in
      f_for_init


(**************************************)
(* par_n                              *)
(**************************************)

    let gamma par_status alpha =
      match par_status, alpha with
      | SUSP, _ -> SUSP
      | _, SUSP -> SUSP
      | TERM _, TERM _ -> TERM ()
      | _ -> STOP

    let par_body body_array =
      let rec self =
	fun () ->
	  let par_status = ref (TERM ()) in
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
	  | TERM _ -> TERM (), rml_nothing
	  | SUSP -> SUSP, self
	  | STOP ->
	      for i = 0 to Array.length body_array - 1 do
		match body_array.(i) with
		| SUSP, _ -> assert false
		| STOP, p ->  body_array.(i) <- SUSP, p
		| TERM _, _ -> ()
	      done;
	      STOP, self
      in self

    let rml_par_n p_list =
      fun () ->
	let n = List.length p_list in
	let tab =
	  Array.make n (Obj.magic())
	in
	let _ =
	  List.fold_left (fun i p -> tab.(i) <- (SUSP, p); i+1) 0 p_list
	in
	par_body tab ()

(**************************************)
(* for_dopar                          *)
(**************************************)
    let rml_fordopar e1 e2 dir p =
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
	par_body tab ()


(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)
    let rml_await expr_evt =
      fun () ->
	let evt = expr_evt () in
	rml_seq (rml_await_immediate' evt) rml_pause ()

    let rml_await' evt =
      rml_seq (rml_await_immediate' evt) rml_pause

    let rml_await_all expr_evt p =
      fun () ->
	let evt = expr_evt () in
	rml_seq (rml_await_immediate' evt) (rml_get' evt p) ()

    let rml_await_all' evt p =
      rml_seq (rml_await_immediate' evt) (rml_get' evt p)

    let rml_await_all_conf expr_cfg p =
      rml_until_handler_conf expr_cfg rml_halt p

    let rml_await_one expr_evt p =
      let pause_p x =
	rml_seq rml_pause (p x)
      in
      fun () ->
	let evt = expr_evt () in
	rml_await_immediate_one' evt pause_p ()

    let rml_await_one' evt p =
      let pause_p x =
	rml_seq rml_pause (p x)
      in
      rml_await_immediate_one' evt pause_p

    let rml_await_conf expr_cfg =
      fun () ->
	let cfg = expr_cfg () in
	rml_seq (rml_await_immediate_conf' cfg) rml_pause ()

    let rml_await_one_match expr_evt matching p =
      rml_await_all_match expr_evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)

    let rml_await_one_match' evt matching p =
      rml_await_all_match' evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)


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
(* rml_make                                       *)
(**************************************************)
    let rml_make (p: 'a process) =
      let current = ref (p()) in
      let rml_react () =
	match sched !current with
	| STOP, p' ->
	    Event.next ();
	    current := p';
	    eoi := false;
	    move := false;
	    None
	| TERM v, _ -> Some v
	| SUSP, _ -> assert false
      in rml_react

(**************************************************)
(* rml_make_unit                                  *)
(**************************************************)
    let rml_make_unit (p: unit process) =
      let current = ref (p()) in
      let rml_react () =
	match sched !current with
	| STOP, p' ->
	    Event.next ();
	    current := p';
	    eoi := false;
	    move := false;
	    None
	| TERM v, _ -> Some v
	| SUSP, _ -> assert false
      in
      let rml_add_process p =
	current := rml_par (p()) !current
      in
      rml_react, rml_add_process

(**************************************************)
(* rml_make_exec_process                          *)
(**************************************************)
    let rml_make_exec_process (p: unit process) =
      let current = ref (p()) in
      let rml_add_process p =
	current := rml_par (p()) !current
      in
      let rml_react proc_list =
	List.iter rml_add_process proc_list;
	match sched !current with
	| STOP, p' ->
	    Event.next ();
	    current := p';
	    eoi := false;
	    move := false;
	    None
	| TERM v, _ -> Some ()
	| SUSP, _ -> assert false
      in
      rml_react

  end (* Module Rml_interpreter *)
