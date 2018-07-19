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


module Distribution = struct
  type proba = float
  type log_proba = float

  type 'a t =
    | Dist_sampler of ((unit -> 'a) * ('a -> log_proba))
    | Dist_support of ('a * proba) list

  let bernoulli p =
    assert (0. <= p && p <= 1.);
    Dist_support [
      (true, p);
      (false, 1. -. p);
    ]

  let gaussian mu sigma =
    let two_pi = 2.0 *. 3.14159265358979323846 in
    let rec rand_pair () =
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      if u1 < epsilon_float then rand_pair ()
      else u1, u2
    in
    Dist_sampler
      ((fun () ->
          let u1, u2 = rand_pair() in
          let z = sqrt (-.2. *. log u1) *. cos (two_pi *. u2) in
          z *. sigma +. mu),
       (fun x ->
          -. 0.5 *. log (two_pi *. sigma ** 2.) -.
          (x -. mu) ** 2. /. (2. *. sigma ** 2.)))

  let draw dist =
    begin match dist with
      | Dist_sampler (sampler, _) -> sampler ()
      | Dist_support sup ->
        let sample = Random.float 1.0 in
        (* TODO data structure for more efficient sampling *)
        let rec draw sum r =
          begin match r with
            | [] -> assert false
            | (v, p) :: r ->
              let sum = sum +. p in
              if sample <= sum then v else draw sum r
          end
        in
        draw 0. sup
    end

  let score dist x =
    begin match dist with
      | Dist_sampler (_, scorer) -> scorer x
      | Dist_support sup -> log (List.assoc x sup)
    end

  let draw_and_score dist =
    begin match dist with
      | Dist_sampler (sampler, scorer) ->
        let x = sampler () in
        x, (scorer x)
      | Dist_support sup ->
        let sample = Random.float 1.0 in
        (* TODO data structure for more efficient sampling *)
        let rec draw sum r =
          begin match r with
            | [] -> assert false
            | (v, p) :: r ->
              let sum = sum +. p in
              if sample <= sum then v, (log p) else draw sum r
          end
        in
        draw  0. sup
    end

  let multivariate dists =
    Dist_sampler
      ((fun () -> List.map (fun dist -> draw dist) dists),
       (fun xs ->
          List.fold_left2
            (fun acc dist x -> acc +. ((score dist) x))
            1.0
            dists xs))

  let sph_gaussian mus sigmas =
    multivariate (List.map2 gaussian mus sigmas)

end

module Rml_interpreter (* : Lco_interpreter.S *) =
  functor (Event: Sig_env.S) ->
  struct
    exception RML

    type ('a, 'b) event = ('a, 'b) Event.t

    and 'a event_cfg = unit -> (unit -> bool) * (unit -> 'a)

    and 'a status = SUSP | STOP | TERM of 'a

    and 'a expr = state option -> 'a status * 'a expr

    and state =
      { mutable st_resample: bool;
        mutable st_score: float;
        st_id: int; }

    and 'a process = unit -> 'a expr


(* manupulation de l'état *)
    let make_state id =
      { st_resample = false;
        st_score = 0.;
        st_id = id; }

    let cp_state src id =
      { st_resample = false;
        st_score = src.st_score;
        st_id = id; }

(* Flag pour le calcul de point fixe *)
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
      fun state -> TERM (), rml_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rec rml_compute e =
      fun state ->
	let v = e() in
	(TERM v, rml_compute (fun () -> v))

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun state ->
	STOP, rml_nothing

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi = rml_pause

(**************************************)
(* halt                               *)
(**************************************)
    let rec rml_halt =
      fun state ->
	STOP, rml_halt

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rec rml_halt_kboi = rml_halt

(**************************************)
(* emit                               *)
(**************************************)
    let rml_emit_val' evt e =
      fun state ->
	move := true;
	Event.emit evt (e());
	(TERM (), rml_nothing)

    let rml_emit_val expr_evt e =
      fun state ->
	let evt = expr_evt () in
	rml_emit_val' evt e ()

    let rml_emit' evt =
      rml_emit_val' evt (fun () -> ())

    let rml_emit expr_evt =
      fun state ->
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
    let rml_await_immediate' evt : _ expr =
      let rec self =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_await_immediate' evt state

(**************************************)
(* await_immediate_conf               *)
(**************************************)
    let rml_await_immediate_conf' (cfg_status, _) =
      let rec self =
	fun state ->
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
      fun state ->
	let cfg = expr_cfg () in
	rml_await_immediate_conf' cfg state

(**************************************)
(* get                                *)
(**************************************)
    let rml_get' evt p : _ expr =
      let rec self =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_get' evt p state

(**************************************)
(* await_immediate_one                *)
(**************************************)
    let rml_await_immediate_one' evt p =
      let rec self =
	fun state ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if Event.status evt
	    then
	      let x = Event.one evt in
	      let f_body = p x in
	      f_body state
	    else
	      (SUSP, self)
      in self

    let rml_await_immediate_one expr_evt p =
      fun state ->
	let evt = expr_evt () in
	rml_await_immediate_one' evt p state

(**************************************)
(* await_all_match                    *)
(**************************************)
    let rml_await_all_match' evt matching p =
      let rec self =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_await_all_match' evt matching p state

(**************************************)
(* await_all_match_conf               *)
(**************************************)
    let rml_await_all_match_conf' (cfg_status, cfg_value) matching p =
      let rec self =
        fun state ->
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
      fun state ->
        let cfg = expr_cfg () in
        rml_await_all_match_conf' cfg matching p state

(**************************************)
(* present                            *)
(**************************************)
    let rml_present' evt p1 p2 =
      let rec self =
	fun state ->
	  if !eoi
	  then
	    (STOP, p2)
	  else
	    if Event.status evt
	    then
	      p1 state
	    else
	      (SUSP, self)
      in self

    let rml_present expr_evt p1 p2 =
      fun state ->
	let evt = expr_evt() in
	rml_present' evt p1 p2 state

(**************************************)
(* present_conf                       *)
(**************************************)
    let rml_present_conf' (cfg_status, _) p1 p2 =
      let rec self =
	fun state ->
	  if !eoi
	  then
	    (STOP, p2)
	  else
	    if cfg_status()
	    then
	      p1 state
	    else
	      (SUSP, self)
      in self

    let rml_present_conf expr_cfg p1 p2 =
      fun state ->
	let cfg = expr_cfg() in
	rml_present_conf' cfg p1 p2 state

(**************************************)
(* seq                                *)
(**************************************)
    let rec rml_seq (p1 : 'a expr) (p2 : 'b expr) : 'b expr =
      fun state ->
        match p1 state with
        | TERM _, _ ->
          move := true;
          SUSP, p2
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
	fun state ->
	  let alpha, p1' = p1 state in
	  let beta, p2' = p2 state in
	  (gamma alpha beta,
	   par (delta_1 alpha beta) (delta_2 alpha beta) p1' p2')

      and par_SUSP_beta beta p1 p2 =
	fun state ->
	  let alpha, p1' = p1 state in
	  (gamma alpha beta,
	   par (delta_1 alpha beta) (delta_2 alpha beta) p1' p2)

      and par_alpha_SUSP alpha p1 p2 =
	fun state ->
	  let beta, p2' = p2 state in
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
	fun state ->
	  match p state with
	  | (SUSP | STOP) as alpha, p' ->
	      (alpha, rml_seq p' self)
	  | _ -> failwith "Instantaneous loop !"
      in self

(**************************************)
(* loop_n                             *)
(**************************************)
    let rml_loop_n e p =
      let rec self n =
	fun state ->
	  if n > 0 then
	    match p state with
	    | (SUSP | STOP) as alpha, p' ->
		(alpha, rml_seq p' (self (n-1)))
	    | _ -> failwith "Instantaneous loop !"
	  else
	    (TERM (), rml_nothing)
      in
      fun state ->
	self (e()) state

(**************************************)
(* signal                             *)
(**************************************)
    let rml_signal p =
      fun state ->
	let evt = new_evt() in
	let f = p evt  in
	f state

    let rml_signal_combine default comb p =
      fun state ->
	let evt = new_evt_combine (default()) (comb()) in
	let f = p evt in
	f state

    let rml_signal_memory_combine default comb p =
      fun state ->
        let evt = new_evt_memory_combine (default()) (comb()) in
        let f = p evt in
        f state

(**************************************)
(* def                                *)
(**************************************)
    let rml_def e p =
      fun state ->
	let f = p (e()) in
	f state

(**************************************)
(* def_dyn                            *)
(**************************************)
    let rec rml_def_dyn p1 p2 =
      fun state ->
	match p1 state with
	| TERM v, _ -> p2 v state
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
	  fun state ->
	    let par_status = ref (TERM ()) in
	    for i = 0 to Array.length body_array - 1 do
	      let status, p = body_array.(i) in
	      match status with
	      | SUSP ->
		  let (alpha, p') as body = p state in
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
		f state
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
	fun state ->
	  for i = 0 to Array.length p_array - 1 do
	    tab.(i) <- (SUSP, p_array.(i))
	  done;
	  f tab p state



(**************************************)
(* match                              *)
(**************************************)
    let rml_match e p =
      fun state ->
	let f = p (e()) in
	f state

(**************************************)
(* run                                *)
(**************************************)
    let rml_run e =
      fun state ->
	let f = e () () in
	f state

(**************************************)
(* until                              *)
(**************************************)
    let rml_until' =
      let rec until evt p =
	fun state ->
	  match p state with
	  | TERM v, _ -> TERM v, rml_nothing
	  | SUSP, p' -> SUSP, until evt p'
	  | STOP, p' -> until_star evt p' state
      and until_star evt p =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_until' evt p state

(**************************************)
(* until_conf                         *)
(**************************************)
    let rml_until_conf' =
      let rec until cfg p =
	fun state ->
	  match p state with
	  | TERM v, _ -> TERM v, rml_nothing
	  | SUSP, p' -> SUSP, until cfg p'
	  | STOP, p' -> until_star cfg p' state
      and until_star ((cfg_status, _) as cfg) p =
	fun state ->
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
      fun state ->
	let cfg = expr_cfg () in
	rml_until_conf' cfg p state

(**************************************)
(* until handler                      *)
(**************************************)
    let rml_until_handler' =
      let rec until evt p hdl =
	fun state ->
	  match p state with
	  | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
	  | SUSP, p' -> SUSP, until evt p' hdl
	  | STOP, p' -> until_star evt p' hdl state
      and until_star evt p hdl =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_until_handler' evt p hdl state

(**************************************)
(* until handler match                *)
(**************************************)
    let rml_until_handler_match' =
      let rec until evt matching p hdl =
	fun state ->
	  match p state with
	  | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
	  | SUSP, p' -> SUSP, until evt matching p' hdl
	  | STOP, p' -> until_star evt matching p' hdl state
      and until_star evt matching p hdl =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_until_handler_match' evt matching p hdl state


    let rml_until_handler_match_conf' =
      let rec until cfg matching p hdl =
        fun state ->
          match p state with
          | TERM v, _ -> TERM v, rml_compute (fun _ -> v)
          | SUSP, p' -> SUSP, until cfg matching p' hdl
          | STOP, p' -> until_star cfg matching p' hdl state
      and until_star ((cfg_status, cfg_value) as cfg) matching p hdl =
        fun state ->
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
      fun state ->
        let cfg = expr_cfg () in
        rml_until_handler_match_conf' cfg (fun _ -> true) p hdl state

    let rml_until_handler_match_conf expr_cfg matching p hdl =
      fun state ->
        let cfg = expr_cfg () in
        rml_until_handler_match_conf' cfg matching p hdl state

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
	fun state ->
	  match p state with
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
	fun state ->
	  if !eoi then
	    if cond evt
	    then
	      (STOP, suspended evt p)
	    else
	      (STOP, active evt p)
	  else (SUSP, active_await evt p)
      and suspended evt p =
	fun state ->
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
      fun state ->
	let evt = expr_evt () in
	rml_control' evt p state


(**************************************)
(* control_match                      *)
(**************************************)
    let rml_control_match' evt matching p =
      rml_control_aux
	(fun evt -> Event.status evt && matching (Event.value evt)) evt p

    let rml_control_match expr_evt matching p =
      fun state ->
	let evt = expr_evt () in
	rml_control_match' evt matching p state

    let rml_control_match_conf' cfg matching p =
      rml_control_aux
        (fun (cfg_status, cfg_value) ->
          cfg_status () && matching (cfg_value ()))
        cfg p

    let rml_control_match_conf expr_cfg matching p =
      fun state ->
        let cfg = expr_cfg () in
        rml_control_match_conf' cfg matching p state

(**************************************)
(* control_conf                       *)
(**************************************)
    let rml_control_conf' =
      let rec active ((cfg_status, _) as cfg) p =
	fun state ->
	  match p state with
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
	fun state ->
	  if !eoi then
	    if cfg_status ()
	    then
	      (STOP, suspended cfg p)
	    else
	      (STOP, active cfg p)
	  else (SUSP, active_await cfg p)
      and suspended ((cfg_status, _) as cfg) p =
	fun state ->
	  if !eoi then
	    if cfg_status ()
	    then
	      (STOP, active cfg p)
	    else
	      (STOP, suspended cfg p)
	  else (SUSP, suspended cfg p)
      in active


    let rml_control_conf expr_cfg p =
      fun state ->
	let cfg = expr_cfg () in
	rml_control_conf' cfg p state

(**************************************)
(* when                               *)
(**************************************)
    let rec rml_when' evt p =
      let rec self =
	fun state ->
	  if Event.status evt
	  then
	    match p state with
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
      fun state ->
	let evt = expr_evt () in
	rml_when' evt p state

(**************************************)
(* when_conf                          *)
(**************************************)
    let rec rml_when_conf' ((cfg_status, _) as cfg) p =
      let rec self =
	fun state ->
	  if !eoi
	  then
	    (STOP, self)
	  else
	    if cfg_status()
	    then
	      match p state with
	      | TERM v, _ -> TERM v, rml_compute (fun () -> v)
	      | alpha, p' -> alpha, rml_when_conf' cfg p'
	    else
	      (SUSP, self)
      in self

    let rml_when_conf expr_cfg p =
      fun state ->
	let cfg = expr_cfg () in
	rml_when_conf' cfg p state

(**************************************)
(* if                                 *)
(**************************************)
    let rml_if e p1 p2 =
      fun state ->
	if e() then
	  p1 state
	else
	  p2 state

(**************************************)
(* while                              *)
(**************************************)
    let rec rml_while e p =
      fun state ->
	if e() then
	  rml_seq p (rml_while e p) state
	else
	  TERM (), rml_nothing


(**************************************)
(* for                                *)
(**************************************)
    let rml_for e1 e2 dir p =
      let (succ, cmp) = if dir then succ, (<=) else pred, (>=) in
      let rec f_for i v2 =
	fun state ->
	  let i = succ i in
	  if cmp i v2
	  then rml_seq (p i) (f_for i v2) state
	  else TERM (), rml_nothing
      in
      let f_for_init =
	fun state ->
	  let i = e1() in
	  let v2 = e2() in
	  if cmp i v2
	  then rml_seq (p i) (f_for i v2) state
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

    let rec par_body body_list =
      fun state ->
        let par_status = ref (TERM ()) in
        let body_list =
          List.map
            (fun ((status, p) as body) ->
               match status with
               | SUSP ->
                 let (alpha, p') as body = p state in
                 par_status := gamma !par_status alpha;
                 body
               | _ ->
                 par_status := gamma !par_status status;
                 body)
            body_list
        in
        match !par_status with
        | TERM _ -> TERM (), rml_nothing
        | SUSP -> SUSP, par_body body_list
        | STOP ->
          let body_list =
            List.map
              (fun body ->
                 match body with
                 | SUSP, _ -> assert false
                 | STOP, p ->  SUSP, p
                 | TERM _, _ -> body)
              body_list
          in
          STOP, par_body body_list

    let rml_par_n p_list =
      fun state ->
        let body_list =
          List.map (fun p -> (SUSP, p)) p_list
        in
        par_body body_list state

(**************************************)
(* for_dopar                          *)
(**************************************)
    let rml_fordopar e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun state ->
	let i = ref (e1()) in
	let v2 = e2() in
	let body_list = ref [] in
	while (cmp !i v2) do
	  body_list := (SUSP, (p !i)) :: !body_list;
	  incr i
	done;
	par_body (List.rev !body_list) state


(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)
    let rml_await expr_evt =
      fun state ->
	let evt = expr_evt () in
	rml_seq (rml_await_immediate' evt) rml_pause state

    let rml_await' evt =
      rml_seq (rml_await_immediate' evt) rml_pause

    let rml_await_all expr_evt p =
      fun state ->
	let evt = expr_evt () in
	rml_seq (rml_await_immediate' evt) (rml_get' evt p) state

    let rml_await_all' evt p : _ expr =
      rml_seq (rml_await_immediate' evt) (rml_get' evt p)

    let rml_await_all_conf expr_cfg p =
      rml_until_handler_conf expr_cfg rml_halt p

    let rml_await_one expr_evt p =
      let pause_p x =
	rml_seq rml_pause (p x)
      in
      fun state ->
	let evt = expr_evt () in
	rml_await_immediate_one' evt pause_p state

    let rml_await_one' evt p =
      let pause_p x =
	rml_seq rml_pause (p x)
      in
      rml_await_immediate_one' evt pause_p

    let rml_await_conf expr_cfg =
      fun state ->
	let cfg = expr_cfg () in
	rml_seq (rml_await_immediate_conf' cfg) rml_pause state

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
(* sample                             *)
(**************************************)

    let rml_sample_v dist =
      fun state ->
        let v = Distribution.draw dist in
        TERM v, rml_compute (fun _ -> v)

    let rml_sample dist_expr =
      fun state ->
        let dist = dist_expr () in
        rml_sample_v dist state

(**************************************)
(* factor                             *)
(**************************************)

    let rml_factor_v score =
      fun state ->
        let state =
          match state with
          | Some s -> s
          | None -> make_state (-1)
        in
        state.st_score <- state.st_score +. score;
        state.st_resample <- true;
        move := true;
        TERM (), rml_nothing

    let rml_factor score_expr =
      fun state ->
        let score = score_expr () in
        rml_factor_v score state

(**************************************)
(* infer                              *)
(**************************************)
    let nb_particules = 10

    let list_init n f =
      let rec loop i =
        if i < n then
          f i :: loop (i + 1)
        else
          []
      in
      if n < 0 then
        raise (Invalid_argument "list_init")
      else
        loop 0

    let rec list_replace_assoc x f l =
      begin match l with
      | [] -> [ (x, f None) ]
      | (y, v) :: l ->
          if x = y then
            (x, f (Some v)) :: l
          else
            (y, v) :: list_replace_assoc x f l
      end

    let do_resample body_list =
      let at_least_one = ref false in
      let susp =
        List.for_all
          (fun (status, _, state) ->
             match status with
             | SUSP ->
                 at_least_one := !at_least_one || state.st_resample;
                 state.st_resample
             | STOP | TERM _ -> true)
          body_list
      in
      !at_least_one && susp

    let resample particules =
      if do_resample particules then
        let weights, norm =
          List.fold_left
            (fun (acc, sum) ((status, p, state) as body) ->
               let w = max (exp state.st_score) epsilon_float in
               ((body, w) :: acc, sum +. w))
            ([], 0.) particules
        in
        let dist =
          Distribution.Dist_support
            (List.map (fun (b, w) -> (b, w /. norm)) weights)
        in
        move := true;
        List.map
          (fun (_, _, old_state) ->
             let (status, p, state) = Distribution.draw dist in
             (status, p, cp_state state old_state.st_id))
          particules
      else
        particules

    let normalize particules =
      let norm = float (List.length particules) in
      let return_histogram =
        List.fold_left
          (fun acc (status, _, _) ->
             begin match status with
             | TERM v ->
               list_replace_assoc v
                 (function None -> 1
                         | Some n -> n + 1)
                 acc
             | SUSP | STOP -> raise RML
             end)
          [] particules
      in
      Distribution.Dist_support
        (List.map (fun (v, n) -> (v, float n /. norm)) return_histogram)

    let rec rml_infer_body particules =
      fun state ->
        let infer_status = ref (TERM ()) in
        let particules =
          List.mapi
            (fun i body ->
               match body with
               | SUSP, p, ({ st_resample = false} as sample_state) ->
                 let alpha, p' = p (Some sample_state) in
                 infer_status := gamma !infer_status alpha;
                 alpha, p', sample_state
               | _, _, _ ->
                 body)
            particules
        in
        let particules =
          resample particules
        in
        match !infer_status with
        | TERM _ ->
          let v = normalize particules in
          TERM v, rml_compute (fun () -> v)
        | SUSP -> SUSP, rml_infer_body particules
        | STOP ->
          let particules =
            List.map
              (fun body ->
                 match body with
                 | SUSP, _, _ -> assert false
                 | STOP, p, state ->  SUSP, p, state
                 | TERM _, _, state -> body)
              particules
          in
          STOP, rml_infer_body particules

    let rml_infer_v_v propose_s p =
      fun state ->
        let f = p () in
        let particules =
          list_init nb_particules (fun i -> (SUSP, f, make_state i))
        in
        rml_infer_body particules state

(* ------------------------------------------------------------------------ *)
(**************************************)
(* sched                              *)
(**************************************)
    let rec sched p =
      match p None with
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
