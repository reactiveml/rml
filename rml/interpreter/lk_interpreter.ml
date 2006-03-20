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
      and 'a step
      and control_tree
      and 'a expr
      and 'a process

      val rml_compute : (unit -> 'a) -> 'a step -> 'b step
      val rml_pause : unit step -> control_tree -> 'a step
      val rml_halt : unit step -> control_tree -> 'a step
      val rml_emit_val :
        (unit -> ('a, 'b) event) -> (unit -> 'a) -> unit step -> 'c step
      val rml_emit :
        (unit -> (unit, 'a) event) -> unit step -> 'b step
      val rml_await_immediate :
        (unit -> ('a, 'b) event) -> unit step -> control_tree -> 'c step
      val rml_get :
        (unit -> ('a, 'b) event) -> ('b -> unit step) -> control_tree -> 
	  'c step
      val await_immediate_one :
        (unit -> ('a, 'a list) event) -> ('a -> unit step) -> control_tree ->
	  'b step
    end

module Lk_interpreter: Lk_interpreter =
  functor (Event: Sig_env.S) ->
  struct
    
    type ('a, 'b) event = 
	('a,'b) Event.t * unit step list ref * unit step list ref  
    and event_cfg = bool -> (unit -> bool) * unit step list ref list

    and control_tree = 
	{ kind: control_type;
	  mutable active: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable fils: control_tree list;
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
    and 'a expr = 'a step -> control_tree -> unit step
    and 'a process = unit -> 'a expr


	
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


(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create default combine, ref [], ref [])

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)

    let eoi = ref false
    let weoi = ref ([]: waiting)

    let dummy_step _ = ()

(* ------------------------------------------------------------------------ *)
    let sched () = ()

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
    let rml_halt k ctrl _ =
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

    let step_await_immediate (n,_,wp) k =
      failwith "Not yet implemented"

    let rml_await_immediate expr_evt k ctrl _ =
      if ctrl.kind = Top then 
	step_await_immediate_top (expr_evt()) k ()
      else
	step_await_immediate (expr_evt()) k ()

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

    let step_await_immediate_one (n, _, wp) f =
      failwith "Not yet implemented"

    let await_immediate_one expr_evt f ctrl _ =
      if ctrl.kind = Top then 
	step_await_immediate_one_top (expr_evt()) f ()
      else
	step_await_immediate_one (expr_evt()) f ()
  end
