(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 10/05/2004                                      *)
(* Fichier : lk_interpreter.mli                                       *)
(*                                                                    *)
(**********************************************************************)

module type S =
  sig
    
    exception RML
    exception End
	
    type ('a, 'b) event 
    and proc
	  
    val rml_exec: (unit -> proc -> proc) -> unit
    val rml_exec_n: (unit -> proc -> proc) -> int -> unit
    val rml_exec_sampling: (unit -> proc -> proc) -> float -> unit
    val rml_exec_n_sampling: (unit -> proc -> proc) -> int -> float -> unit
	  
    val rml_pre_status: ('a , 'b) event -> bool
    val rml_pre_value: ('a , 'b) event -> 'b

    val rml_global_signal: unit -> ('a, 'a list) event
    val rml_global_signal_combine: 
	(unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) -> ('a, 'b) event 

    val rml_term: proc
    val rml_pause: proc -> proc 
    val rml_compute: (unit -> 'a) -> proc -> proc
    val rml_emit: (unit, 'b) event -> proc -> proc
    val rml_emit_val: ('a , 'b) event -> (unit -> 'a) -> proc -> proc
    val rml_loop: proc -> proc
    val rml_while: (unit -> bool) -> proc -> proc -> proc
    val rml_for: 
	(unit -> int) -> (unit -> int) -> bool -> (int -> proc) -> proc -> proc
    val rml_par: proc -> proc -> proc -> proc
    val rml_merge: proc -> proc -> proc -> proc
    val rml_signal: (('a , 'a list) event -> proc) -> proc 
    val rml_signal_combine: 
	(unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
	  (('a , 'b) event -> proc) -> proc 
    val rml_def: (unit -> 'a) -> ('a -> proc) -> proc
    val rml_match: (unit -> 'a) -> ('a -> proc) -> proc
    val rml_run: (unit -> 'a -> proc -> proc) -> 'a -> proc -> proc
    val rml_until: ('a , 'b) event -> proc -> proc -> proc
    val rml_when: ('a , 'b) event -> proc -> proc -> proc
    val rml_control: ('a , 'b) event -> proc -> proc -> proc
    val rml_values: ('a , 'b) event -> ('b -> proc) -> proc 
    val rml_present: ('a , 'b) event -> proc -> proc -> proc 
    val rml_if: (unit -> bool) -> proc -> proc -> proc
    val rml_await: ('a , 'b) event -> proc -> proc
    val rml_await_immediate: ('a , 'b) event -> proc -> proc
    val rml_await_all: ('a , 'b) event -> ('b -> proc) -> proc
    val rml_await_one: ('a , 'a list) event -> ('a -> proc) -> proc 
    val rml_await_immediate_one: ('a , 'a list) event -> ('a -> proc) -> proc 
  end
