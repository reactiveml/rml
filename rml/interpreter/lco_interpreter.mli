(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 04/06/2004                                      *)
(* Fichier : lco_interpreter.mli                                      *)
(*                                                                    *)
(**********************************************************************)

module type S =
  sig
    
    exception RML
    exception End
	
    type ('a, 'b) event 
    and proc
	  
    val rml_exec: (unit -> proc) -> unit
    val rml_exec_n: (unit -> proc) -> int -> unit
    val rml_exec_sampling: (unit -> proc) -> float -> unit
    val rml_exec_n_sampling: (unit -> proc) -> int -> float -> unit
	  
    val rml_pre_status: (unit -> ('a, 'b) event) -> bool
    val rml_pre_value: (unit -> ('a, 'b) event) -> 'b
    val rml_expr_emit: (unit -> (unit, 'b) event) -> unit 
    val rml_expr_emit_val: (unit -> ('a, 'b) event) -> (unit -> 'a) -> unit

    val rml_global_signal: unit -> ('a, 'a list) event
    val rml_global_signal_combine: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event 

    val rml_nothing: proc
    val rml_compute: (unit -> 'a) -> proc
    val rml_pause: proc 
    val rml_emit: (unit -> (unit, 'b) event) -> proc 
    val rml_emit_val: (unit -> ('a, 'b) event) -> (unit -> 'a) -> proc
    val rml_get: (unit -> ('a, 'b) event) -> ('b -> proc) -> proc
    val rml_await: (unit -> ('a, 'b) event) -> proc
    val rml_await_immediate: (unit -> ('a, 'b) event) -> proc
    val rml_await_all: (unit -> ('a, 'b) event) -> ('b -> proc) -> proc
    val rml_await_one: (unit -> ('a , 'a list) event) -> ('a -> proc) -> proc 
    val rml_await_immediate_one: 
	(unit -> ('a , 'a list) event) -> ('a -> proc) -> proc 
    val rml_present: (unit -> ('a, 'b) event) -> proc -> proc -> proc
    val rml_seq: proc -> proc -> proc
    val rml_par: proc -> proc -> proc
    val rml_merge: proc -> proc -> proc
    val rml_loop: proc -> proc
    val rml_while: (unit -> bool) -> proc -> proc
    val rml_for: 
	(unit -> int) -> (unit -> int) -> bool -> (int -> proc) -> proc 
    val rml_signal: (('a, 'a list) event -> proc) -> proc 
    val rml_signal_combine: 
	(unit -> 'b) -> (unit -> ('a -> 'b -> 'b)) ->
	  (('a, 'b) event -> proc) -> proc 
    val rml_def: (unit -> 'a) -> ('a -> proc) -> proc
    val rml_match: (unit -> 'a) -> ('a -> proc) -> proc
    val rml_run: (unit -> unit ->proc) -> proc
    val rml_until: (unit -> ('a, 'b) event) -> proc -> proc
    val rml_until_handler: 
	(unit -> ('a, 'b) event) -> proc -> ('b -> proc) -> proc
    val rml_control: (unit -> ('a, 'b) event) -> proc -> proc
    val rml_when: (unit -> ('a, 'b) event) -> proc -> proc
    val rml_if: (unit -> bool) -> proc -> proc -> proc

(*
    val rml_par_n : proc list -> proc 
    val rml_seq_n : proc list -> proc 
*)

  end
