(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 23/09/2005                                      *)
(* Fichier : rmltop_global.ml                                         *)
(*                                                                    *)
(**********************************************************************)

open Implantation;;

type 'a rml_process = 'a Lco_ctrl_tree_record.process

let sampling = ref 0.01

let sampled = ref None
let suspend_resume = ref None
let step_by_step = ref None
let step = ref None
let add = ref None

let global_mutex = Mutex.create ()
let lock () = Mutex.lock global_mutex
let unlock () = Mutex.unlock global_mutex

let rml_nothing =  
  fun () -> Lco_ctrl_tree_record.rml_nothing

let rml_halt = 
(*  (fun () -> Lco_ctrl_tree_record.rml_loop Lco_ctrl_tree_record.rml_pause) *)
  fun () -> Lco_ctrl_tree_record.rml_halt

let combine_process p q =
  fun () ->
    Lco_ctrl_tree_record.rml_par
      (Lco_ctrl_tree_record.rml_run (fun () -> q))
      (Lco_ctrl_tree_record.rml_run (fun () -> p))

let rml_react, add_process = 
  let step, add =
    Lco_ctrl_tree_record.rml_make_unit rml_halt
  in
  (fun () -> ignore (step())),
  add
