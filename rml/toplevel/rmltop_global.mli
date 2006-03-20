(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 23/09/2005                                      *)
(* Fichier : rmltop_global.mli                                        *)
(*                                                                    *)
(**********************************************************************)

type 'a rml_process = 'a Implantation.Lco_ctrl_tree_record.process

val sampling : float ref

val sampled : unit option ref
val suspend_resume : unit option ref
val step_by_step : unit option ref
val step : unit option ref
val add : unit rml_process option ref

val lock : unit -> unit
val unlock : unit -> unit

val rml_nothing : unit rml_process
val rml_halt : unit rml_process
val combine_process : unit rml_process -> unit rml_process -> unit rml_process 

val rml_react : unit -> unit
val add_process : unit rml_process -> unit
