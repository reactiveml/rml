(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 25/10/2005                                      *)
(* Fichier : rmltop_directives.ml                                     *)
(*                                                                    *)
(**********************************************************************)

let set ref n =
  Rmltop_global.lock();
  ref := Some n;
  Rmltop_global.unlock()

let set_sampling n =  Rmltop_global.sampling := n

let set_sampled = set Rmltop_global.sampled
let set_suspend_resume = set Rmltop_global.suspend_resume 
let set_step_by_step = set Rmltop_global.step_by_step 
let set_step = set Rmltop_global.step 
let set_add = set Rmltop_global.add 
