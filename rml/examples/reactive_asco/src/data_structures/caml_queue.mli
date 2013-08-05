(***************************************************************)
(*                        Reactive Asco                        *)
(*             http://reactiveml.org/reactive_asco             *)
(*                                                             *)
(*                                                             *)
(*  Authors: Guillaume Baudart (guillaume.baudart@ens.fr)      *)
(*           Louis Mandel (louis.mandel@lri.fr)                *)
(*                                                             *)
(***************************************************************)

type ('prio, 'a) queue
val empty : ('prio, 'a) queue
val is_empty : ('prio, 'a) queue -> bool
val push : ('a, 'b) queue -> 'a * 'b -> ('a, 'b) queue
val remove_top : ('a, 'b) queue -> ('a, 'b) queue
val pop : ('a, 'b) queue -> ('a * 'b * ('a, 'b) queue) option
