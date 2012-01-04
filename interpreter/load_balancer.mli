module type S = sig
  type site
  type state

  val mk_top_balancer : unit -> state
  (* [new_child s] returns the site for the new child and its state. *)
  val new_child : state -> site * state
end

module Local : functor (C : Communication.S) -> S with type site = C.site
module RoundRobin : functor (C : Communication.S) -> S with type site = C.site
