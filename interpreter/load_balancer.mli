
type policy = Plocal | Pround_robin
val load_balancing_policy : policy ref
val set_load_balancing_policy : string -> unit

module type S = sig
  type site

  class virtual load_balancer :
  object
    method virtual new_child : unit -> site * load_balancer
  end

  val mk_top_balancer : unit -> load_balancer
end

module Make : functor (C : Communication.S) -> S with type site = C.site
