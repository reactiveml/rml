
type policy = Plocal | Pround_robin | Puser_local | Puser_robin
val load_balancing_policy : policy ref
val set_load_balancing_policy : string -> unit

module type S = sig
  type site
  type kind = Lany | Lleaf

  class virtual load_balancer :
  object
    method virtual new_child : (int -> int * int) option -> site * kind * load_balancer
  end

  val mk_top_balancer : unit -> load_balancer
end

module Make : functor (C : Communication.S) -> S with type site = C.site
