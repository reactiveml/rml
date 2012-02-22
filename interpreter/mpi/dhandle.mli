
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  type local_fun = { c_local_value : 'a 'b. key -> ('a, 'b) value -> ('a, 'b) value; }
  type cache
  val mk_cache : local_fun -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
  val set_valid : ('a, 'b) handle -> unit
end


module Make : functor (T : DHANDLE_TYPE) ->
  S with type ('a, 'b) value = ('a, 'b) T.value and type key = T.key
