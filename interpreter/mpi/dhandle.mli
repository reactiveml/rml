
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  module type LOCAL_VALUE = sig
    val local_value : key -> ('a, 'b) value -> ('a, 'b) value
  end

  type cache
  val mk_cache : (module LOCAL_VALUE) -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
  val set_valid : ('a, 'b) handle -> unit
end


module Make : functor (T : DHANDLE_TYPE) -> functor (C : Communication.S) ->
  S with type ('a, 'b) value = ('a, 'b) T.value and type key = T.key
