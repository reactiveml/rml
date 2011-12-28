
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  type cache
  val mk_cache : (key -> ('a, 'b) value -> ('a, 'b) value) -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
end


module Make : functor (T : DHANDLE_TYPE) -> functor (C : Communication.S) ->
  S with type ('a, 'b) value = ('a, 'b) T.value and type key = T.key
