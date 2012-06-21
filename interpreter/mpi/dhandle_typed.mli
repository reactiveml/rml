module type S = sig
  type 'a handle
  type 'a cache
  type key

  val mk_cache : ('a -> 'a) -> 'a cache

  val init : 'a cache -> key -> 'a -> 'a handle
  val get : 'a cache -> 'a handle -> 'a

  val get_stored : 'a handle -> 'a
  val get_local : 'a cache -> 'a handle -> 'a
end

module Make : functor (T : Map.OrderedType) ->
  S with type key = T.t
