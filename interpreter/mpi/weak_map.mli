module type S = sig
  type key
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val clean : 'a t -> 'a t
end

module Make : functor (Ord : Map.OrderedType) -> S with type key = Ord.t
