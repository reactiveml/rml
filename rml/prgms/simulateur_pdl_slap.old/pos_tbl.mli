type 'position t
val no_info : int
val make : int -> 'position * int -> 'position t
val get : 'position t -> int -> 'position * int
val set : 'position t -> int -> 'position -> int -> unit
val update : 'position t -> int -> 'position -> int -> unit
