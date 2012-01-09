type counter

val mk_counter : unit -> counter
val incr : counter -> unit
val decr : counter -> unit
val set : counter -> int -> unit

val await_zero : counter -> unit
val is_zero : counter -> bool
