type 'a behaviour =
    O of 'a
  | P of 'a behaviour list
  | S of 'a behaviour * 'a behaviour
  | N
type 'a stack = {
  mutable s_stack : ('a * 'a behaviour) list;
  mutable s_next_b : 'a behaviour list;
  mutable s_ttl : int;
}
val error_on_unexpected : bool
val unexpected_error_code : int
val error_on_unfired : bool
val unfired_error_code : int
val print_debug : bool
val print_status : bool
val debug : string -> unit
val debug_int : string -> int -> unit
val status : string -> unit
val status_int : string -> int -> unit
val checkers : int stack list ref
val mk_stack : 'a behaviour list -> int -> 'a stack
val check_empty : 'a stack -> unit
val add_to_stack : int stack -> int behaviour -> unit
val step_stack : int stack -> unit
val act : int stack -> int -> unit
val mk_checker : int behaviour list -> int -> int -> unit
val step : unit -> unit
