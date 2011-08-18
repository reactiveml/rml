type 'a behaviour =
    O of 'a
  | P of 'a behaviour list
  | S of 'a behaviour * 'a behaviour
  | N
type 'a stack = {
  s_name : string;
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
val debug : 'a stack -> string -> unit
val debug_int : 'a stack -> string -> int -> unit
val status : 'a stack -> string -> unit
val status_int : 'a stack -> string -> int -> unit
val checkers : int stack list ref
val mk_stack : string -> 'a behaviour list -> int -> 'a stack
val check_empty : int stack -> unit
val add_to_stack : int stack -> int behaviour -> unit
val step_stack : int stack -> bool
val assoc_rm : 'a -> ('a * 'b) list -> ('a * 'b) list * 'b list
val act : int stack -> int -> unit
val mk_checker : string -> int behaviour list -> (int -> unit) * int
val step : unit -> unit
val end_program : unit -> unit
