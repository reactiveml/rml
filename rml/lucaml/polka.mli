(*i $Id: polka.mli 1.2 Fri, 19 Dec 2003 15:39:58 +0100 jahier $ i*)

type dimsup = {
  pos: int;
  nbdims: int;
}

val strict : bool ref
val dec : int ref
val print_limit : int ref

val initialize : bool -> int -> int -> unit
val finalize :  unit -> unit
external set_gc : int -> unit = "camlidl_polka_set_gc"

val denominator_of_list : (int * int * string) list -> int

val to_constraint : (int -> string) -> (int -> int) -> int -> string
val to_frame : (int -> string) -> (int -> int) -> int -> string
val to_expr : (int -> string) -> (int -> int) -> int -> string


val print_list : 
  Format.formatter -> 
  (unit,Format.formatter,unit) format -> 
  (unit,Format.formatter,unit) format -> 
  (unit,Format.formatter,unit) format -> 
  (Format.formatter -> 'a -> unit) -> 'a list -> unit

type cons = Egal | SupEgal | Sup
type gen = Vertex | Ray | Line

