(* the initial scalar module : taken from Pervasive module of O'caml 1.5*)
(* WARNING : many values from the Ocaml pervasive module are not provided *)

(* the predefined types *)
(* type int  *)
(* type bool *)
(* type float  *)
(* type char *)
(* type string *)
(* type unit *)

(* integer operations *)
val (+) : int -> int -> int
val (~-) : int -> int (* prefix minus *)
val (-) : int -> int -> int
val ( * ) : int -> int -> int 
val (/) : int -> int -> int
val succ : int -> int
val pred : int -> int
val (mod): int -> int -> int
val abs : int -> int
val max_int : int
val min_int : int

(* boolean operations *)
val not : bool -> bool
val (or) : bool -> bool -> bool
val (&) : bool -> bool -> bool
val (&&) : bool -> bool -> bool

(* Bitwise operations *)

val (land) : int -> int -> int
val (lor) : int -> int -> int
val (lxor) : int -> int -> int
val lnot: int -> int
val (lsl) : int -> int -> int
val (lsr) : int -> int -> int
val (asr) : int -> int -> int

(* comparisons*)
val (<) : 'a -> 'a -> bool
val (>) : 'a -> 'a -> bool
val (<=) : 'a -> 'a -> bool
val (>=) : 'a -> 'a -> bool
val (=) : 'a -> 'a -> bool
val (<>) : 'a -> 'a -> bool

(* floats *)
val (~-.) : float -> float
val (+.) : float -> float -> float
val (-.) : float -> float -> float
val ( *. ) : float -> float -> float
val (/.) : float -> float -> float
val ( ** ) : float -> float -> float
val exp : float -> float
val acos : float -> float
val asin : float -> float
val atan : float -> float
val atan2 : float -> float -> float
val cos : float -> float
val cosh : float -> float

val log : float -> float
val log10 : float -> float

val sin : float -> float
val sinh : float -> float
val sqrt : float -> float
val tan : float -> float
val tanh : float -> float
val ceil : float -> float
val floor : float -> float
val abs_float : float -> float
val mod_float : float -> float -> float
val frexp : float -> float * int
val ldexp : float -> int -> float
val modf : float -> float * float
val float : int -> float
val truncate : float -> int

(*string operations *)

val (^) : string -> string -> string

(* string conversion functions *)

val string_of_bool : bool -> string
val string_of_int : int -> string
val int_of_string : string -> int
val string_of_float : float -> string
val float_of_string : string -> float

(* pair operations *)

val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

(** Output functions on standard output *)

val print_char : char -> unit
val print_string : string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_endline : string -> unit
val print_newline : unit -> unit

(** Output functions on standard error *)

val prerr_char : char -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_float : float -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit
val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float
