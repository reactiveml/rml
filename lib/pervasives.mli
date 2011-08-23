open Implem;;

exception Match_failure of (string * int * int)
;;


exception Assert_failure of (string * int * int)
;;


exception Invalid_argument of string
;;


exception Failure of string
;;


exception Not_found
;;


exception Out_of_memory
;;


exception Stack_overflow
;;


exception Sys_error of string
;;


exception End_of_file
;;


exception Division_by_zero
;;


exception Sys_blocked_io
;;


exception Undefined_recursive_module of (string * int * int)
;;


val Pervasives.raise : (exn -> 'a)
;;


val Pervasives.invalid_arg : (string -> 'a)
;;


val Pervasives.failwith : (string -> 'a)
;;


exception Pervasives.Exit
;;


val Pervasives.(=) : ('a -> ('a -> bool))
;;


val Pervasives.(<>) : ('a -> ('a -> bool))
;;


val Pervasives.(<) : ('a -> ('a -> bool))
;;


val Pervasives.(>) : ('a -> ('a -> bool))
;;


val Pervasives.(<=) : ('a -> ('a -> bool))
;;


val Pervasives.(>=) : ('a -> ('a -> bool))
;;


val Pervasives.compare : ('a -> ('a -> int))
;;


val Pervasives.min : ('a -> ('a -> 'a))
;;


val Pervasives.max : ('a -> ('a -> 'a))
;;


val Pervasives.(==) : ('a -> ('a -> bool))
;;


val Pervasives.(!=) : ('a -> ('a -> bool))
;;


val Pervasives.not : (bool -> bool)
;;


val Pervasives.(&&) : (bool -> (bool -> bool))
;;


val Pervasives.(&) : (bool -> (bool -> bool))
;;


val Pervasives.(or) : (bool -> (bool -> bool))
;;


val Pervasives.(~-) : (int -> int)
;;


val Pervasives.succ : (int -> int)
;;


val Pervasives.pred : (int -> int)
;;


val Pervasives.(+) : (int -> (int -> int))
;;


val Pervasives.(-) : (int -> (int -> int))
;;


val Pervasives.( * ) : (int -> (int -> int))
;;


val Pervasives.(/) : (int -> (int -> int))
;;


val Pervasives.(mod) : (int -> (int -> int))
;;


val Pervasives.abs : (int -> int)
;;


val Pervasives.max_int : int
;;


val Pervasives.min_int : int
;;


val Pervasives.land : (int -> (int -> int))
;;


val Pervasives.lor : (int -> (int -> int))
;;


val Pervasives.(lxor) : (int -> (int -> int))
;;


val Pervasives.(lnot) : (int -> int)
;;


val Pervasives.(lsl) : (int -> (int -> int))
;;


val Pervasives.(lsr) : (int -> (int -> int))
;;


val Pervasives.(asr) : (int -> (int -> int))
;;


val Pervasives.(~-.) : (float -> float)
;;


val Pervasives.(+.) : (float -> (float -> float))
;;


val Pervasives.(-.) : (float -> (float -> float))
;;


val Pervasives.( *. ) : (float -> (float -> float))
;;


val Pervasives.(/.) : (float -> (float -> float))
;;


val Pervasives.( ** ) : (float -> (float -> float))
;;


val Pervasives.sqrt : (float -> float)
;;


val Pervasives.exp : (float -> float)
;;


val Pervasives.log : (float -> float)
;;


val Pervasives.log10 : (float -> float)
;;


val Pervasives.cos : (float -> float)
;;


val Pervasives.sin : (float -> float)
;;


val Pervasives.tan : (float -> float)
;;


val Pervasives.acos : (float -> float)
;;


val Pervasives.asin : (float -> float)
;;


val Pervasives.atan : (float -> float)
;;


val Pervasives.atan2 : (float -> (float -> float))
;;


val Pervasives.cosh : (float -> float)
;;


val Pervasives.sinh : (float -> float)
;;


val Pervasives.tanh : (float -> float)
;;


val Pervasives.ceil : (float -> float)
;;


val Pervasives.floor : (float -> float)
;;


val Pervasives.abs_float : (float -> float)
;;


val Pervasives.mod_float : (float -> (float -> float))
;;


val Pervasives.frexp : (float -> (float * int))
;;


val Pervasives.ldexp : (float -> (int -> float))
;;


val Pervasives.modf : (float -> (float * float))
;;


val float : (int -> float)
;;


val Pervasives.float_of_int : (int -> float)
;;


val Pervasives.truncate : (float -> int)
;;


val Pervasives.int_of_float : (float -> int)
;;


val Pervasives.infinity : float
;;


val Pervasives.neg_infinity : float
;;


val Pervasives.nan : float
;;


val Pervasives.max_float : float
;;


val Pervasives.min_float : float
;;


val Pervasives.epsilon_float : float
;;


type  Pervasives.fpclass
=
  Pervasives.FP_normal | 
  Pervasives.FP_subnormal | 
  Pervasives.FP_zero |  Pervasives.FP_infinite |  Pervasives.FP_nan ;;


val Pervasives.classify_float : (float -> Pervasives.fpclass)
;;


val Pervasives.(^) : (string -> (string -> string))
;;


val Pervasives.int_of_char : (char -> int)
;;


val Pervasives.char_of_int : (int -> char)
;;


val Pervasives.ignore : ('a -> unit)
;;


val Pervasives.string_of_bool : (bool -> string)
;;


val Pervasives.bool_of_string : (string -> bool)
;;


val Pervasives.string_of_int : (int -> string)
;;


val Pervasives.int_of_string : (string -> int)
;;


val Pervasives.string_of_float : (float -> string)
;;


val Pervasives.float_of_string : (string -> float)
;;


val Pervasives.fst : (('a * 'b) -> 'a)
;;


val Pervasives.snd : (('a * 'b) -> 'b)
;;


val Pervasives.(@) : (('a) list -> (('a) list -> ('a) list))
;;


type  Pervasives.in_channel
 ;;


type  Pervasives.out_channel
 ;;


val Pervasives.stdin : Pervasives.in_channel
;;


val Pervasives.stdout : Pervasives.out_channel
;;


val Pervasives.stderr : Pervasives.out_channel
;;


val Pervasives.print_char : (char -> unit)
;;


val Pervasives.print_string : (string -> unit)
;;


val Pervasives.print_int : (int -> unit)
;;


val Pervasives.print_float : (float -> unit)
;;


val Pervasives.print_endline : (string -> unit)
;;


val Pervasives.print_newline : (unit -> unit)
;;


val Pervasives.prerr_char : (char -> unit)
;;


val Pervasives.prerr_string : (string -> unit)
;;


val Pervasives.prerr_int : (int -> unit)
;;


val Pervasives.prerr_float : (float -> unit)
;;


val Pervasives.prerr_endline : (string -> unit)
;;


val Pervasives.prerr_newline : (unit -> unit)
;;


val Pervasives.read_line : (unit -> string)
;;


val Pervasives.read_int : (unit -> int)
;;


val Pervasives.read_float : (unit -> float)
;;


type  Pervasives.open_flag
=
  Pervasives.Open_rdonly | 
  Pervasives.Open_wronly | 
  Pervasives.Open_append | 
  Pervasives.Open_creat | 
  Pervasives.Open_trunc | 
  Pervasives.Open_excl | 
  Pervasives.Open_binary |  Pervasives.Open_text |  Pervasives.Open_nonblock
  ;;


val Pervasives.open_out : (string -> Pervasives.out_channel)
;;


val Pervasives.open_out_bin : (string -> Pervasives.out_channel)
;;


val Pervasives.open_out_gen :
((Pervasives.open_flag) list -> (int -> (string -> Pervasives.out_channel)))
;;


val Pervasives.flush : (Pervasives.out_channel -> unit)
;;


val Pervasives.flush_all : (unit -> unit)
;;


val Pervasives.output_char : (Pervasives.out_channel -> (char -> unit))
;;


val Pervasives.output_string : (Pervasives.out_channel -> (string -> unit))
;;


val Pervasives.output :
(Pervasives.out_channel -> (string -> (int -> (int -> unit))))
;;


val Pervasives.output_byte : (Pervasives.out_channel -> (int -> unit))
;;


val Pervasives.output_binary_int : (Pervasives.out_channel -> (int -> unit))
;;


val Pervasives.output_value : (Pervasives.out_channel -> ('a -> unit))
;;


val Pervasives.seek_out : (Pervasives.out_channel -> (int -> unit))
;;


val Pervasives.pos_out : (Pervasives.out_channel -> int)
;;


val Pervasives.out_channel_length : (Pervasives.out_channel -> int)
;;


val Pervasives.close_out : (Pervasives.out_channel -> unit)
;;


val Pervasives.close_out_noerr : (Pervasives.out_channel -> unit)
;;


val Pervasives.set_binary_mode_out :
(Pervasives.out_channel -> (bool -> unit))
;;


val Pervasives.open_in : (string -> Pervasives.in_channel)
;;


val Pervasives.open_in_bin : (string -> Pervasives.in_channel)
;;


val Pervasives.open_in_gen :
((Pervasives.open_flag) list -> (int -> (string -> Pervasives.in_channel)))
;;


val Pervasives.input_char : (Pervasives.in_channel -> char)
;;


val Pervasives.input_line : (Pervasives.in_channel -> string)
;;


val Pervasives.input :
(Pervasives.in_channel -> (string -> (int -> (int -> int))))
;;


val Pervasives.really_input :
(Pervasives.in_channel -> (string -> (int -> (int -> unit))))
;;


val Pervasives.input_byte : (Pervasives.in_channel -> int)
;;


val Pervasives.input_binary_int : (Pervasives.in_channel -> int)
;;


val Pervasives.input_value : (Pervasives.in_channel -> 'a)
;;


val Pervasives.seek_in : (Pervasives.in_channel -> (int -> unit))
;;


val Pervasives.pos_in : (Pervasives.in_channel -> int)
;;


val Pervasives.in_channel_length : (Pervasives.in_channel -> int)
;;


val Pervasives.close_in : (Pervasives.in_channel -> unit)
;;


val Pervasives.close_in_noerr : (Pervasives.in_channel -> unit)
;;


val Pervasives.set_binary_mode_in : (Pervasives.in_channel -> (bool -> unit))
;;


type 'a Pervasives.ref
= { mutable Pervasives.contents: 'a} ;;


val Pervasives.ref : ('a -> ('a) Pervasives.ref)
;;


val Pervasives.(!) : (('a) Pervasives.ref -> 'a)
;;


val Pervasives.(:=) : (('a) Pervasives.ref -> ('a -> unit))
;;


val Pervasives.incr : ((int) Pervasives.ref -> unit)
;;


val Pervasives.decr : ((int) Pervasives.ref -> unit)
;;


type ('a,  'b,  'c,  'd) format4
 ;;


type ('a,  'b,  'c) Pervasives.format
= ('a, 'b, 'c, 'c) format4 ;;


val Pervasives.string_of_format : (('a, 'b, 'c, 'd) format4 -> string)
;;


val Pervasives.format_of_string :
(('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4)
;;


val Pervasives.(^^) :
(('a, 'b, 'c, 'd) format4 ->
  (('d, 'b, 'c, 'e) format4 -> ('a, 'b, 'c, 'e) format4))
;;


val Pervasives.exit : (int -> 'a)
;;


val Pervasives.at_exit : (((unit -> unit)) -> unit)
;;


val Pervasives.valid_float_lexem : (string -> string)
;;


val Pervasives.unsafe_really_input :
(Pervasives.in_channel -> (string -> (int -> (int -> unit))))
;;


val Pervasives.do_at_exit : (unit -> unit)
;;

