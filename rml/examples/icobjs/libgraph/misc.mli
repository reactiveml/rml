open Lco_ctrl_tree;;

type  t
 ;;


type  load_option
 ;;


val misc_load : string -> (load_option) list -> t
;;


val misc_draw_image : t -> int -> int -> unit
;;


val misc_size : t -> (int * int)
;;

