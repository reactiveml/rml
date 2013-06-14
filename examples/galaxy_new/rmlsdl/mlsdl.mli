
type 'a pos = { p_x : 'a; p_y : 'a; }
type 'a rect = { r_x : 'a; r_y : 'a; r_w : 'a; r_h : 'a; }
type color = Black | White | Yellow | Green | Blue

val mk_rect : 'a -> 'a -> 'a -> 'a -> 'a rect
val mk_pos : 'a -> 'a -> 'a pos
val null_int_pos : int pos
val null_float_pos : float pos

type window_desc = Fullscreen | Window of int * int
val create_window : window_desc -> unit
val update : int rect option -> unit

type keyboard_event = string
type mouse_event =
    Mleft_down
  | Mleft_up
  | Mright_down
  | Mright_up
  | Mmotion of (int * int)
val flush_key_events : unit -> string list
val flush_mouse_events : unit -> mouse_event list
val process_events : unit -> unit

type 'a viewport = { vp_tr_coords : 'a pos -> int pos; }
val create_float_viewport : float rect -> int rect -> float viewport

val create_world : int -> int -> color -> int viewport

type 'a sprite

val sprite_pos : 'a sprite -> 'a pos

val create_sprite_circle : 'a viewport -> 'a pos -> int -> color -> 'a sprite

val create_sprite_from_file : 'a viewport -> 'a pos -> string -> 'a sprite

val set_sprite_pos : 'a sprite -> 'a -> 'a -> unit
val set_sprite_zoom : 'a sprite -> float -> unit
val draw_sprite : 'a sprite -> unit


val clear_info_box : unit -> unit
val draw_info_box : string -> unit
