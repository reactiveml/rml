(* misc utils *)
let rec last_list l = match l with
  | [a] -> a
  | [] -> assert false
  | _::l -> last_list l

module StringMap =
  Map.Make (struct
    type t = string
    let compare = compare
  end)

type ident = int

module IdentEnv =
  Map.Make (struct
    type t = ident
    let compare = compare
  end)

let fresh_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    (!counter * Mpi.communicator_size) + Mpi.communicator_rank ()

(* definition of types used by the user *)
type 'a pos = { p_x : 'a; p_y : 'a }

type 'a rect = { r_x : 'a; r_y : 'a; r_w : 'a; r_h : 'a }

type color = Black | White | Yellow | Green | Blue | Red | Grey

let color_to_sdl_color c =
  match c with
    | Black -> (1, 1, 1)
    | White -> Sdlvideo.white
    | Yellow -> Sdlvideo.yellow
    | Green -> Sdlvideo.green
    | Blue -> Sdlvideo.blue
    | Red -> Sdlvideo.red
    | Grey -> (128, 128, 128)

let color_to_sdl s c =
  Sdlvideo.map_RGB s (color_to_sdl_color c)

let rect_to_sdl r =
  { Sdlvideo.r_x = r.r_x; Sdlvideo.r_y = r.r_y;
    Sdlvideo.r_h = r.r_h; Sdlvideo.r_w = r.r_w }

let mk_rect x y w h =
  { r_x = x; r_y = y; r_w = w; r_h = h }

let mk_pos x y =
  { p_x = x; p_y = y }

let sdlrect_of_pos p =
  Sdlvideo.rect p.p_x p.p_y 0 0

let null_int_pos = mk_pos 0 0
let null_float_pos = mk_pos 0.0 0.0

(* events *)
type keyboard_event = string
type mouse_event = Mleft_down | Mleft_up | Mright_down | Mright_up | Mmotion of (int * int)
type any_event = Key_event of keyboard_event | Mouse_event of mouse_event * int pos

(* window *)
type window_desc = Fullscreen | Window of int * int

type sprite_kind =
    | Box of int * int * color (* w, h, color*)
    | Circle of int * color
    | File of string
    | Text of string * color

type screen_sprite =
    { ss_id : ident;
      ss_kind : sprite_kind;
      ss_pos : int pos;
      ss_width : int;
      ss_height : int; }

let mk_screen_sprite k w h =
  { ss_id = fresh_id ();
    ss_kind = k;
    ss_pos = null_int_pos;
    ss_height = h;
    ss_width = w }

type screen_op =
    | Init of window_desc * int (*w*) * int (*h*) * color * int (*fps*)
    | Draw of screen_sprite
    | Clear of screen_sprite

(* Mpi related functions *)

let events_tag = 10001
let screen_tag = 10002

let screen_id = 0
let program_id = 1

let receive_op () =
  (Mpi.receive Mpi.any_source screen_tag:screen_op)

let enqueue_op (op:screen_op) =
  Mpi.send op screen_id screen_tag

let receive_event () =
  (Mpi.receive Mpi.any_source events_tag:any_event)

let add_event (ev:any_event) =
  Mpi.send ev program_id events_tag
