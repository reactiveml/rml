open Mlsdl_types

type client =
    { mutable c_key_events : keyboard_event list;
      mutable c_mouse_events : (mouse_event * int pos) list;
      c_events_mutex : Mutex.t }

let client =
  { c_key_events = [];
    c_mouse_events = [];
    c_events_mutex = Mutex.create () }

(* Events *)
let flush_key_events () =
  Thread.yield ();
  Mutex.lock client.c_events_mutex;
  let l = client.c_key_events in
  client.c_key_events <- [];
  Mutex.unlock client.c_events_mutex;
  l

let flush_mouse_events () =
  Thread.yield ();
  Mutex.lock client.c_events_mutex;
  let l = client.c_mouse_events in
  client.c_mouse_events <- [];
  Mutex.unlock client.c_events_mutex;
  l

let add_key_event ev =
  client.c_key_events <- ev :: client.c_key_events

let add_mouse_event ev =
  client.c_mouse_events <- ev :: client.c_mouse_events

let add_any_event ev = match ev with
  | Key_event kev -> add_key_event kev
  | Mouse_event (mev, pos) -> add_mouse_event (mev, pos)

let listen_events () =
  try
    while true do
      let ev = receive_event () in
      Mutex.lock client.c_events_mutex;
      add_any_event ev;
      Mutex.unlock client.c_events_mutex
    done
  with
    | _ -> Format.eprintf "Listening to screen ops crashed @."; exit 2

let start_screen wd w h color fps =
  ignore (Thread.create listen_events ());
  enqueue_op (Init (wd, w, h, color, fps))

(* Viewports *)

type 'a viewport =
    { vp_tr_coords : 'a pos -> int pos;
      vp_tr_coords_reverse : int pos -> 'a pos }

let create_float_viewport vp_rect screen_rect =
  let tr_coords pos =
    let newdx = ((pos.p_x -. vp_rect.r_x) /. vp_rect.r_w) *. (float_of_int screen_rect.r_w) in
    let newx = screen_rect.r_x + (int_of_float newdx) in
    let newdy = ((pos.p_y -. vp_rect.r_y) /. vp_rect.r_h) *. (float_of_int screen_rect.r_h) in
    let newy = screen_rect.r_y + (int_of_float newdy) in
    mk_pos newx newy
  in
  let tr_coords_reverse pos =
    let newdx = ((float_of_int (pos.p_x - screen_rect.r_x)) /.
                    (float_of_int screen_rect.r_w)) *. vp_rect.r_w in
    let newx = vp_rect.r_x +. newdx in
    let newdy = ((float_of_int (pos.p_y - screen_rect.r_y)) /.
                    (float_of_int screen_rect.r_h)) *. vp_rect.r_h in
    let newy = vp_rect.r_y +. newdy in
    mk_pos newx newy
  in
  { vp_tr_coords = tr_coords; vp_tr_coords_reverse = tr_coords_reverse }

let create_movable_float_viewport vp_rect_ref screen_rect =
  let tr_coords pos =
    let vp_rect = !vp_rect_ref in
    let newdx = ((pos.p_x -. vp_rect.r_x) /. vp_rect.r_w) *. (float_of_int screen_rect.r_w) in
    let newx = screen_rect.r_x + (int_of_float newdx) in
    let newdy = ((pos.p_y -. vp_rect.r_y) /. vp_rect.r_h) *. (float_of_int screen_rect.r_h) in
    let newy = screen_rect.r_y + (int_of_float newdy) in
    mk_pos newx newy
  in
  let tr_coords_reverse pos =
    let vp_rect = !vp_rect_ref in
    let newdx = ((float_of_int (pos.p_x - screen_rect.r_x)) /.
                    (float_of_int screen_rect.r_w)) *. vp_rect.r_w in
    let newx = vp_rect.r_x +. newdx in
    let newdy = ((float_of_int (pos.p_y - screen_rect.r_y)) /.
                    (float_of_int screen_rect.r_h)) *. vp_rect.r_h in
    let newy = vp_rect.r_y +. newdy in
    mk_pos newx newy
  in
  { vp_tr_coords = tr_coords; vp_tr_coords_reverse = tr_coords_reverse }

let identity_viewport =
  let id p = p in
  { vp_tr_coords = id; vp_tr_coords_reverse = id }

let translate_pos p vp =
  vp.vp_tr_coords_reverse p

(* Sprites *)
type 'a sprite =
    { sp_screen_sprite : screen_sprite;
      sp_viewport : 'a viewport;
      mutable sp_moved : bool;
      mutable sp_pos : 'a pos;
      mutable sp_zoom : float; }

let mk_sprite k vp pos w h =
  { sp_screen_sprite = mk_screen_sprite k w h;
    sp_viewport = vp;
    sp_pos = pos;
    sp_moved = true;
    sp_zoom = 1.0 }

let sprite_pos sp =
  sp.sp_pos

let create_sprite_circle vp pos rad color =
  mk_sprite (Circle (rad, color)) vp pos (2*rad) (2*rad)

let create_sprite_box vp pos w h color =
  mk_sprite (Box (w, h, color)) vp pos w h

let create_sprite_from_file vp pos w h f =
(*  let s =
    try
      Sdlloader.load_image f
    with _ ->
      Format.eprintf "Error reading %s@." f; assert false
  in
  Format.eprintf "Create sprite from file %s@." f;
  let s_info = Sdlvideo.surface_info s in
  Format.eprintf "Create sprite from file %s@." f;
  mk_sprite (File f) vp pos s_info.Sdlvideo.w s_info.Sdlvideo.h
  *)
  mk_sprite (File f) vp pos w h

let set_sprite_pos sp x y =
  sp.sp_pos <- mk_pos x y;
  sp.sp_moved <- true

let set_sprite_zoom sp z =
  sp.sp_zoom <- z;
  sp.sp_moved <- true

let draw_sprite sp =
  if sp.sp_moved then (
    sp.sp_moved <- false;
    let pos = sp.sp_viewport.vp_tr_coords sp.sp_pos in
    let ss = { sp.sp_screen_sprite with ss_pos = pos } in
    enqueue_op (Draw ss)
  )

let clear_sprite sp =
  enqueue_op (Clear sp.sp_screen_sprite)


(** Info box *)
let infobox_pos = mk_pos 10 10
let infobox_sprite =
  let ss = mk_screen_sprite (Text (" ", Blue)) 400 40 in
  { ss with ss_pos = infobox_pos }

let clear_info_box () =
  enqueue_op (Clear infobox_sprite)

let draw_info_box s =
  let s = match s with
    | "" -> " " (* sdlttf crashes if the string is empty *)
    | _ -> s
  in
  let infobox_sprite = { infobox_sprite with ss_kind = Text (s, Blue) } in
  enqueue_op (Draw infobox_sprite)

(*
  ignore (Sdlgfx.stringRGBA !world_ref.w_buffer infobox_pos
            s Sdlvideo.blue 255)
*)

