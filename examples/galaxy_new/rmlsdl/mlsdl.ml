
(* misc utils *)
let rec last_list l = match l with
  | [a] -> a
  | [] -> assert false
  | _::l -> last_list l

(* internal types for the screen and the back buffer *)
type screen =
    { mutable s_surface : Sdlvideo.surface;
      mutable s_width : int;
      mutable s_height : int;
      mutable s_fullscreen : bool;
      mutable s_old_height : int;
      mutable s_old_width : int;
    }

type world =
    { w_buffer : Sdlvideo.surface;
      w_backgd_color : int32;
      mutable w_height : int;
      mutable w_width : int;
      mutable w_zoom : float }

let screen_ref = ref (Obj.magic () : screen)

let update_screen_surface screen s w h =
  screen.s_surface <- s;
  screen.s_height <- h;
  screen.s_width <- w

let world_ref = ref (Obj.magic () : world)

let infobox_font = ref (Obj.magic () : Sdlttf.font)

(* definition of types used by the user *)
type 'a pos = { p_x : 'a; p_y : 'a }

type 'a rect = { r_x : 'a; r_y : 'a; r_w : 'a; r_h : 'a }

type color = Black | White | Yellow | Green | Blue

let color_to_sdl_color c =
  match c with
    | Black -> (1, 1, 1)
    | White -> Sdlvideo.white
    | Yellow -> Sdlvideo.yellow
    | Green -> Sdlvideo.green
    | Blue -> Sdlvideo.blue

let color_to_sdl s c =
  Sdlvideo.map_RGB s (color_to_sdl_color c)

let rect_to_sdl r =
  { Sdlvideo.r_x = r.r_x; Sdlvideo.r_y = r.r_y;
    Sdlvideo.r_h = r.r_h; Sdlvideo.r_w = r.r_w }

let mk_rect x y w h =
  { r_x = x; r_y = y; r_w = w; r_h = h }

let mk_pos x y =
  { p_x = x; p_y = y }

let null_int_pos = mk_pos 0 0
let null_float_pos = mk_pos 0.0 0.0

(* Drawing and screen updating functions*)
let use_smoothing = false

let sdl_init_options = [`VIDEO; `NOPARACHUTE]
let screen_options = [`HWSURFACE; `RESIZABLE]
let fullscreen_options = [`HWSURFACE; `FULLSCREEN]

let create_surface w h =
  Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:w ~h:h ~bpp:32
    ~rmask:0l ~gmask:0l ~bmask:0l ~amask:0l

let clear_world ?(rect=None)() =
  match rect with
    | None ->
      Sdlvideo.fill_rect !world_ref.w_buffer !world_ref.w_backgd_color
    | Some rect ->
      Sdlvideo.fill_rect ~rect:rect !world_ref.w_buffer !world_ref.w_backgd_color

let update_zoom screen w =
  let zoom =
    min ((float_of_int screen.s_width) /. (float_of_int w.w_width))
      ((float_of_int screen.s_height) /. (float_of_int w.w_height))
  in
  w.w_zoom <- zoom

let ensure_unique_window =
  let has_window = ref false in
  fun () ->
    if !has_window then (
      Format.eprintf "Only one window can be created at a time@.";
      exit 2
    ) else
      has_window := true

type window_desc = Fullscreen | Window of int * int

let create_window wd =
  (* init sdl *)
  ensure_unique_window ();
  Sdl.init sdl_init_options;
  Sdlttf.init ();
  infobox_font := Sdlttf.open_font "Helvetica.ttf" 36;
  at_exit Sdl.quit;
  at_exit Sdlttf.quit;
  (* create the window*)
  match wd with
    | Fullscreen ->
      let modes = Sdlvideo.list_modes fullscreen_options in
      (match modes with
        | Sdlvideo.DIM l ->
          let (w, h) = last_list l in
          let s = Sdlvideo.set_video_mode w h fullscreen_options in
          let screen =
            { s_surface = s; s_height = h; s_width = w;
              s_old_height = 600; s_old_width = 800; s_fullscreen = true } in
          screen_ref := screen
        | _ ->
          Format.eprintf "Cannot go to fullscreen mode@.";
          exit 1
      )

    | Window (w, h) ->
      let s = Sdlvideo.set_video_mode w h screen_options in
      let screen =
        { s_surface = s; s_height = h; s_width = w;
          s_old_height = h; s_old_width = w; s_fullscreen = false } in
      screen_ref := screen

let update rect =
  let s = !screen_ref.s_surface in
  let scaled_buffer =
    if !world_ref.w_zoom = 1.0 then
      !world_ref.w_buffer
    else
      Sdlgfx.rotozoomSurface !world_ref.w_buffer 0.0 !world_ref.w_zoom use_smoothing
  in
  match rect with
    | None ->
      Sdlvideo.blit_surface ~src:scaled_buffer ~dst:s ();
      Sdlvideo.update_rect s
    | Some rect ->
      let rect = rect_to_sdl rect in
      Sdlvideo.blit_surface ~src:scaled_buffer ~src_rect:rect ~dst:s ~dst_rect:rect ();
      Sdlvideo.update_rect ~rect:rect s

(* events *)
type keyboard_event = string
type mouse_event = Mleft_down | Mleft_up | Mright_down | Mright_up | Mmotion of (int * int)

let pending_key_events = ref ([]:string list)
let pending_mouse_events = ref []

let add_key_event ev =
  pending_key_events := ev :: !pending_key_events

let flush_key_events () =
  let l = !pending_key_events in
  pending_key_events := [];
  l

let add_mouse_event ev =
  pending_mouse_events := ev :: !pending_mouse_events

let flush_mouse_events () =
  let l = !pending_mouse_events in
  pending_mouse_events := [];
  l

let process_events () =
  let react_event ev = match ev with
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } | Sdlevent.QUIT ->
      (*todo: notify termination ??*)
      exit 0

    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_f } ->
      let sc_info = Sdlvideo.surface_info !screen_ref.s_surface in
      if List.mem `FULLSCREEN sc_info.Sdlvideo.flags then (
        let s =
          Sdlvideo.set_video_mode !screen_ref.s_old_width
            !screen_ref.s_old_height screen_options
        in
        update_screen_surface !screen_ref s !screen_ref.s_old_width !screen_ref.s_old_height
      ) else (
        let modes = Sdlvideo.list_modes fullscreen_options in
        match modes with
          | Sdlvideo.DIM l ->
            let (w, h) = last_list l in
            !screen_ref.s_old_height <- sc_info.Sdlvideo.h;
            !screen_ref.s_old_height <- sc_info.Sdlvideo.w;
            let s = Sdlvideo.set_video_mode w h fullscreen_options in
            update_screen_surface !screen_ref s w h
          | _ -> assert false
      );
      update_zoom !screen_ref !world_ref;
      update None

    | Sdlevent.KEYDOWN ev ->
      add_key_event (Sdlkey.name ev.Sdlevent.keysym)

    | Sdlevent.MOUSEBUTTONDOWN ev ->
      if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_LEFT then
        add_mouse_event Mleft_down
      else if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_RIGHT then
        add_mouse_event Mright_down
      else
        Format.printf "Unsupported event@."

    | Sdlevent.MOUSEBUTTONUP ev ->
      if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_LEFT then
        add_mouse_event Mleft_up
      else if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_RIGHT then
        add_mouse_event Mright_up
      else
        Format.printf "Unsupported event@."

    | Sdlevent.MOUSEMOTION ev ->
      add_mouse_event (Mmotion (ev.Sdlevent.mme_x, ev.Sdlevent.mme_y))

    | Sdlevent.VIDEOEXPOSE -> update None
    | Sdlevent.VIDEORESIZE (w, h) ->
      let s = Sdlvideo.set_video_mode w h screen_options in
      update_screen_surface !screen_ref s w h;
      update_zoom !screen_ref !world_ref;
      update None

    | _ -> ()
  in
  let rec sort_events () =
    match Sdlevent.poll () with
      | None -> ()
      | Some ev -> react_event ev
  in
  Sdlevent.pump ();
  sort_events ();

(* Viewports *)

type 'a viewport =
    { vp_tr_coords : 'a pos -> int pos }

let create_float_viewport vp_rect screen_rect =
  let tr_coords pos =
    let newdx = ((pos.p_x -. vp_rect.r_x) /. vp_rect.r_w) *. (float_of_int screen_rect.r_w) in
    let newx = screen_rect.r_x + (int_of_float newdx) in
    let newdy = ((pos.p_y -. vp_rect.r_y) /. vp_rect.r_h) *. (float_of_int screen_rect.r_h) in
    let newy = screen_rect.r_y + (int_of_float newdy) in
    mk_pos newx newy
  in
  { vp_tr_coords = tr_coords }

let identity_viewport =
  { vp_tr_coords = fun p -> p }

let create_world w h backgd =
  let s = create_surface w h in
  let w = { w_buffer = s; w_height = h; w_width = w;
            w_backgd_color = color_to_sdl s backgd; w_zoom = 1.0 } in
  world_ref := w;
  update_zoom !screen_ref w;
  clear_world ();
  identity_viewport

(* Sprites *)
type 'a sprite =
    { sp_surface : Sdlvideo.surface;
      sp_viewport : 'a viewport;
      mutable sp_pos : 'a pos;
      mutable sp_width : int;
      mutable sp_height : int;
      mutable sp_prev_pos : Sdlvideo.rect;
      mutable sp_moved : bool;
      mutable sp_zoom : float; }

let mk_sprite s vp pos w h =
  { sp_surface = s;
    sp_viewport = vp;
    sp_pos = pos;
    sp_width = w; sp_height = h;
    sp_prev_pos = Sdlvideo.rect 0 0 0 0; sp_moved = true;
    sp_zoom = 1.0 }

let sprite_pos sp =
  sp.sp_pos

let create_sprite_circle vp pos rad color =
  let h = rad + rad in
  let w = rad + rad in
  let s = create_surface w h in
  (* fill with transparent background *)
  let transpc = Sdlvideo.map_RGB s Sdlvideo.red in
  Sdlvideo.set_color_key s transpc;
  Sdlvideo.fill_rect s transpc;
  (* draw circle*)
  let c = color_to_sdl_color color in
  ignore (Sdlgfx.filledCircleRGBA s (Sdlvideo.rect rad rad 0 0) rad c 255);
  mk_sprite s vp pos w h
(*
let create_sprite_box vp x y w h color =
  let s = create_surface w h in
  let c = color_to_sdl_color color in
  ignore (Sdlgfx.boxRGBA s (Sdlvideo.rect x y 0 0) (Sdlvideo.rect (x+w) (y+h) 0 0) c 255);
  mk_sprite s vp pos w h
*)
module StringMap =
  Map.Make (struct
    type t = string
    let compare = compare
  end)

let surface_env = ref StringMap.empty
let create_sprite_from_file vp pos f =
  let s =
    try
      StringMap.find f !surface_env
    with
        Not_found ->
          let s = Sdlloader.load_image f in
          surface_env := StringMap.add f s !surface_env;
          s
  in
  let s_info = Sdlvideo.surface_info s in
  mk_sprite s vp pos s_info.Sdlvideo.h s_info.Sdlvideo.w

(*val create_sprite_from_file : string -> sprite*)
(*val sprite_process : sprite -> unit process *)

let set_sprite_pos sp x y =
  sp.sp_pos <- mk_pos x y;
  sp.sp_moved <- true

let set_sprite_zoom sp z =
  sp.sp_zoom <- z;
  sp.sp_moved <- true

let draw_sprite sp =
  if sp.sp_moved then (
    sp.sp_moved <- false;
    (* clear old rect *)
    clear_world ~rect:(Some sp.sp_prev_pos) ();
    let pos = sp.sp_viewport.vp_tr_coords sp.sp_pos in
    let pos = Sdlvideo.rect pos.p_x pos.p_y sp.sp_width sp.sp_height in
    sp.sp_prev_pos <- pos;
    (* blit at new position *)
    Sdlvideo.blit_surface ~src:sp.sp_surface ~dst:!world_ref.w_buffer ~dst_rect:pos ()
  )


(** Info box *)
let infobox_pos = Sdlvideo.rect 10 10 400 40

let clear_info_box () =
  clear_world ~rect:(Some infobox_pos) ()

let draw_info_box s =
  let s = match s with
    | "" -> " " (* sdlttf crashes if the string is empty *)
    | _ -> s
  in
  clear_info_box ();
  let text = Sdlttf.render_text_solid !infobox_font s ~fg:Sdlvideo.blue in
  Sdlvideo.blit_surface ~src:text ~dst:!world_ref.w_buffer ~dst_rect:infobox_pos ()
(*
  ignore (Sdlgfx.stringRGBA !world_ref.w_buffer infobox_pos
            s Sdlvideo.blue 255)
*)
