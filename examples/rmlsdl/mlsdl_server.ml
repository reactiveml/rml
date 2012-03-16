open Mlsdl_types

(* internal types for the screen and the back buffer *)
type screen =
    { mutable s_surface : Sdlvideo.surface;
      mutable s_width : int;
      mutable s_height : int;
      mutable s_fullscreen : bool;
      mutable s_old_height : int;
      mutable s_old_width : int;
      mutable s_op_queue : screen_op list;
    }

type world =
    { w_buffer : Sdlvideo.surface;
      w_backgd_color : int32;
      mutable w_height : int;
      mutable w_width : int;
      mutable w_zoom : float }

let screen_mutex = Mutex.create ()
let start_mutex = Mutex.create ()
let start_condition = Condition.create ()
let screen_arguments = ref None

let screen_ref = ref (Obj.magic () : screen)

let world_ref = ref (Obj.magic () : world)

let infobox_font = ref (Obj.magic () : Sdlttf.font)

(* Drawing and screen updating functions*)

let use_smoothing = false

let sdl_init_options = [`VIDEO; `NOPARACHUTE]
let screen_options = [`HWSURFACE; `RESIZABLE]
let fullscreen_options = [`HWSURFACE; `FULLSCREEN]

let create_surface w h =
  Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:w ~h:h ~bpp:32
    ~rmask:0l ~gmask:0l ~bmask:0l ~amask:0l

let update_screen_surface screen s w h =
  screen.s_surface <- s;
  screen.s_height <- h;
  screen.s_width <- w

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

let init_screen s h w fullscreen =
  let old_width = if fullscreen then 800 else w in
  let old_height = if fullscreen then 600 else h in
  screen_ref := { s_surface = s; s_height = h; s_width = w;
                  s_old_height = old_height; s_old_width = old_width;
                  s_fullscreen = false;
                  s_op_queue = [] }

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
          init_screen s h w true
        | _ ->
          Format.eprintf "Cannot go to fullscreen mode@.";
          exit 1
      )

    | Window (w, h) ->
      let s = Sdlvideo.set_video_mode w h screen_options in
      init_screen s h w false

let create_world w h backgd =
  let s = create_surface w h in
  let w = { w_buffer = s; w_height = h; w_width = w;
            w_backgd_color = color_to_sdl s backgd; w_zoom = 1.0 } in
  world_ref := w;
  update_zoom !screen_ref w;
  clear_world ()

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

let init_server wd w h color =
  create_window wd;
  create_world w h color

(** Separate process drawing *)
let screen_env = ref StringMap.empty
let surface_from_filename f =
  try
    StringMap.find f !screen_env
  with
      Not_found ->
        let s = Sdlloader.load_image f in
        screen_env := StringMap.add f s !screen_env;
        s

let old_pos_env = ref IdentEnv.empty
let draw_screen_sprite k r = match k with
  | File filename ->
    let s = surface_from_filename filename in
    Sdlvideo.blit_surface ~src:s ~dst:!world_ref.w_buffer ~dst_rect:r ()
  | Circle (r, color) -> ()
(*
 (* fill with transparent background *)
  let transpc = Sdlvideo.map_RGB s Sdlvideo.red in
  Sdlvideo.set_color_key s transpc;
  Sdlvideo.fill_rect s transpc;
  (* draw circle*)
  let c = color_to_sdl_color color in
  ignore (Sdlgfx.filledCircleRGBA s (Sdlvideo.rect rad rad 0 0) rad c 255);
  mk_sprite s vp pos w h
  *)
  | Box (w, h, color) ->
      let c = Sdlvideo.map_RGB !world_ref.w_buffer (color_to_sdl_color color) in
        Sdlvideo.fill_rect ~rect:r !world_ref.w_buffer c
  | Text (s, color) ->
    let text = Sdlttf.render_text_solid !infobox_font s ~fg:(color_to_sdl_color color) in
    Sdlvideo.blit_surface ~src:text ~dst:!world_ref.w_buffer ~dst_rect:r ()

let clear_sprite sp =
  try
    let r = IdentEnv.find sp.ss_id !old_pos_env in
    clear_world ~rect:(Some r) ()
  with
    | Not_found -> ()

let do_screen_op op = match op with
  | Init _ -> ()
  | Draw sp ->
    clear_sprite sp;
    let r = Sdlvideo.rect sp.ss_pos.p_x sp.ss_pos.p_y sp.ss_width sp.ss_height in
    old_pos_env := IdentEnv.add sp.ss_id r !old_pos_env;
    draw_screen_sprite sp.ss_kind r
  | Clear sp ->
    clear_sprite sp

let listen_queue ic =
  try
    while true do
      match receive_op () with
        | Init (wd, w, h, color, fps) ->
            Mutex.lock start_mutex;
            screen_arguments := Some (wd, w, h, color, fps);
            Mutex.unlock start_mutex;
            Condition.signal start_condition;
            (* wait for the screen to be started *)
        | op ->
            Mutex.lock screen_mutex;
            !screen_ref.s_op_queue <- op :: !screen_ref.s_op_queue;
            Mutex.unlock screen_mutex
    done
  with
    | _ -> Format.eprintf "Listening to screen ops crashed @."; exit 2

(* events *)

let pos_of_event x y =
  let newx = int_of_float ((float_of_int x) /. !world_ref.w_zoom) in
  let newy = int_of_float ((float_of_int y) /. !world_ref.w_zoom) in
  mk_pos newx newy

let process_events () =
  let react_event ev = match ev with
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } | Sdlevent.QUIT ->
      (*todo: notify termination ??*)
      Format.eprintf "Terminating after user request@.";
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
      add_event (Key_event (Sdlkey.name ev.Sdlevent.keysym))

    | Sdlevent.MOUSEBUTTONDOWN ev ->
      let pos = pos_of_event ev.Sdlevent.mbe_x ev.Sdlevent.mbe_y in
      if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_LEFT then
        add_event (Mouse_event (Mleft_down, pos))
      else if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_RIGHT then
        add_event (Mouse_event (Mright_down, pos))
      else
        Format.printf "Unsupported event@."

    | Sdlevent.MOUSEBUTTONUP ev ->
      let pos = pos_of_event ev.Sdlevent.mbe_x ev.Sdlevent.mbe_y in
      if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_LEFT then
        add_event (Mouse_event (Mleft_up, pos))
      else if ev.Sdlevent.mbe_button = Sdlmouse.BUTTON_RIGHT then
        add_event (Mouse_event (Mright_up, pos))
      else
        Format.printf "Unsupported event@."

    | Sdlevent.MOUSEMOTION ev ->
      let pos = pos_of_event ev.Sdlevent.mme_x ev.Sdlevent.mme_y in
      add_event (Mouse_event (Mmotion (ev.Sdlevent.mme_xrel, ev.Sdlevent.mme_yrel), pos))

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
  sort_events ()


let refresh_screen () =
  (* draw screen*)
 (* clear_world (); *)
  Mutex.lock screen_mutex;
  let l = !screen_ref.s_op_queue in
  !screen_ref.s_op_queue <- [];
  Mutex.unlock screen_mutex;
  List.iter do_screen_op l;
  process_events ();
  update None

let refresh_screen fps =
  let period = 1.0 /. (float_of_int fps) in
  while true do
    refresh_screen ();
    Thread.delay period
  done

let _ =
  Mutex.lock screen_mutex;
  ignore (Thread.create listen_queue ());
  Mutex.lock start_mutex;
  Condition.wait start_condition start_mutex;
  Mutex.unlock start_mutex;
  let wd, w, h, color, fps = match !screen_arguments with Some o -> o | none -> assert false in
  init_server wd w h color;
  Mutex.unlock screen_mutex;
  refresh_screen fps
