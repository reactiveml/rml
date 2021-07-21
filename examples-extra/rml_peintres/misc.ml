(* Graphics and events *)
let init_sdl () =
  let open Sdlvideo in
  let open Sdl in
  init [`VIDEO];
  at_exit quit;
  let screen = set_video_mode 800 800 [`DOUBLEBUF; `RESIZABLE] in
  fill_rect ~rect:(rect 0 0 800 800) screen (map_RGB screen red);
  screen

let draw_brush image w h screen x old_x y old_y =
  let open Sdlvideo in
  fill_rect ~rect:(rect old_x old_y w h) screen (map_RGB screen black);
  blit_surface ~src:image ~dst:screen ~dst_rect:(rect x y w h) ();
  flip screen

let check_escape () =
  let open Sdlevent in
  let open Sdlkey in
  let react_event ev = match ev with
    | KEYDOWN {keysym=KEY_ESCAPE}
    | QUIT -> Format.eprintf "Exiting@."; exit 0
    | _ -> ()
  in
  let rec sort_events () = match Sdlevent.poll () with
    | None -> ()
    | Some ev -> react_event ev
  in
  pump ();
  sort_events ()

(* behaviour *)

type orientation = UpDown | LeftRight
type direction = Pos | Neg
type state =
    { mutable x : int; mutable y : int;
      mutable orientation : orientation; mutable dir : direction;
      xmin : int; xmax : int;
      ymin : int; ymax : int }
let new_state (xmin, xmax, ymin, ymax) =
  { x = xmin; y = ymax; xmin = xmin; xmax = xmax;
    ymin = ymin; ymax = ymax; orientation = UpDown; dir = Neg }

let speed = 10

let move draw st =
  check_escape ();
  let old_x, old_y = st.x, st.y in
  (match st.orientation, st.dir with
    | LeftRight, Pos ->
      if st.x > st.xmax
      then st.dir <- Neg
      else st.x <- st.x + speed
    | LeftRight, Neg ->
      if st.x < st.xmin
      then (st.orientation <- UpDown; st.dir <- Neg)
      else st.x <- st.x - speed
    | UpDown, Pos ->
      if st.y > st.ymax
      then (st.orientation <- LeftRight; st.dir <- Pos)
      else st.y <- st.y + speed
    | UpDown, Neg ->
      if st.y < st.ymin
      then st.dir <- Pos
      else st.y <- st.y - speed);
  draw st.x old_x st.y old_y

let img_w, img_h = 50, 107

let init () =
  let screen = init_sdl () in
  let image = Sdlloader.load_image "brush.png" in
  draw_brush image 50 107 screen

let p1 = 100, 400, 100, 400
let p2 = 300, 600, 300, 600
