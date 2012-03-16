(* Some helper functions*)
let random_int min max =
  min + Random.int (max - min)

let random_float min max =
  min +. Random.float (max -. min)

(* Vectors*)
type vector = { v_x : float; v_y : float }

let mk_vector x y =
  { v_x = x; v_y = y }

let zero_vector = { v_x = 0.0; v_y = 0.0 }

let random_vector xmin ymin xmax ymax =
  { v_x = random_float xmin xmax;
    v_y = random_float ymin ymax }

let random_vector_from_center c_x c_y dmin dmax =
  let rand_away z d =
    if Random.bool () then z +. d else z -. d
  in
  let d = random_float dmin dmax in
  let x = random_float (c_x -. d) (c_x +. d) in
  let dy = sqrt ( (d *. d) -. ((x -. c_x) *. (x -. c_x)) ) in
  let y = rand_away c_y dy in
  mk_vector x y

let norm2 v =
  v.v_x *. v.v_x +. v.v_y *. v.v_y

let dist2 v1 v2 =
  (v2.v_x -. v1.v_x) *. (v2.v_x -. v1.v_x)
    +. (v2.v_y -. v1.v_y) *. (v2.v_y -. v1.v_y)

let unit_vector v1 v2 =
  let d2 = dist2 v1 v2 in
  let d = sqrt d2 in
  { v_x = (v2.v_x -. v1.v_x) /. d;
    v_y = (v2.v_y -. v1.v_y) /. d }

let normal_unit_vector v1 v2 =
  let d2 = dist2 v1 v2 in
  let d = sqrt d2 in
  { v_x = -. (v2.v_y -. v1.v_y) /. d;
    v_y = (v2.v_x -. v1.v_x) /. d }

let add_vector v1 v2 =
  { v_x = v1.v_x +. v2.v_x;
    v_y = v1.v_y +. v2.v_y }

let scalar_mult a v =
  { v_x = a *. v.v_x; v_y = a *. v.v_y;  }

let vector_error v1 v2 =
  (dist2 v1 v2) /. (norm2 v1)

(* Rects *)

(*
     y
     ^
   h |  NW    NE
     |
     |
     |  SW    SE
       _ _ _ _ _ _ >  x
    (x, y)         w
*)


type rect = { r_x : float; r_y : float; r_w : float; r_h : float; }

type direction = S | N | E | W

type quad = SW | SE | NW | NE

let mk_rect x y w h =
  { r_x = x; r_y = y; r_w = w; r_h = h; }

let center_float r =
  { v_x = r.r_x +. (r.r_w /. 2.0);
    v_y = r.r_y +. (r.r_h /. 2.0) }

let sub_rect r quad = match quad with
| SW -> { r with r_w = r.r_w *. 0.5; r_h = r.r_h *. 0.5 }
| SE ->
    { r with r_w = r.r_w -. (r.r_w *. 0.5);
      r_h = r.r_h *. 0.5;
      r_x = r.r_x +. (r.r_w *. 0.5) }
| NW ->
    { r with r_w = r.r_w *. 0.5;
      r_h = r.r_h -. (r.r_h*. 0.5);
      r_y = r.r_y +. (r.r_h *. 0.5) }
| NE ->
    { r_w =  r.r_w -. (r.r_w *. 0.5);
      r_h = r.r_h -. (r.r_h*. 0.5);
      r_y = r.r_y +. (r.r_h *. 0.5);
      r_x = r.r_x +. (r.r_w *. 0.5) }

let is_inside r pos =
  r.r_x <= pos.v_x && pos.v_x <= r.r_x +. r.r_w
    && r.r_y <= pos.v_y && pos.v_y <= r.r_y +. r.r_w

let delta = 10.0

let is_in_border r pos =
  pos.v_x -. r.r_x <= delta or (r.r_x +. r.r_w) -. pos.v_x <= delta
  or
  pos.v_y -. r.r_y <= delta || (r.r_y +. r.r_w) -. pos.v_y <= delta

let quad_from_pos r pos =
  let dx = pos.v_x -. r.r_x in
  let dy = pos.v_y -. r.r_y in
  let is_w = dx <= (r.r_w *. 0.5) in
  let is_s = dy <= (r.r_h *. 0.5) in
    if is_w then
      (if is_s then SW else NW)
    else
      (if is_s then SE else NE)

let near_quads_from_pos r pos =
  let dx = pos.v_x -. r.r_x in
  let dy = pos.v_y -. r.r_y in
  let is_w = dx <= (r.r_w *. 0.5 -. delta) in
  let is_we = (r.r_w *. 0.5 -. delta) < dx && dx < (r.r_w *. 0.5 +. delta) in
  let is_s = dy <= (r.r_h *. 0.5 +. delta) in
  let is_sn = (r.r_h *. 0.5 -. delta) < dy && dy < (r.r_h *. 0.5 +. delta) in
    if is_w then
      (if is_s then [SW] else (if is_sn then [SW; NW] else [NW]))
    else if is_we then
      (if is_s then [SW; SE] else (if is_sn then [SW; NW; SE; NE] else [NW; NE]))
    else
      (if is_s then [SE] else (if is_sn then [SE; NE] else [NE]))

let string_of_quad q = match q with
  | SW -> "SW"
  | SE -> "SE"
  | NW -> "NW"
  | NE -> "NE"

let string_of_rect r =
  "(" ^ string_of_float r.r_x ^ ", " ^ string_of_float r.r_y ^ ") -> ("
    ^ string_of_float (r.r_x +. r.r_w) ^ ", " ^ string_of_float (r.r_y +. r.r_h) ^ ")"
