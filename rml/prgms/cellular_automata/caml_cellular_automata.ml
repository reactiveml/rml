(********************************************************************)
(* Constantes                                                       *)
(********************************************************************)
let maxx = 300
let maxy = 300

let neighbors = 4


type status_t =
  | Status_Top
  | Status_Right 
  | Status_Bottom
  | Status_Left
  | Status_Quiescent
  | Status_Wall
  | Status_None


(********************************************************************)
(* Direction                                                        *)
(********************************************************************)
type dir_t =
  | Up
  | Down
  | Left
  | Right
 
let opposite dir =
  match dir with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left


(**************************************************************************)
(* image                                                                  *)
(**************************************************************************)
type image_t =
    {x: int;
     y: int;
     mutable color: Graphics.color; }

let new_image x y col =
  { x = x;
    y = y;
    color = col; }

let draw_image img = 
  Graphics.set_color img.color;
  Graphics.plot img.x img.y


let color_of_status s =
  match s with 
  | Status_Top -> Graphics.blue
  | Status_Right -> Graphics.blue
  | Status_Bottom -> Graphics.blue
  | Status_Left -> Graphics.blue
  | Status_Quiescent -> Graphics.red
  | Status_Wall -> Graphics.green
  | Status_None -> Graphics.black
 

(**************************************************************************)
(* neighborhood                                                           *)
(**************************************************************************)

type neighborhood_t = (dir_t * cell_t) list


(**************************************************************************)
(* cell                                                                   *)
(**************************************************************************)

and cell_t =
    { cell_x: int;
      cell_y: int;
      mutable cell_status: status_t;
      mutable cell_pre_status: status_t;
      mutable cell_image: image_t;
      mutable cell_neighborhood: neighborhood_t; }
      
let new_cell x y status pre_status =
  { cell_x = x;
    cell_y = y;
    cell_status = status;
    cell_pre_status = pre_status;
    cell_image = new_image x y (color_of_status status);
    cell_neighborhood = []; }

let empty_cell = new_cell (-1) (-1) Status_None Status_None

let draw_cell c = 
  draw_image c.cell_image 


(**************************************************************************)
(* neighborhood functions                                                 *)
(**************************************************************************)

let get_von_neumann_neighbors cell cell_array =
  let x = cell.cell_x in
  let y = cell.cell_y in
  let neighbors = ref [] in
  if 0 <= x-1 then neighbors := (Left, cell_array.(x-1).(y)) :: !neighbors;
  if x+1 < maxx then neighbors := (Right, cell_array.(x+1).(y)) :: !neighbors;
  if 0 <= y-1 then neighbors := (Down, cell_array.(x).(y-1)) :: !neighbors;
  if y+1 < maxy then neighbors := (Up, cell_array.(x).(y+1)) :: !neighbors;
  !neighbors

let get_von_neumann_neighbors_circular cell cell_array =
  let x = cell.cell_x in
  let y = cell.cell_y in
  let neighbors = ref [] in
  neighbors := (Left, cell_array.((maxx+x-1) mod maxx).(y)) :: !neighbors;
  neighbors := (Right, cell_array.((maxx+x+1) mod maxx).(y)) :: !neighbors;
  neighbors := (Down, cell_array.(x).((maxy+y-1) mod maxy)) :: !neighbors;
  neighbors := (Up, cell_array.(x).((maxy+y+1) mod maxy)) :: !neighbors;
  !neighbors

let get_neighbors = get_von_neumann_neighbors_circular


(**************************************************************************)
(* cell process                                                           *)
(**************************************************************************)

let active_cell self cell_behavior cell_array =
  let i = self.cell_x in
  let j = self.cell_y in
  cell_array.(i).(j).cell_neighborhood <- 
    get_neighbors cell_array.(i).(j) cell_array;
  cell_behavior self

let save_state self =
  self.cell_pre_status <- self.cell_status;
  draw_cell self


(**************************************************************************)
(* cell behaviors                                                         *)
(**************************************************************************)

let fredkin cell = 
  let cpt = ref 0 in
  List.iter 
    (fun (_,info) -> if info.cell_pre_status <> Status_Quiescent then incr cpt)
    cell.cell_neighborhood;
  cell.cell_status <- 
    if (!cpt mod 2) = 1 then Status_Wall else Status_Quiescent; 
  cell.cell_image.color <- color_of_status cell.cell_status


(**************************************************************************)
(* cellular automaton                                                     *)
(**************************************************************************)
let cellular_automaton_create () =
  Array.make_matrix maxx maxy empty_cell

let rec cellular_automaton_init cell_array i j =
  if i < maxx then
    if j < maxy then
      let status = 
(*	if i = 0 or i = maxx/2 or i = (maxx-1) or j = 0 or j = (maxy-1)  *)
(*	if i = maxx/2 *) 
	if i = maxx/2 && j = maxy/2  
	then Status_Wall 
	else Status_Quiescent
      in
      cell_array.(i).(j) <- (new_cell i j status status);
      cellular_automaton_init cell_array i (j+1)
    else
      cellular_automaton_init cell_array (i+1) 0
  else
    ()

let init_neighborhood cell_array =
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      cell_array.(i).(j).cell_neighborhood <- 
	get_neighbors cell_array.(i).(j) cell_array
    done
  done

let active_cell_array cell_array =
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      active_cell cell_array.(i).(j) fredkin cell_array
    done
  done

let save_cell_array cell_array =
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      save_state cell_array.(i).(j) 
    done
  done


(**************************************************************************)
(* main                                                                   *)
(**************************************************************************)
let main () =
  let cell_array = cellular_automaton_create() in
  cellular_automaton_init cell_array 0 0; 
  init_neighborhood cell_array;
  Graphics.open_graph (" "^(string_of_int maxx)^"x"^(string_of_int maxy));
  Graphics.auto_synchronize false;
(*  while true *)
  for i = 1 to 100 
  do
    active_cell_array cell_array;
    save_cell_array cell_array;
    Graphics.synchronize ()
  done
;;

main()
