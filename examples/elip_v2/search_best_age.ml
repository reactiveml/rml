open Position
open Global

(* !!!!

(*

(* Array : NE MARCHE PAS AVEC DE LA CREATION DYNAMIQUE *)
let create () = Array.make nb_nodes false
let mem visited id = visited.(id)
let add visited id v = visited.(id) <- true


(* List *)
let create () = ref []
let mem visited id = List.mem id !visited
let add visited id v = visited := id :: !visited

(* Map *)
module OrderedInt =
  struct
    type t = int
    let compare = compare
  end
module M = Map.Make (OrderedInt)

let create () = ref M.empty
let mem visited id = M.mem id !visited
let add visited id v = visited := M.add id () !visited

*)


(* Hashtbl *)
let create () = Hashtbl.create 141
let mem visited id = Hashtbl.mem visited id
let add visited id v = Hashtbl.add visited id v


let search_best_age kind src_node dest age_limit =
  let level = ref 0 in
  let node_cpt = ref 0 in
  let visited = create() in
  let rec add_next_neighbors neighbors next =
    match neighbors with
    | [] -> next
    | node::neighbors ->
	if mem visited node.id then
	  add_next_neighbors neighbors next
	else
	  (add visited node.id ();
	   add_next_neighbors neighbors (node::next))
  in
  let rec aux neighbors =
    incr level;
    let current_level = !level in
    let visited_cpt = ref 0 in
    let next_neighbors = ref [] in
    let best =
      List.fold_left
	(fun best n ->
	  let pos_tbl =
	    match kind with
	    | LE -> n.pos_tbl_le
	    | ELIP -> n.pos_tbl_elip
	  in
	  incr node_cpt;
	  incr visited_cpt;
	  next_neighbors :=
	    add_next_neighbors n.neighbors !next_neighbors;
	  match best with
	  | None ->
	      let (pos, date) = Pos_tbl.get pos_tbl dest in
	      if date <> Pos_tbl.no_info &&
		abs (n.date - date) <= age_limit
	      then
		Some (n, (pos, abs (n.date - date)))
	      else
		None
	  | Some (_, (_, best_age)) ->
	      let (pos, date) = Pos_tbl.get pos_tbl dest in
	      if abs (n.date - date) < best_age then
		Some (n, (pos, abs (n.date - date)))
	      else
		best)
	None
	neighbors
    in
    if (best = None) && (!next_neighbors <> []) then
      let (best, lvl, node_cpt, overhead) = aux !next_neighbors in
      (best, lvl, node_cpt, (1+lvl-current_level) * !visited_cpt + overhead)
    else
      (best, !level, !node_cpt, !visited_cpt)
  in
  fun neighbors ->
    let () =
      add visited src_node.id ();
      List.iter
	(fun node ->
	  if node.id <> src_node.id then
	    add visited node.id ())
	neighbors
    in
    aux neighbors

!!!! *)

(* ----------------------------------------------------------------------- *)

let rec set_not_visited neighbors =
  begin match neighbors with
  | [] -> ()
  | n::l ->
      n.visited <- false;
      set_not_visited l
  end

let search_best_age kind src_node dest age_limit =
  let level = ref 0 in
  let node_cpt = ref 0 in
  let rec add_next_neighbors neighbors next =
    match neighbors with
    | [] -> next
    | node::neighbors ->
	if node.visited then
	  add_next_neighbors neighbors next
	else
	  (node.visited <- true;
	   add_next_neighbors neighbors (node::next))
  in
  let rec aux neighbors =
    incr level;
    let current_level = !level in
    let visited_cpt = ref 0 in
    let next_neighbors = ref [] in
    let best =
      List.fold_left
	(fun best n ->
	  let pos_tbl =
	    match kind with
	    | LE -> n.pos_tbl_le
	    | ELIP -> n.pos_tbl_elip
	  in
	  incr node_cpt;
	  incr visited_cpt;
	  next_neighbors :=
	    add_next_neighbors n.neighbors !next_neighbors;
	  match best with
	  | None ->
	      let (pos, date) = Pos_tbl.get pos_tbl dest in
	      if date <> Pos_tbl.no_info &&
		abs (n.date - date) <= age_limit
	      then
		Some (n, (pos, abs (n.date - date)))
	      else
		None
	  | Some (_, (_, best_age)) ->
	      let (pos, date) = Pos_tbl.get pos_tbl dest in
	      if abs (n.date - date) < best_age then
		Some (n, (pos, abs (n.date - date)))
	      else
		best)
	None
	neighbors
    in
    if (best = None) && (!next_neighbors <> []) then
      let (best, lvl, node_cpt, overhead) = aux !next_neighbors in
      set_not_visited !next_neighbors;
      (best, lvl, node_cpt, (1+lvl-current_level) * !visited_cpt + overhead)
    else
      (set_not_visited !next_neighbors;
       (best, !level, !node_cpt, !visited_cpt))
  in
  fun neighbors ->
    let () =
      src_node.visited <- true;
      List.iter
	(fun node ->
	  if node.id <> src_node.id then
	    node.visited <- true)
	neighbors
    in
    let res = aux neighbors in
    src_node.visited <- false;
    set_not_visited neighbors;
    res



