module Order =
  struct
    type 'a t = 'a
    let compare = compare
  end

module My_map = Map.Make( Order)


type 'position t = ('position * int) My_map.t 

let no_info = -10000

let make size init = ref My_map.empty

let get tbl id = 
  try
    My_map.find id tbl 
  with
  | Not_found -> 


let set tbl id pos date = 
  My_map.add id (pos,date) tbl

let update tbl id pos date = 
  try
    
  with
  | Not_found -> My_map.add id (pos,date) tbl

  let _, date_tbl = tbl.(id) in
  if date > date_tbl then
    tbl.(id) <- (pos,date)


(*
let update tbl neighbors date =
   List.iter
    (fun node -> set tbl node.id node.pos date)
    neighbors
*)
