open Position

module Order =
  struct
    type t = int
    let compare = compare
  end

module My_map = Map.Make( Order)


type t = (position * int) My_map.t ref

let no_info = -10000

let make size = ref My_map.empty

let get tbl id =
  try
    My_map.find id !tbl
  with
  | Not_found -> ({x=0; y=0}, no_info)


let set tbl id pos date =
  tbl := My_map.add id (pos,date) !tbl


let update tbl id pos date =
  try
    let _, date_tbl = My_map.find id !tbl in
    if date > date_tbl then tbl := My_map.add id (pos,date) !tbl
  with
  | Not_found -> tbl := My_map.add id (pos,date) !tbl


