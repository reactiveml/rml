open Position

type t = (position * int) array

let no_info = -10000

let make size = Array.make size ({x=0; y=0}, no_info)

let get tbl id = tbl.(id)

let set tbl id pos date =
  tbl.(id) <- (pos,date)

let update tbl id pos date =
  let _, date_tbl = tbl.(id) in
  if date > date_tbl then
    tbl.(id) <- (pos,date)
