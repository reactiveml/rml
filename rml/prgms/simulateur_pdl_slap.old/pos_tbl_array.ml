type 'position t = ('position * int) array 

let no_info = -10000

let make size init = Array.make size init

let get tbl id = tbl.(id)

let set tbl id pos date = 
  tbl.(id) <- (pos,date)

let update tbl id pos date = 
  let _, date_tbl = tbl.(id) in
  if date > date_tbl then
    tbl.(id) <- (pos,date)


(*
let update tbl neighbors date =
   List.iter
    (fun node -> set tbl node.id node.pos date)
    neighbors
*)
