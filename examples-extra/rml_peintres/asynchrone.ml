open Misc

let brush draw p () =
  let st = new_state p in
  while true do
    move draw st
  done

let _ =
  let draw = init () in
  let t1 = Thread.create (brush draw p1) ()
  and t2 = Thread.create (brush draw p2) () in
  Thread.join t1; Thread.join t2
