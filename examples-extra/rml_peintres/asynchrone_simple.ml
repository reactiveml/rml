open Misc

let brush draw xmin ymin xmax ymax () =
  let st = new_state xmin xmax ymin ymax in
  while true do
    move draw st
  done

let main () =
  let draw = init () in
  let t1 = Thread.create (brush draw 100 100 400 400) ()
  and t2 = Thread.create (brush draw 300 300 600 600) () in
  Thread.join t1; Thread.join t2

let _ =
  main ()
