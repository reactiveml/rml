open Misc

let brush draw p m1 m2 () =
  let st = new_state p in
  while true do
    move draw st;
    Mutex.unlock m2; Mutex.lock m1
  done

let main () =
  let draw = init () in
  let m1, m2 = Mutex.create (), Mutex.create () in
  Mutex.lock m1; Mutex.lock m2;
  let t1 = Thread.create (brush draw p1 m1 m2) ()
  and t2 = Thread.create (brush draw p2 m2 m1) () in
  Thread.join t1; Thread.join t2

let _ =
  main ()
