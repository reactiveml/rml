let color_of_int = function
  | 0 -> Graphics.yellow
  | 1 -> Graphics.blue
  | 2 -> Graphics.green
  | 3 -> Graphics.red
  | 4 -> Graphics.cyan
  | 5 -> Graphics.black
  | 6 -> Graphics.magenta
  | _ -> Graphics.black
;;

let maj_abs pas x () =
  let alpha = ref 0.0 in
  while (true) do
    x := cos(!alpha);
    alpha := !alpha +. pas;
(*    Thread.yield()*)
  done

let maj_ord pas y () =
  let alpha = ref 0.0 in
  while (true) do
    y := sin(!alpha);
    alpha := !alpha +. pas;
(*    Thread.yield()*)
  done

let aff coul c_x c_y x y r () =
  while (true) do
    coul := (!coul + 1) mod 6;
    Graphics.set_color (color_of_int !coul);
    Graphics.fill_circle 
      (int_of_float (c_x +. !x *. r)) 
      (int_of_float (c_y +. !y *. r))
      5;
(*    Thread.yield()*)
  done

let cercle c_x c_y r () =
  let x = ref 0.0 in
  let y = ref 0.0 in
  let coul = ref 0 in
  ignore (Thread.create (maj_abs 0.05 x) ());
  ignore (Thread.create (maj_ord 0.05 y) ());
  (Thread.create (aff coul c_x c_y x y r) ())

let main () =
  Graphics.open_graph "";
  Thread.join ((cercle 200.0 200.0 100.0) ())
  (*while true do () done*)
(*  Unix.pause() *)

let _ = main()
