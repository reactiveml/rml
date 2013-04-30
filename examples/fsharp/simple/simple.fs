module Simple

module Interpreter = Lco

let do_stuff i__val_rml_4 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_compute
      (fun () ->
         let x__val_rml_5 = Pervasives.ref i__val_rml_4
         in
           for i__val_ml_6 = 1 to 19324 do
             Pervasives.( := ) x__val_rml_5
               (Pervasives.( + )
                  (Pervasives.( * ) (Pervasives.( ! ) x__val_rml_5) 435) 4223)
           done)
  
let main : _ Interpreter.process =
  fun () ->
    Interpreter.rml_fordopar (fun () -> 1) (fun () -> 8821) true
      (fun i__val_rml_8 ->
         Interpreter.rml_run (fun () -> do_stuff i__val_rml_8))
         
let _ = Machine.rml_exec main
