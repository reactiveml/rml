(* THIS FILE IS GENERATED. *)
(* /Users/ccpasteur/Documents/work/git/rml/compiler/rpmlc.byte -runtime Fsharp_LcoThread -s main domains.rml  *)
open Caml_compat
  
let Interpreter = Machine.LcoSeq
  
let Machine = Machine.SeqMachine
  
let do_stuff i__val_rml_4 =
  let x__val_rml_5 = Pervasives.ref i__val_rml_4
  in
    for i__val_ml_6 = 1 to 89332934 do
      Pervasives.( := ) x__val_rml_5
        (Pervasives.( + )
           (Pervasives.( * ) (Pervasives.( ! ) x__val_rml_5) 435) 4223)
    done
  
let main () =
  Interpreter.rml_fordopar (fun () -> 1) (fun () -> 8) true
    (fun i__val_rml_8 ->
       Interpreter.rml_newclock None None
         (fun ck__clock_9 ->
            Interpreter.rml_for (fun () -> 0) (fun () -> 10) true
              (fun j__val_ml_10 ->
                 Interpreter.rml_seq
                   (Interpreter.rml_compute (fun () -> do_stuff j__val_ml_10))
                   (Interpreter.rml_seq
                      (Interpreter.rml_compute
                         (fun () ->
                            Pervasives.print_endline
                              (Pervasives.( ^ )
                                 (Pervasives.string_of_int i__val_rml_8)
                                 (Pervasives.( ^ ) ":"
                                    (Pervasives.string_of_int j__val_ml_10)))))
                      (Interpreter.rml_pause' ck__clock_9)))))
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

