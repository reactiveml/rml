open Lk_rewrite;;

let f = function | x__val_ml_2 -> (+) x__val_ml_2 1  
;;


let p =
      function
        | () ->
            function
              | k__loc_1 ->
                  Rml_interpreter.rml_def
                    (function
                      | () -> let cpt__val_ml_4 = ref 0  in (cpt__val_ml_4) )
                    (function
                      | (cpt__val_rml_5) ->
                          Rml_interpreter.rml_loop
                            (Rml_interpreter.rml_compute
                              (function
                                | () ->
                                    (:=)
                                      cpt__val_rml_5 (f ((!) cpt__val_rml_5))
                                )
                              (Rml_interpreter.rml_compute
                                (function
                                  | () -> print_int ((!) cpt__val_rml_5) )
                                (Rml_interpreter.rml_compute
                                  (function | () -> print_newline () )
                                  (Rml_interpreter.rml_pause
                                    Rml_interpreter.rml_term))))
                      )
                
;;


let p2 =
      function
        | () ->
            function
              | k__loc_2 ->
                  Rml_interpreter.rml_compute (function | () -> () ) k__loc_2
                
;;

let _ = Rml_interpreter.rml_exec p
