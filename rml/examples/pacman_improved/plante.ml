open Lk_rewrite;;

let a =
      function
        | __sig_2 ->
            function
              | __loc_1 ->
                  Rml_interpreter.rml_while
                    (function
                      | () ->
                          (=) ([]) (Rml_interpreter.rml_pre_value __sig_2)
                      )
                    (Rml_interpreter.rml_await_one
                      __sig_2
                      (function
                        | __val_rml_3 ->
                            Rml_interpreter.rml_pause
                              Rml_interpreter.rml_term
                        ))
                    __loc_1
                
;;

