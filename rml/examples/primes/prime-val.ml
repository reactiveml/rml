open Lco_ctrl_tree;;

let producer =
      (function
        | __val_ml_2 ->
            (function
              | () ->
                  Rml_interpreter.rml_def
                    (function
                      | () -> (let __val_ml_3 = ref 1  in (__val_ml_3)) )
                    (function
                      | (__val_rml_4) ->
                          Rml_interpreter.rml_loop
                            (Rml_interpreter.rml_seq
                              (Rml_interpreter.rml_compute
                                (function
                                  | () ->
                                      (:=)
                                        __val_rml_4 ((+) ((!) __val_rml_4) 1)
                                  ))
                              (Rml_interpreter.rml_seq
                                (Rml_interpreter.rml_emit_val
                                  (function | () -> __val_ml_2 )
                                  (function | () -> (!) __val_rml_4 ))
                                Rml_interpreter.rml_pause))
                      )
              )
        ) 
;;


let rec filter =
          (function
            | __val_ml_6 ->
                (function
                  | __val_ml_7 ->
                      (function
                        | () ->
                            Rml_interpreter.rml_await_all
                              (function | () -> __val_ml_6 )
                              (function
                                | __val_rml_8 ->
                                    Rml_interpreter.rml_seq
                                      (Rml_interpreter.rml_emit_val
                                        (function | () -> __val_ml_7 )
                                        (function | () -> __val_rml_8 ))
                                      (Rml_interpreter.rml_signal_combine
                                        (function | () -> 0 )
                                        (function
                                          | () ->
                                              (function
                                                | __val_ml_9 ->
                                                    (function
                                                      | __val_ml_10 ->
                                                          __val_ml_9
                                                      )
                                                )
                                          )
                                        (function
                                          | __sig_11 ->
                                              Rml_interpreter.rml_par
                                                (Rml_interpreter.rml_run
                                                  (function
                                                    | () ->
                                                        filter
                                                          __sig_11 __val_ml_7
                                                    ))
                                                (Rml_interpreter.rml_loop
                                                  (Rml_interpreter.rml_await_all
                                                    (function
                                                      | () -> __val_ml_6 )
                                                    (function
                                                      | __val_rml_12 ->
                                                          Rml_interpreter.rml_if
                                                            (function
                                                              | () ->
                                                                  (!=)
                                                                    ((mod)
                                                                    __val_rml_12
                                                                    __val_rml_8)
                                                                    0
                                                              )
                                                            (Rml_interpreter.rml_emit_val
                                                              (function
                                                                | () ->
                                                                    __sig_11
                                                                )
                                                              (function
                                                                | () ->
                                                                    __val_rml_12
                                                                ))
                                                            Rml_interpreter.rml_nothing
                                                      )))
                                          ))
                                )
                        )
                  )
            ) 
;;


let output =
      (function
        | __val_ml_14 ->
            (function
              | () ->
                  Rml_interpreter.rml_loop
                    (Rml_interpreter.rml_await_all
                      (function | () -> __val_ml_14 )
                      (function
                        | __val_rml_15 ->
                            Rml_interpreter.rml_compute
                              (function
                                | () ->
                                    List.iter
                                      (function
                                        | __val_ml_16 ->
                                            print_int __val_ml_16;
                                              print_string " "; flush stdout
                                        )
                                      __val_rml_15
                                )
                        ))
              )
        ) 
;;


let main =
      (function
        | () ->
            Rml_interpreter.rml_signal
              (function
                | __sig_18 ->
                    Rml_interpreter.rml_signal_combine
                      (function | () -> 0 )
                      (function
                        | () ->
                            (function
                              | __val_ml_19 ->
                                  (function | __val_ml_20 -> __val_ml_19 )
                              )
                        )
                      (function
                        | __sig_21 ->
                            Rml_interpreter.rml_par
                              (Rml_interpreter.rml_run
                                (function | () -> producer __sig_21 ))
                              (Rml_interpreter.rml_par
                                (Rml_interpreter.rml_run
                                  (function | () -> filter __sig_21 __sig_18
                                    ))
                                (Rml_interpreter.rml_run
                                  (function | () -> output __sig_18 )))
                        )
                )
        ) 
;;

