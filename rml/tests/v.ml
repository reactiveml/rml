open Lco_ctrl_tree;;

let p =
      function
        | () ->
            Rml_interpreter.rml_signal
              (function
                | __sig_2 ->
                    Rml_interpreter.rml_par
                      (Rml_interpreter.rml_loop
                        (Rml_interpreter.rml_seq
                          (Rml_interpreter.rml_until
                            __sig_2
                            (Rml_interpreter.rml_seq
                              (Rml_interpreter.rml_par
                                (Rml_interpreter.rml_compute
                                  (function | () -> print_string "1
" ))
                                (Rml_interpreter.rml_loop
                                  (Rml_interpreter.rml_seq
                                    (Rml_interpreter.rml_compute
                                      (function | () -> print_string "2
" ))
                                    Rml_interpreter.rml_pause)))
                              (Rml_interpreter.rml_compute
                                (function | () -> print_string "3
" ))))
                          (Rml_interpreter.rml_compute
                            (function | () -> print_string "4
" ))))
                      (Rml_interpreter.rml_loop
                        (Rml_interpreter.rml_seq
                          (Rml_interpreter.rml_for
                            (function | () -> 0 )
                            (function | () -> 5 )
                            true
                            (function
                              | __val_rml_3 ->
                                  Rml_interpreter.rml_seq
                                    (Rml_interpreter.rml_compute
                                      (function | () -> print_string "i = " ))
                                    (Rml_interpreter.rml_seq
                                      (Rml_interpreter.rml_compute
                                        (function
                                          | () -> print_int __val_rml_3 ))
                                      (Rml_interpreter.rml_seq
                                        (Rml_interpreter.rml_compute
                                          (function | () -> print_newline ()
                                            ))
                                        Rml_interpreter.rml_pause))
                              ))
                          (Rml_interpreter.rml_seq
                            (Rml_interpreter.rml_compute
                              (function | () -> print_string "avant emit s
"
                                ))
                            (Rml_interpreter.rml_seq
                              (Rml_interpreter.rml_emit __sig_2)
                              (Rml_interpreter.rml_compute
                                (function
                                  | () -> print_string "apres emit s
" ))))))
                )
         
;;

let _ = Rml_interpreter.rml_exec_n_sampling p 15 2.
