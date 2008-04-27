open Implantation;;

let p =
      (function
        | () ->
            Lco_ctrl_tree_record.rml_signal_combine
              (function | () -> 0 )
              (function | () -> (+) )
              (function
                | s__sig_2 ->
                    Lco_ctrl_tree_record.rml_par
                      (Lco_ctrl_tree_record.rml_seq
                        (Lco_ctrl_tree_record.rml_seq
                          (Lco_ctrl_tree_record.rml_seq
                            (Lco_ctrl_tree_record.rml_seq
                              (Lco_ctrl_tree_record.rml_emit_val'
                                s__sig_2 (function | () -> 42 ))
                              Lco_ctrl_tree_record.rml_pause)
                            Lco_ctrl_tree_record.rml_pause)
                          Lco_ctrl_tree_record.rml_pause)
                        (Lco_ctrl_tree_record.rml_emit_val'
                          s__sig_2 (function | () -> 43 )))
                      (Lco_ctrl_tree_record.rml_def
                        (function | () -> ref 0 )
                        (function
                          | instant__val_rml_3 ->
                              Lco_ctrl_tree_record.rml_loop
                                (Lco_ctrl_tree_record.rml_seq
                                  (Lco_ctrl_tree_record.rml_compute
                                    (function
                                      | () ->
                                          incr instant__val_rml_3;
                                            print_int
                                              ((!) instant__val_rml_3);
                                            print_string " = ";
                                            print_int
                                              (Lco_ctrl_tree_record.rml_pre_value
                                                s__sig_2);
                                            print_newline ()
                                      ))
                                  Lco_ctrl_tree_record.rml_pause)
                          ))
                )
        ) 
;;

module Rml_machine = Rml_machine.M(Lco_ctrl_tree_record);;
let _ = Rml_machine.rml_exec_n p 10
