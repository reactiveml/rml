(* THIS FILE IS GENERATED. *)
(* rmlc alarm.rml  *)

open Implem_lco_ctrl_tree_record;;
let sleep =
      (function
        | d__val_rml_2  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_loop_n
                     (function
                       | ()  ->
                           Pervasives.(+) (Pervasives.(-) d__val_rml_2 1) 1
                       )
                     Lco_ctrl_tree_record.rml_pause
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let alarm =
      (function
        | off__val_rml_5  ->
            (function
              | arm__val_rml_6  ->
                  (function
                    | snooze__val_rml_7  ->
                        ((function
                           | ()  ->
                               (let (idle__val_rml_8, armed__val_rml_9) =
                                      (let rec idle__val_rml_8 =
                                                 ((function
                                                    | ()  ->
                                                        Lco_ctrl_tree_record.rml_until_handler'
                                                          arm__val_rml_6
                                                          Lco_ctrl_tree_record.rml_halt
                                                          (function
                                                            | d__val_rml_10
                                                                 ->
                                                                Lco_ctrl_tree_record.rml_run
                                                                  (function
                                                                    | 
                                                                    ()  ->
                                                                    armed__val_rml_9
                                                                    d__val_rml_10
                                                                    )
                                                            )
                                                    ):
                                                   (_)
                                                     Lco_ctrl_tree_record.process)
                                                and
                                               armed__val_rml_9 =
                                                 (function
                                                   | d__val_rml_11  ->
                                                       ((function
                                                          | ()  ->
                                                              Lco_ctrl_tree_record.rml_until_handler_match_conf
                                                                (Lco_ctrl_tree_record.cfg_or_option
                                                                  (Lco_ctrl_tree_record.cfg_present'
                                                                    snooze__val_rml_7)
                                                                  (Lco_ctrl_tree_record.cfg_present'
                                                                    off__val_rml_5))
                                                                (function
                                                                  | (Some
                                                                    (d__val_rml_12),
                                                                    _)  ->
                                                                    true
                                                                  | (_,
                                                                    Some (_))
                                                                     -> 
                                                                    true
                                                                  | _  ->
                                                                    false
                                                                  )
                                                                (Lco_ctrl_tree_record.rml_run
                                                                  (function
                                                                    | 
                                                                    ()  ->
                                                                    sleep
                                                                    d__val_rml_11
                                                                    ))
                                                                (function
                                                                  | (Some
                                                                    (d__val_rml_12),
                                                                    _)  ->
                                                                    Lco_ctrl_tree_record.rml_run
                                                                    (function
                                                                    | ()  ->
                                                                    armed__val_rml_9
                                                                    d__val_rml_12
                                                                    )
                                                                  | (_,
                                                                    Some (_))
                                                                     ->
                                                                    Lco_ctrl_tree_record.rml_run
                                                                    (function
                                                                    | ()  ->
                                                                    idle__val_rml_8
                                                                    )
                                                                  | _  ->
                                                                    Pervasives.raise
                                                                    Lco_ctrl_tree_record.RML
                                                                  )
                                                          ):
                                                         (_)
                                                           Lco_ctrl_tree_record.process)
                                                   )
                                                in
                                        (idle__val_rml_8, armed__val_rml_9))
                                  in
                                 Lco_ctrl_tree_record.rml_run
                                   (function | ()  -> idle__val_rml_8 ))
                           ):
                          (_) Lco_ctrl_tree_record.process)
                    )
              )
        ) 
;;
