open Lco_ctrl_tree;;

open Position
;;


open Global
;;


open Node
;;


let preemptible_node =
      (function
        | pos_init__val_ml_2 ->
            (function
              | move__val_ml_3 ->
                  (function
                    | make_msg__val_ml_4 ->
                        (function
                          | kill__val_ml_5 ->
                              (function
                                | () ->
                                    Rml_interpreter.rml_until
                                      (function | () -> kill__val_ml_5 )
                                      (Rml_interpreter.rml_run
                                        (function
                                          | () ->
                                              Node.node
                                                pos_init__val_ml_2
                                                move__val_ml_3
                                                make_msg__val_ml_4
                                          ))
                                )
                          )
                    )
              )
        ) 
;;


let rec add =
          (function
            | new_node__val_ml_7 ->
                (function
                  | start__val_ml_8 ->
                      (function
                        | () ->
                            Rml_interpreter.rml_await_all
                              (function | () -> new_node__val_ml_7 )
                              (function
                                | pos__val_rml_9 ->
                                    Rml_interpreter.rml_par
                                      (Rml_interpreter.rml_run
                                        (function
                                          | () ->
                                              add
                                                new_node__val_ml_7
                                                start__val_ml_8
                                          ))
                                      (Rml_interpreter.rml_seq
                                        (Rml_interpreter.rml_await_immediate
                                          (function | () -> start__val_ml_8 ))
                                        (Rml_interpreter.rml_def
                                          (function
                                            | () ->
                                                (let pos__val_ml_10 =
                                                       {Position.x=(Random.int
                                                                    Global.max_x);
                                                        Position.y=(Random.int
                                                                    Global.max_y)}
                                                   in (pos__val_ml_10))
                                            )
                                          (function
                                            | (pos__val_rml_11) ->
                                                Rml_interpreter.rml_run
                                                  (function
                                                    | () ->
                                                        Node.node
                                                          pos__val_rml_11
                                                          (Move.random_waypoint
                                                            ((/)
                                                              Global.max_x 4)
                                                            pos__val_rml_11)
                                                          Msg.make
                                                    )
                                            )))
                                )
                        )
                  )
            ) 
;;

