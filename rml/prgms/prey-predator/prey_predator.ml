open Lco_ctrl_tree;;

type  sprite
= {
  mutable x: int ; 
  mutable y: int ; 
  mutable sx: int ; 
  mutable sy: int ; 
   index: int ; 
  mutable destroy: (unit, unit) Rml_interpreter.event ;  mutable dead: bool}
  ;;


let inertia_factor = 10 
;;


let min_border_x = 10 
;;


let max_border_x = 210 
;;


let min_border_y = 10 
;;


let max_border_y = 130 
;;


let min_collision = 400 
;;


let kill_dist2 = 200 
;;


let max_pred_speed = 10 
;;


let pred_run_step = 1 
;;


let predator_visibility = 40000 
;;


let a_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    b_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    up_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    down_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    left_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    right_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    r_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
     and
    l_pressed =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) )
    
;;


let (pred_evt: (sprite, (sprite) list) Rml_interpreter.event) =
      Rml_interpreter.rml_global_signal_combine
        ([])
        (function
          | x__val_ml_20 ->
              (function | y__val_ml_21 -> x__val_ml_20 :: y__val_ml_21 )
          ) 
;;


let (prey_evt: (sprite, (sprite) list) Rml_interpreter.event) =
      Rml_interpreter.rml_global_signal_combine
        ([])
        (function
          | x__val_ml_23 ->
              (function | y__val_ml_24 -> x__val_ml_23 :: y__val_ml_24 )
          ) 
;;


let next_sprite =
      (let cpt__val_ml_26 = ref 0  in
        function | () -> incr cpt__val_ml_26; (!) cpt__val_ml_26 ) 
;;


let tmp =
      Rml_interpreter.rml_global_signal_combine
        () (function | () -> (function | () -> () ) ) 
;;


let new_sprite =
      (function
        | speed__val_ml_29 ->
            {x=((+)
                 (Random.int ((-) max_border_x min_border_x)) min_border_x);
             y=((+)
                 (Random.int ((-) max_border_y min_border_y)) min_border_y);
             sx=(Random.int speed__val_ml_29);
             sy=(Random.int speed__val_ml_29);
             index=(next_sprite ()); destroy=(tmp); dead=(false)}
        ) 
;;


let read_key =
      (function
        | key__val_ml_31 ->
            (function
              | () ->
                  Rml_interpreter.rml_loop
                    (Rml_interpreter.rml_seq
                      (Rml_interpreter.rml_if
                        (function | () -> Graphics.key_pressed () )
                        (Rml_interpreter.rml_emit_val
                          (function | () -> key__val_ml_31 )
                          (function | () -> Graphics.read_key () ))
                        Rml_interpreter.rml_nothing)
                      Rml_interpreter.rml_pause)
              )
        ) 
;;


let key_handler =
      (function
        | () ->
            Rml_interpreter.rml_loop
              (Rml_interpreter.rml_seq
                (Rml_interpreter.rml_if
                  (function | () -> Graphics.key_pressed () )
                  (Rml_interpreter.rml_match
                    (function | () -> Graphics.read_key () )
                    (function
                      | 'a' ->
                          Rml_interpreter.rml_emit
                            (function | () -> a_pressed )
                      | 'b' ->
                          Rml_interpreter.rml_emit
                            (function | () -> b_pressed )
                      | 'i' ->
                          Rml_interpreter.rml_emit
                            (function | () -> up_pressed )
                      | 'k' ->
                          Rml_interpreter.rml_emit
                            (function | () -> down_pressed )
                      | 'j' ->
                          Rml_interpreter.rml_emit
                            (function | () -> left_pressed )
                      | 'l' ->
                          Rml_interpreter.rml_emit
                            (function | () -> right_pressed )
                      | 'o' ->
                          Rml_interpreter.rml_emit
                            (function | () -> r_pressed )
                      | 'p' ->
                          Rml_interpreter.rml_emit
                            (function | () -> l_pressed )
                      | _ -> Rml_interpreter.rml_nothing ))
                  Rml_interpreter.rml_nothing)
                Rml_interpreter.rml_pause)
        ) 
;;


let key_processing =
      (function
        | key_pressed__val_ml_34 ->
            (function
              | callback__val_ml_35 ->
                  (function
                    | () ->
                        Rml_interpreter.rml_loop
                          (Rml_interpreter.rml_seq
                            (Rml_interpreter.rml_await_immediate
                              (function | () -> key_pressed__val_ml_34 ))
                            (Rml_interpreter.rml_seq
                              (Rml_interpreter.rml_compute
                                (function | () -> callback__val_ml_35 () ))
                              (Rml_interpreter.rml_for
                                (function | () -> 1 )
                                (function | () -> 10 )
                                true
                                (function
                                  | i__val_rml_36 ->
                                      Rml_interpreter.rml_pause
                                  ))))
                    )
              )
        ) 
;;


let inertia =
      (function
        | s__val_ml_38 ->
            (function
              | () ->
                  Rml_interpreter.rml_loop
                    (Rml_interpreter.rml_seq
                      (Rml_interpreter.rml_compute
                        (function
                          | () ->
                              s__val_ml_38.x <-
                                (+)
                                  ((/) (s__val_ml_38).sx inertia_factor)
                                  (s__val_ml_38).sx
                          ))
                      (Rml_interpreter.rml_seq
                        (Rml_interpreter.rml_compute
                          (function
                            | () ->
                                s__val_ml_38.y <-
                                  (+)
                                    ((/) (s__val_ml_38).sy inertia_factor)
                                    (s__val_ml_38).sy
                            ))
                        Rml_interpreter.rml_pause))
              )
        ) 
;;


let go_right =
      (function
        | s__val_ml_40 ->
            (function
              | dist__val_ml_41 ->
                  s__val_ml_40.x <- (+) (s__val_ml_40).x dist__val_ml_41;
                    if (>) (s__val_ml_40).x max_border_x then
                      s__val_ml_40.x <- max_border_x else ()
              )
        ) 
;;


let go_left =
      (function
        | s__val_ml_43 ->
            (function
              | dist__val_ml_44 ->
                  s__val_ml_43.x <- (-) (s__val_ml_43).x dist__val_ml_44;
                    if (<) (s__val_ml_43).x min_border_x then
                      s__val_ml_43.x <- min_border_x else ()
              )
        ) 
;;


let go_up =
      (function
        | s__val_ml_46 ->
            (function
              | dist__val_ml_47 ->
                  s__val_ml_46.y <- (+) (s__val_ml_46).y dist__val_ml_47;
                    if (>) (s__val_ml_46).y max_border_y then
                      s__val_ml_46.y <- max_border_y else ()
              )
        ) 
;;


let go_down =
      (function
        | s__val_ml_49 ->
            (function
              | dist__val_ml_50 ->
                  s__val_ml_49.y <- (-) (s__val_ml_49).y dist__val_ml_50;
                    if (<) (s__val_ml_49).y min_border_y then
                      s__val_ml_49.y <- min_border_y else ()
              )
        ) 
;;


let move =
      (function
        | s__val_ml_52 ->
            (function
              | dir_pressed__val_ml_53 ->
                  (function
                    | mvt__val_ml_54 ->
                        (function
                          | dist__val_ml_55 ->
                              (function
                                | () ->
                                    Rml_interpreter.rml_loop
                                      (Rml_interpreter.rml_seq
                                        (Rml_interpreter.rml_await_immediate
                                          (function
                                            | () -> dir_pressed__val_ml_53 ))
                                        (Rml_interpreter.rml_seq
                                          (Rml_interpreter.rml_compute
                                            (function
                                              | () ->
                                                  mvt__val_ml_54
                                                    s__val_ml_52
                                                    dist__val_ml_55
                                              ))
                                          Rml_interpreter.rml_pause))
                                )
                          )
                    )
              )
        ) 
;;


let move_right =
      (function
        | s__val_ml_57 ->
            (function
              | dist__val_ml_58 ->
                  move s__val_ml_57 right_pressed go_right dist__val_ml_58
              )
        ) 
;;


let dist2 =
      (function
        | s1__val_ml_60 ->
            (function
              | s2__val_ml_61 ->
                  (let dx__val_ml_62 =
                         (-) (s2__val_ml_61).x (s1__val_ml_60).x
                     in
                    let dy__val_ml_63 =
                          (-) (s2__val_ml_61).y (s1__val_ml_60).y
                       in
                      (+)
                        (( * ) dx__val_ml_62 dx__val_ml_62)
                        (( * ) dy__val_ml_63 dy__val_ml_63))
              )
        ) 
;;


let collision =
      (function
        | s1__val_ml_65 ->
            (function
              | s2__val_ml_66 ->
                  (let dx__val_ml_67 =
                         (-) (s2__val_ml_66).x (s1__val_ml_65).x
                     in
                    let dy__val_ml_68 =
                          (-) (s2__val_ml_66).y (s1__val_ml_65).y
                       in
                      if
                        (<=)
                          (dist2 s1__val_ml_65 s2__val_ml_66) min_collision
                        then
                        ((=) (s1__val_ml_65).sx ((~-) (s1__val_ml_65).sx);
                          (=) (s1__val_ml_65).sy ((~-) (s1__val_ml_65).sy);
                            if (>) dx__val_ml_67 0 then
                              go_left s1__val_ml_65 dx__val_ml_67 else
                              go_right s1__val_ml_65 ((~-) dx__val_ml_67);
                              if (>) dy__val_ml_68 0 then
                                go_up s1__val_ml_65 dy__val_ml_68 else
                                go_down s1__val_ml_65 ((~-) dy__val_ml_68))
                        else ())
              )
        ) 
;;


let collide =
      (function
        | me__val_ml_70 ->
            (function
              | collide_evt__val_ml_71 ->
                  (function
                    | () ->
                        Rml_interpreter.rml_loop
                          (Rml_interpreter.rml_seq
                            (Rml_interpreter.rml_emit_val
                              (function | () -> collide_evt__val_ml_71 )
                              (function | () -> me__val_ml_70 ))
                            (Rml_interpreter.rml_await_all
                              (function | () -> collide_evt__val_ml_71 )
                              (function
                                | all__val_rml_72 ->
                                    Rml_interpreter.rml_compute
                                      (function
                                        | () ->
                                            List.iter
                                              (function
                                                | other__val_ml_73 ->
                                                    collision
                                                      me__val_ml_70
                                                      other__val_ml_73
                                                )
                                              all__val_rml_72
                                        )
                                )))
                    )
              )
        ) 
;;


let bounce_on_borders =
      (function
        | s__val_ml_75 ->
            (function
              | () ->
                  Rml_interpreter.rml_loop
                    (Rml_interpreter.rml_seq
                      (Rml_interpreter.rml_compute
                        (function
                          | () ->
                              if (<) (s__val_ml_75).x min_border_x then
                                (s__val_ml_75.sx <- (~-) (s__val_ml_75).sx;
                                  s__val_ml_75.x <- min_border_x)
                                else ()
                          ))
                      (Rml_interpreter.rml_seq
                        (Rml_interpreter.rml_compute
                          (function
                            | () ->
                                if (<) (s__val_ml_75).y min_border_y then
                                  (s__val_ml_75.sy <- (~-) (s__val_ml_75).sy;
                                    s__val_ml_75.y <- min_border_y)
                                  else ()
                            ))
                        (Rml_interpreter.rml_seq
                          (Rml_interpreter.rml_compute
                            (function
                              | () ->
                                  if (>) (s__val_ml_75).x max_border_x then
                                    (s__val_ml_75.sx <-
                                       (~-) (s__val_ml_75).sx;
                                      s__val_ml_75.x <- max_border_x)
                                    else ()
                              ))
                          (Rml_interpreter.rml_seq
                            (Rml_interpreter.rml_compute
                              (function
                                | () ->
                                    if (>) (s__val_ml_75).y max_border_y then
                                      (s__val_ml_75.sy <-
                                         (~-) (s__val_ml_75).sy;
                                        s__val_ml_75.y <- max_border_y)
                                      else ()
                                ))
                            Rml_interpreter.rml_pause))))
              )
        ) 
;;


let chase =
      (function
        | me__val_ml_77 ->
            (function
              | target__val_ml_78 ->
                  (let dx__val_ml_79 =
                         (-) (target__val_ml_78).x (me__val_ml_77).x
                     in
                    let dy__val_ml_80 =
                          (-) (target__val_ml_78).y (me__val_ml_77).y
                       in
                      let d2__val_ml_81 =
                            dist2 me__val_ml_77 target__val_ml_78
                         in
                        if (<) d2__val_ml_81 kill_dist2 then
                          (Rml_interpreter.rml_expr_emit
                             (function | () -> (target__val_ml_78).destroy );
                            target__val_ml_78.dead <- true)
                          else
                          if
                            (&&)
                              ((>) dx__val_ml_79 0)
                              ((<) (me__val_ml_77).sx max_pred_speed)
                            then
                            me__val_ml_77.sx <-
                              (+) (me__val_ml_77).sx pred_run_step
                            else
                            if
                              (&&)
                                ((<) dx__val_ml_79 0)
                                ((<)
                                  ((~-) max_pred_speed) (me__val_ml_77).sx)
                              then
                              me__val_ml_77.sx <-
                                (-) (me__val_ml_77).sx pred_run_step
                              else
                              if
                                (&&)
                                  ((>) dy__val_ml_80 0)
                                  ((<) (me__val_ml_77).sy max_pred_speed)
                                then
                                me__val_ml_77.sy <-
                                  (+) (me__val_ml_77).sy pred_run_step
                                else
                                if
                                  (&&)
                                    ((<) dy__val_ml_80 0)
                                    ((<)
                                      ((~-) max_pred_speed)
                                      (me__val_ml_77).sy)
                                  then
                                  me__val_ml_77.sy <-
                                    (-) (me__val_ml_77).sy pred_run_step
                                  else ())
              )
        ) 
;;


let predator =
      (function
        | me__val_ml_83 ->
            (function
              | pred_evt__val_ml_84 ->
                  (function
                    | prey_evt__val_ml_85 ->
                        (function
                          | () ->
                              Rml_interpreter.rml_loop
                                (Rml_interpreter.rml_seq
                                  (Rml_interpreter.rml_emit_val
                                    (function | () -> pred_evt__val_ml_84 )
                                    (function | () -> me__val_ml_83 ))
                                  (Rml_interpreter.rml_await_all
                                    (function | () -> prey_evt__val_ml_85 )
                                    (function
                                      | all__val_rml_86 ->
                                          Rml_interpreter.rml_match
                                            (function
                                              | () ->
                                                  List.fold_left
                                                    (function
                                                      | (min__val_ml_88,
                                                         closest__val_ml_89) ->
                                                          (function
                                                            | other__val_ml_90 ->
                                                                (let 
                                                                  d2__val_ml_91
                                                                    =
                                                                    dist2
                                                                    me__val_ml_83
                                                                    other__val_ml_90
                                                                   in
                                                                  if
                                                                    (&&)
                                                                    ((!=)
                                                                    me__val_ml_83
                                                                    other__val_ml_90)
                                                                    ((<)
                                                                    d2__val_ml_91
                                                                    min__val_ml_88)
                                                                    then
                                                                    (d2__val_ml_91,
                                                                    (Some
                                                                    other__val_ml_90))
                                                                    else
                                                                    (min__val_ml_88,
                                                                    closest__val_ml_89))
                                                            )
                                                      )
                                                    (predator_visibility,
                                                     None)
                                                    all__val_rml_86
                                              )
                                            (function
                                              | (_, None) ->
                                                  Rml_interpreter.rml_nothing
                                              | (_, Some closest__val_rml_87) ->
                                                  Rml_interpreter.rml_compute
                                                    (function
                                                      | () ->
                                                          chase
                                                            me__val_ml_83
                                                            closest__val_rml_87
                                                      )
                                              )
                                      )))
                          )
                    )
              )
        ) 
;;


let predator_sprite =
      (function
        | () ->
            Rml_interpreter.rml_def
              (function
                | () -> (let s__val_ml_93 = new_sprite 0  in (s__val_ml_93))
                )
              (function
                | (s__val_rml_94) ->
                    Rml_interpreter.rml_par
                      (Rml_interpreter.rml_run
                        (function
                          | () -> predator s__val_rml_94 pred_evt prey_evt ))
                      (Rml_interpreter.rml_par
                        (Rml_interpreter.rml_run
                          (function
                            | () ->
                                move
                                  s__val_rml_94
                                  right_pressed go_right pred_run_step
                            ))
                        (Rml_interpreter.rml_par
                          (Rml_interpreter.rml_run
                            (function
                              | () ->
                                  move
                                    s__val_rml_94
                                    left_pressed go_left pred_run_step
                              ))
                          (Rml_interpreter.rml_par
                            (Rml_interpreter.rml_run
                              (function
                                | () ->
                                    move
                                      s__val_rml_94
                                      up_pressed go_up pred_run_step
                                ))
                            (Rml_interpreter.rml_par
                              (Rml_interpreter.rml_run
                                (function
                                  | () ->
                                      move
                                        s__val_rml_94
                                        down_pressed go_down pred_run_step
                                  ))
                              (Rml_interpreter.rml_par
                                (Rml_interpreter.rml_run
                                  (function
                                    | () -> bounce_on_borders s__val_rml_94 ))
                                (Rml_interpreter.rml_par
                                  (Rml_interpreter.rml_run
                                    (function
                                      | () -> collide s__val_rml_94 pred_evt
                                      ))
                                  (Rml_interpreter.rml_signal_combine
                                    (function | () -> () )
                                    (function
                                      | () ->
                                          (function
                                            | () -> (function | () -> () ) )
                                      )
                                    (function
                                      | suspend__sig_95 ->
                                          Rml_interpreter.rml_par
                                            (Rml_interpreter.rml_control
                                              (function
                                                | () -> suspend__sig_95 )
                                              (Rml_interpreter.rml_run
                                                (function
                                                  | () ->
                                                      inertia s__val_rml_94
                                                  )))
                                            (Rml_interpreter.rml_loop
                                              (Rml_interpreter.rml_seq
                                                (Rml_interpreter.rml_await
                                                  (function | () -> a_pressed
                                                    ))
                                                (Rml_interpreter.rml_seq
                                                  (Rml_interpreter.rml_emit
                                                    (function
                                                      | () -> suspend__sig_95
                                                      ))
                                                  (Rml_interpreter.rml_seq
                                                    (Rml_interpreter.rml_await
                                                      (function
                                                        | () -> b_pressed ))
                                                    (Rml_interpreter.rml_emit
                                                      (function
                                                        | () ->
                                                            suspend__sig_95
                                                        ))))))
                                      ))))))))
                )
        ) 
;;

