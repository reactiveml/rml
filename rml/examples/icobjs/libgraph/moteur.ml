open Lk_rewrite;;

open Misc
;;


open Unix
;;


type  pos
= {  xi: int ;   yi: int} ;;


type  color
= Graphics.color ;;


type  objet_graphic
=
  Image of ((Misc.t * pos)) | 
  Texte of ((string * Graphics.color * pos)) | 
  Fill_rect of ((pos * pos * Graphics.color)) | 
  Line of ((pos * pos * Graphics.color)) | 
  Rect of ((pos * pos * Graphics.color)) |  Circle of
  ((pos * int * Graphics.color)) ;;


let get_mouse_pos =
      function
        | () ->
            let status__val_ml_2 =
                  Graphics.wait_next_event (Graphics.Poll :: ([]))
               in
              {xi=(status__val_ml_2).Graphics.mouse_x;
               yi=(status__val_ml_2).Graphics.mouse_y}
         
;;


let get_key = Graphics.read_key 
;;


let color_of_int =
      function | 0 -> Graphics.rgb 0 0 0 | 1 -> Graphics.rgb 0 0 192
        | 2 -> Graphics.rgb 0 204 0 | 3 -> Graphics.rgb 255 255 255
        | 4 -> Graphics.rgb 150 0 0 | 5 -> Graphics.rgb 255 89 65
        | 6 -> Graphics.rgb 255 240 0 | _ -> Graphics.rgb 128 128 0  
;;


let affiche_objet =
      function
        | Image (t__val_ml_6, p__val_ml_7) ->
            Misc.misc_draw_image
              t__val_ml_6 (p__val_ml_7).xi (p__val_ml_7).yi
        | Texte (s__val_ml_8, c__val_ml_9, p__val_ml_10) ->
            Graphics.moveto (p__val_ml_10).xi (p__val_ml_10).yi;
              Graphics.set_color c__val_ml_9;
                Graphics.draw_string s__val_ml_8
        | Rect (x1__val_ml_11, x2__val_ml_12, c__val_ml_13) ->
            Graphics.set_color c__val_ml_13;
              Graphics.fill_rect
                (x1__val_ml_11).xi
                (x1__val_ml_11).yi (x2__val_ml_12).xi (x2__val_ml_12).yi
        | Line (p1__val_ml_14, p2__val_ml_15, c__val_ml_16) ->
            Graphics.set_color c__val_ml_16;
              Graphics.moveto (p1__val_ml_14).xi (p1__val_ml_14).yi;
                Graphics.lineto (p2__val_ml_15).xi (p2__val_ml_15).yi
        | Circle (p__val_ml_17, r__val_ml_18, c__val_ml_19) ->
            Graphics.set_color c__val_ml_19;
              Graphics.fill_circle
                (p__val_ml_17).xi (p__val_ml_17).yi r__val_ml_18
         
;;


let affiche_all =
      function
        | l__val_ml_21 ->
            List.iter
              (function
                | l__val_ml_22 -> List.iter affiche_objet l__val_ml_22 )
              l__val_ml_21
         
;;


let draw_image =
      function
        | t__val_ml_24 ->
            function
              | {xi=x__val_ml_25; yi=y__val_ml_26} ->
                  Misc.misc_draw_image t__val_ml_24 x__val_ml_25 y__val_ml_26
                
;;


let draw_texte =
      function
        | str__val_ml_28 ->
            function
              | col__val_ml_29 ->
                  function
                    | {xi=x__val_ml_30; yi=y__val_ml_31} ->
                        Graphics.moveto x__val_ml_30 y__val_ml_31;
                          Graphics.set_color col__val_ml_29;
                            Graphics.draw_string str__val_ml_28
                       
;;


let draw_rect =
      function
        | {xi=x1__val_ml_33; yi=y1__val_ml_34} ->
            function
              | {xi=x2__val_ml_35; yi=y2__val_ml_36} ->
                  function
                    | col__val_ml_37 ->
                        Graphics.set_color col__val_ml_37;
                          Graphics.fill_rect
                            x1__val_ml_33
                            y1__val_ml_34 x2__val_ml_35 y2__val_ml_36
                       
;;


let fenetre =
      let rec read_click__val_ml_39 =
                function
                  | button_p__val_ml_40 ->
                      function
                        | pos_p__val_ml_41 ->
                            function
                              | k__loc_3 ->
                                  function
                                    | (a_click__sig_44,
                                       a_move__sig_43, mouse_pos__sig_42) ->
                                        Rml_interpreter.rml_def
                                          (function
                                            | () ->
                                                let button__val_ml_45 =
                                                      Graphics.button_down ()
                                                   in (button__val_ml_45)
                                            )
                                          (function
                                            | (button__val_rml_46) ->
                                                Rml_interpreter.rml_def
                                                  (function
                                                    | () ->
                                                        let pos__val_ml_47 =
                                                              get_mouse_pos
                                                                ()
                                                           in
                                                          (pos__val_ml_47)
                                                    )
                                                  (function
                                                    | (pos__val_rml_48) ->
                                                        Rml_interpreter.rml_if
                                                          (function
                                                            | () ->
                                                                button_p__val_ml_40
                                                            )
                                                          (Rml_interpreter.rml_if
                                                            (function
                                                              | () ->
                                                                  not
                                                                    button__val_rml_46
                                                              )
                                                            (Rml_interpreter.rml_emit_val
                                                              a_click__sig_44
                                                              (function
                                                                | () ->
                                                                    pos__val_rml_48
                                                                )
                                                              (Rml_interpreter.rml_emit_val
                                                                mouse_pos__sig_42
                                                                (function
                                                                  | () ->
                                                                    pos__val_rml_48
                                                                  )
                                                                (Rml_interpreter.rml_pause
                                                                  (Rml_interpreter.rml_run
                                                                    (function
                                                                    | () ->
                                                                    read_click__val_ml_39
                                                                    button__val_rml_46
                                                                    pos__val_rml_48
                                                                    )
                                                                    (a_click__sig_44,
                                                                    a_move__sig_43,
                                                                    mouse_pos__sig_42)
                                                                    k__loc_3))))
                                                            (Rml_interpreter.rml_emit_val
                                                              a_move__sig_43
                                                              (function
                                                                | () ->
                                                                    pos__val_rml_48
                                                                )
                                                              (Rml_interpreter.rml_emit_val
                                                                mouse_pos__sig_42
                                                                (function
                                                                  | () ->
                                                                    pos__val_rml_48
                                                                  )
                                                                (Rml_interpreter.rml_pause
                                                                  (Rml_interpreter.rml_run
                                                                    (function
                                                                    | () ->
                                                                    read_click__val_ml_39
                                                                    button__val_rml_46
                                                                    pos__val_rml_48
                                                                    )
                                                                    (a_click__sig_44,
                                                                    a_move__sig_43,
                                                                    mouse_pos__sig_42)
                                                                    k__loc_3)))))
                                                          (Rml_interpreter.rml_emit_val
                                                            a_move__sig_43
                                                            (function
                                                              | () ->
                                                                  {xi=
                                                                   (0);
                                                                   yi=
                                                                   (0)}
                                                              )
                                                            (Rml_interpreter.rml_emit_val
                                                              mouse_pos__sig_42
                                                              (function
                                                                | () ->
                                                                    pos__val_rml_48
                                                                )
                                                              (Rml_interpreter.rml_pause
                                                                (Rml_interpreter.rml_run
                                                                  (function
                                                                    | 
                                                                    () ->
                                                                    read_click__val_ml_39
                                                                    button__val_rml_46
                                                                    pos__val_rml_48
                                                                    )
                                                                  (a_click__sig_44,
                                                                   a_move__sig_43,
                                                                   mouse_pos__sig_42)
                                                                  k__loc_3))))
                                                    )
                                            )
                                         in
        let read_key__val_ml_49 =
              function
                | k__loc_2 ->
                    function
                      | key_push__sig_50 ->
                          Rml_interpreter.rml_loop
                            (Rml_interpreter.rml_if
                              (function | () -> Graphics.key_pressed () )
                              (Rml_interpreter.rml_emit_val
                                key_push__sig_50
                                (function | () -> get_key () )
                                (Rml_interpreter.rml_pause
                                  Rml_interpreter.rml_term))
                              (Rml_interpreter.rml_pause
                                Rml_interpreter.rml_term))
                         in
          function
            | taille_x__val_ml_51 ->
                function
                  | taille_y__val_ml_52 ->
                      function
                        | k__loc_1 ->
                            function
                              | (a_click__sig_56,
                                 a_move__sig_55,
                                 key_push__sig_54, p__sig_57, pos__sig_53) ->
                                  Rml_interpreter.rml_par
                                    (Rml_interpreter.rml_compute
                                      (function
                                        | () ->
                                            Graphics.open_graph
                                              ((^)
                                                " "
                                                ((^)
                                                  (string_of_int
                                                    taille_x__val_ml_51)
                                                  ((^)
                                                    "x"
                                                    (string_of_int
                                                      taille_y__val_ml_52))))
                                        )
                                      (Rml_interpreter.rml_compute
                                        (function
                                          | () -> Graphics.set_line_width 4 )
                                        (Rml_interpreter.rml_compute
                                          (function
                                            | () ->
                                                Graphics.set_color
                                                  (color_of_int 0)
                                            )
                                          (Rml_interpreter.rml_loop
                                            (Rml_interpreter.rml_await_all
                                              p__sig_57
                                              (function
                                                | all__val_rml_58 ->
                                                    Rml_interpreter.rml_compute
                                                      (function
                                                        | () ->
                                                            affiche_all
                                                              all__val_rml_58
                                                        )
                                                      Rml_interpreter.rml_term
                                                ))))))
                                    (Rml_interpreter.rml_par
                                      (Rml_interpreter.rml_run
                                        (function
                                          | () ->
                                              read_click__val_ml_39
                                                false (get_mouse_pos ())
                                          )
                                        (a_click__sig_56,
                                         a_move__sig_55, pos__sig_53)
                                        Rml_interpreter.rml_term)
                                      (Rml_interpreter.rml_run
                                        (function | () -> read_key__val_ml_49
                                          )
                                        key_push__sig_54
                                        Rml_interpreter.rml_term)
                                      Rml_interpreter.rml_term)
                                    k__loc_1
                                  
;;

