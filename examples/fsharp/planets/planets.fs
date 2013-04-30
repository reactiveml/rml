module Planets

open Caml_compat
module Interpreter = Lco
  
type coord = { x : float; y : float; z : float }

type planet = { id : int; masse : float; pos : coord; v : coord }

let g = 6.67
  
let dt = 0.1
  
let v_init () =
  {
    x = Pervasives.( -. ) (Random.float 100.) 50.;
    y = Pervasives.( -. ) (Random.float 100.) 50.;
    z = Pervasives.( -. ) (Random.float 100.) 50.;
  }
  
let new_pos x__val_rml_7 y__val_rml_8 =
  let max_x_2__val_rml_9 = Pervasives.( / ) (Graphics.size_x ()) 2 in
  let max_y_2__val_rml_10 = Pervasives.( / ) (Graphics.size_y ()) 2
  in
    {
      x =
        Pervasives.float_of_int
          (Pervasives.( - ) x__val_rml_7 max_x_2__val_rml_9);
      y =
        Pervasives.float_of_int
          (Pervasives.( - ) y__val_rml_8 max_y_2__val_rml_10);
      z = Pervasives.( -. ) (Random.float 200.) 100.;
    }
  
let color_of_int =
  function
  | 0 -> Graphics.yellow
  | 1 -> Graphics.blue
  | 2 -> Graphics.green
  | 3 -> Graphics.red
  | 4 -> Graphics.cyan
  | 5 -> Graphics.black
  | 6 -> Graphics.magenta
  | _ -> Graphics.black
  
let compute_pos =
  let distance2__val_rml_13 pos1__val_rml_14 pos2__val_rml_15 =
    Pervasives.( +. )
      (Pervasives.( +. )
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_15.x pos1__val_rml_14.x)
            (Pervasives.( -. ) pos2__val_rml_15.x pos1__val_rml_14.x))
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_15.y pos1__val_rml_14.y)
            (Pervasives.( -. ) pos2__val_rml_15.y pos1__val_rml_14.y)))
      (Pervasives.( *. )
         (Pervasives.( -. ) pos2__val_rml_15.z pos1__val_rml_14.z)
         (Pervasives.( -. ) pos2__val_rml_15.z pos1__val_rml_14.z)) in
  let force__val_rml_16 p1__val_rml_17 p2__val_rml_18 =
    let d2__val_rml_19 =
      distance2__val_rml_13 p1__val_rml_17.pos p2__val_rml_18.pos in
    let d__val_rml_20 = Pervasives.sqrt d2__val_rml_19
    in
      if Pervasives.( <> ) d__val_rml_20 0.
      then
        (let f12__val_rml_21 =
           Pervasives.( /. ) (Pervasives.( *. ) g p2__val_rml_18.masse)
             d2__val_rml_19
         in
           ((Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_21
                  (Pervasives.( -. ) p2__val_rml_18.pos.x
                     p1__val_rml_17.pos.x))
               d__val_rml_20),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_21
                  (Pervasives.( -. ) p2__val_rml_18.pos.y
                     p1__val_rml_17.pos.y))
               d__val_rml_20),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_21
                  (Pervasives.( -. ) p2__val_rml_18.pos.z
                     p1__val_rml_17.pos.z))
               d__val_rml_20)))
      else (0., 0., 0.)
  in
    fun me__val_rml_22 all__val_rml_23 ->
      let (fx__val_rml_24, fy__val_rml_25, fz__val_rml_26) =
        List.fold_left
          (fun (fx__val_rml_27, fy__val_rml_28, fz__val_rml_29) p__val_rml_30
             ->
             let (x__val_rml_31, y__val_rml_32, z__val_rml_33) =
               force__val_rml_16 me__val_rml_22 p__val_rml_30
             in
               ((Pervasives.( +. ) fx__val_rml_27 x__val_rml_31),
                (Pervasives.( +. ) fy__val_rml_28 y__val_rml_32),
                (Pervasives.( +. ) fz__val_rml_29 z__val_rml_33)))
          (0., 0., 0.) all__val_rml_23 in
      let v__val_rml_34 =
        {
          x =
            Pervasives.( +. ) me__val_rml_22.v.x
              (Pervasives.( *. ) fx__val_rml_24 dt);
          y =
            Pervasives.( +. ) me__val_rml_22.v.y
              (Pervasives.( *. ) fy__val_rml_25 dt);
          z =
            Pervasives.( +. ) me__val_rml_22.v.z
              (Pervasives.( *. ) fz__val_rml_26 dt);
        } in
      let pos__val_rml_35 =
        {
          x =
            Pervasives.( +. ) me__val_rml_22.pos.x
              (Pervasives.( *. ) v__val_rml_34.x dt);
          y =
            Pervasives.( +. ) me__val_rml_22.pos.y
              (Pervasives.( *. ) v__val_rml_34.y dt);
          z =
            Pervasives.( +. ) me__val_rml_22.pos.z
              (Pervasives.( *. ) v__val_rml_34.z dt);
        }
      in
        {
          id = me__val_rml_22.id;
          masse = me__val_rml_22.masse;
          pos = pos__val_rml_35;
          v = v__val_rml_34;
        }
  
let maj_aff all__val_rml_37 =
  (Graphics.clear_graph ();
   let max_x_2__val_rml_38 = Pervasives.( / ) (Graphics.size_x ()) 2 in
   let max_y_2__val_rml_39 = Pervasives.( / ) (Graphics.size_y ()) 2
   in
     List.iter
       (fun p__val_rml_40 ->
          (Graphics.set_color
             (color_of_int (Pervasives.( mod ) p__val_rml_40.id 7));
           Graphics.fill_circle
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_40.pos.x)
                max_x_2__val_rml_38)
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_40.pos.y)
                max_y_2__val_rml_39)
             (if
                Pervasives.( && )
                  (Pervasives.( < ) (-200.) p__val_rml_40.pos.z)
                  (Pervasives.( < ) p__val_rml_40.pos.z 5000.)
              then
                Pervasives.( + ) 5
                  (Pervasives.( / )
                     (Pervasives.int_of_float p__val_rml_40.pos.z) 50)
              else 1)))
       all__val_rml_37)
  
let planet id__val_rml_42 masse__val_rml_43 pos_init__val_rml_44
  v_init__val_rml_45 p__val_rml_46 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_def
      (fun () ->
         Pervasives.ref
           {
             id = id__val_rml_42;
             masse = masse__val_rml_43;
             pos = pos_init__val_rml_44;
             v = v_init__val_rml_45;
           })
      (fun me__val_rml_47 ->
         Interpreter.rml_loop
           (Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () ->
                    Interpreter.rml_expr_emit_val p__val_rml_46
                      (Pervasives.( ! ) me__val_rml_47)))
              (Interpreter.rml_await_all' p__val_rml_46
                 (fun all__val_rml_48 ->
                    Interpreter.rml_compute
                      (fun () ->
                         Pervasives.( := ) me__val_rml_47
                           (compute_pos (Pervasives.( ! ) me__val_rml_47)
                              all__val_rml_48))))))
  
let soleil id__val_rml_50 masse__val_rml_51 pos_init__val_rml_52
  p__val_rml_53 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_def
      (fun () ->
         {
           id = id__val_rml_50;
           masse = masse__val_rml_51;
           pos = pos_init__val_rml_52;
           v = { x = 0.; y = 0.; z = 0.; };
         })
      (fun me__val_rml_54 ->
         Interpreter.rml_loop
           (Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () ->
                    Interpreter.rml_expr_emit_val p__val_rml_53
                      me__val_rml_54))
              (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock))))
  
let click_of_button_down click__val_rml_56 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_loop
      (Interpreter.rml_seq
         (Interpreter.rml_compute
            (fun () ->
               if Graphics.button_down ()
               then
                 Interpreter.rml_expr_emit_val click__val_rml_56
                   (Graphics.mouse_pos ())
               else ()))
         (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let key_of_key_pressed key__val_rml_58 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_loop
      (Interpreter.rml_seq
         (Interpreter.rml_compute
            (fun () ->
               if Graphics.key_pressed ()
               then
                 Interpreter.rml_expr_emit_val key__val_rml_58
                   (Graphics.read_key ())
               else ()))
         (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let fenetre new_planet__val_rml_60 p__val_rml_61 kill_sun__val_rml_62
  kill__val_rml_63 suspend__val_rml_64 : _ Interpreter.process =
  fun () ->
    let read_click__val_rml_65 : _ Interpreter.process =
      fun () ->
        Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
          Interpreter.rml_base_clock None
          (fun click__sig_66 ->
             Interpreter.rml_par
               (Interpreter.rml_run
                  (fun () -> click_of_button_down click__sig_66))
               (Interpreter.rml_loop
                  (Interpreter.rml_present' click__sig_66
                     (Interpreter.rml_pause
                        (fun () -> Interpreter.rml_base_clock))
                     (Interpreter.rml_await_immediate_one' click__sig_66
                        (fun (x__val_rml_67, y__val_rml_68) ->
                           Interpreter.rml_seq
                             (Interpreter.rml_compute
                                (fun () ->
                                   Interpreter.rml_expr_emit_val
                                     new_planet__val_rml_60
                                     (new_pos x__val_rml_67 y__val_rml_68)))
                             (Interpreter.rml_pause
                                (fun () -> Interpreter.rml_base_clock))))))) in
    let read_key__val_rml_69 : _ Interpreter.process =
      fun () ->
        Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
          Interpreter.rml_base_clock None
          (fun key__sig_70 ->
             Interpreter.rml_par
               (Interpreter.rml_run
                  (fun () -> key_of_key_pressed key__sig_70))
               (Interpreter.rml_loop
                  (Interpreter.rml_await_immediate_one' key__sig_70
                     (fun c__val_rml_71 ->
                        Interpreter.rml_seq
                          (Interpreter.rml_compute
                             (fun () ->
                                Pervasives.print_string "key_pressed = "))
                          (Interpreter.rml_seq
                             (Interpreter.rml_compute
                                (fun () ->
                                   Pervasives.print_char c__val_rml_71))
                             (Interpreter.rml_seq
                                (Interpreter.rml_compute
                                   (fun () -> Pervasives.print_newline ()))
                                (Interpreter.rml_seq
                                   (Interpreter.rml_match
                                      (fun () -> c__val_rml_71)
                                      (function
                                       | 's' ->
                                           Interpreter.rml_compute
                                             (fun () ->
                                                Interpreter.rml_expr_emit
                                                  kill_sun__val_rml_62)
                                       | 'k' ->
                                           Interpreter.rml_compute
                                             (fun () ->
                                                Interpreter.rml_expr_emit
                                                  kill__val_rml_63)
                                       | 'p' ->
                                           Interpreter.rml_compute
                                             (fun () ->
                                                Interpreter.rml_expr_emit
                                                  suspend__val_rml_64)
                                       | 'q' ->
                                           Interpreter.rml_compute
                                             (fun () -> Pervasives.exit 0)
                                       | _ -> Interpreter.rml_nothing))
                                   (Interpreter.rml_pause
                                      (fun () -> Interpreter.rml_base_clock)))))))))
    in
      Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
        Interpreter.rml_base_clock None
        (fun aff__sig_72 ->
           Interpreter.rml_seq
             (Interpreter.rml_compute (fun () -> Graphics.open_graph ""))
             (Interpreter.rml_seq
                (Interpreter.rml_compute
                   (fun () -> Graphics.auto_synchronize false))
                (Interpreter.rml_par
                   (Interpreter.rml_loop
                      (Interpreter.rml_await_all' p__val_rml_61
                         (fun all__val_rml_73 ->
                            Interpreter.rml_seq
                              (Interpreter.rml_compute
                                 (fun () -> maj_aff all__val_rml_73))
                              (Interpreter.rml_seq
                                 (Interpreter.rml_compute
                                    (fun () -> Graphics.synchronize ()))
                                 (Interpreter.rml_seq
                                    (Interpreter.rml_pause
                                       (fun () -> Interpreter.rml_base_clock))
                                    (Interpreter.rml_pause
                                       (fun () -> Interpreter.rml_base_clock)))))))
                   (Interpreter.rml_par
                      (Interpreter.rml_run (fun () -> read_click__val_rml_65))
                      (Interpreter.rml_run (fun () -> read_key__val_rml_69))))))
  
let rec add_aux id__val_rml_75 new_planet__val_rml_76 kill__val_rml_77
  kill_new__val_rml_78 p__val_rml_79 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_await_all' new_planet__val_rml_76
      (fun pos__val_rml_80 ->
         Interpreter.rml_par
           (Interpreter.rml_until' kill_new__val_rml_78
              (Interpreter.rml_run
                 (fun () ->
                    planet id__val_rml_75 1. (List.hd pos__val_rml_80)
                      (v_init ()) p__val_rml_79)))
           (Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
              Interpreter.rml_base_clock None
              (fun new_kill_new__sig_81 ->
                 Interpreter.rml_par
                   (Interpreter.rml_run
                      (fun () ->
                         add_aux (Pervasives.( + ) id__val_rml_75 1)
                           new_planet__val_rml_76 kill__val_rml_77
                           new_kill_new__sig_81 p__val_rml_79))
                   (Interpreter.rml_seq
                      (Interpreter.rml_par
                         (Interpreter.rml_await' new_planet__val_rml_76)
                         (Interpreter.rml_await_immediate'
                            kill_new__val_rml_78))
                      (Interpreter.rml_seq
                         (Interpreter.rml_await' kill__val_rml_77)
                         (Interpreter.rml_compute
                            (fun () ->
                               Interpreter.rml_expr_emit new_kill_new__sig_81)))))))
  
let add id__val_rml_83 new_planet__val_rml_84 kill__val_rml_85
  p__val_rml_86 : _ Interpreter.process =
  fun () ->
    Interpreter.rml_run
      (fun () ->
         add_aux id__val_rml_83 new_planet__val_rml_84 kill__val_rml_85
           kill__val_rml_85 p__val_rml_86)
  
let systeme : _ Interpreter.process =
  fun () ->
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun p__sig_88 ->
         Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
           Interpreter.rml_base_clock None
           (fun new_planet__sig_89 ->
              Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
                Interpreter.rml_base_clock None
                (fun kill_sun__sig_90 ->
                   Interpreter.rml_signal Types.Signal Interpreter.
                     rml_base_clock Interpreter.rml_base_clock None
                     (fun kill__sig_91 ->
                        Interpreter.rml_signal Types.Signal Interpreter.
                          rml_base_clock Interpreter.rml_base_clock None
                          (fun suspend__sig_92 ->
                             Interpreter.rml_signal Types.Signal Interpreter.
                               rml_base_clock Interpreter.rml_base_clock None
                               (fun active__sig_93 ->
                                  Interpreter.rml_par
                                    (Interpreter.rml_run
                                       (fun () ->
                                          fenetre new_planet__sig_89
                                            p__sig_88 kill_sun__sig_90
                                            kill__sig_91 suspend__sig_92))
                                    (Interpreter.rml_control' suspend__sig_92
                                       (Interpreter.rml_par
                                          (Interpreter.rml_until'
                                             kill_sun__sig_90
                                             (Interpreter.rml_run
                                                (fun () ->
                                                   soleil 0 30000.
                                                     {
                                                       x = 0.;
                                                       y = 0.;
                                                       z = 0.;
                                                     } p__sig_88)))
                                          (Interpreter.rml_par
                                             (Interpreter.rml_run
                                                (fun () ->
                                                   planet 1 1.
                                                     {
                                                       x = 0.;
                                                       y = (-200.);
                                                       z = 0.;
                                                     }
                                                     {
                                                       x = 30.;
                                                       y = 0.;
                                                       z = 0.;
                                                     } p__sig_88))
                                             (Interpreter.rml_par
                                                (Interpreter.rml_run
                                                   (fun () ->
                                                      planet 2 1.
                                                        {
                                                          x = 0.;
                                                          y = 200.;
                                                          z = 0.;
                                                        }
                                                        {
                                                          x = (-30.);
                                                          y = 0.;
                                                          z = 0.;
                                                        } p__sig_88))
                                                (Interpreter.rml_par
                                                   (Interpreter.rml_run
                                                      (fun () ->
                                                         planet 3 1.
                                                           {
                                                             x = 200.;
                                                             y = 0.;
                                                             z = 0.;
                                                           }
                                                           {
                                                             x = 0.;
                                                             y = 0.;
                                                             z = 30.;
                                                           } p__sig_88))
                                                   (Interpreter.rml_run
                                                      (fun () ->
                                                         add 4
                                                           new_planet__sig_89
                                                           kill__sig_91
                                                           p__sig_88)))))))))))))
  
let _ = Random.self_init ()
  
let main = systeme
  
let _ = Machine.rml_exec main
  

