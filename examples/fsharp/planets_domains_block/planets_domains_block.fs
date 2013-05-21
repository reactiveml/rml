(* THIS FILE IS GENERATED. *)
(* C:\Users\ccpasteur\Dropbox\Sauvegardes\git\rml\compiler\_build\main\rpmlc.byte -I ../../../lib/ -runtime Fsharp_LcoThread -s main planets_domains_block.rml  *)

#indent "off"
open Caml_compat
let Interpreter = Machine.LcoThread
let Machine = Machine.ThreadMachine
let nox = true
  
let nb_planets = 400
  
let nb_blocks = 1
  
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
  
let x_init () =
  {
    x = Random.float 800.;
    y = Random.float 800.;
    z = Pervasives.( -. ) (Random.float 200.) 100.;
  }
  
let new_pos x__val_rml_11 y__val_rml_12 =
  let max_x_2__val_rml_13 = Pervasives.( / ) (Graphics.size_x ()) 2 in
  let max_y_2__val_rml_14 = Pervasives.( / ) (Graphics.size_y ()) 2
  in
    {
      x =
        Pervasives.float_of_int
          (Pervasives.( - ) x__val_rml_11 max_x_2__val_rml_13);
      y =
        Pervasives.float_of_int
          (Pervasives.( - ) y__val_rml_12 max_y_2__val_rml_14);
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
  let distance2__val_rml_17 pos1__val_rml_18 pos2__val_rml_19 =
    Pervasives.( +. )
      (Pervasives.( +. )
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_19.x pos1__val_rml_18.x)
            (Pervasives.( -. ) pos2__val_rml_19.x pos1__val_rml_18.x))
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_19.y pos1__val_rml_18.y)
            (Pervasives.( -. ) pos2__val_rml_19.y pos1__val_rml_18.y)))
      (Pervasives.( *. )
         (Pervasives.( -. ) pos2__val_rml_19.z pos1__val_rml_18.z)
         (Pervasives.( -. ) pos2__val_rml_19.z pos1__val_rml_18.z)) in
  let force__val_rml_20 p1__val_rml_21 p2__val_rml_22 =
    let d2__val_rml_23 =
      distance2__val_rml_17 p1__val_rml_21.pos p2__val_rml_22.pos in
    let d__val_rml_24 = Pervasives.sqrt d2__val_rml_23
    in
      if Pervasives.( <> ) d__val_rml_24 0.
      then
        (let f12__val_rml_25 =
           Pervasives.( /. ) (Pervasives.( *. ) g p2__val_rml_22.masse)
             d2__val_rml_23
         in
           ((Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_25
                  (Pervasives.( -. ) p2__val_rml_22.pos.x
                     p1__val_rml_21.pos.x))
               d__val_rml_24),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_25
                  (Pervasives.( -. ) p2__val_rml_22.pos.y
                     p1__val_rml_21.pos.y))
               d__val_rml_24),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_25
                  (Pervasives.( -. ) p2__val_rml_22.pos.z
                     p1__val_rml_21.pos.z))
               d__val_rml_24)))
      else (0., 0., 0.)
  in
    fun me__val_rml_26 all__val_rml_27 ->
      let (fx__val_rml_28, fy__val_rml_29, fz__val_rml_30) =
        List.fold_left
          (fun (fx__val_rml_31, fy__val_rml_32, fz__val_rml_33) p__val_rml_34
             ->
             let (x__val_rml_35, y__val_rml_36, z__val_rml_37) =
               force__val_rml_20 me__val_rml_26 p__val_rml_34
             in
               ((Pervasives.( +. ) fx__val_rml_31 x__val_rml_35),
                (Pervasives.( +. ) fy__val_rml_32 y__val_rml_36),
                (Pervasives.( +. ) fz__val_rml_33 z__val_rml_37)))
          (0., 0., 0.) all__val_rml_27 in
      let v__val_rml_38 =
        {
          x =
            Pervasives.( +. ) me__val_rml_26.v.x
              (Pervasives.( *. ) fx__val_rml_28 dt);
          y =
            Pervasives.( +. ) me__val_rml_26.v.y
              (Pervasives.( *. ) fy__val_rml_29 dt);
          z =
            Pervasives.( +. ) me__val_rml_26.v.z
              (Pervasives.( *. ) fz__val_rml_30 dt);
        } in
      let pos__val_rml_39 =
        {
          x =
            Pervasives.( +. ) me__val_rml_26.pos.x
              (Pervasives.( *. ) v__val_rml_38.x dt);
          y =
            Pervasives.( +. ) me__val_rml_26.pos.y
              (Pervasives.( *. ) v__val_rml_38.y dt);
          z =
            Pervasives.( +. ) me__val_rml_26.pos.z
              (Pervasives.( *. ) v__val_rml_38.z dt);
        }
      in
        {
          id = me__val_rml_26.id;
          masse = me__val_rml_26.masse;
          pos = pos__val_rml_39;
          v = v__val_rml_38;
        }
  
let maj_aff all__val_rml_41 =
  (Graphics.clear_graph ();
   let max_x_2__val_rml_42 = Pervasives.( / ) (Graphics.size_x ()) 2 in
   let max_y_2__val_rml_43 = Pervasives.( / ) (Graphics.size_y ()) 2
   in
     List.iter
       (fun p__val_rml_44 ->
          (Graphics.set_color
             (color_of_int (Pervasives.( mod ) p__val_rml_44.id 7));
           Graphics.fill_circle
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_44.pos.x)
                max_x_2__val_rml_42)
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_44.pos.y)
                max_y_2__val_rml_43)
             (if
                Pervasives.( && )
                  (Pervasives.( < ) (-200.) p__val_rml_44.pos.z)
                  (Pervasives.( < ) p__val_rml_44.pos.z 5000.)
              then
                Pervasives.( + ) 5
                  (Pervasives.( / )
                     (Pervasives.int_of_float p__val_rml_44.pos.z) 50)
              else 1)))
       all__val_rml_41)
  
let planet id__val_rml_46 masse__val_rml_47 pos_init__val_rml_48
           v_init__val_rml_49 p__val_rml_50 () =
  Interpreter.rml_def
    (fun () ->
       Pervasives.ref
         {
           id = id__val_rml_46;
           masse = masse__val_rml_47;
           pos = pos_init__val_rml_48;
           v = v_init__val_rml_49;
         })
    (fun me__val_rml_51 ->
       Interpreter.rml_loop
         (Interpreter.rml_seq
            (Interpreter.rml_compute
               (fun () ->
                  Interpreter.rml_expr_emit_val p__val_rml_50
                    (Pervasives.( ! ) me__val_rml_51)))
            (Interpreter.rml_await_all' p__val_rml_50
               (fun all__val_rml_52 ->
                  Interpreter.rml_compute
                    (fun () ->
                       Pervasives.( := ) me__val_rml_51
                         (compute_pos (Pervasives.( ! ) me__val_rml_51)
                            all__val_rml_52))))))
  
let soleil id__val_rml_54 masse__val_rml_55 pos_init__val_rml_56
           p__val_rml_57 () =
  Interpreter.rml_def
    (fun () ->
       {
         id = id__val_rml_54;
         masse = masse__val_rml_55;
         pos = pos_init__val_rml_56;
         v = { x = 0.; y = 0.; z = 0.; };
       })
    (fun me__val_rml_58 ->
       Interpreter.rml_loop
         (Interpreter.rml_seq
            (Interpreter.rml_compute
               (fun () ->
                  Interpreter.rml_expr_emit_val p__val_rml_57 me__val_rml_58))
            (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock))))
  
let click_of_button_down click__val_rml_60 () =
  Interpreter.rml_loop
    (Interpreter.rml_seq
       (Interpreter.rml_compute
          (fun () ->
             if Graphics.button_down ()
             then
               Interpreter.rml_expr_emit_val click__val_rml_60
                 (Graphics.mouse_pos ())
             else ()))
       (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let key_of_key_pressed key__val_rml_62 () =
  Interpreter.rml_loop
    (Interpreter.rml_seq
       (Interpreter.rml_compute
          (fun () ->
             if Graphics.key_pressed ()
             then
               Interpreter.rml_expr_emit_val key__val_rml_62
                 (Graphics.read_key ())
             else ()))
       (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let fenetre new_planet__val_rml_64 p__val_rml_65 kill_sun__val_rml_66
            kill__val_rml_67 suspend__val_rml_68 () =
  let read_click__val_rml_69 () =
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun click__sig_70 ->
         Interpreter.rml_par
           (Interpreter.rml_run
              (fun () -> click_of_button_down click__sig_70))
           (Interpreter.rml_loop
              (Interpreter.rml_present' click__sig_70
                 (Interpreter.rml_pause
                    (fun () -> Interpreter.rml_base_clock))
                 (Interpreter.rml_await_immediate_one' click__sig_70
                    (fun (x__val_rml_71, y__val_rml_72) ->
                       Interpreter.rml_seq
                         (Interpreter.rml_compute
                            (fun () ->
                               Interpreter.rml_expr_emit_val
                                 new_planet__val_rml_64
                                 (new_pos x__val_rml_71 y__val_rml_72)))
                         (Interpreter.rml_pause
                            (fun () -> Interpreter.rml_base_clock))))))) in
  let read_key__val_rml_73 () =
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun key__sig_74 ->
         Interpreter.rml_par
           (Interpreter.rml_run (fun () -> key_of_key_pressed key__sig_74))
           (Interpreter.rml_loop
              (Interpreter.rml_await_immediate_one' key__sig_74
                 (fun c__val_rml_75 ->
                    Interpreter.rml_seq
                      (Interpreter.rml_compute
                         (fun () -> Pervasives.print_string "key_pressed = "))
                      (Interpreter.rml_seq
                         (Interpreter.rml_compute
                            (fun () -> Pervasives.print_char c__val_rml_75))
                         (Interpreter.rml_seq
                            (Interpreter.rml_compute
                               (fun () -> Pervasives.print_newline ()))
                            (Interpreter.rml_seq
                               (Interpreter.rml_match
                                  (fun () -> c__val_rml_75)
                                  (function
                                   | 's' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              kill_sun__val_rml_66)
                                   | 'k' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              kill__val_rml_67)
                                   | 'p' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              suspend__val_rml_68)
                                   | 'q' ->
                                       Interpreter.rml_compute
                                         (fun () -> Pervasives.exit 0)
                                   | _ -> Interpreter.rml_nothing))
                               (Interpreter.rml_pause
                                  (fun () -> Interpreter.rml_base_clock)))))))))
  in
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun aff__sig_76 ->
         Interpreter.rml_seq
           (Interpreter.rml_compute (fun () -> Graphics.open_graph ""))
           (Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () -> Graphics.auto_synchronize false))
              (Interpreter.rml_par
                 (Interpreter.rml_loop
                    (Interpreter.rml_await_all' p__val_rml_65
                       (fun all__val_rml_77 ->
                          Interpreter.rml_seq
                            (Interpreter.rml_compute
                               (fun () -> maj_aff all__val_rml_77))
                            (Interpreter.rml_seq
                               (Interpreter.rml_compute
                                  (fun () -> Graphics.synchronize ()))
                               (Interpreter.rml_seq
                                  (Interpreter.rml_pause
                                     (fun () -> Interpreter.rml_base_clock))
                                  (Interpreter.rml_pause
                                     (fun () -> Interpreter.rml_base_clock)))))))
                 (Interpreter.rml_par
                    (Interpreter.rml_run (fun () -> read_click__val_rml_69))
                    (Interpreter.rml_run (fun () -> read_key__val_rml_73))))))
  
let rec
  add_aux id__val_rml_79 new_planet__val_rml_80 kill__val_rml_81
          kill_new__val_rml_82 p__val_rml_83 () =
  Interpreter.rml_await_all' new_planet__val_rml_80
    (fun pos__val_rml_84 ->
       Interpreter.rml_par
         (Interpreter.rml_until' kill_new__val_rml_82
            (Interpreter.rml_run
               (fun () ->
                  planet id__val_rml_79 1. (List.hd pos__val_rml_84)
                    (v_init ()) p__val_rml_83)))
         (Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
            Interpreter.rml_base_clock None
            (fun new_kill_new__sig_85 ->
               Interpreter.rml_par
                 (Interpreter.rml_run
                    (fun () ->
                       add_aux (Pervasives.( + ) id__val_rml_79 1)
                         new_planet__val_rml_80 kill__val_rml_81
                         new_kill_new__sig_85 p__val_rml_83))
                 (Interpreter.rml_seq
                    (Interpreter.rml_par
                       (Interpreter.rml_await' new_planet__val_rml_80)
                       (Interpreter.rml_await_immediate' kill_new__val_rml_82))
                    (Interpreter.rml_seq
                       (Interpreter.rml_await' kill__val_rml_81)
                       (Interpreter.rml_compute
                          (fun () ->
                             Interpreter.rml_expr_emit new_kill_new__sig_85)))))))
  
let add id__val_rml_87 new_planet__val_rml_88 kill__val_rml_89 p__val_rml_90
        () =
  Interpreter.rml_run
    (fun () ->
       add_aux id__val_rml_87 new_planet__val_rml_88 kill__val_rml_89
         kill__val_rml_89 p__val_rml_90)
  
let systeme () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun p__sig_92 ->
       Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock None
         (fun new_planet__sig_93 ->
            Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
              Interpreter.rml_base_clock None
              (fun kill_sun__sig_94 ->
                 Interpreter.rml_signal Types.Signal Interpreter.
                   rml_base_clock Interpreter.rml_base_clock None
                   (fun kill__sig_95 ->
                      Interpreter.rml_signal Types.Signal Interpreter.
                        rml_base_clock Interpreter.rml_base_clock None
                        (fun suspend__sig_96 ->
                           Interpreter.rml_signal Types.Signal Interpreter.
                             rml_base_clock Interpreter.rml_base_clock None
                             (fun active__sig_97 ->
                                Interpreter.rml_par
                                  (Interpreter.rml_if
                                     (fun () -> Pervasives.not nox)
                                     (Interpreter.rml_run
                                        (fun () ->
                                           fenetre new_planet__sig_93
                                             p__sig_92 kill_sun__sig_94
                                             kill__sig_95 suspend__sig_96))
                                     (Interpreter.rml_compute (fun () -> ())))
                                  (Interpreter.rml_control' suspend__sig_96
                                     (Interpreter.rml_par
                                        (Interpreter.rml_until'
                                           kill_sun__sig_94
                                           (Interpreter.rml_run
                                              (fun () ->
                                                 soleil 0 30000.
                                                   { x = 0.; y = 0.; z = 0.;
                                                   } p__sig_92)))
                                        (Interpreter.rml_par
                                           (Interpreter.rml_fordopar
                                              (fun () -> 0)
                                              (fun () ->
                                                 Pervasives.( - ) nb_blocks 1)
                                              true
                                              (fun j__val_rml_98 ->
                                                 Interpreter.rml_newclock
                                                   None (Some 1)
                                                   (fun ck__clock_99 ->
                                                      Interpreter.
                                                        rml_fordopar
                                                        (fun () -> 1)
                                                        (fun () ->
                                                           Pervasives.( / )
                                                             nb_planets
                                                             nb_blocks)
                                                        true
                                                        (fun i__val_rml_100
                                                           ->
                                                           Interpreter.
                                                             rml_run
                                                             (fun () ->
                                                                planet
                                                                  (Pervasives.
                                                                    ( + )
                                                                    (Pervasives.
                                                                    ( * )
                                                                    j__val_rml_98
                                                                    (Pervasives.
                                                                    ( / )
                                                                    nb_planets
                                                                    nb_blocks))
                                                                    i__val_rml_100)
                                                                  1.
                                                                  (x_init ())
                                                                  (v_init ())
                                                                  p__sig_92)))))
                                           (Interpreter.rml_run
                                              (fun () ->
                                                 add 4 new_planet__sig_93
                                                   kill__sig_95 p__sig_92)))))))))))
  
let _ = Random.self_init ()
  
let main = systeme
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

