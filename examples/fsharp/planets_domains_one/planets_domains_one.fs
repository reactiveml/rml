(* THIS FILE IS GENERATED. *)
(* C:\Users\ccpasteur\Dropbox\Sauvegardes\git\rml\compiler\_build\main\rpmlc.byte -I ../../../lib/ -runtime Fsharp_LcoThread -s main planets_domains_one.rml  *)

#indent "off"
open Caml_compat
let Interpreter = Machine.LcoThread
let Machine = Machine.ThreadMachine
let nox = false
  
let nb_planets = 2
  
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
  
let new_pos x__val_rml_10 y__val_rml_11 =
  let max_x_2__val_rml_12 = Pervasives.( / ) (Graphics.size_x ()) 2 in
  let max_y_2__val_rml_13 = Pervasives.( / ) (Graphics.size_y ()) 2
  in
    {
      x =
        Pervasives.float_of_int
          (Pervasives.( - ) x__val_rml_10 max_x_2__val_rml_12);
      y =
        Pervasives.float_of_int
          (Pervasives.( - ) y__val_rml_11 max_y_2__val_rml_13);
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
  let distance2__val_rml_16 pos1__val_rml_17 pos2__val_rml_18 =
    Pervasives.( +. )
      (Pervasives.( +. )
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_18.x pos1__val_rml_17.x)
            (Pervasives.( -. ) pos2__val_rml_18.x pos1__val_rml_17.x))
         (Pervasives.( *. )
            (Pervasives.( -. ) pos2__val_rml_18.y pos1__val_rml_17.y)
            (Pervasives.( -. ) pos2__val_rml_18.y pos1__val_rml_17.y)))
      (Pervasives.( *. )
         (Pervasives.( -. ) pos2__val_rml_18.z pos1__val_rml_17.z)
         (Pervasives.( -. ) pos2__val_rml_18.z pos1__val_rml_17.z)) in
  let force__val_rml_19 p1__val_rml_20 p2__val_rml_21 =
    let d2__val_rml_22 =
      distance2__val_rml_16 p1__val_rml_20.pos p2__val_rml_21.pos in
    let d__val_rml_23 = Pervasives.sqrt d2__val_rml_22
    in
      if Pervasives.( <> ) d__val_rml_23 0.
      then
        (let f12__val_rml_24 =
           Pervasives.( /. ) (Pervasives.( *. ) g p2__val_rml_21.masse)
             d2__val_rml_22
         in
           ((Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_24
                  (Pervasives.( -. ) p2__val_rml_21.pos.x
                     p1__val_rml_20.pos.x))
               d__val_rml_23),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_24
                  (Pervasives.( -. ) p2__val_rml_21.pos.y
                     p1__val_rml_20.pos.y))
               d__val_rml_23),
            (Pervasives.( /. )
               (Pervasives.( *. ) f12__val_rml_24
                  (Pervasives.( -. ) p2__val_rml_21.pos.z
                     p1__val_rml_20.pos.z))
               d__val_rml_23)))
      else (0., 0., 0.)
  in
    fun me__val_rml_25 all__val_rml_26 ->
      let (fx__val_rml_27, fy__val_rml_28, fz__val_rml_29) =
        List.fold_left
          (fun (fx__val_rml_30, fy__val_rml_31, fz__val_rml_32) p__val_rml_33
             ->
             let (x__val_rml_34, y__val_rml_35, z__val_rml_36) =
               force__val_rml_19 me__val_rml_25 p__val_rml_33
             in
               ((Pervasives.( +. ) fx__val_rml_30 x__val_rml_34),
                (Pervasives.( +. ) fy__val_rml_31 y__val_rml_35),
                (Pervasives.( +. ) fz__val_rml_32 z__val_rml_36)))
          (0., 0., 0.) all__val_rml_26 in
      let v__val_rml_37 =
        {
          x =
            Pervasives.( +. ) me__val_rml_25.v.x
              (Pervasives.( *. ) fx__val_rml_27 dt);
          y =
            Pervasives.( +. ) me__val_rml_25.v.y
              (Pervasives.( *. ) fy__val_rml_28 dt);
          z =
            Pervasives.( +. ) me__val_rml_25.v.z
              (Pervasives.( *. ) fz__val_rml_29 dt);
        } in
      let pos__val_rml_38 =
        {
          x =
            Pervasives.( +. ) me__val_rml_25.pos.x
              (Pervasives.( *. ) v__val_rml_37.x dt);
          y =
            Pervasives.( +. ) me__val_rml_25.pos.y
              (Pervasives.( *. ) v__val_rml_37.y dt);
          z =
            Pervasives.( +. ) me__val_rml_25.pos.z
              (Pervasives.( *. ) v__val_rml_37.z dt);
        }
      in
        {
          id = me__val_rml_25.id;
          masse = me__val_rml_25.masse;
          pos = pos__val_rml_38;
          v = v__val_rml_37;
        }
  
let maj_aff all__val_rml_40 =
  (Graphics.clear_graph ();
   let max_x_2__val_rml_41 = Pervasives.( / ) (Graphics.size_x ()) 2 in
   let max_y_2__val_rml_42 = Pervasives.( / ) (Graphics.size_y ()) 2
   in
     List.iter
       (fun p__val_rml_43 ->
          (Graphics.set_color
             (color_of_int (Pervasives.( mod ) p__val_rml_43.id 7));
           Graphics.fill_circle
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_43.pos.x)
                max_x_2__val_rml_41)
             (Pervasives.( + ) (Pervasives.int_of_float p__val_rml_43.pos.y)
                max_y_2__val_rml_42)
             (if
                Pervasives.( && )
                  (Pervasives.( < ) (-200.) p__val_rml_43.pos.z)
                  (Pervasives.( < ) p__val_rml_43.pos.z 5000.)
              then
                Pervasives.( + ) 5
                  (Pervasives.( / )
                     (Pervasives.int_of_float p__val_rml_43.pos.z) 50)
              else 1)))
       all__val_rml_40)
  
let planet id__val_rml_45 masse__val_rml_46 pos_init__val_rml_47
           v_init__val_rml_48 p__val_rml_49 () =
  Interpreter.rml_def
    (fun () ->
       Pervasives.ref
         {
           id = id__val_rml_45;
           masse = masse__val_rml_46;
           pos = pos_init__val_rml_47;
           v = v_init__val_rml_48;
         })
    (fun me__val_rml_50 ->
       Interpreter.rml_newclock None (Some 1)
         (fun ck__clock_51 ->
            Interpreter.rml_loop
              (Interpreter.rml_seq
                 (Interpreter.rml_compute
                    (fun () ->
                       Interpreter.rml_expr_emit_val p__val_rml_49
                         (Pervasives.( ! ) me__val_rml_50)))
                 (Interpreter.rml_await_all' p__val_rml_49
                    (fun all__val_rml_52 ->
                       Interpreter.rml_compute
                         (fun () ->
                            Pervasives.( := ) me__val_rml_50
                              (compute_pos (Pervasives.( ! ) me__val_rml_50)
                                 all__val_rml_52)))))))
  
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
       Interpreter.rml_newclock None (Some 1)
         (fun ck__clock_59 ->
            Interpreter.rml_loop
              (Interpreter.rml_seq
                 (Interpreter.rml_compute
                    (fun () ->
                       Interpreter.rml_expr_emit_val p__val_rml_57
                         me__val_rml_58))
                 (Interpreter.rml_pause
                    (fun () -> Interpreter.rml_base_clock)))))
  
let click_of_button_down click__val_rml_61 () =
  Interpreter.rml_loop
    (Interpreter.rml_seq
       (Interpreter.rml_compute
          (fun () ->
             if Graphics.button_down ()
             then
               Interpreter.rml_expr_emit_val click__val_rml_61
                 (Graphics.mouse_pos ())
             else ()))
       (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let key_of_key_pressed key__val_rml_63 () =
  Interpreter.rml_loop
    (Interpreter.rml_seq
       (Interpreter.rml_compute
          (fun () ->
             if Graphics.key_pressed ()
             then
               Interpreter.rml_expr_emit_val key__val_rml_63
                 (Graphics.read_key ())
             else ()))
       (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock)))
  
let fenetre new_planet__val_rml_65 p__val_rml_66 kill_sun__val_rml_67
            kill__val_rml_68 suspend__val_rml_69 () =
  let read_click__val_rml_70 () =
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun click__sig_71 ->
         Interpreter.rml_par
           (Interpreter.rml_run
              (fun () -> click_of_button_down click__sig_71))
           (Interpreter.rml_loop
              (Interpreter.rml_present' click__sig_71
                 (Interpreter.rml_pause
                    (fun () -> Interpreter.rml_base_clock))
                 (Interpreter.rml_await_immediate_one' click__sig_71
                    (fun (x__val_rml_72, y__val_rml_73) ->
                       Interpreter.rml_seq
                         (Interpreter.rml_compute
                            (fun () ->
                               Interpreter.rml_expr_emit_val
                                 new_planet__val_rml_65
                                 (new_pos x__val_rml_72 y__val_rml_73)))
                         (Interpreter.rml_pause
                            (fun () -> Interpreter.rml_base_clock))))))) in
  let read_key__val_rml_74 () =
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun key__sig_75 ->
         Interpreter.rml_par
           (Interpreter.rml_run (fun () -> key_of_key_pressed key__sig_75))
           (Interpreter.rml_loop
              (Interpreter.rml_await_immediate_one' key__sig_75
                 (fun c__val_rml_76 ->
                    Interpreter.rml_seq
                      (Interpreter.rml_compute
                         (fun () -> Pervasives.print_string "key_pressed = "))
                      (Interpreter.rml_seq
                         (Interpreter.rml_compute
                            (fun () -> Pervasives.print_char c__val_rml_76))
                         (Interpreter.rml_seq
                            (Interpreter.rml_compute
                               (fun () -> Pervasives.print_newline ()))
                            (Interpreter.rml_seq
                               (Interpreter.rml_match
                                  (fun () -> c__val_rml_76)
                                  (function
                                   | 's' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              kill_sun__val_rml_67)
                                   | 'k' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              kill__val_rml_68)
                                   | 'p' ->
                                       Interpreter.rml_compute
                                         (fun () ->
                                            Interpreter.rml_expr_emit
                                              suspend__val_rml_69)
                                   | 'q' ->
                                       Interpreter.rml_compute
                                         (fun () -> Pervasives.exit 0)
                                   | _ -> Interpreter.rml_nothing))
                               (Interpreter.rml_pause
                                  (fun () -> Interpreter.rml_base_clock)))))))))
  in
    Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
      Interpreter.rml_base_clock None
      (fun aff__sig_77 ->
         Interpreter.rml_seq
           (Interpreter.rml_compute (fun () -> Graphics.open_graph ""))
           (Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () -> Graphics.auto_synchronize false))
              (Interpreter.rml_loop
                 (Interpreter.rml_await_all' p__val_rml_66
                    (fun all__val_rml_78 ->
                       Interpreter.rml_compute
                         (fun () ->
                            (maj_aff all__val_rml_78;
                             Graphics.synchronize ())))))))
  
let rec
  add_aux id__val_rml_80 new_planet__val_rml_81 kill__val_rml_82
          kill_new__val_rml_83 p__val_rml_84 () =
  Interpreter.rml_await_all' new_planet__val_rml_81
    (fun pos__val_rml_85 ->
       Interpreter.rml_par
         (Interpreter.rml_until' kill_new__val_rml_83
            (Interpreter.rml_run
               (fun () ->
                  planet id__val_rml_80 1. (List.hd pos__val_rml_85)
                    (v_init ()) p__val_rml_84)))
         (Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
            Interpreter.rml_base_clock None
            (fun new_kill_new__sig_86 ->
               Interpreter.rml_par
                 (Interpreter.rml_run
                    (fun () ->
                       add_aux (Pervasives.( + ) id__val_rml_80 1)
                         new_planet__val_rml_81 kill__val_rml_82
                         new_kill_new__sig_86 p__val_rml_84))
                 (Interpreter.rml_seq
                    (Interpreter.rml_par
                       (Interpreter.rml_await' new_planet__val_rml_81)
                       (Interpreter.rml_await_immediate' kill_new__val_rml_83))
                    (Interpreter.rml_seq
                       (Interpreter.rml_await' kill__val_rml_82)
                       (Interpreter.rml_compute
                          (fun () ->
                             Interpreter.rml_expr_emit new_kill_new__sig_86)))))))
  
let add id__val_rml_88 new_planet__val_rml_89 kill__val_rml_90 p__val_rml_91
        () =
  Interpreter.rml_run
    (fun () ->
       add_aux id__val_rml_88 new_planet__val_rml_89 kill__val_rml_90
         kill__val_rml_90 p__val_rml_91)
  
let systeme () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun p__sig_93 ->
       Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock None
         (fun new_planet__sig_94 ->
            Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
              Interpreter.rml_base_clock None
              (fun kill_sun__sig_95 ->
                 Interpreter.rml_signal Types.Signal Interpreter.
                   rml_base_clock Interpreter.rml_base_clock None
                   (fun kill__sig_96 ->
                      Interpreter.rml_signal Types.Signal Interpreter.
                        rml_base_clock Interpreter.rml_base_clock None
                        (fun suspend__sig_97 ->
                           Interpreter.rml_signal Types.Signal Interpreter.
                             rml_base_clock Interpreter.rml_base_clock None
                             (fun active__sig_98 ->
                                Interpreter.rml_until' suspend__sig_97
                                  (Interpreter.rml_par
                                     (Interpreter.rml_if
                                        (fun () -> Pervasives.not nox)
                                        (Interpreter.rml_run
                                           (fun () ->
                                              fenetre new_planet__sig_94
                                                p__sig_93 kill_sun__sig_95
                                                kill__sig_96 suspend__sig_97))
                                        (Interpreter.rml_compute
                                           (fun () -> ())))
                                     (Interpreter.rml_par
                                        (Interpreter.rml_until'
                                           kill_sun__sig_95
                                           (Interpreter.rml_run
                                              (fun () ->
                                                 soleil 0 30000.
                                                   { x = 0.; y = 0.; z = 0.;
                                                   } p__sig_93)))
                                        (Interpreter.rml_par
                                           (Interpreter.rml_run
                                              (fun () ->
                                                 planet 1 1.
                                                   {
                                                     x = 0.;
                                                     y = (-200.);
                                                     z = 0.;
                                                   }
                                                   { x = 30.; y = 0.; z = 0.;
                                                   } p__sig_93))
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
                                                      } p__sig_93))
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
                                                         } p__sig_93))
                                                 (Interpreter.rml_fordopar
                                                    (fun () -> 4)
                                                    (fun () -> nb_planets)
                                                    true
                                                    (fun i__val_rml_99 ->
                                                       Interpreter.rml_run
                                                         (fun () ->
                                                            planet
                                                              i__val_rml_99
                                                              1. (x_init ())
                                                              (v_init ())
                                                              p__sig_93))))))))))))))
  
let _ = Random.self_init ()
  
let main = systeme
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

