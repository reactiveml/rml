(* THIS FILE IS GENERATED. *)
(* /Users/ccpasteur/Documents/work/git/rml/compiler/rpmlc.byte -runtime Fsharp_Lco -s main boids.rml  *)

open Caml_compat
let Interpreter = Machine.LcoSeq
let Machine = Machine.Machine<_,_>(Machine.SeqRuntime)
type coord = { mutable x : float; mutable y : float }

type boid = { id : int; pos : coord; v : coord; acc : coord }

let coef_sep = 5.
  
let coef_cohes = 1.1
  
let coef_alig = 1.1
  
let vision_sep2 = 100.
  
let vision_cohes2 = 2000.
  
let vision_alig2 = 1000.
  
let vmax = 1000.
  
let dt = 0.1
  
let bounds = (5., 405., 5., 405.)
  
let random_pos () =
  {
    x = Pervasives.( +. ) (Random.float 200.) 100.;
    y = Pervasives.( +. ) (Random.float 200.) 100.;
  }
  
let get_mouse_pos () =
  let (x__val_rml_14, y__val_rml_15) = Graphics.mouse_pos ()
  in
    {
      x = Pervasives.float_of_int x__val_rml_14;
      y = Pervasives.float_of_int y__val_rml_15;
    }
  
let distance2 pos1__val_rml_17 pos2__val_rml_18 =
  Pervasives.( +. )
    (Pervasives.( *. )
       (Pervasives.( -. ) pos2__val_rml_18.x pos1__val_rml_17.x)
       (Pervasives.( -. ) pos2__val_rml_18.x pos1__val_rml_17.x))
    (Pervasives.( *. )
       (Pervasives.( -. ) pos2__val_rml_18.y pos1__val_rml_17.y)
       (Pervasives.( -. ) pos2__val_rml_18.y pos1__val_rml_17.y))
  
let get_key () = Graphics.read_key ()
  
let draw_bounds (min_x__val_rml_21, max_x__val_rml_22, min_y__val_rml_23,
                 max_y__val_rml_24)
                =
  let (min_x__val_rml_25, max_x__val_rml_26, min_y__val_rml_27,
       max_y__val_rml_28) =
    ((Pervasives.int_of_float min_x__val_rml_21),
     (Pervasives.int_of_float max_x__val_rml_22),
     (Pervasives.int_of_float min_y__val_rml_23),
     (Pervasives.int_of_float max_y__val_rml_24))
  in
    (Graphics.moveto min_x__val_rml_25 min_y__val_rml_27;
     Graphics.lineto min_x__val_rml_25 max_y__val_rml_28;
     Graphics.lineto max_x__val_rml_26 max_y__val_rml_28;
     Graphics.lineto max_x__val_rml_26 min_y__val_rml_27;
     Graphics.lineto min_x__val_rml_25 min_y__val_rml_27)
  
let draw_boid b__val_rml_30 =
  Graphics.draw_circle (Pervasives.int_of_float b__val_rml_30.pos.x)
    (Pervasives.int_of_float b__val_rml_30.pos.y) 2
  
let maj_aff all__val_rml_32 =
  (Graphics.clear_graph ();
   draw_bounds bounds;
   List.iter draw_boid all__val_rml_32;
   Graphics.synchronize ())
  
let check_bounds (min_x__val_rml_34, max_x__val_rml_35, min_y__val_rml_36,
                  max_y__val_rml_37)
                 b__val_rml_38 =
  (if Pervasives.( < ) b__val_rml_38.pos.x min_x__val_rml_34
   then
     (b__val_rml_38.pos.x <- min_x__val_rml_34;
      b__val_rml_38.v.x <- Pervasives.abs_float b__val_rml_38.v.x)
   else ();
   if Pervasives.( > ) b__val_rml_38.pos.x max_x__val_rml_35
   then
     (b__val_rml_38.pos.x <- max_x__val_rml_35;
      b__val_rml_38.v.x <-
        Pervasives.( -. ) 0. (Pervasives.abs_float b__val_rml_38.v.x))
   else ();
   if Pervasives.( < ) b__val_rml_38.pos.y min_y__val_rml_36
   then
     (b__val_rml_38.pos.y <- min_y__val_rml_36;
      b__val_rml_38.v.y <- Pervasives.abs_float b__val_rml_38.v.y)
   else ();
   if Pervasives.( > ) b__val_rml_38.pos.y max_y__val_rml_37
   then
     (b__val_rml_38.pos.y <- max_y__val_rml_37;
      b__val_rml_38.v.y <-
        Pervasives.( -. ) 0. (Pervasives.abs_float b__val_rml_38.v.y))
   else ())
  
let separation =
  let sum_f__val_rml_40 f__val_rml_41 b1__val_rml_42
                        (b2__val_rml_43, dist2__val_rml_44) =
    if
      Pervasives.( or )
        (Pervasives.( <= ) dist2__val_rml_44 Pervasives.epsilon_float)
        (Pervasives.( > ) dist2__val_rml_44 vision_sep2)
    then ()
    else
      (f__val_rml_41.x <-
         Pervasives.( -. ) f__val_rml_41.x
           (Pervasives.( /. )
              (Pervasives.( -. ) b2__val_rml_43.pos.x b1__val_rml_42.pos.x)
              dist2__val_rml_44);
       f__val_rml_41.y <-
         Pervasives.( -. ) f__val_rml_41.y
           (Pervasives.( /. )
              (Pervasives.( -. ) b2__val_rml_43.pos.y b1__val_rml_42.pos.y)
              dist2__val_rml_44))
  in
    fun f__val_rml_45 b__val_rml_46 all__val_rml_47 ->
      (f__val_rml_45.x <- 0.;
       f__val_rml_45.y <- 0.;
       List.iter (sum_f__val_rml_40 f__val_rml_45 b__val_rml_46)
         all__val_rml_47)
  
let cohesion =
  let sum_f__val_rml_49 cpt__val_rml_50 f__val_rml_51 b1__val_rml_52
                        (b2__val_rml_53, dist2__val_rml_54) =
    if
      Pervasives.( or )
        (Pervasives.( <= ) dist2__val_rml_54 Pervasives.epsilon_float)
        (Pervasives.( > ) dist2__val_rml_54 vision_cohes2)
    then ()
    else
      (Pervasives.( := ) cpt__val_rml_50
         (Pervasives.( + ) (Pervasives.( ! ) cpt__val_rml_50) 1);
       f__val_rml_51.x <-
         Pervasives.( +. ) f__val_rml_51.x b2__val_rml_53.pos.x;
       f__val_rml_51.y <-
         Pervasives.( +. ) f__val_rml_51.y b2__val_rml_53.pos.y)
  in
    fun f__val_rml_55 b__val_rml_56 all__val_rml_57 ->
      let cpt__val_rml_58 = Pervasives.ref 0
      in
        (f__val_rml_55.x <- 0.;
         f__val_rml_55.y <- 0.;
         List.iter
           (sum_f__val_rml_49 cpt__val_rml_58 f__val_rml_55 b__val_rml_56)
           all__val_rml_57;
         if Pervasives.( <> ) (Pervasives.( ! ) cpt__val_rml_58) 0
         then
           (f__val_rml_55.x <-
              Pervasives.( -. )
                (Pervasives.( /. ) f__val_rml_55.x
                   (Pervasives.float_of_int
                      (Pervasives.( ! ) cpt__val_rml_58)))
                b__val_rml_56.pos.x;
            f__val_rml_55.y <-
              Pervasives.( -. )
                (Pervasives.( /. ) f__val_rml_55.y
                   (Pervasives.float_of_int
                      (Pervasives.( ! ) cpt__val_rml_58)))
                b__val_rml_56.pos.y)
         else (f__val_rml_55.x <- 0.; f__val_rml_55.y <- 0.))
  
let alignment =
  let sum_f__val_rml_60 cpt__val_rml_61 f__val_rml_62 b1__val_rml_63
                        (b2__val_rml_64, dist2__val_rml_65) =
    if
      Pervasives.( or )
        (Pervasives.( <= ) dist2__val_rml_65 Pervasives.epsilon_float)
        (Pervasives.( > ) dist2__val_rml_65 vision_alig2)
    then ()
    else
      (let norme__val_rml_66 =
         Pervasives.( +. )
           (Pervasives.( *. ) b2__val_rml_64.v.x b2__val_rml_64.v.x)
           (Pervasives.( *. ) b2__val_rml_64.v.y b2__val_rml_64.v.y)
       in
         if Pervasives.( <> ) norme__val_rml_66 0.
         then
           (Pervasives.( := ) cpt__val_rml_61
              (Pervasives.( + ) (Pervasives.( ! ) cpt__val_rml_61) 1);
            f__val_rml_62.x <-
              Pervasives.( +. ) f__val_rml_62.x
                (Pervasives.( /. ) b2__val_rml_64.v.x norme__val_rml_66);
            f__val_rml_62.y <-
              Pervasives.( +. ) f__val_rml_62.y
                (Pervasives.( /. ) b2__val_rml_64.v.y norme__val_rml_66))
         else ())
  in
    fun f__val_rml_67 b__val_rml_68 all__val_rml_69 ->
      let cpt__val_rml_70 = Pervasives.ref 0
      in
        (f__val_rml_67.x <- 0.;
         f__val_rml_67.y <- 0.;
         List.iter
           (sum_f__val_rml_60 cpt__val_rml_70 f__val_rml_67 b__val_rml_68)
           all__val_rml_69;
         if Pervasives.( <> ) (Pervasives.( ! ) cpt__val_rml_70) 0
         then
           (f__val_rml_67.x <-
              Pervasives.( /. ) f__val_rml_67.x
                (Pervasives.float_of_int (Pervasives.( ! ) cpt__val_rml_70));
            f__val_rml_67.y <-
              Pervasives.( /. ) f__val_rml_67.y
                (Pervasives.float_of_int (Pervasives.( ! ) cpt__val_rml_70)))
         else (f__val_rml_67.x <- 0.; f__val_rml_67.y <- 0.))
  
let set_acc b__val_rml_72 f_sep__val_rml_73 f_cohes__val_rml_74
            f_alig__val_rml_75 =
  (b__val_rml_72.acc.x <-
     Pervasives.( +. )
       (Pervasives.( +. ) (Pervasives.( *. ) f_sep__val_rml_73.x coef_sep)
          (Pervasives.( *. ) f_cohes__val_rml_74.x coef_cohes))
       (Pervasives.( *. ) f_alig__val_rml_75.x coef_alig);
   b__val_rml_72.acc.y <-
     Pervasives.( +. )
       (Pervasives.( +. ) (Pervasives.( *. ) f_sep__val_rml_73.y coef_sep)
          (Pervasives.( *. ) f_cohes__val_rml_74.y coef_cohes))
       (Pervasives.( *. ) f_alig__val_rml_75.y coef_alig))
  
let set_v b__val_rml_77 =
  let vx__val_rml_78 =
    Pervasives.( +. ) (Pervasives.( *. ) b__val_rml_77.acc.x dt)
      b__val_rml_77.v.x in
  let vy__val_rml_79 =
    Pervasives.( +. ) (Pervasives.( *. ) b__val_rml_77.acc.y dt)
      b__val_rml_77.v.y in
  let v__val_rml_80 =
    Pervasives.( +. ) (Pervasives.( *. ) vx__val_rml_78 vx__val_rml_78)
      (Pervasives.( *. ) vy__val_rml_79 vy__val_rml_79)
  in
    (b__val_rml_77.v.x <-
       if Pervasives.( < ) v__val_rml_80 vmax
       then vx__val_rml_78
       else
         Pervasives.( *. ) (Pervasives.( /. ) vx__val_rml_78 v__val_rml_80)
           vmax;
     b__val_rml_77.v.y <-
       if Pervasives.( < ) v__val_rml_80 vmax
       then vy__val_rml_79
       else
         Pervasives.( *. ) (Pervasives.( /. ) vy__val_rml_79 v__val_rml_80)
           vmax)
  
let set_pos b__val_rml_82 =
  (b__val_rml_82.pos.x <-
     Pervasives.( +. ) (Pervasives.( *. ) b__val_rml_82.v.x dt)
       b__val_rml_82.pos.x;
   b__val_rml_82.pos.y <-
     Pervasives.( +. ) (Pervasives.( *. ) b__val_rml_82.v.y dt)
       b__val_rml_82.pos.y)
  
let fenetre cpt__val_rml_84 new_boid__val_rml_85 kill_boid__val_rml_86
            p__val_rml_87 () =
  let read_click__val_rml_88 new_boid__val_rml_89 () =
    Interpreter.rml_def (fun () -> Pervasives.ref false)
      (fun click__val_rml_90 ->
         Interpreter.rml_def (fun () -> Pervasives.ref false)
           (fun pre_click__val_rml_91 ->
              Interpreter.rml_def
                (fun () -> Pervasives.ref { x = 0.; y = 0.; })
                (fun pos__val_rml_92 ->
                   Interpreter.rml_loop
                     (Interpreter.rml_seq
                        (Interpreter.rml_pause
                           (fun () -> Interpreter.rml_base_clock))
                        (Interpreter.rml_compute
                           (fun () ->
                              (Pervasives.( := ) click__val_rml_90
                                 (Graphics.button_down ());
                               if
                                 Pervasives.( && )
                                   (Pervasives.( ! ) click__val_rml_90)
                                   (Pervasives.not
                                      (Pervasives.( ! ) pre_click__val_rml_91))
                               then
                                 (Pervasives.( := ) pos__val_rml_92
                                    (get_mouse_pos ());
                                  Interpreter.rml_expr_emit_val
                                    new_boid__val_rml_89
                                    (Pervasives.( ! ) pos__val_rml_92);
                                  Pervasives.print_string
                                    (Pervasives.( ^ ) "new_boid "
                                       (Pervasives.string_of_int
                                          (Pervasives.( + )
                                             (Pervasives.( ! )
                                                cpt__val_rml_84)
                                             1)));
                                  Pervasives.print_newline ())
                               else ();
                               Pervasives.( := ) pre_click__val_rml_91
                                 (Pervasives.( ! ) click__val_rml_90)))))))) in
  let read_key__val_rml_93 kill_boid__val_rml_94 () =
    Interpreter.rml_def (fun () -> Pervasives.ref 0)
      (fun cpt__val_rml_95 ->
         Interpreter.rml_loop
           (Interpreter.rml_seq
              (Interpreter.rml_pause (fun () -> Interpreter.rml_base_clock))
              (Interpreter.rml_compute
                 (fun () ->
                    if Graphics.key_pressed ()
                    then
                      (let c__val_rml_96 = get_key ()
                       in
                         (Pervasives.print_string "key_pressed = ";
                          Pervasives.print_char c__val_rml_96;
                          Pervasives.print_newline ();
                          if Pervasives.( = ) c__val_rml_96 'k'
                          then
                            (Interpreter.rml_expr_emit_val
                               kill_boid__val_rml_94
                               (Pervasives.( ! ) cpt__val_rml_95);
                             Pervasives.print_string "emit kill ";
                             Pervasives.print_int
                               (Pervasives.( ! ) cpt__val_rml_95);
                             Pervasives.print_newline ();
                             Pervasives.incr cpt__val_rml_95)
                          else ()))
                    else ()))))
  in
    Interpreter.rml_par
      (Interpreter.rml_seq
         (Interpreter.rml_compute (fun () -> Graphics.open_graph ""))
         (Interpreter.rml_seq
            (Interpreter.rml_compute
               (fun () -> Graphics.auto_synchronize false))
            (Interpreter.rml_seq
               (Interpreter.rml_compute (fun () -> draw_bounds bounds))
               (Interpreter.rml_loop
                  (Interpreter.rml_await_all' p__val_rml_87
                     (fun all__val_rml_97 ->
                        Interpreter.rml_compute
                          (fun () -> maj_aff all__val_rml_97)))))))
      (Interpreter.rml_par
         (Interpreter.rml_run
            (fun () -> read_click__val_rml_88 new_boid__val_rml_85))
         (Interpreter.rml_run
            (fun () -> read_key__val_rml_93 kill_boid__val_rml_86)))
  
let boid id__val_rml_99 pos_init__val_rml_100 p__val_rml_101
         kill_boid__val_rml_102 () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun kill_me__sig_103 ->
       Interpreter.rml_until' kill_me__sig_103
         (Interpreter.rml_def
            (fun () ->
               {
                 id = id__val_rml_99;
                 pos = pos_init__val_rml_100;
                 v = { x = 0.; y = 0.; };
                 acc = { x = 0.; y = 0.; };
               })
            (fun me__val_rml_104 ->
               Interpreter.rml_def (fun () -> { x = 0.; y = 0.; })
                 (fun f_sep__val_rml_105 ->
                    Interpreter.rml_def (fun () -> { x = 0.; y = 0.; })
                      (fun f_cohes__val_rml_106 ->
                         Interpreter.rml_def (fun () -> { x = 0.; y = 0.; })
                           (fun f_alig__val_rml_107 ->
                              Interpreter.rml_def
                                (fun () -> { x = 0.; y = 0.; })
                                (fun dep__val_rml_108 ->
                                   Interpreter.rml_par
                                     (Interpreter.rml_loop
                                        (Interpreter.rml_seq
                                           (Interpreter.rml_compute
                                              (fun () ->
                                                 Interpreter.
                                                   rml_expr_emit_val
                                                   p__val_rml_101
                                                   me__val_rml_104))
                                           (Interpreter.rml_await_all'
                                              p__val_rml_101
                                              (fun all__val_rml_110 ->
                                                 Interpreter.rml_compute
                                                   (fun () ->
                                                      let all__val_rml_111 
                                                        =
                                                        List.rev_map
                                                          (fun b__val_rml_112
                                                             ->
                                                             (b__val_rml_112,
                                                              (distance2
                                                                 me__val_rml_104.
                                                                   pos
                                                                 b__val_rml_112.
                                                                   pos)))
                                                          all__val_rml_110
                                                      in
                                                        (separation
                                                           f_sep__val_rml_105
                                                           me__val_rml_104
                                                           all__val_rml_111;
                                                         cohesion
                                                           f_cohes__val_rml_106
                                                           me__val_rml_104
                                                           all__val_rml_111;
                                                         alignment
                                                           f_alig__val_rml_107
                                                           me__val_rml_104
                                                           all__val_rml_111;
                                                         set_acc
                                                           me__val_rml_104
                                                           f_sep__val_rml_105
                                                           f_cohes__val_rml_106
                                                           f_alig__val_rml_107;
                                                         set_v
                                                           me__val_rml_104;
                                                         set_pos
                                                           me__val_rml_104;
                                                         check_bounds bounds
                                                           me__val_rml_104))))))
                                     (Interpreter.rml_loop
                                        (Interpreter.rml_await_all'
                                           kill_boid__val_rml_102
                                           (fun n__val_rml_109 ->
                                              Interpreter.rml_compute
                                                (fun () ->
                                                   if
                                                     List.mem
                                                       me__val_rml_104.id
                                                       n__val_rml_109
                                                   then
                                                     Interpreter.
                                                       rml_expr_emit
                                                       kill_me__sig_103
                                                   else ())))))))))))
  
let rec
  add cpt__val_rml_114 new_boid__val_rml_115 p__val_rml_116
      kill_boid__val_rml_117 () =
  Interpreter.rml_await_one' new_boid__val_rml_115
    (fun pos__val_rml_118 ->
       Interpreter.rml_par
         (Interpreter.rml_seq
            (Interpreter.rml_compute
               (fun () ->
                  Pervasives.( := ) cpt__val_rml_114
                    (Pervasives.( + ) (Pervasives.( ! ) cpt__val_rml_114) 1)))
            (Interpreter.rml_run
               (fun () ->
                  boid (Pervasives.( ! ) cpt__val_rml_114) pos__val_rml_118
                    p__val_rml_116 kill_boid__val_rml_117)))
         (Interpreter.rml_run
            (fun () ->
               add cpt__val_rml_114 new_boid__val_rml_115 p__val_rml_116
                 kill_boid__val_rml_117)))
  
let nb_threads = 4
  
let boids n__val_rml_121 p__val_rml_122 kill_boid__val_rml_123 () =
  let aux__val_rml_124 () =
    Interpreter.rml_fordopar (fun () -> 1)
      (fun () -> Pervasives.( / ) n__val_rml_121 nb_threads) true
      (fun i__val_rml_125 ->
         Interpreter.rml_run
           (fun () ->
              boid i__val_rml_125 (random_pos ()) p__val_rml_122
                kill_boid__val_rml_123))
  in
    Interpreter.rml_fordopar (fun () -> 1) (fun () -> nb_threads) true
      (fun i__val_rml_126 -> Interpreter.rml_run (fun () -> aux__val_rml_124))
  
let systeme n__val_rml_128 cpt__val_rml_129 () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun p__sig_130 ->
       Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock None
         (fun new_boid__sig_131 ->
            Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
              Interpreter.rml_base_clock None
              (fun kill_boid__sig_132 ->
                 Interpreter.rml_par
                   (Interpreter.rml_run
                      (fun () ->
                         add cpt__val_rml_129 new_boid__sig_131 p__sig_130
                           kill_boid__sig_132))
                   (Interpreter.rml_par
                      (Interpreter.rml_run
                         (fun () ->
                            fenetre cpt__val_rml_129 new_boid__sig_131
                              kill_boid__sig_132 p__sig_130))
                      (Interpreter.rml_run
                         (fun () ->
                            boids n__val_rml_128 p__sig_130
                              kill_boid__sig_132))))))
  
let usage =
  Pervasives.( ^ ) "Usage: "
    (Pervasives.( ^ ) (Array.get Sys.argv 0) " [nb boids]")
  
let main () =
  Interpreter.rml_def (fun () -> Random.self_init ())
    (fun x__val_rml_135 ->
       Interpreter.rml_def (fun () -> Pervasives.ref 0)
         (fun cpt__val_rml_136 ->
            Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () ->
                    Rmlarg.parse []
                      (fun n__val_rml_137 ->
                         Pervasives.( := ) cpt__val_rml_136
                           (Pervasives.int_of_string n__val_rml_137))
                      usage))
              (Interpreter.rml_run
                 (fun () ->
                    systeme (Pervasives.( ! ) cpt__val_rml_136)
                      cpt__val_rml_136))))
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

