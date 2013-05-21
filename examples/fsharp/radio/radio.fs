(* THIS FILE IS GENERATED. *)
(* C:\Users\ccpasteur\Dropbox\Sauvegardes\git\rml\compiler\_build\main\rpmlc.byte -I ../../../lib/ -runtime Fsharp_LcoThread -s main radio.rml  *)

#indent "off"
open Caml_compat
let Interpreter = Machine.LcoThread
let Machine = Machine.ThreadMachine
open Rmlarg
  
let e_0 = 10.
  
let e_min = 1.
  
let max_power = 0.5
  
let on_power = 0.1
  
let send_power = 0.4
  
let packet_send_time = 3
  
let rec iter p__val_rml_10 l__val_rml_11 () =
  Interpreter.rml_match (fun () -> l__val_rml_11)
    (function
     | [] -> Interpreter.rml_compute (fun () -> ())
     | x__val_rml_12 :: l__val_rml_13 ->
         Interpreter.rml_seq
           (Interpreter.rml_run (fun () -> p__val_rml_10 x__val_rml_12))
           (Interpreter.rml_run (fun () -> iter p__val_rml_10 l__val_rml_13)))
  
let init p__val_rml_15 n__val_rml_16 () =
  let aux__val_rml_17 =
    let rec aux__val_rml_17 i__val_rml_18 () =
      Interpreter.rml_match (fun () -> i__val_rml_18)
        (function
         | i__val_rml_19 when Pervasives.( = ) i__val_rml_19 n__val_rml_16 ->
             Interpreter.rml_compute (fun () -> [])
         | _ ->
             Interpreter.rml_def
               (fun () -> ((Pervasives.ref None), (Pervasives.ref None)))
               (fun (v0__loc_84, v1__loc_85) ->
                  Interpreter.rml_seq
                    (Interpreter.rml_par
                       (Interpreter.rml_def_dyn
                          (Interpreter.rml_run
                             (fun () -> p__val_rml_15 i__val_rml_18))
                          (fun x__loc_87 ->
                             Interpreter.rml_compute
                               (fun () ->
                                  Pervasives.( := ) v0__loc_84
                                    (Some x__loc_87))))
                       (Interpreter.rml_def_dyn
                          (Interpreter.rml_run
                             (fun () ->
                                aux__val_rml_17
                                  (Pervasives.( + ) i__val_rml_18 1)))
                          (fun x__loc_86 ->
                             Interpreter.rml_compute
                               (fun () ->
                                  Pervasives.( := ) v1__loc_85
                                    (Some x__loc_86)))))
                    (Interpreter.rml_def
                       (fun () ->
                          match ((Pervasives.( ! ) v0__loc_84),
                                 (Pervasives.( ! ) v1__loc_85))
                          with
                          | (Some v0__loc_84, Some v1__loc_85) ->
                              (v0__loc_84, v1__loc_85)
                          | _ -> Pervasives.raise Types.RML)
                       (fun (x__val_rml_20, l__val_rml_21) ->
                          Interpreter.rml_compute
                            (fun () -> x__val_rml_20 :: l__val_rml_21)))))
    in aux__val_rml_17
  in Interpreter.rml_run (fun () -> aux__val_rml_17 0)
  
let node me__val_rml_23 neighbors__val_rml_24 () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun dead__sig_25 ->
       Interpreter.rml_signal_combine Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock (fun () -> e_0)
         (fun () x__val_rml_26 _ -> x__val_rml_26) None
         (fun energy__sig_27 ->
            let send__val_rml_28 msg__val_rml_29 n__val_rml_30 () =
              Interpreter.rml_compute
                (fun () ->
                   Interpreter.rml_expr_emit_val n__val_rml_30
                     msg__val_rml_29) in
            let forward_msg__val_rml_31 msg__val_rml_32 () =
              Interpreter.rml_if
                (fun () -> Pervasives.( > ) msg__val_rml_32 1)
                (Interpreter.rml_run
                   (fun () ->
                      iter
                        (send__val_rml_28
                           (Pervasives.( - ) msg__val_rml_32 1))
                        neighbors__val_rml_24))
                (Interpreter.rml_compute (fun () -> ()))
            in
              Interpreter.rml_until' dead__sig_25
                (Interpreter.rml_par
                   (Interpreter.rml_loop
                      (Interpreter.rml_await_all' me__val_rml_23
                         (fun msgs__val_rml_33 ->
                            Interpreter.rml_run
                              (fun () ->
                                 iter forward_msg__val_rml_31
                                   msgs__val_rml_33))))
                   (Interpreter.rml_loop
                      (Interpreter.rml_seq
                         (Interpreter.rml_compute
                            (fun () ->
                               if
                                 Pervasives.( < )
                                   (Interpreter.rml_last energy__sig_27)
                                   e_min
                               then Interpreter.rml_expr_emit dead__sig_25
                               else
                                 Interpreter.rml_expr_emit_val energy__sig_27
                                   (Pervasives.( -. )
                                      (Interpreter.rml_last energy__sig_27)
                                      max_power)))
                         (Interpreter.rml_pause
                            (fun () -> Interpreter.rml_base_clock)))))))
  
let node_with_energy me__val_rml_35 neighbors__val_rml_36 () =
  Interpreter.rml_newclock None (Some 1000)
    (fun us__clock_37 ->
       Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock None
         (fun dead__sig_38 ->
            Interpreter.rml_signal_combine Types.Signal Interpreter.
              rml_base_clock Interpreter.rml_base_clock (fun () -> e_0)
              (fun () x__val_rml_39 _ -> x__val_rml_39) None
              (fun energy__sig_40 ->
                 Interpreter.rml_signal_combine Types.Signal Interpreter.
                   rml_base_clock Interpreter.rml_base_clock
                   (fun () -> on_power) (fun () -> Pervasives.( +. )) None
                   (fun power__sig_41 ->
                      Interpreter.rml_signal_combine Types.Signal
                        Interpreter.rml_base_clock Interpreter.rml_base_clock
                        (fun () -> (0, me__val_rml_35))
                        (fun () x__val_rml_42 _ -> x__val_rml_42) None
                        (fun radio_in__sig_43 ->
                           Interpreter.rml_signal Types.Signal Interpreter.
                             rml_base_clock Interpreter.rml_base_clock None
                             (fun radio_ack__sig_44 ->
                                let send__val_rml_45 msg__val_rml_46
                                                     n__val_rml_47 () =
                                  Interpreter.rml_seq
                                    (Interpreter.rml_compute
                                       (fun () ->
                                          Interpreter.rml_expr_emit_val
                                            radio_in__sig_43
                                            (msg__val_rml_46, n__val_rml_47)))
                                    (Interpreter.rml_await_immediate'
                                       radio_ack__sig_44) in
                                let forward_msg__val_rml_48 msg__val_rml_49
                                                            () =
                                  Interpreter.rml_if
                                    (fun () ->
                                       Pervasives.( > ) msg__val_rml_49 1)
                                    (Interpreter.rml_run
                                       (fun () ->
                                          iter
                                            (send__val_rml_45
                                               (Pervasives.( - )
                                                  msg__val_rml_49 1))
                                            neighbors__val_rml_36))
                                    (Interpreter.rml_compute (fun () -> ()))
                                in
                                  Interpreter.rml_until' dead__sig_38
                                    (Interpreter.rml_par
                                       (Interpreter.rml_loop
                                          (Interpreter.rml_await_all'
                                             me__val_rml_35
                                             (fun msgs__val_rml_53 ->
                                                Interpreter.rml_run
                                                  (fun () ->
                                                     iter
                                                       forward_msg__val_rml_48
                                                       msgs__val_rml_53))))
                                       (Interpreter.rml_par
                                          (Interpreter.rml_loop
                                             (Interpreter.rml_await_all'
                                                radio_in__sig_43
                                                (fun
                                                   (msg__val_rml_50,
                                                    n__val_rml_51)
                                                   ->
                                                   Interpreter.rml_seq
                                                     (Interpreter.rml_for
                                                        (fun () -> 1)
                                                        (fun () ->
                                                           packet_send_time)
                                                        true
                                                        (fun i__val_ml_52 ->
                                                           Interpreter.
                                                             rml_seq
                                                             (Interpreter.
                                                                rml_compute
                                                                (fun () ->
                                                                   Interpreter.
                                                                    rml_expr_emit_val
                                                                    power__sig_41
                                                                    send_power))
                                                             (Interpreter.
                                                                rml_pause'
                                                                us__clock_37)))
                                                     (Interpreter.rml_compute
                                                        (fun () ->
                                                           (Interpreter.
                                                              rml_expr_emit_val
                                                              n__val_rml_51
                                                              msg__val_rml_50;
                                                            Interpreter.
                                                              rml_expr_emit
                                                              radio_ack__sig_44))))))
                                          (Interpreter.rml_loop
                                             (Interpreter.rml_seq
                                                (Interpreter.rml_compute
                                                   (fun () ->
                                                      if
                                                        Pervasives.( < )
                                                          (Interpreter.
                                                             rml_last
                                                             energy__sig_40)
                                                          e_min
                                                      then
                                                        Interpreter.
                                                          rml_expr_emit
                                                          dead__sig_38
                                                      else
                                                        Interpreter.
                                                          rml_expr_emit_val
                                                          energy__sig_40
                                                          (Pervasives.( -. )
                                                             (Interpreter.
                                                                rml_last
                                                                energy__sig_40)
                                                             (Pervasives.
                                                                ( /. )
                                                                (Interpreter.
                                                                   rml_last
                                                                   power__sig_41)
                                                                1000.))))
                                                (Interpreter.rml_pause'
                                                   us__clock_37)))))))))))
  
let nb_agents = Pervasives.ref 5
  
let set_nb_agents i__val_rml_56 = Pervasives.( := ) nb_agents i__val_rml_56
  
let init_ttl = Pervasives.ref 5
  
let set_init_ttl i__val_rml_59 = Pervasives.( := ) init_ttl i__val_rml_59
  
let msg_freq = Pervasives.ref 10000
  
let set_msg_freq i__val_rml_62 = Pervasives.( := ) msg_freq i__val_rml_62
  
let no_simulate_power = Pervasives.ref false
  
let args =
  [ ("-nb-agents", (Rmlarg.Int set_nb_agents), "Number of agents");
    ("-ttl", (Rmlarg.Int set_init_ttl), "TTL of messages");
    ("-msg-period", (Rmlarg.Int set_msg_freq), "Message period");
    ("-no-power", (Rmlarg.Set no_simulate_power),
     "Approximate power consumption") ]
  
let main () =
  Interpreter.rml_seq
    (Interpreter.rml_compute
       (fun () -> Rmlarg.parse args Pervasives.ignore "Usage: "))
    (let mk_signal__val_rml_66 i__val_rml_67 () =
       Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock
         Interpreter.rml_base_clock None
         (fun s__sig_68 -> Interpreter.rml_compute (fun () -> s__sig_68))
     in
       Interpreter.rml_def_dyn
         (Interpreter.rml_run
            (fun () ->
               init mk_signal__val_rml_66 (Pervasives.( ! ) nb_agents)))
         (fun agents__val_rml_69 ->
            let run_nodes__val_rml_70 =
              let rec
                run_nodes__val_rml_70 l__val_rml_71 neighbors__val_rml_72 ()
                                      =
                Interpreter.rml_match (fun () -> l__val_rml_71)
                  (function
                   | [] -> Interpreter.rml_compute (fun () -> ())
                   | n__val_rml_73 :: l__val_rml_74 ->
                       Interpreter.rml_par
                         (Interpreter.rml_if
                            (fun () -> Pervasives.( ! ) no_simulate_power)
                            (Interpreter.rml_run
                               (fun () ->
                                  node n__val_rml_73 neighbors__val_rml_72))
                            (Interpreter.rml_run
                               (fun () ->
                                  node_with_energy n__val_rml_73
                                    neighbors__val_rml_72)))
                         (Interpreter.rml_run
                            (fun () ->
                               run_nodes__val_rml_70 l__val_rml_74
                                 [ n__val_rml_73 ])))
              in run_nodes__val_rml_70
            in
              Interpreter.rml_par
                (Interpreter.rml_run
                   (fun () ->
                      run_nodes__val_rml_70 agents__val_rml_69
                        (List.tl agents__val_rml_69)))
                (Interpreter.rml_loop
                   (Interpreter.rml_seq
                      (Interpreter.rml_compute
                         (fun () ->
                            Interpreter.rml_expr_emit_val
                              (List.hd agents__val_rml_69)
                              (Pervasives.( ! ) init_ttl)))
                      (Interpreter.rml_for (fun () -> 1)
                         (fun () -> Pervasives.( ! ) msg_freq) true
                         (fun i__val_ml_75 ->
                            Interpreter.rml_pause
                              (fun () -> Interpreter.rml_base_clock)))))))
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

