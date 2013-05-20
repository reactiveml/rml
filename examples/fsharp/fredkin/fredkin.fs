(* THIS FILE IS GENERATED. *)
(* C:\Users\ccpasteur\Dropbox\Sauvegardes\git\rml\compiler\_build\main\rpmlc.byte -I ../../../lib/ -s main -runtime Fsharp_LcoThread fredkin.rml  *)

#indent "off"
open Caml_compat
let Interpreter = Machine.LcoRmlThread
let Machine = Machine.RmlThreadMachine
let maxx = Pervasives.ref 400
  
let maxy = Pervasives.ref 400
  
let set_maxx i__val_rml_6 = Pervasives.( := ) maxx i__val_rml_6
  
let set_maxy i__val_rml_8 = Pervasives.( := ) maxy i__val_rml_8
  
type status = | Status_Quiescent | Status_Active

type cell =
  { cell_x : int; cell_y : int; cell_activation : (unit, int) Runtime.REvent;
    mutable cell_status : status;
    mutable cell_neighborhood : ((unit, int) Runtime.REvent) list
  }

let new_cell x__val_rml_10 y__val_rml_11 activation__val_rml_12
             status__val_rml_13 =
  {
    cell_x = x__val_rml_10;
    cell_y = y__val_rml_11;
    cell_activation = activation__val_rml_12;
    cell_status = status__val_rml_13;
    cell_neighborhood = [];
  }
  
let nox = Pervasives.ref false
  
let zoom = Pervasives.ref 2
  
let set_zoom i__val_rml_17 = Pervasives.( := ) zoom i__val_rml_17
  
let no_draw c__val_rml_19 = ()
  
let draw_cell_gen color_of_cell__val_rml_21 c__val_rml_22 =
  (Graphics.set_color (color_of_cell__val_rml_21 c__val_rml_22);
   Graphics.fill_rect
     (Pervasives.( * ) c__val_rml_22.cell_x (Pervasives.( ! ) zoom))
     (Pervasives.( * ) c__val_rml_22.cell_y (Pervasives.( ! ) zoom))
     (Pervasives.( ! ) zoom) (Pervasives.( ! ) zoom))
  
let color_of_status s__val_rml_24 =
  match s__val_rml_24.cell_status with
  | Status_Quiescent -> Graphics.red
  | Status_Active -> Graphics.blue
  
let get_von_neumann_neighbors cell__val_rml_26 cell_array__val_rml_27 =
  let x__val_rml_28 = cell__val_rml_26.cell_x in
  let y__val_rml_29 = cell__val_rml_26.cell_y in
  let neighbors__val_rml_30 = Pervasives.ref []
  in
    (if Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_28 1)
     then
       Pervasives.( := ) neighbors__val_rml_30
         ((Array.get
             (Array.get cell_array__val_rml_27
                (Pervasives.( - ) x__val_rml_28 1))
             y__val_rml_29) ::
           (Pervasives.( ! ) neighbors__val_rml_30))
     else ();
     if
       Pervasives.( < ) (Pervasives.( + ) x__val_rml_28 1)
         (Pervasives.( ! ) maxx)
     then
       Pervasives.( := ) neighbors__val_rml_30
         ((Array.get
             (Array.get cell_array__val_rml_27
                (Pervasives.( + ) x__val_rml_28 1))
             y__val_rml_29) ::
           (Pervasives.( ! ) neighbors__val_rml_30))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_29 1)
     then
       Pervasives.( := ) neighbors__val_rml_30
         ((Array.get (Array.get cell_array__val_rml_27 x__val_rml_28)
             (Pervasives.( - ) y__val_rml_29 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_30))
     else ();
     if
       Pervasives.( < ) (Pervasives.( + ) y__val_rml_29 1)
         (Pervasives.( ! ) maxy)
     then
       Pervasives.( := ) neighbors__val_rml_30
         ((Array.get (Array.get cell_array__val_rml_27 x__val_rml_28)
             (Pervasives.( + ) y__val_rml_29 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_30))
     else ();
     Pervasives.( ! ) neighbors__val_rml_30)
  
let get_von_neumann_neighbors_circular cell__val_rml_32
                                       cell_array__val_rml_33 =
  let maxx__val_rml_34 = Pervasives.( ! ) maxx in
  let maxy__val_rml_35 = Pervasives.( ! ) maxy in
  let x__val_rml_36 =
    Pervasives.( + ) cell__val_rml_32.cell_x maxx__val_rml_34 in
  let y__val_rml_37 =
    Pervasives.( + ) cell__val_rml_32.cell_y maxy__val_rml_35 in
  let neighbors__val_rml_38 = Pervasives.ref []
  in
    (Pervasives.( := ) neighbors__val_rml_38
       ((Array.get
           (Array.get cell_array__val_rml_33
              (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_36 1)
                 maxx__val_rml_34))
           y__val_rml_37) ::
         (Pervasives.( ! ) neighbors__val_rml_38));
     Pervasives.( := ) neighbors__val_rml_38
       ((Array.get
           (Array.get cell_array__val_rml_33
              (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_36 1)
                 maxx__val_rml_34))
           y__val_rml_37) ::
         (Pervasives.( ! ) neighbors__val_rml_38));
     Pervasives.( := ) neighbors__val_rml_38
       ((Array.get (Array.get cell_array__val_rml_33 x__val_rml_36)
           (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_37 1)
              maxy__val_rml_35)) ::
         (Pervasives.( ! ) neighbors__val_rml_38));
     Pervasives.( := ) neighbors__val_rml_38
       ((Array.get (Array.get cell_array__val_rml_33 x__val_rml_36)
           (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_37 1)
              maxy__val_rml_35)) ::
         (Pervasives.( ! ) neighbors__val_rml_38));
     Pervasives.( ! ) neighbors__val_rml_38)
  
let get_moore_neighbors cell__val_rml_40 cell_array__val_rml_41 =
  let maxx__val_rml_42 = Pervasives.( ! ) maxx in
  let maxy__val_rml_43 = Pervasives.( ! ) maxy in
  let x__val_rml_44 = cell__val_rml_40.cell_x in
  let y__val_rml_45 = cell__val_rml_40.cell_y in
  let neighbors__val_rml_46 = Pervasives.ref []
  in
    (if
       Pervasives.( && )
         (Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_44 1))
         (Pervasives.( < ) (Pervasives.( + ) y__val_rml_45 1)
            maxy__val_rml_43)
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( - ) x__val_rml_44 1))
             (Pervasives.( + ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if Pervasives.( < ) (Pervasives.( + ) y__val_rml_45 1) maxy__val_rml_43
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get (Array.get cell_array__val_rml_41 x__val_rml_44)
             (Pervasives.( + ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( < ) (Pervasives.( + ) x__val_rml_44 1)
            maxx__val_rml_42)
         (Pervasives.( < ) (Pervasives.( + ) y__val_rml_45 1)
            maxy__val_rml_43)
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( + ) x__val_rml_44 1))
             (Pervasives.( + ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if Pervasives.( < ) (Pervasives.( + ) x__val_rml_44 1) maxx__val_rml_42
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( + ) x__val_rml_44 1))
             y__val_rml_45) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( < ) (Pervasives.( + ) x__val_rml_44 1)
            maxx__val_rml_42)
         (Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_45 1))
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( + ) x__val_rml_44 1))
             (Pervasives.( - ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_45 1)
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get (Array.get cell_array__val_rml_41 x__val_rml_44)
             (Pervasives.( - ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_44 1))
         (Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_45 1))
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( - ) x__val_rml_44 1))
             (Pervasives.( - ) y__val_rml_45 1)) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_44 1)
     then
       Pervasives.( := ) neighbors__val_rml_46
         ((Array.get
             (Array.get cell_array__val_rml_41
                (Pervasives.( - ) x__val_rml_44 1))
             y__val_rml_45) ::
           (Pervasives.( ! ) neighbors__val_rml_46))
     else ();
     Pervasives.( ! ) neighbors__val_rml_46)
  
let get_moore_neighbors_circular cell__val_rml_48 cell_array__val_rml_49 =
  let maxx__val_rml_50 = Pervasives.( ! ) maxx in
  let maxy__val_rml_51 = Pervasives.( ! ) maxy in
  let x__val_rml_52 =
    Pervasives.( + ) cell__val_rml_48.cell_x maxx__val_rml_50 in
  let y__val_rml_53 =
    Pervasives.( + ) cell__val_rml_48.cell_y maxy__val_rml_51 in
  let neighbors__val_rml_54 = Pervasives.ref []
  in
    (Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) x__val_rml_52 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) y__val_rml_53 maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) x__val_rml_52 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_53 1)
              maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( := ) neighbors__val_rml_54
       ((Array.get
           (Array.get cell_array__val_rml_49
              (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_52 1)
                 maxx__val_rml_50))
           (Pervasives.( mod ) y__val_rml_53 maxy__val_rml_51)) ::
         (Pervasives.( ! ) neighbors__val_rml_54));
     Pervasives.( ! ) neighbors__val_rml_54)
  
let rec activate_neighborhood self__val_rml_56 neighbors__val_rml_57 =
  match neighbors__val_rml_57 with
  | [] -> ()
  | activation_sig__val_rml_58 :: neighbors__val_rml_59 ->
      (Interpreter.rml_expr_emit activation_sig__val_rml_58;
       activate_neighborhood self__val_rml_56 neighbors__val_rml_59)
  
let cell draw_cell__val_rml_61 get_neighbors__val_rml_62
         cell_behavior__val_rml_63 x__val_rml_64 y__val_rml_65
         status_init__val_rml_66 cell_array__val_rml_67 () =
  Interpreter.rml_signal_combine Types.Signal Interpreter.rml_base_clock
    Interpreter.rml_base_clock (fun () -> 0)
    (fun () x__val_rml_68 y__val_rml_69 -> Pervasives.( + ) y__val_rml_69 1)
    None
    (fun activation__sig_70 ->
       Interpreter.rml_def
         (fun () ->
            new_cell x__val_rml_64 y__val_rml_65 activation__sig_70
              status_init__val_rml_66)
         (fun self__val_rml_71 ->
            Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () ->
                    Array.set
                      (Array.get cell_array__val_rml_67 x__val_rml_64)
                      y__val_rml_65 activation__sig_70))
              (Interpreter.rml_seq
                 (Interpreter.rml_compute
                    (fun () -> draw_cell__val_rml_61 self__val_rml_71))
                 (Interpreter.rml_seq
                    (Interpreter.rml_pause
                       (fun () -> Interpreter.rml_base_clock))
                    (Interpreter.rml_seq
                       (Interpreter.rml_compute
                          (fun () ->
                             self__val_rml_71.cell_neighborhood <-
                               get_neighbors__val_rml_62 self__val_rml_71
                                 cell_array__val_rml_67))
                       (Interpreter.rml_loop
                          (Interpreter.rml_seq
                             (Interpreter.rml_if
                                (fun () ->
                                   Pervasives.( <> )
                                     self__val_rml_71.cell_status
                                     Status_Quiescent)
                                (Interpreter.rml_compute
                                   (fun () ->
                                      activate_neighborhood self__val_rml_71
                                        self__val_rml_71.cell_neighborhood))
                                (Interpreter.rml_await_immediate'
                                   activation__sig_70))
                             (Interpreter.rml_seq
                                (Interpreter.rml_pause
                                   (fun () -> Interpreter.rml_base_clock))
                                (Interpreter.rml_compute
                                   (fun () ->
                                      let cpt__val_rml_72 =
                                        Interpreter.rml_pre_value
                                          activation__sig_70
                                      in
                                        (cell_behavior__val_rml_63
                                           self__val_rml_71 cpt__val_rml_72;
                                         draw_cell__val_rml_61
                                           self__val_rml_71)))))))))))
  
let fredkin cell__val_rml_74 cpt__val_rml_75 =
  cell__val_rml_74.cell_status <-
    if Pervasives.( = ) (Pervasives.( mod ) cpt__val_rml_75 2) 1
    then Status_Active
    else Status_Quiescent
  
let game_of_life cell__val_rml_77 cpt__val_rml_78 =
  cell__val_rml_77.cell_status <-
    if
      Pervasives.( && )
        (Pervasives.( = ) cell__val_rml_77.cell_status Status_Quiescent)
        (Pervasives.( = ) cpt__val_rml_78 3)
    then Status_Active
    else
      if
        Pervasives.( && )
          (Pervasives.( <> ) cell__val_rml_77.cell_status Status_Quiescent)
          (Pervasives.( && ) (Pervasives.( <> ) cpt__val_rml_78 2)
             (Pervasives.( <> ) cpt__val_rml_78 3))
      then Status_Quiescent
      else cell__val_rml_77.cell_status
  
let cell_array_create tmp__val_rml_80 =
  Array.make_matrix (Pervasives.( ! ) maxx) (Pervasives.( ! ) maxy)
    tmp__val_rml_80
  
let get_status_empty i__val_rml_82 j__val_rml_83 = Status_Quiescent
  
let get_status_center i__val_rml_85 j__val_rml_86 =
  if
    Pervasives.( && )
      (Pervasives.( = ) i__val_rml_85
         (Pervasives.( / ) (Pervasives.( ! ) maxx) 2))
      (Pervasives.( = ) j__val_rml_86
         (Pervasives.( / ) (Pervasives.( ! ) maxy) 2))
  then Status_Active
  else Status_Quiescent
  
let get_status_center_line i__val_rml_88 j__val_rml_89 =
  if
    Pervasives.( = ) i__val_rml_88
      (Pervasives.( / ) (Pervasives.( ! ) maxx) 2)
  then Status_Active
  else Status_Quiescent
  
let get_status_center_line_wall i__val_rml_91 j__val_rml_92 =
  if
    Pervasives.( or ) (Pervasives.( = ) i__val_rml_91 0)
      (Pervasives.( or )
         (Pervasives.( = ) i__val_rml_91
            (Pervasives.( / ) (Pervasives.( ! ) maxx) 2))
         (Pervasives.( or )
            (Pervasives.( = ) i__val_rml_91
               (Pervasives.( - ) (Pervasives.( ! ) maxx) 1))
            (Pervasives.( or ) (Pervasives.( = ) j__val_rml_92 0)
               (Pervasives.( = ) j__val_rml_92
                  (Pervasives.( - ) (Pervasives.( ! ) maxy) 1)))))
  then Status_Active
  else Status_Quiescent
  
let cellular_automaton_start draw_cell__val_rml_94 get_neighbors__val_rml_95
                             get_status__val_rml_96 cell_behavior__val_rml_97
                             cell_array__val_rml_98 () =
  Interpreter.rml_fordopar (fun () -> 0)
    (fun () -> Pervasives.( - ) (Pervasives.( ! ) maxx) 1) true
    (fun i__val_rml_99 ->
       Interpreter.rml_fordopar (fun () -> 0)
         (fun () -> Pervasives.( - ) (Pervasives.( ! ) maxy) 1) true
         (fun j__val_rml_100 ->
            Interpreter.rml_run
              (fun () ->
                 cell draw_cell__val_rml_94 get_neighbors__val_rml_95
                   cell_behavior__val_rml_97 i__val_rml_99 j__val_rml_100
                   (get_status__val_rml_96 i__val_rml_99 j__val_rml_100)
                   cell_array__val_rml_98)))
  
let behavior = Pervasives.ref "fredkin"
  
let set_behavior s__val_rml_103 = Pervasives.( := ) behavior s__val_rml_103
  
let configure () =
  let doc_zoom__val_rml_105 = "<n> set the size of a cell" in
  let doc_width__val_rml_106 = "<n> set the number of cells in the width" in
  let doc_height__val_rml_107 =
    "<n> set the number of cells in the height" in
  let doc_behavior__val_rml_108 =
    "<s> select the behavior of the automaton" in
  let doc_nox__val_rml_109 = "disable graphical output" in
  let errmsg__val_rml_110 = "Options are:"
  in
    Rmlarg.parse
      [ ("-zoom", (Rmlarg.Int set_zoom), doc_zoom__val_rml_105);
        ("-width", (Rmlarg.Int set_maxx), doc_width__val_rml_106);
        ("-w", (Rmlarg.Int set_maxx), doc_width__val_rml_106);
        ("-height", (Rmlarg.Int set_maxy), doc_height__val_rml_107);
        ("-h", (Rmlarg.Int set_maxy), doc_height__val_rml_107);
        ("-b", (Rmlarg.String set_behavior), doc_behavior__val_rml_108);
        ("-nox", (Rmlarg.Set nox), doc_nox__val_rml_109) ]
      (fun s__val_rml_111 ->
         Pervasives.raise (Invalid_argument s__val_rml_111))
      errmsg__val_rml_110
  
let main () =
  Interpreter.rml_seq
    (Interpreter.rml_compute (fun () -> Random.self_init ()))
    (Interpreter.rml_seq (Interpreter.rml_compute (fun () -> configure ()))
       (Interpreter.rml_seq
          (Interpreter.rml_compute
             (fun () ->
                if Pervasives.( ! ) nox
                then ()
                else
                  (Graphics.open_graph
                     (Pervasives.( ^ ) " "
                        (Pervasives.( ^ )
                           (Pervasives.string_of_int
                              (Pervasives.( * ) (Pervasives.( ! ) maxx)
                                 (Pervasives.( ! ) zoom)))
                           (Pervasives.( ^ ) "x"
                              (Pervasives.string_of_int
                                 (Pervasives.( * ) (Pervasives.( ! ) maxy)
                                    (Pervasives.( ! ) zoom))))));
                   Graphics.auto_synchronize false)))
          (Interpreter.rml_signal_combine Types.Signal Interpreter.
             rml_base_clock Interpreter.rml_base_clock (fun () -> 0)
             (fun () x__val_rml_113 y__val_rml_114 ->
                Pervasives.( + ) y__val_rml_114 1)
             None
             (fun tmp__sig_115 ->
                Interpreter.rml_par
                  (Interpreter.rml_match
                     (fun () -> Pervasives.( ! ) behavior)
                     (function
                      | "gol" ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_115)
                            (fun cell_array__val_rml_116 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_117 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_117
                                           get_moore_neighbors_circular
                                           get_status_center_line_wall
                                           game_of_life
                                           cell_array__val_rml_116)))
                      | "fredkin" ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_115)
                            (fun cell_array__val_rml_118 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_119 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_119
                                           get_moore_neighbors_circular
                                           get_status_center fredkin
                                           cell_array__val_rml_118)))
                      | "fredkin2" ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_115)
                            (fun cell_array__val_rml_120 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_121 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_121
                                           get_von_neumann_neighbors
                                           get_status_center fredkin
                                           cell_array__val_rml_120)))
                      | s__val_rml_122 ->
                          Interpreter.rml_compute
                            (fun () ->
                               Pervasives.raise
                                 (Invalid_argument s__val_rml_122))))
                  (Interpreter.rml_if (fun () -> Pervasives.( ! ) nox)
                     (Interpreter.rml_compute (fun () -> ()))
                     (Interpreter.rml_loop
                        (Interpreter.rml_seq
                           (Interpreter.rml_compute
                              (fun () -> Graphics.synchronize ()))
                           (Interpreter.rml_pause
                              (fun () -> Interpreter.rml_base_clock)))))))))
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

