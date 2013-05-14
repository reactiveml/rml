(* THIS FILE IS GENERATED. *)
(* C:\Users\ccpasteur\Dropbox\Sauvegardes\git\rml\compiler\_build\main\rpmlc.byte -I ../../../lib/ -s main -runtime Fsharp_LcoThread cellular_automata.rml  *)

#indent "off"
open Caml_compat
let Interpreter = Machine.LcoThread
let Machine = Machine.ThreadMachine
type status = | Status_Quiescent | Status_Wall | Status_Active

type fire_state = | Fire_empty | Fire_ash | Fire_fire | Fire_tree

type dir =
  | Up | Down | Left | Right | Up_Left | Up_Right | Down_Left | Down_Right

let opposite dir__val_rml_4 =
  match dir__val_rml_4 with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
  | Up_Left -> Down_Right
  | Up_Right -> Down_Left
  | Down_Left -> Up_Right
  | Down_Right -> Up_Left
  
type 'a info = { origine : dir; status : status; info : 'a }

type 'a neighborhood = ('a info) list

let maxx = Pervasives.ref 400
  
let maxy = Pervasives.ref 400
  
let nox = Pervasives.ref false
  
let zoom = Pervasives.ref 2
  
let set_maxx i__val_rml_10 = Pervasives.( := ) maxx i__val_rml_10
  
let set_maxy i__val_rml_12 = Pervasives.( := ) maxy i__val_rml_12
  
let set_zoom i__val_rml_14 = Pervasives.( := ) zoom i__val_rml_14
  
type ('a, 'b) cell =
  { cell_x : int; cell_y : int;
    cell_activation : ('a info, ('a info) list) Runtime.REvent;
    mutable cell_status : status;
    mutable cell_neighborhood :
    (dir * (('a info, ('a info) list) Runtime.REvent)) list;
    mutable cell_ext : 'b
  }

let make_info origine__val_rml_16 cell__val_rml_17 =
  {
    origine = origine__val_rml_16;
    status = cell__val_rml_17.cell_status;
    info = cell__val_rml_17.cell_ext;
  }
  
let new_cell x__val_rml_19 y__val_rml_20 activation__val_rml_21
             status__val_rml_22 ext__val_rml_23 =
  {
    cell_x = x__val_rml_19;
    cell_y = y__val_rml_20;
    cell_activation = activation__val_rml_21;
    cell_status = status__val_rml_22;
    cell_neighborhood = [];
    cell_ext = ext__val_rml_23;
  }
  
let no_draw c__val_rml_25 = ()
  
let draw_cell_gen color_of_cell__val_rml_27 c__val_rml_28 =
  (Graphics.set_color (color_of_cell__val_rml_27 c__val_rml_28);
   Graphics.fill_rect
     (Pervasives.( * ) c__val_rml_28.cell_x (Pervasives.( ! ) zoom))
     (Pervasives.( * ) c__val_rml_28.cell_y (Pervasives.( ! ) zoom))
     (Pervasives.( ! ) zoom) (Pervasives.( ! ) zoom))
  
let color_of_status s__val_rml_30 =
  match s__val_rml_30.cell_status with
  | Status_Quiescent -> Graphics.red
  | Status_Wall -> Graphics.green
  | Status_Active -> Graphics.blue
  
let color_of_fire_state c__val_rml_32 =
  match c__val_rml_32.cell_ext with
  | Fire_empty -> Graphics.rgb 255 255 255
  | Fire_ash -> Graphics.rgb 173 173 173
  | Fire_fire -> Graphics.rgb 255 0 0
  | Fire_tree -> Graphics.rgb 0 255 0
  
let get_von_neumann_neighbors cell__val_rml_34 cell_array__val_rml_35 =
  let x__val_rml_36 = cell__val_rml_34.cell_x in
  let y__val_rml_37 = cell__val_rml_34.cell_y in
  let neighbors__val_rml_38 = Pervasives.ref []
  in
    (if Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_36 1)
     then
       Pervasives.( := ) neighbors__val_rml_38
         ((Left,
           (Array.get
              (Array.get cell_array__val_rml_35
                 (Pervasives.( - ) x__val_rml_36 1))
              y__val_rml_37)) ::
           (Pervasives.( ! ) neighbors__val_rml_38))
     else ();
     if
       Pervasives.( < ) (Pervasives.( + ) x__val_rml_36 1)
         (Pervasives.( ! ) maxx)
     then
       Pervasives.( := ) neighbors__val_rml_38
         ((Right,
           (Array.get
              (Array.get cell_array__val_rml_35
                 (Pervasives.( + ) x__val_rml_36 1))
              y__val_rml_37)) ::
           (Pervasives.( ! ) neighbors__val_rml_38))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_37 1)
     then
       Pervasives.( := ) neighbors__val_rml_38
         ((Down,
           (Array.get (Array.get cell_array__val_rml_35 x__val_rml_36)
              (Pervasives.( - ) y__val_rml_37 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_38))
     else ();
     if
       Pervasives.( < ) (Pervasives.( + ) y__val_rml_37 1)
         (Pervasives.( ! ) maxy)
     then
       Pervasives.( := ) neighbors__val_rml_38
         ((Up,
           (Array.get (Array.get cell_array__val_rml_35 x__val_rml_36)
              (Pervasives.( + ) y__val_rml_37 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_38))
     else ();
     Pervasives.( ! ) neighbors__val_rml_38)
  
let get_von_neumann_neighbors_circular cell__val_rml_40
                                       cell_array__val_rml_41 =
  let maxx__val_rml_42 = Pervasives.( ! ) maxx in
  let maxy__val_rml_43 = Pervasives.( ! ) maxy in
  let x__val_rml_44 =
    Pervasives.( + ) cell__val_rml_40.cell_x maxx__val_rml_42 in
  let y__val_rml_45 =
    Pervasives.( + ) cell__val_rml_40.cell_y maxy__val_rml_43 in
  let neighbors__val_rml_46 = Pervasives.ref []
  in
    (Pervasives.( := ) neighbors__val_rml_46
       ((Left,
         (Array.get
            (Array.get cell_array__val_rml_41
               (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_44 1)
                  maxx__val_rml_42))
            y__val_rml_45)) ::
         (Pervasives.( ! ) neighbors__val_rml_46));
     Pervasives.( := ) neighbors__val_rml_46
       ((Right,
         (Array.get
            (Array.get cell_array__val_rml_41
               (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_44 1)
                  maxx__val_rml_42))
            y__val_rml_45)) ::
         (Pervasives.( ! ) neighbors__val_rml_46));
     Pervasives.( := ) neighbors__val_rml_46
       ((Down,
         (Array.get (Array.get cell_array__val_rml_41 x__val_rml_44)
            (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_45 1)
               maxy__val_rml_43))) ::
         (Pervasives.( ! ) neighbors__val_rml_46));
     Pervasives.( := ) neighbors__val_rml_46
       ((Up,
         (Array.get (Array.get cell_array__val_rml_41 x__val_rml_44)
            (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_45 1)
               maxy__val_rml_43))) ::
         (Pervasives.( ! ) neighbors__val_rml_46));
     Pervasives.( ! ) neighbors__val_rml_46)
  
let get_moore_neighbors cell__val_rml_48 cell_array__val_rml_49 =
  let maxx__val_rml_50 = Pervasives.( ! ) maxx in
  let maxy__val_rml_51 = Pervasives.( ! ) maxy in
  let x__val_rml_52 = cell__val_rml_48.cell_x in
  let y__val_rml_53 = cell__val_rml_48.cell_y in
  let neighbors__val_rml_54 = Pervasives.ref []
  in
    (if
       Pervasives.( && )
         (Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_52 1))
         (Pervasives.( < ) (Pervasives.( + ) y__val_rml_53 1)
            maxy__val_rml_51)
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Up_Left,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( - ) x__val_rml_52 1))
              (Pervasives.( + ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if Pervasives.( < ) (Pervasives.( + ) y__val_rml_53 1) maxy__val_rml_51
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Up,
           (Array.get (Array.get cell_array__val_rml_49 x__val_rml_52)
              (Pervasives.( + ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( < ) (Pervasives.( + ) x__val_rml_52 1)
            maxx__val_rml_50)
         (Pervasives.( < ) (Pervasives.( + ) y__val_rml_53 1)
            maxy__val_rml_51)
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Up_Right,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( + ) x__val_rml_52 1))
              (Pervasives.( + ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if Pervasives.( < ) (Pervasives.( + ) x__val_rml_52 1) maxx__val_rml_50
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Right,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( + ) x__val_rml_52 1))
              y__val_rml_53)) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( < ) (Pervasives.( + ) x__val_rml_52 1)
            maxx__val_rml_50)
         (Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_53 1))
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Down_Right,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( + ) x__val_rml_52 1))
              (Pervasives.( - ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_53 1)
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Down,
           (Array.get (Array.get cell_array__val_rml_49 x__val_rml_52)
              (Pervasives.( - ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if
       Pervasives.( && )
         (Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_52 1))
         (Pervasives.( <= ) 0 (Pervasives.( - ) y__val_rml_53 1))
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Down_Left,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( - ) x__val_rml_52 1))
              (Pervasives.( - ) y__val_rml_53 1))) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     if Pervasives.( <= ) 0 (Pervasives.( - ) x__val_rml_52 1)
     then
       Pervasives.( := ) neighbors__val_rml_54
         ((Left,
           (Array.get
              (Array.get cell_array__val_rml_49
                 (Pervasives.( - ) x__val_rml_52 1))
              y__val_rml_53)) ::
           (Pervasives.( ! ) neighbors__val_rml_54))
     else ();
     Pervasives.( ! ) neighbors__val_rml_54)
  
let get_moore_neighbors_circular cell__val_rml_56 cell_array__val_rml_57 =
  let maxx__val_rml_58 = Pervasives.( ! ) maxx in
  let maxy__val_rml_59 = Pervasives.( ! ) maxy in
  let x__val_rml_60 =
    Pervasives.( + ) cell__val_rml_56.cell_x maxx__val_rml_58 in
  let y__val_rml_61 =
    Pervasives.( + ) cell__val_rml_56.cell_y maxy__val_rml_59 in
  let neighbors__val_rml_62 = Pervasives.ref []
  in
    (Pervasives.( := ) neighbors__val_rml_62
       ((Up_Left,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Up,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) x__val_rml_60 maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Up_Right,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( + ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Right,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) y__val_rml_61 maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Down_Right,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( + ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Down,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) x__val_rml_60 maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Down_Left,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) (Pervasives.( - ) y__val_rml_61 1)
               maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( := ) neighbors__val_rml_62
       ((Left,
         (Array.get
            (Array.get cell_array__val_rml_57
               (Pervasives.( mod ) (Pervasives.( - ) x__val_rml_60 1)
                  maxx__val_rml_58))
            (Pervasives.( mod ) y__val_rml_61 maxy__val_rml_59))) ::
         (Pervasives.( ! ) neighbors__val_rml_62));
     Pervasives.( ! ) neighbors__val_rml_62)
  
let rec activate_neighborhood self__val_rml_64 neighbors__val_rml_65 =
  match neighbors__val_rml_65 with
  | [] -> ()
  | (dir__val_rml_66, activation_sig__val_rml_67) :: neighbors__val_rml_68 ->
      let info__val_rml_69 =
        make_info (opposite dir__val_rml_66) self__val_rml_64
      in
        (Interpreter.rml_expr_emit_val activation_sig__val_rml_67
           info__val_rml_69;
         activate_neighborhood self__val_rml_64 neighbors__val_rml_68)
  
let cell draw_cell__val_rml_71 get_neighbors__val_rml_72
         cell_behavior__val_rml_73 x__val_rml_74 y__val_rml_75
         (status_init__val_rml_76, ext_init__val_rml_77)
         cell_array__val_rml_78 () =
  Interpreter.rml_signal Types.Signal Interpreter.rml_base_clock Interpreter.
    rml_base_clock None
    (fun activation__sig_79 ->
       Interpreter.rml_def
         (fun () ->
            new_cell x__val_rml_74 y__val_rml_75 activation__sig_79
              status_init__val_rml_76 ext_init__val_rml_77)
         (fun self__val_rml_80 ->
            Interpreter.rml_seq
              (Interpreter.rml_compute
                 (fun () ->
                    Array.set
                      (Array.get cell_array__val_rml_78 x__val_rml_74)
                      y__val_rml_75 activation__sig_79))
              (Interpreter.rml_seq
                 (Interpreter.rml_compute
                    (fun () -> draw_cell__val_rml_71 self__val_rml_80))
                 (Interpreter.rml_seq
                    (Interpreter.rml_pause
                       (fun () -> Interpreter.rml_base_clock))
                    (Interpreter.rml_seq
                       (Interpreter.rml_compute
                          (fun () ->
                             self__val_rml_80.cell_neighborhood <-
                               get_neighbors__val_rml_72 self__val_rml_80
                                 cell_array__val_rml_78))
                       (Interpreter.rml_loop
                          (Interpreter.rml_seq
                             (Interpreter.rml_if
                                (fun () ->
                                   Pervasives.( <> )
                                     self__val_rml_80.cell_status
                                     Status_Quiescent)
                                (Interpreter.rml_compute
                                   (fun () ->
                                      activate_neighborhood self__val_rml_80
                                        self__val_rml_80.cell_neighborhood))
                                (Interpreter.rml_await_immediate'
                                   activation__sig_79))
                             (Interpreter.rml_seq
                                (Interpreter.rml_pause
                                   (fun () -> Interpreter.rml_base_clock))
                                (Interpreter.rml_compute
                                   (fun () ->
                                      let neighbors__val_rml_81 =
                                        Interpreter.rml_pre_value
                                          activation__sig_79
                                      in
                                        (cell_behavior__val_rml_73
                                           self__val_rml_80
                                           neighbors__val_rml_81;
                                         draw_cell__val_rml_71
                                           self__val_rml_80)))))))))))
  
let fredkin cell__val_rml_83 neighbors__val_rml_84 =
  let cpt__val_rml_85 = Pervasives.ref 0
  in
    (List.iter
       (fun info__val_rml_86 ->
          if Pervasives.( <> ) info__val_rml_86.status Status_Quiescent
          then Pervasives.incr cpt__val_rml_85
          else ())
       neighbors__val_rml_84;
     cell__val_rml_83.cell_status <-
       if
         Pervasives.( = )
           (Pervasives.( mod ) (Pervasives.( ! ) cpt__val_rml_85) 2) 1
       then Status_Active
       else Status_Quiescent)
  
let game_of_life cell__val_rml_88 neighbors__val_rml_89 =
  let cpt__val_rml_90 = Pervasives.ref 0
  in
    (List.iter
       (fun info__val_rml_91 ->
          if Pervasives.( <> ) info__val_rml_91.status Status_Quiescent
          then Pervasives.incr cpt__val_rml_90
          else ())
       neighbors__val_rml_89;
     cell__val_rml_88.cell_status <-
       if
         Pervasives.( && )
           (Pervasives.( = ) cell__val_rml_88.cell_status Status_Quiescent)
           (Pervasives.( = ) (Pervasives.( ! ) cpt__val_rml_90) 3)
       then Status_Active
       else
         if
           Pervasives.( && )
             (Pervasives.( <> ) cell__val_rml_88.cell_status Status_Quiescent)
             (Pervasives.( && )
                (Pervasives.( <> ) (Pervasives.( ! ) cpt__val_rml_90) 2)
                (Pervasives.( <> ) (Pervasives.( ! ) cpt__val_rml_90) 3))
         then Status_Quiescent
         else cell__val_rml_88.cell_status)
  
let fire_behavior cell__val_rml_93 neighbors__val_rml_94 =
  match cell__val_rml_93.cell_ext with
  | Fire_empty -> cell__val_rml_93.cell_status <- Status_Quiescent
  | Fire_ash ->
      (cell__val_rml_93.cell_ext <- Fire_empty;
       cell__val_rml_93.cell_status <- Status_Quiescent)
  | Fire_fire ->
      (cell__val_rml_93.cell_ext <- Fire_ash;
       cell__val_rml_93.cell_status <- Status_Active)
  | Fire_tree ->
      if
        List.exists
          (fun info__val_rml_95 ->
             Pervasives.( = ) info__val_rml_95.info Fire_fire)
          neighbors__val_rml_94
      then
        (cell__val_rml_93.cell_ext <- Fire_fire;
         cell__val_rml_93.cell_status <- Status_Active)
      else cell__val_rml_93.cell_status <- Status_Quiescent
  
let cell_array_create tmp__val_rml_97 =
  Array.make_matrix (Pervasives.( ! ) maxx) (Pervasives.( ! ) maxy)
    tmp__val_rml_97
  
let get_status_empty i__val_rml_99 j__val_rml_100 = (Status_Quiescent, ())
  
let get_status_center i__val_rml_102 j__val_rml_103 =
  if
    Pervasives.( && )
      (Pervasives.( = ) i__val_rml_102
         (Pervasives.( / ) (Pervasives.( ! ) maxx) 2))
      (Pervasives.( = ) j__val_rml_103
         (Pervasives.( / ) (Pervasives.( ! ) maxy) 2))
  then (Status_Active, ())
  else (Status_Quiescent, ())
  
let get_status_center_line i__val_rml_105 j__val_rml_106 =
  if
    Pervasives.( = ) i__val_rml_105
      (Pervasives.( / ) (Pervasives.( ! ) maxx) 2)
  then (Status_Active, ())
  else (Status_Quiescent, ())
  
let get_status_center_line_wall i__val_rml_108 j__val_rml_109 =
  if
    Pervasives.( or ) (Pervasives.( = ) i__val_rml_108 0)
      (Pervasives.( or )
         (Pervasives.( = ) i__val_rml_108
            (Pervasives.( / ) (Pervasives.( ! ) maxx) 2))
         (Pervasives.( or )
            (Pervasives.( = ) i__val_rml_108
               (Pervasives.( - ) (Pervasives.( ! ) maxx) 1))
            (Pervasives.( or ) (Pervasives.( = ) j__val_rml_109 0)
               (Pervasives.( = ) j__val_rml_109
                  (Pervasives.( - ) (Pervasives.( ! ) maxy) 1)))))
  then (Status_Active, ())
  else (Status_Quiescent, ())
  
let get_status_fire i__val_rml_111 j__val_rml_112 =
  if
    Pervasives.( && )
      (Pervasives.( = ) i__val_rml_111
         (Pervasives.( / ) (Pervasives.( ! ) maxx) 2))
      (Pervasives.( = ) j__val_rml_112
         (Pervasives.( / ) (Pervasives.( ! ) maxy) 2))
  then (Status_Active, Fire_fire)
  else
    if Pervasives.( < ) (Random.float 1.) 0.6
    then (Status_Quiescent, Fire_tree)
    else (Status_Quiescent, Fire_empty)
  
let cellular_automaton_start draw_cell__val_rml_114
                             get_neighbors__val_rml_115
                             get_status__val_rml_116
                             cell_behavior__val_rml_117
                             cell_array__val_rml_118 () =
  Interpreter.rml_fordopar (fun () -> 0)
    (fun () -> Pervasives.( - ) (Pervasives.( ! ) maxx) 1) true
    (fun i__val_rml_119 ->
       Interpreter.rml_fordopar (fun () -> 0)
         (fun () -> Pervasives.( - ) (Pervasives.( ! ) maxy) 1) true
         (fun j__val_rml_120 ->
            Interpreter.rml_run
              (fun () ->
                 cell draw_cell__val_rml_114 get_neighbors__val_rml_115
                   cell_behavior__val_rml_117 i__val_rml_119 j__val_rml_120
                   (get_status__val_rml_116 i__val_rml_119 j__val_rml_120)
                   cell_array__val_rml_118)))
  
let behavior = Pervasives.ref "fire"
  
let set_behavior s__val_rml_123 = Pervasives.( := ) behavior s__val_rml_123
  
let configure () =
  let doc_zoom__val_rml_125 = "<n> set the size of a cell" in
  let doc_width__val_rml_126 = "<n> set the number of cells in the width" in
  let doc_height__val_rml_127 =
    "<n> set the number of cells in the height" in
  let doc_behavior__val_rml_128 =
    "<s> select the behavior of the automaton" in
  let doc_nox__val_rml_129 = "disable graphical output" in
  let errmsg__val_rml_130 = "Options are:"
  in
    Rmlarg.parse
      [ ("-zoom", (Rmlarg.Int set_zoom), doc_zoom__val_rml_125);
        ("-width", (Rmlarg.Int set_maxx), doc_width__val_rml_126);
        ("-w", (Rmlarg.Int set_maxx), doc_width__val_rml_126);
        ("-height", (Rmlarg.Int set_maxy), doc_height__val_rml_127);
        ("-h", (Rmlarg.Int set_maxy), doc_height__val_rml_127);
        ("-b", (Rmlarg.String set_behavior), doc_behavior__val_rml_128);
        ("-nox", (Rmlarg.Set nox), doc_nox__val_rml_129) ]
      (fun s__val_rml_131 ->
         Pervasives.raise (Invalid_argument s__val_rml_131))
      errmsg__val_rml_130
  
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
          (Interpreter.rml_par
             (Interpreter.rml_match (fun () -> Pervasives.( ! ) behavior)
                (function
                 | "gol" ->
                     Interpreter.rml_signal Types.Signal Interpreter.
                       rml_base_clock Interpreter.rml_base_clock None
                       (fun tmp__sig_133 ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_133)
                            (fun cell_array__val_rml_134 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_135 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_135
                                           get_moore_neighbors_circular
                                           get_status_center_line_wall
                                           game_of_life
                                           cell_array__val_rml_134))))
                 | "fredkin" ->
                     Interpreter.rml_signal Types.Signal Interpreter.
                       rml_base_clock Interpreter.rml_base_clock None
                       (fun tmp__sig_136 ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_136)
                            (fun cell_array__val_rml_137 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_138 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_138
                                           get_moore_neighbors_circular
                                           get_status_center fredkin
                                           cell_array__val_rml_137))))
                 | "fredkin2" ->
                     Interpreter.rml_signal Types.Signal Interpreter.
                       rml_base_clock Interpreter.rml_base_clock None
                       (fun tmp__sig_139 ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_139)
                            (fun cell_array__val_rml_140 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_status)
                                 (fun draw__val_rml_141 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_141
                                           get_von_neumann_neighbors
                                           get_status_center fredkin
                                           cell_array__val_rml_140))))
                 | "fire" ->
                     Interpreter.rml_signal Types.Signal Interpreter.
                       rml_base_clock Interpreter.rml_base_clock None
                       (fun tmp__sig_142 ->
                          Interpreter.rml_def
                            (fun () -> cell_array_create tmp__sig_142)
                            (fun cell_array__val_rml_143 ->
                               Interpreter.rml_def
                                 (fun () ->
                                    if Pervasives.( ! ) nox
                                    then no_draw
                                    else draw_cell_gen color_of_fire_state)
                                 (fun draw__val_rml_144 ->
                                    Interpreter.rml_run
                                      (fun () ->
                                         cellular_automaton_start
                                           draw__val_rml_144
                                           get_von_neumann_neighbors
                                           get_status_fire fire_behavior
                                           cell_array__val_rml_143))))
                 | s__val_rml_145 ->
                     Interpreter.rml_compute
                       (fun () ->
                          Pervasives.raise (Invalid_argument s__val_rml_145))))
             (Interpreter.rml_if (fun () -> Pervasives.( ! ) nox)
                (Interpreter.rml_compute (fun () -> ()))
                (Interpreter.rml_loop
                   (Interpreter.rml_seq
                      (Interpreter.rml_compute
                         (fun () -> Graphics.synchronize ()))
                      (Interpreter.rml_pause
                         (fun () -> Interpreter.rml_base_clock))))))))
  
let _ = Machine.rml_exec Interpreter.rml_make main
  

