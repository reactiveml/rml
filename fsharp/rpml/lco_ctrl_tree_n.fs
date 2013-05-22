module Lco

open Types
open Runtime

let unit_value = ()
let dummy_step _ = ()
let default_combine x y = x :: y
let default_default = []
let true_cond _ = true

type Interpreter<'ctrl>(R:Runtime<'ctrl>) =

    let eval_clock_expr (current_cd:ClockDomain<_>) ce = match ce with
      | CkLocal -> current_cd.clock
      | CkTop -> R.top_clock ()
      | CkExpr e -> e

    let ensure_clock_expr ce = match ce with
      | CkExpr e -> e
      | CkTop -> R.top_clock ()
      | CkLocal -> (* should be rejected by compiler *)
          raise Types.RML

    let rec on_event_at_eoi evt ctrl f =
      let eoi_work _ =
        try
          f ()
        with
          | Wait_again -> on_event_at_eoi evt ctrl f
      in
      R.on_event evt ctrl (fun () -> R.on_eoi evt.clock eoi_work)

    let on_event_cfg_at_eoi evt_cfg ctrl f = ()

    (* ------------------------------------------------------------------------ *)
    member this.rml_pre_status (evt:#REvent<_, _>) = evt.pre_status

    member this.rml_pre_value (evt:#REvent<_, _>) = evt.pre_value

    member this.rml_last (evt:#REvent<_, _>) = evt.last

    member this.rml_default (evt:#REvent<_, _>) = evt._default

    member this.rml_clock (evt:#REvent<_, _>) = CkExpr evt.clock

    (* ------------------------------------------------------------------------ *)

    member this.rml_global_signal_combine k _default combine =
      R.new_evt_global k _default combine

    member this.rml_global_signal k =
      this.rml_global_signal_combine k default_default default_combine

    (* ------------------------------------------------------------------------ *)

    member this.cfg_present' evt _ =
      R.cfg_present evt
    member this.cfg_present expr_evt _ =
      let evt = expr_evt () in
      R.cfg_present evt
    member this.cfg_and ev1 ev2 _ =
      R.cfg_and (ev1 ()) (ev2 ())
    member this.cfg_or ev1 ev2 _ =
      R.cfg_or (ev1 ()) (ev2 ())

    (**************************************)
    (* nothing                            *)
    (**************************************)
    member this.rml_nothing =
      fun (f_k:unit Step.t) (ctrl:'ctrl) (jp: Join option) (cd:ClockDomain<'ctrl>) () ->
        f_k unit_value

    (**************************************)
    (* compute                            *)
    (**************************************)
    member this.rml_compute e =
      fun f_k ctrl jp cd _ ->
        let v = e () in
        f_k v

    (**************************************)
    (* pause                              *)
    (**************************************)

    member this.rml_pause' pause_ce =
      fun f_k ctrl jp cd _ ->
        let pause_ck = eval_clock_expr cd pause_ce in
        R.on_eoi pause_ck (fun () -> R.on_next_instant Strong ctrl f_k)

    member this.rml_pause e =
      fun f_k ctrl jp cd _ ->
        this.rml_pause' (e ()) f_k ctrl jp cd unit_value

    (**************************************)
    (* weak pause                              *)
    (**************************************)

    member this.rml_weak_pause' pause_ce =
      fun f_k ctrl jp cd _ ->
        let pause_ck = eval_clock_expr cd pause_ce in
        R.on_eoi pause_ck (fun () -> R.on_next_instant Weak ctrl f_k)

    member this.rml_weak_pause e =
      fun f_k ctrl jp cd _ ->
        this.rml_weak_pause' (e ()) f_k ctrl jp cd unit_value

    (**************************************)
    (* halt                               *)
    (**************************************)
    member this.rml_halt =
      fun f_k ctrl jp cd _ ->
        ()

    (**************************************)
    (* emit                               *)
    (**************************************)
    member this.rml_emit_val' (evt:REvent<_,_>) e =
      fun f_k ctrl jp cd _ ->
        evt.emit (e());
        f_k unit_value

    member this.rml_emit_val expr_evt e =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        this.rml_emit_val' evt e f_k ctrl jp cd unit_value

    member this.rml_emit expr_evt = this.rml_emit_val expr_evt (fun() -> ())
    member this.rml_emit' evt = this.rml_emit_val' evt (fun() -> ())

    member this.rml_expr_emit_val (evt:REvent<_,_>) v =
      evt.emit v

    member this.rml_expr_emit evt =
      this.rml_expr_emit_val evt ()

    (**************************************)
    (* await_immediate                    *)
    (**************************************)

    member this.rml_await_immediate' evt =
      fun f_k ctrl jp cd _ ->
        R.on_event evt ctrl f_k

    member this.rml_await_immediate expr_evt =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        this.rml_await_immediate' evt f_k ctrl jp cd unit_value

    (**************************************)
    (* await_immediate_conf               *)
    (**************************************)

    member this.rml_await_immediate_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        R.on_event_cfg (expr_cfg ()) ctrl f_k

    (**************************************)
    (* get                                *)
    (**************************************)
    member this.rml_get' (evt:REvent<_,_>) p =
      fun f_k ctrl jp cd _ ->
        let f_get _ =
          let x = if evt.status false then evt.value else evt._default in
          let f_body = p x f_k ctrl jp cd in
          R.on_next_instant Strong ctrl f_body
        in
        R.on_eoi evt.clock f_get

    member this.rml_get expr_evt p =
      let evt = expr_evt() in
      this.rml_get' evt p

    (**************************************)
    (* await_immediate_one                *)
    (**************************************)


    member this.rml_await_immediate_one' (evt:REvent<_,_>) p =
      fun f_k ctrl jp cd _ ->
        let f _ =
          let x = evt.one () in
          p x f_k ctrl jp cd unit_value
        in
        R.on_event evt ctrl f

    member this.rml_await_immediate_one expr_evt p =
       fun f_k ctrl jp cd _ ->
         let evt = expr_evt() in
         this.rml_await_immediate_one' evt p f_k ctrl jp cd unit_value

    (**************************************)
    (* await_all_match                    *)
    (**************************************)

    member this.rml_await_all_match' (evt:REvent<_,_>) matching p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = evt.value in
           if matching v then
             R.on_next_instant Strong ctrl (p v f_k ctrl jp cd)
           else
             raise Wait_again
         in
         on_event_at_eoi evt ctrl await_eoi

    member this.rml_await_all_match expr_evt matching p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt() in
        this.rml_await_all_match' evt matching p f_k ctrl jp cd unit_value

    (**************************************)
    (* await                              *)
    (**************************************)

    member this.rml_await' evt =
      fun f_k ctrl jp cd _ ->
        let await_eoi _ = R.on_next_instant Strong ctrl f_k in
        on_event_at_eoi evt ctrl await_eoi

    member this.rml_await expr_evt =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        this.rml_await' evt f_k ctrl jp cd unit_value

    member this.rml_await_all' evt p =
      this.rml_await_all_match' evt (fun _ -> true) p

    member this.rml_await_all expr_evt p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        this.rml_await_all_match' evt (fun _ -> true) p f_k ctrl jp cd unit_value

    member this.rml_await_one' (evt:REvent<_,_>) p =
       fun f_k ctrl jp cd _ ->
         let await_eoi _ =
           let v = evt.one () in
           R.on_next_instant Strong ctrl (p v f_k ctrl jp cd)
         in
         on_event_at_eoi evt ctrl await_eoi

    member this.rml_await_one expr_evt p =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        this.rml_await_one' evt p f_k ctrl jp cd unit_value

    member this.rml_await_conf expr_cfg =
      fun f_k ctrl jp cd _ ->
        on_event_cfg_at_eoi (expr_cfg ()) ctrl (fun () -> R.on_next_instant Strong ctrl f_k)

    (**************************************)
    (* present                            *)
    (**************************************)

    member this.rml_present' evt p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          R.on_event_or_next evt f_1 cd ctrl f_2

    member this.rml_present expr_evt p_1 p_2 =
      fun f_k ctrl jp cd _ ->
        let evt = expr_evt () in
        this.rml_present' evt p_1 p_2 f_k ctrl jp cd unit_value

    (**************************************)
    (* present_conf                       *)
    (**************************************)

    member this.rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          R.on_event_cfg_or_next (expr_cfg ()) f_1 cd ctrl f_2

    (**************************************)
    (* seq                                *)
    (**************************************)

    member this.rml_seq p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_2 = p_2 f_k ctrl jp cd in
        let f_1 = p_1 (fun _ -> f_2 ()) ctrl None cd in
        f_1

    (**************************************)
    (* par                                *)
    (**************************************)
    (* Utilisation de Obj.magic pour le pb de la generalisation des *)
    (* applications partielles.                                     *)

    member this.join_n (cpt:#Join) =
      fun f_k ctrl jp cd _ ->
        if cpt.decr () then
          f_k unit_value

    member this.rml_ignore f_k _ =
      f_k ()

    member this.rml_par p_1 p_2 =
      fun f_k ctrl jp cd ->
        let i = match jp with None -> 2 | Some _ -> 1 in
        let cpt, j =
          match jp with
            | None ->
                let cpt = R.new_join_point 0 in
                cpt, this.join_n cpt f_k ctrl jp cd
            | Some cpt -> cpt, f_k
        in
        let f_1 = p_1 (this.rml_ignore j) ctrl (Some cpt) cd in
        let f_2 = p_2 (this.rml_ignore j) ctrl (Some cpt) cd in
        fun () ->
          cpt.incr i;
          R.on_current_instant cd f_2;
          f_1 unit_value

    (**************************************)
    (* loop                               *)
    (**************************************)
    (*
    let rec rml_loop p f_k ctrl _ =
    p (rml_loop p f_k ctrl) ctrl unit_value
    *)

    (*
    let rml_loop p =
    fun f_k ctrl ->
    let rec f_1 = lazy (p f ctrl)
    and f =
      fun _ ->
        Lazy.force f_1 unit_value
    in
    f
    *)

    member this.rml_loop p =
      fun _ ctrl jp cd ->
        let f_1 = ref dummy_step in
        let f_loop = p (fun _ -> !f_1 unit_value) ctrl None cd in
        f_1 := f_loop;
        f_loop

    (**************************************)
    (* loop_n                             *)
    (**************************************)

    member this.rml_loop_n e p =
      fun f_k ctrl jp cd ->
        let cpt = R.new_join_point 0 in
        let f_1 = ref dummy_step in
        let f_loop =
          p
            (fun _ ->
              if cpt.decr () then
                f_k unit_value
              else
                !f_1 unit_value)
            ctrl None cd
        in
        f_1 := f_loop;
        fun _ ->
          let n = e() in
          if n > 0 then
            (cpt.incr (n - 1);
             f_loop unit_value)
          else
            f_k unit_value


    (**************************************)
    (* signal                             *)
    (**************************************)


    member this._rml_signal f_k ctrl jp cd  kind ce re _default combine reset p =
      let ck = eval_clock_expr cd ce in
      let r = R.region_of_clock (eval_clock_expr cd re) in
      let reset =
        match reset with
          | None -> None
          | Some r -> Some (eval_clock_expr cd r)
      in
      let k evt = p evt f_k ctrl jp cd in
      R.new_evt cd ck r kind _default combine reset k ()

    member this.rml_signal_combine kind ce re _default combine reset p =
      fun f_k ctrl jp cd _ ->
        this._rml_signal f_k ctrl jp cd kind ce re (_default ()) (combine ()) reset p

    member this.rml_signal kind ce re reset p =
      fun f_k ctrl jp cd _ ->
        this._rml_signal f_k ctrl jp cd kind ce re default_default default_combine reset p

    (**************************************)
    (* def                                *)
    (**************************************)

    member this.rml_def e p =
      fun f_k ctrl jp cd _ ->
        p (e()) f_k ctrl jp cd unit_value

    (**************************************)
    (* def_dyn                            *)
    (**************************************)

    member this.rml_def_dyn p1 p2 =
      fun f_k ctrl jp cd ->
        p1 (fun v -> p2 v f_k ctrl None cd unit_value) ctrl jp cd

    (**************************************)
    (* def_and_dyn                        *)
    (**************************************)
    (*
    let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
        fun f_k ctrl jp cd ->
          fun x ->
            value_array.(i) <- x;
            decr cpt;
            if !cpt = 0 then
              let f = p3 value_array f_k ctrl jp cd in
              f unit_value
            else
              sched()
      in
      fun p_array p3 ->
        fun f_k ctrl jp cd ->
          let n = Array.length p_array in
          let cpt = ref n in
          let value_array = Array.make n (Obj.magic()) in
          let step_init =
            fun _ ->
              cpt := n;
              for i = 0 to n - 1 do
                let f =
                  p_array.(i)
                    (join_n cpt value_array p3 i f_k ctrl jp cd)
                    ctrl (Some cpt)
                in
                current := f :: !current
               done;
              sched()
          in step_init
    *)

    (**************************************)
    (* match                              *)
    (**************************************)

    member this.rml_match e p =
      fun f_k ctrl jp cd _ ->
        p (e()) f_k ctrl jp cd unit_value


    (**************************************)
    (* run                                *)
    (**************************************)

    member this.rml_run e =
      fun f_k ctrl jp cd _ ->
        (e ()) () f_k ctrl jp cd unit_value


    (**************************************)
    (* if                                 *)
    (**************************************)

    member this.rml_if e p_1 p_2 =
      fun f_k ctrl jp cd ->
        let f_1 = p_1 f_k ctrl jp cd in
        let f_2 = p_2 f_k ctrl jp cd in
        fun () ->
          if e() then
            f_1 unit_value
          else
            f_2 unit_value

    (**************************************)
    (* while                              *)
    (**************************************)

    member this.rml_while e p =
      fun f_k ctrl jp cd ->
        let f_body = ref dummy_step in
        let f_while _ =
          if e()
          then !f_body unit_value
          else f_k unit_value
        in
        f_body := p f_while ctrl None cd;
        f_while


    (**************************************)
    (* for                                *)
    (**************************************)

    member this.rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k ctrl jp cd ->
        let rec f_for i v2 _ =
          incr i;
          if cmp !i v2
          then p !i (f_for i v2) ctrl None cd unit_value
          else f_k unit_value
        in
        let f_for_init _ =
          let i = ref (e1()) in
          let v2 = e2() in
          if cmp !i v2
          then p !i (f_for i v2) ctrl None cd unit_value
          else f_k unit_value
        in
        f_for_init


    (**************************************)
    (* for_dopar                          *)
    (**************************************)

    member this.rml_fordopar e1 e2 dir p =
      fun f_k ctrl jp cd ->
        let cpt = R.new_join_point 0 in
        let j = this.join_n cpt f_k jp ctrl cd in
        let f_fordopar _ =
          if dir then
            begin
              let min = e1() in
              let max = e2() in
              let n = max - min + 1 in
              cpt.incr n;
              if n <= 0 then
                f_k unit_value
              else
                for i = max downto min do
                  let f = p i j ctrl (Some cpt) cd in
                  R.on_current_instant cd f
                done
            end
          else
            begin
              let max = e1() in
              let min = e2() in
              let n = max - min + 1 in
              cpt.incr n;
              if n <= 0 then
                f_k unit_value
              else
                for i = min to max do
                  let f = p i j ctrl (Some cpt) cd in
                  R.on_current_instant cd f
                done
            end
        in
        f_fordopar

    member this.rml_par_n p_list =
      fun f_k ctrl jp cd ->
        let nb = List.length p_list in
        let i = match jp with None -> nb | Some _ -> nb - 1 in
        let cpt, j =
          match jp with
            | None ->
                let cpt = R.new_join_point 0 in
                cpt, this.join_n cpt f_k ctrl jp cd
            | Some cpt -> cpt, f_k
        in
        let f_list = List.rev_map (fun p -> p j ctrl (Some cpt) cd) p_list in
        fun _ ->
          cpt.incr i;
          R.on_current_instant_list cd f_list

    member this.rml_seq_n =
      let rec fold p_list k ctrl jp cd =
        match p_list with
        | [] -> k
        | [ p ] -> p k ctrl jp cd
        | p::p_list -> p (fold p_list k ctrl jp cd) ctrl None cd
      in
      fun p_list ->
        fun f_k ctrl jp cd ->
          let f =
            (* List.fold_right (fun p -> fun k -> p k ctrl None) p_list f_k  *)
            fold p_list f_k ctrl jp cd
          in f



    (**************************************)
    (* until                              *)
    (**************************************)


    member this.rml_until expr_evt p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control (Kill f_k) body f_k ctrl cd in
        fun () ->
          let evt = expr_evt () in
          f evt true_cond ()

    member this.rml_until' evt p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        R.create_control (Kill f_k) body f_k ctrl cd evt true_cond

    member this.rml_until_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control_evt_conf (Kill f_k) body f_k ctrl cd in
        fun () ->
          let evt_cfg = expr_cfg () in
          f evt_cfg ()

    (**************************************)
    (* until handler                      *)
    (**************************************)

    member this.rml_until_handler_local expr_evt matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let evt = ref (Obj.magic() : REvent<_,_>) in
        let handler _ =
          let x = (!evt).value in
          p_handler x f_k ctrl jp cd
        in
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control (Kill_handler handler) body f_k ctrl cd in
        let cond =
          match matching_opt with
            | None -> true_cond
            | Some cond -> cond
        in
        fun () ->
          evt := expr_evt ();
          f !evt cond ()

    member this.rml_until_handler_local' (evt:REvent<_,_>) matching_opt p p_handler =
      fun f_k ctrl jp cd ->
        let handler _ =
          let x = evt.value in
          p_handler x f_k ctrl jp cd
        in
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control (Kill_handler handler) body f_k ctrl cd in
        let cond =
          match matching_opt with
            | None -> true_cond
            | Some cond -> cond
        in
        f evt cond

    member this.rml_until_handler expr_evt p p_handler =
      this.rml_until_handler_local expr_evt None p p_handler

    member this.rml_until_handler' evt p p_handler =
      this.rml_until_handler_local' evt None p p_handler

    member this.rml_until_handler_match expr_evt matching p p_handler =
      this.rml_until_handler_local expr_evt (Some matching) p p_handler

    member this.rml_until_handler_match' evt matching p p_handler =
      this.rml_until_handler_local' evt (Some matching) p p_handler


    (**************************************)
    (* control                            *)
    (**************************************)

    member this.step_control_static evt cond p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        R.create_control Susp body f_k ctrl cd evt cond

    member this.step_control expr_evt cond p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control Susp body f_k ctrl cd in
        fun () ->
          let evt = expr_evt () in
          f evt cond ()

    member this.rml_control' evt p = this.step_control_static evt true_cond p

    member this.rml_control expr_evt p = this.step_control expr_evt true_cond p

    member this.rml_control_match' evt matching p = this.step_control_static evt matching p

    member this.rml_control_match expr_evt matching p = this.step_control expr_evt matching p

    member this.rml_control_conf expr_cfg p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control_evt_conf Susp body f_k ctrl cd in
        fun () ->
          let evt_cfg = expr_cfg () in
          f evt_cfg ()

    (**************************************)
    (* when                               *)
    (**************************************)

    member this.rml_when expr_evt p =
      fun f_k ctrl jp cd ->
       let body f_k new_ctrl = p f_k new_ctrl None cd in
        let f = R.create_control When body f_k ctrl cd in
        fun () ->
          let evt = expr_evt () in
          f evt true_cond ()

    member this.rml_when' evt p =
      fun f_k ctrl jp cd ->
        let body f_k new_ctrl = p f_k new_ctrl None cd in
        R.create_control When body f_k ctrl cd evt true_cond

    member this.rml_when_conf expr_cfg =
      fun f_k ctrl ->
        fun _ -> raise Types.RML

    (**************************************)
    (* clock domain                       *)
    (**************************************)

    member this.rml_newclock sch period p =
      fun f_k ctrl jp cd ->
        R.new_clock_domain cd ctrl
          (fun cd ctrl f_k -> p (CkExpr cd.clock) f_k ctrl None cd) sch period f_k

    member this.rml_top_clock = (CkTop:Clock clock)
    member this.rml_base_clock = (CkLocal:Clock clock)

    (**************************************)
    (* rml_make                           *)
    (**************************************)

    member this.rml_make (cd:#ClockDomain<_>) result p =
     (* Function to create the last continuation of a toplevel process *)
      let jp, join_end =
        let term_cpt = R.new_join_point 0 in
        Some term_cpt,
        fun () ->
          term_cpt.incr 1;
          let f x =
            if term_cpt.decr () then
              result := Some x
          in f
      in
      p () (join_end()) cd.control_tree jp cd

    member this.rml_make_n (cd:#ClockDomain<_>) result pl =
      let jp, join_end =
        let term_cpt = R.new_join_point 0 in
        Some term_cpt,
        fun () ->
          term_cpt.incr (List.length pl);
          let f x =
            if term_cpt.decr () then
              result := Some x
          in f
      in
      List.map (fun p -> p () (join_end ()) cd.control_tree jp cd) pl

