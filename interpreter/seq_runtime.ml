
module SeqListRuntime (S : Runtime.STEP) (Event: Sig_env.S) =
struct
    module Step = S
    module Event = Event

    type 'a step = 'a S.t
    type next = unit step list ref
    type current = unit step list ref
    type waiting_list = unit step list ref

    exception RML

    type control_tree =
        { kind: control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (unit -> bool);
          mutable children: control_tree list;
          next: next;
          next_tmp : next;
          next_boi: next; }
    and control_type =
      | Clock_domain (*of clock_domain*)
      | Kill of unit step
      | Kill_handler of (unit -> unit step)
      | Susp
      | When of unit step ref

    and clock_domain =
        { cd_current : current;
          cd_pause_clock: bool ref; (* end of macro instant *)
          cd_eoi : bool ref; (* is it the eoi of this clock *)
          cd_weoi : waiting_list; (* processes waiting for eoi *)
          mutable cd_wake_up : waiting_list list;
          mutable cd_wake_up_next : next list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : Event.clock;
          mutable cd_top : control_tree;
        }

    type ('a, 'b) event = ('a,'b, clock_domain) Event.t * waiting_list * waiting_list

    let mk_current () = ref ([] : unit step list)
    let add_current p ctx =
      ctx.cd_current := p :: !(ctx.cd_current)
    let add_current_list pl ctx =
      ctx.cd_current := List.rev_append pl !(ctx.cd_current)
    let add_current_waiting_list w ctx =
      ctx.cd_current := List.rev_append !w !(ctx.cd_current);
      w := []
    let add_current_next next ctx =
      ctx.cd_current := List.rev_append !next !(ctx.cd_current);
      next := []
    let mk_waiting_list () = ref ([]:unit step list)
    let add_waiting p w =
      w := p :: ! w
    let mk_next () = ref ([]:unit step list)
    let add_next p next =
      next := p :: !next
      (*Format.eprintf "Adding to next: %d@." (List.length !next)*)
    let add_next_next n1 n2 =
      n2 := List.rev_append !n1 !n2
    let clear_next next =
      next := []


    let new_ctrl ?(cond = (fun () -> false)) kind =
      { kind = kind;
        alive = true;
        susp = false;
        children = [];
        cond = cond;
        next = mk_next ();
        next_tmp = mk_next ();
        next_boi = mk_next (); }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      clear_next p.next;
      clear_next p.next_boi;
      List.iter set_kill p.children;
      p.children <- []

    let is_toplevel ctrl = match ctrl.kind with
      | Clock_domain _ -> true
      | _ -> false

(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current cd =
      let rec eval pere p active =
        if p.alive then
          match p.kind with
            | Clock_domain _ -> true
            | Kill f_k ->
              if p.cond()
              then
                (add_next f_k pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active [];
                 if active then next_to_current cd p
                 else next_to_father pere p;
                 true)
          | Kill_handler handler ->
              if p.cond()
              then
                (add_next (handler()) pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active [];
                 if active then next_to_current cd p
                 else next_to_father pere p;
                 true)
          | Susp ->
              let pre_susp = p.susp in
              if p.cond() then p.susp <- not pre_susp;
              let active = active && not p.susp in
              if pre_susp
              then
                (if active then next_to_current cd p;
                 true)
              else
                (p.children <- eval_children p p.children active [];
                 if active then next_to_current cd p
                 else if not p.susp then next_to_father pere p;
                 true)
          | When f_when ->
              if p.susp
              then true
              else
                (p.susp <- true;
                 add_next !f_when pere.next;
                 p.children <- eval_children p p.children false [];
                 true)
        else
          (set_kill p;
           false)

      and eval_children p nodes active acc =
        match nodes with
        | [] -> acc
        | node :: nodes ->
            if eval p node active
            then eval_children p nodes active (node :: acc)
            else eval_children p nodes active acc

      and next_to_current ck node =
        (*Format.eprintf "Adding %d elets@." (List.length !(node.next));*)
        add_current_next node.next ck;
        add_current_next node.next_boi ck;
      and next_to_father pere node =
        add_next_next node.next pere.next;
        add_next_next node.next_boi pere.next_boi;
      in
        cd.cd_top.children <- eval_children cd.cd_top cd.cd_top.children true [];
        next_to_current cd cd.cd_top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current cd p =
      if p.alive && not p.susp then
        (add_current_next p.next cd;
         add_current_next p.next_tmp cd;
         List.iter (next_to_current cd) p.children;
         add_current_next p.next_boi cd)

    let is_active ctrl =
      ctrl.alive && not ctrl.susp

    let mk_clock_domain () =
      let cd = { cd_current = mk_current ();
                 cd_pause_clock = ref false;
                 cd_eoi = ref false;
                 cd_weoi = mk_waiting_list ();
                 cd_wake_up = [];
                 cd_wake_up_next =  [];
                 cd_clock = Event.init_clock ();
                 cd_top = new_ctrl Clock_domain; (* use a phony value*)
               }
      in
      (*cd.cd_top <- new_ctrl (Clock_domain cd);*) (*and set the correct value here*)
      cd

    let top_clock_domain = mk_clock_domain ()
    let is_eoi cd = !(cd.cd_eoi)
    let set_pauseclock cd =
      cd.cd_pause_clock := true
    let control_tree cd = cd.cd_top
    let add_weoi cd p =
      add_waiting p cd.cd_weoi
    let add_weoi_waiting_list cd w =
      cd.cd_wake_up <- w :: cd.cd_wake_up
    let add_weoi_next cd n =
      cd.cd_wake_up_next <- n :: cd.cd_wake_up_next

(* debloquer les processus en attent d'un evt *)
    let wake_up ck w =
      add_current_waiting_list w ck

    let wake_up_all ck =
      List.iter (fun wp -> add_current_waiting_list wp ck) ck.cd_wake_up;
      ck.cd_wake_up <- []

    let wake_up_all_next cd =
      List.iter (fun n -> add_current_next n cd) cd.cd_wake_up_next;
      cd.cd_wake_up_next <- []

(* creation d'evenements *)
    let new_evt_combine cd default combine =
      (Event.create cd cd.cd_clock default combine, mk_waiting_list (), mk_waiting_list ())

    let new_evt cd =
      new_evt_combine cd [] (fun x y -> x :: y)
(* ------------------------------------------------------------------------ *)

    let schedule cd =
      let ssched () =
      match !(cd.cd_current) with
      | f :: c ->
        cd.cd_current := c;
        Step.exec f
      | [] -> ()
      in
      (*Format.eprintf "Exec %d elts@." (List.length !(cd.cd_current));*)
      ssched ();
      while !(cd.cd_current) <> [] do
        ssched ();
      done

    let eoi cd =
      cd.cd_eoi := true;
      wake_up cd cd.cd_weoi;
      wake_up_all cd;
      wake_up_all_next cd;
      schedule cd

    let next_instant cd =
      eval_control_and_next_to_current cd;
      Event.next cd.cd_clock;
      cd.cd_eoi := false;
      cd.cd_pause_clock := false

    let rec has_next ctrl =
      if not ctrl.alive then
        false
      else
        match ctrl.kind with
          | Clock_domain -> has_next_children ctrl
          | Kill _ | Kill_handler _ ->
            ctrl.cond () || has_next_children ctrl
          | Susp ->
            let active = (ctrl.susp && ctrl.cond ()) || (not ctrl.susp && not (ctrl.cond ())) in
              active && has_next_children ctrl
          | When _ ->
            not ctrl.susp && has_next_children ctrl
    and has_next_children ctrl =
      !(ctrl.next) <> [] || List.exists has_next ctrl.children

    let macro_step_done cd =
      !(cd.cd_pause_clock) || not (has_next cd.cd_top)

    (* the react function *)
    let react cd =
      Format.printf "React top cd@.";
      schedule cd;
      Format.printf "Eoi cd@.";
      eoi cd;
      Format.printf "Next_instant cd@.";
      next_instant cd
end

module SimpleStep =
struct
  type 'a t = 'a -> unit

  let exec p =
    p ()
end

(*
module LkSeqI (Interpreter:Lk_interpreter.S) =
struct
  module R = SeqListRuntime(SimpleStep)(Sig_env.Record)
  module I = Interpreter(R)

  let rml_make p =
    let result = ref None in
    let step = I.rml_make result p in
    (*R.init ();*)
    R.add_step step;
    let react () =
      R.react ();
      !result
    in
    react
end
*)

(*module LkSeqInterpreter = LkSeqI(Lk_implem.Rml_interpreter)*)
