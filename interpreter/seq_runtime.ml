
module SeqListRuntime (Event: Sig_env.S) : Runtime.R =
struct
    module Event = Event

    type 'a step = 'a -> unit
    type next = unit step list ref
    type current = unit step list ref
    type waiting_list = unit step list ref
    type ('a, 'b) event = ('a,'b) Event.t * waiting_list * waiting_list

    exception RML

    type control_tree =
        { kind: control_type;
          mutable alive: bool;
          mutable susp: bool;
          mutable cond: (unit -> bool);
          mutable children: control_tree list;
          next: next;
          next_boi: next; }
    and control_type =
        Top
      | Kill of unit step
      | Kill_handler of (unit -> unit step)
      | Susp
      | When of unit step ref
      (*| Clock_domain of clock_domain *)

    and clock_domain =
        { cd_current : current;
          cd_pause_clock: bool ref; (* end of macro instant *)
          cd_eoi : bool ref; (* is it the eoi of this clock *)
          cd_weoi : waiting_list; (* processes waiting for eoi *)
          mutable cd_wake_up : waiting_list list;
          (* waiting lists to wake up at the end of the instant*)
          cd_clock : Event.clock;
        }

    type context = clock_domain

    let rec rev_app x1 x2 =
      match x1 with
      | [] -> x2
      | f :: x1' -> rev_app x1' (f::x2)

    let mk_current () = ref ([] : unit step list)
    let add_current p ctx =
      ctx.cd_current := p :: !(ctx.cd_current)
    let add_current_list pl ctx =
      ctx.cd_current := rev_app pl !(ctx.cd_current)
    let add_current_waiting_list w ctx =
      ctx.cd_current := rev_app !w !(ctx.cd_current);
      w := []
    let add_current_next next ctx =
      ctx.cd_current := rev_app !next !(ctx.cd_current);
      next := []
    let mk_waiting_list () = ref ([]:unit step list)
    let add_waiting p w =
      w := p :: ! w
    let mk_next () = ref ([]:unit step list)
    let add_next p next =
      next := p :: !next
    let add_next_next n1 n2 =
      n2 := rev_app !n1 !n2
    let clear_next next =
      next := []

    let mk_context () =
      { cd_current = mk_current ();
        cd_pause_clock = ref false;
        cd_eoi = ref false;
        cd_weoi = mk_waiting_list ();
        cd_wake_up = [];
        cd_clock = Event.init_clock ();
      }
    let main_context = mk_context ()
    let is_eoi context = !(context.cd_eoi)
    let add_weoi context p =
      add_waiting p context.cd_weoi
    let add_weoi_waiting_list context w =
      context.cd_wake_up <- w :: context.cd_wake_up

    let wake_up_all ck =
      List.iter (fun wp -> add_current_waiting_list wp ck) ck.cd_wake_up;
      ck.cd_wake_up <- []

    (* racine de l'arbre de control *)
    let top =
      { kind = Top;
        alive = true;
        susp = false;
        children = [];
        cond = (fun () -> false);
        next = mk_next ();
        next_boi = mk_next (); }

    (* tuer un arbre p *)
    let rec set_kill p =
      p.alive <- true;
      p.susp <- false;
      clear_next p.next;
      clear_next p.next_boi;
      List.iter set_kill p.children;
      p.children <- []


(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current =
      let rec eval pere p active =
        if p.alive then
          match p.kind with
          | Top -> raise RML
          | Kill f_k ->
              if p.cond()
              then
                (add_next f_k pere.next;
                 set_kill p;
                 false)
              else
                (p.children <- eval_children p p.children active [];
                 if active then next_to_current main_context p
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
                 if active then next_to_current main_context p
                 else next_to_father pere p;
                 true)
          | Susp ->
              let pre_susp = p.susp in
              if p.cond() then p.susp <- not pre_susp;
              let active = active && not p.susp in
              if pre_susp
              then
                (if active then next_to_current main_context p;
                 true)
              else
                (p.children <- eval_children p p.children active [];
                 if active then next_to_current main_context p
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
        add_current_next node.next ck;
        add_current_next node.next_boi ck;
      and next_to_father pere node =
        add_next_next node.next pere.next;
        add_next_next node.next_boi pere.next_boi;
      in
      fun () ->
        top.children <- eval_children top top.children true [];
        next_to_current main_context top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current ck p =
      if p.alive && not p.susp then
        (add_current_next p.next ck;
         List.iter (next_to_current main_context) p.children;
         add_current_next p.next_boi ck)

(* debloquer les processus en attent d'un evt *)
    let wake_up ck w =
      add_current_waiting_list w ck


(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create main_context.cd_clock default combine, mk_waiting_list (), mk_waiting_list ())

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)
(* ------------------------------------------------------------------------ *)

    let add_step p =
      add_current p main_context

    let schedule current =
      let ssched () =
      match !current with
      | f :: c ->
        current := c;
        f ()
      | [] -> ()
      in
      ssched ();
      while !current <> [] do
        ssched ();
      done


    (* the react function *)
    let react () =
      schedule main_context.cd_current;
      main_context.cd_eoi := true;
      wake_up main_context main_context.cd_weoi;
      wake_up_all main_context;
      schedule main_context.cd_current;
      eval_control_and_next_to_current ();
      Event.next main_context.cd_clock;
      main_context.cd_eoi := false
end

(*
module SimpleStep =
struct
  type 'a t = 'a -> unit

  let exec p =
    p ()
end
  *)

module SeqInterpreter (Interpreter:Lco_interpreter.S) =
struct
  module R = SeqListRuntime(Sig_env.Record)
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

module LcoSeqInterpreter = SeqInterpreter(Lco_ctrl_tree_n.Rml_interpreter)
