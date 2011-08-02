
module type STEP =
sig
  type 'a t
  val exec : unit t -> unit
end


module type R =
sig
  module Step : STEP
  module Event : Sig_env.S

  type 'a step = 'a Step.t
  type current
  type waiting_list
  type next
  type ('a, 'b) event = ('a, 'b) Event.t * waiting_list * waiting_list

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
    | Clock_domain
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
          (* waiting lists to wake up at the end of the instant*)
        cd_clock : Event.clock;
        mutable cd_top : control_tree;
      }

  (* functions on the control tree *)
  val new_ctrl : ?cond: (unit -> bool) -> control_type -> control_tree
  val is_toplevel : control_tree -> bool
  val set_kill : control_tree -> unit
  val next_to_current : clock_domain -> control_tree -> unit

  (* functions on the current data structure *)
  val mk_current : unit -> current
  val add_current : unit step -> clock_domain -> unit
  val add_current_list : unit step list -> clock_domain -> unit
  (* Adds all elements of a waiting list or next to current and empty it. *)
  val add_current_waiting_list : waiting_list -> clock_domain -> unit
  val add_current_next : next -> clock_domain -> unit

  (* scheduling *)
  val react : clock_domain -> unit
  val schedule : clock_domain -> unit
  val eoi : clock_domain -> unit
  val macro_step_done : clock_domain -> bool

  (*functions on waiting list*)
  val mk_waiting_list : unit -> waiting_list
  val add_waiting : unit step -> waiting_list -> unit

  (* functions on next lists *)
  val mk_next : unit -> next
  val add_next : unit step -> next -> unit
  val add_next_next : next -> next -> unit
  val clear_next : next -> unit

  (* events *)
  val new_evt : clock_domain -> ('a, 'a list) event
  val new_evt_combine : clock_domain -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event

  (* various functions on the context *)
  val top_clock_domain : clock_domain
  val mk_clock_domain : unit -> clock_domain
  val is_eoi : clock_domain -> bool
  val set_pauseclock : clock_domain -> unit
  val add_weoi : clock_domain -> unit step -> unit
  val add_weoi_waiting_list : clock_domain -> waiting_list -> unit
end
