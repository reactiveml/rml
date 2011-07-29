
module type Step =
sig
  type 'a t
  val exec : unit t -> unit
end


module type R =
sig
  module Event : Sig_env.S

  type 'a step = 'a -> unit
  type current
  type waiting_list
  type next
  type ('a, 'b) event = ('a, 'b) Event.t * waiting_list * waiting_list
  type context

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
    (*| Clock_domain of clock_domain*)
  (* functions on the control tree *)
  val new_ctrl : ?cond: (unit -> bool) -> control_type -> control_tree
  val set_kill : control_tree -> unit
  val next_to_current : context -> control_tree -> unit

  (* functions on the current data structure *)
  val mk_current : unit -> current
  val add_current : unit step -> context -> unit
  val add_current_list : unit step list -> context -> unit
  (* Adds all elements of a waiting list or next to current and empty it. *)
  val add_current_waiting_list : waiting_list -> context -> unit
  val add_current_next : next -> context -> unit

  (* scheduling *)
  val add_step : unit step -> unit
  val react : unit -> unit
  val schedule : current -> unit

  (*functions on waiting list*)
  val mk_waiting_list : unit -> waiting_list
  val add_waiting : unit step -> waiting_list -> unit

  (* functions on next lists *)
  val mk_next : unit -> next
  val add_next : unit step -> next -> unit
  val add_next_next : next -> next -> unit
  val clear_next : next -> unit

  (* events *)
  val new_evt : unit -> ('a, 'a list) event
  val new_evt_combine : 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event

  (* various functions on the context *)
  val mk_context : unit -> context
  val is_eoi : context -> bool
  val add_weoi : context -> unit step -> unit
  val add_weoi_waiting_list : context -> waiting_list -> unit

  val main_context : context
  val top : control_tree
end
