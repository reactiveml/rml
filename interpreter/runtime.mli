
module type STEP =
sig
  type 'a t
  (*val exec : unit t -> unit*)
end

module type SEQ_DATA_STRUCT = functor (Step:STEP) ->
sig
  type next
  type current
  type waiting_list

  exception Empty_current
  (* functions on the current data structure *)
  val mk_current : unit -> current
  val take_current : current -> unit Step.t
  val add_current : unit Step.t -> current -> unit
  val add_current_list : unit Step.t list -> current -> unit
  (* Adds all elements of a waiting list or next to current and empty it. *)
  val add_current_waiting_list : waiting_list -> current -> unit
  val add_current_next : next -> current -> unit

  val current_length : current -> int
  val waiting_length : waiting_list -> int

  (*functions on waiting list*)
  val mk_waiting_list : unit -> waiting_list
  val add_waiting : unit Step.t -> waiting_list -> unit
  val take_all : waiting_list -> unit Step.t list

  (* functions on next lists *)
  val mk_next : unit -> next
  val add_next : unit Step.t -> next -> unit
  val add_next_next : next -> next -> unit
  val clear_next : next -> unit
  val is_empty_next : next -> bool
end

type ('step, 'clock) control_type =
    | Clock_domain of 'clock
    | Kill of 'step
    | Kill_handler of (unit -> 'step)
    | Susp
    | When

module type CONTROL_TREE_R =
sig
  type 'a step

  type control_tree
  and clock_domain
  and clock
  type region

  type ('a, 'b) event
  type event_cfg
  module Event :
    (sig
      val new_evt : clock -> region -> ('a, 'a list) event
      val new_evt_combine : clock -> region -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) event

      val status: ?only_at_eoi:bool -> ('a, 'b) event -> bool
      val value: ('a, 'b) event -> 'b
      val one: ('a, 'a list) event -> 'a
      val pre_status: ('a, 'b) event -> bool
      val pre_value: ('a, 'b) event -> 'b
      val last: ('a, 'b) event -> 'b
      val default: ('a, 'b) event -> 'b
      val emit: ('a, 'b) event -> 'a -> unit

      val clock : ('a, 'b) event -> clock
      val region_of_clock : clock -> region

      val cfg_present : ('a, 'b) event -> event_cfg
      val cfg_or : event_cfg -> event_cfg -> event_cfg
      val cfg_and : event_cfg -> event_cfg -> event_cfg
      val cfg_status: ?only_at_eoi:bool -> event_cfg -> bool
     end)

  (* functions on the control tree *)
  (* let f = create_control kind (fun f_k new_ctrl -> body new_ctrl f_k) f_k ctrl s (fun v -> .) in]
     f ()

     OR for partial application:

     let f = create_control kind (fun f_k new_ctrl -> body new_ctrl f_k) f_k ctrl in
     fun () ->
       let s = e () in
       f s (fun v -> .) ()
  *)
  val create_control : (unit step, clock) control_type ->
    ('a step -> control_tree -> unit step) -> 'a step -> control_tree -> clock_domain ->
    ('b, 'c) event -> ('c -> bool) -> unit step

  val create_control_evt_conf : (unit step, clock) control_type ->
    ('a step -> control_tree -> unit step) -> 'a step -> control_tree -> clock_domain ->
    event_cfg -> unit step

  (* various functions on the clock domain *)
  val is_eoi : clock_domain -> bool
  val set_pauseclock : clock_domain -> clock -> unit
  val control_tree : clock_domain -> control_tree
  val clock : clock_domain -> clock
  val top_clock : unit -> clock
  val new_clock_domain : clock_domain -> control_tree ->
    (clock_domain -> control_tree -> unit step -> unit step) ->
    (int -> int * int) option -> unit step -> unit step

  (* step scheduling *)
  exception Wait_again

  (** [on_current_instant cd f] executes 'f ()' during the current step of [cd]. *)
  val on_current_instant : clock_domain -> unit step -> unit
  (** [on_current_instant_list cd fl] executes the list of step functions [fl]
      during the current step of [cd]. *)
  val on_current_instant_list : clock_domain -> unit step list -> unit
  (** [on_next_instant ctrl f] executes 'f ()' during the next activation of [ctrl]. *)
  val on_next_instant : control_tree -> unit step -> unit
  (** [on_eoi cd f v] executes 'f v' during the eoi of cd. *)
  val on_eoi : clock -> unit step -> unit

  (** [on_event evt ctrl f v] executes 'f v' if evt is emitted and
      ctrl is active in the same step.
      It waits for the next activation of w otherwise,
      or if the call raises Wait_again *)
  val on_event : ('a, 'b) event -> control_tree -> 'c step -> 'c -> unit
  (** [on_event_cfg evt_cfg ctrl f v] executes 'f v' if evt_cfg is true and
      ctrl is active in the same step.
      It waits for the next activation of evt_cfg otherwise,
      or if the call raises Wait_again *)
  val on_event_cfg : event_cfg -> control_tree -> 'a step -> 'a -> unit
  (** [on_event_at_eoi evt ctrl f] executes 'f ()' during the eoi
      (of evt's clock domain) if ctrl is active in the same step.
      Waits for the next activation of evt otherwise, or if the call
      raises Wait_again *)
  val on_event_at_eoi : ('a, 'b) event -> control_tree -> unit step -> unit
  (** [on_event_cfg_at_eoi evt ctrl f] executes 'f ()' during the eoi
      (of evt_cfg's clock domain) if ctrl is active in the same step.
      Waits for the next activation of evt otherwise. *)
  val on_event_cfg_at_eoi : event_cfg -> control_tree -> unit step -> unit
  (** [on_event_or_next evt f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  val on_event_or_next : ('a, 'b) event -> 'c step -> 'c ->
    clock_domain -> control_tree -> unit step -> unit
 (** [on_event_cfg_or_next evt_cfg f_w v_w cd ctrl f_next] executes 'f_w v_w' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  val on_event_cfg_or_next : event_cfg -> 'c step -> 'c ->
    clock_domain -> control_tree -> unit step -> unit

  (* scheduling *)
  val mk_top_clock_domain : unit -> clock_domain
  val react : clock_domain -> unit
  (** [step_clock_domain ctrl new_ctrl cd new_cd] creates the step
      function of the new clock domain [new_cd]. *)
(*  val step_clock_domain :
    control_tree -> control_tree -> clock_domain -> clock_domain -> unit -> unit *)


  (* Only for distributed runtimes *)
  val start_slave : unit -> unit
  val finalize_top_clock_domain : clock_domain -> unit
  val is_master : unit -> bool
end
