module Runtime
    
exception Wait_again

module Step =
  type 'a t = 'a -> unit 
  
type control_type<'ck> =
    | Clock_domain of 'ck
    | Kill of unit Step.t
    | Kill_handler of (unit -> unit Step.t)
    | Susp
    | When

type Next =
  abstract member Add : unit Step.t -> unit
  abstract member AddNext : Next -> unit
  abstract member Clear : unit -> unit
  abstract member IsEmpty : bool

type WaitingList =
  abstract member Add : unit Step.t -> unit
  abstract member TakeAll : unit -> unit Step.t list
  abstract member Length : int

type Current =
  abstract member Take : unit -> (unit Step.t) option
  abstract member Add : unit Step.t -> unit
  abstract member AddList : unit Step.t list -> unit
  (* Adds all elements of a waiting list or next to current and empty it. *)
  abstract member AddWaitingList : WaitingList -> unit
  abstract member AddNext : Next -> unit
  abstract member Length : int

type SeqDataStruct =
  abstract member mk_current : unit -> Current
  abstract member mk_next : unit -> Next
  abstract member mk_waiting_list : unit -> WaitingList

type REvent<'a, 'b, 'ck> =
  abstract member status : bool -> bool
  abstract member value: 'b
  abstract member one: unit -> 'a
  abstract member pre_status: bool
  abstract member pre_value: 'b
  abstract member last: 'b
  abstract member _default: 'b
  abstract member emit: 'a -> unit
  abstract member clock : 'ck

type REventCfg =
  abstract member cfg_status: bool -> bool

type EventFactory<'cd, 'ck, 'region> =
  abstract member new_evt_global: Types.signal_kind -> 'b -> ('a -> 'b -> 'b) -> REvent<'a, 'b, 'ck>
  abstract member new_evt : 'cd -> 'ck -> 'region ->
        Types.signal_kind -> 'b -> ('a -> 'b -> 'b) -> 'ck option ->
        (REvent<'a, 'b, 'ck> -> unit Step.t) -> unit Step.t
  abstract member cfg_present : #REvent<'a, 'b, 'ck> -> REventCfg
  abstract member cfg_or : #REventCfg -> #REventCfg -> REventCfg
  abstract member cfg_and : #REventCfg -> #REventCfg -> REventCfg
  abstract member region_of_clock : 'ck -> 'region

type Join =
  abstract member incr : int -> unit
  (* Returns whether the join point counter has reached zero *)
  abstract member decr : unit -> bool

type JoinFactory =
  abstract member new_join_point : int -> Join


type ClockDomain<'ck, 'ctrl> =
  abstract member is_eoi : unit -> bool
  abstract member control_tree : 'ctrl
  abstract member clock : 'ck


type Runtime<'ck, 'ctrl> =
  inherit EventFactory<ClockDomain<'ck, 'ctrl>, 'ck, 'ck>
  inherit JoinFactory
  
  abstract member create_control : control_type<'ck> ->
    ('a Step.t -> 'ctrl -> unit Step.t) -> 'a Step.t -> 'ctrl -> ClockDomain<'ck, 'ctrl> ->
    (#REvent<'b, 'c, 'ck> -> ('c -> bool) -> unit Step.t)
  abstract member create_control_evt_conf : control_type<'ck> ->
    ('a Step.t -> 'ctrl -> unit Step.t) -> 'a Step.t -> 'ctrl -> ClockDomain<'ck, 'ctrl> ->
    (#REventCfg -> unit Step.t)
    
  (** [on_current_instant cd f] executes 'f ()' during the current step of [cd]. *)
  abstract member on_current_instant : ClockDomain<'ck, 'ctrl> -> unit Step.t -> unit
  (** [on_current_instant_list cd fl] executes the list of step functions [fl]
      during the current step of [cd]. *)
  abstract member on_current_instant_list : ClockDomain<'ck, 'ctrl> -> unit Step.t list -> unit
  (** [on_next_instant ctrl f] executes 'f ()' during the next activation of [ctrl]. *)
  abstract member on_next_instant : Types.pause_kind -> 'ctrl -> unit Step.t -> unit
  (** [on_eoi cd f v] executes 'f v' during the eoi of cd. *)
  abstract member on_eoi : 'ck -> unit Step.t -> unit  
  (** [on_event evt ctrl f] executes 'f ()' if evt is emitted and
      ctrl is active in the same step.
      It waits for the next activation of w otherwise,
      or if the call raises Wait_again *)
  abstract member on_event : #REvent<'a, 'b, 'ck> -> 'ctrl -> unit Step.t -> unit
  (** [on_event_cfg evt_cfg ctrl f] executes 'f ()' if evt_cfg is true and
      ctrl is active in the same step.
      It waits for the next activation of evt_cfg otherwise,
      or if the call raises Wait_again *)
  abstract member on_event_cfg : #REventCfg -> 'ctrl -> unit Step.t -> unit
  (** [on_event_or_next evt f_w cd ctrl f_next] executes 'f_w ()' if
      evt is emitted before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  abstract member on_event_or_next : #REvent<'a, 'b, 'ck> -> unit Step.t ->
    ClockDomain<'ck, 'ctrl> -> 'ctrl -> unit Step.t -> unit
 (** [on_event_cfg_or_next evt_cfg f_w cd ctrl f_next] executes 'f_w ()' if
      evt_cfg is true before the end of instant of cd.
      Otherwise, executes 'f_next ()' during the next instant. *)
  abstract member on_event_cfg_or_next : #REventCfg -> unit Step.t ->
    ClockDomain<'ck, 'ctrl> -> 'ctrl -> unit Step.t -> unit
    
  abstract member new_clock_domain : ClockDomain<'ck, 'ctrl> -> 'ctrl ->
    (ClockDomain<'ck, 'ctrl> -> 'ctrl -> unit Step.t -> unit Step.t) ->
    (int -> int * int) option -> int option -> unit Step.t -> unit Step.t    
  abstract member react : ClockDomain<'ck, 'ctrl> -> unit
    
  abstract member init : unit -> unit
  abstract member get_top_clock_domain : unit -> ClockDomain<'ck, 'ctrl> 
  abstract member top_clock : unit -> 'ck
  abstract member finalize_top_clock_domain : ClockDomain<'ck, 'ctrl> -> unit
 
   