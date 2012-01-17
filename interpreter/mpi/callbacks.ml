
open Runtime_options

module type S = sig
  type tag
  type msg
  type msg_queue
  type dispatcher

  type callback = msg -> unit
  type callback_kind = Once | Forever

  val mk_queue : unit -> msg_queue
  val dispatch_all : msg_queue -> dispatcher -> unit
  val recv_given_msg : msg_queue -> tag -> msg
  val recv_n_given_msg : msg_queue -> tag -> int -> unit
  val await_new_msg : msg_queue -> unit

  val add_callback : ?kind:callback_kind -> tag -> callback -> dispatcher -> unit
  val remove_callback : tag -> dispatcher -> unit

  val mk_dispatcher : unit -> dispatcher
  val receive : msg_queue -> unit
  val stop_receiving : msg_queue -> unit
end

module Make (C : Communication.S) = struct
  module MyMap = Map.Make(struct
    type t = C.gid C.tag
    let compare = compare
  end)

  type tag = C.gid C.tag
  type msg = C.msg
  type msg_queue = {
    mutable q_alive : bool;
    q_mutex : Mutex.t;
    q_queue_filled : Condition.t;
    mutable q_queue : (tag * msg) list;
  }

  type callback = C.msg -> unit
  type callback_kind = Once | Forever
  type dispatcher = {
    mutable d_handlers : (callback * callback_kind) MyMap.t;
    d_mutex : Mutex.t;
  }

  let mk_dispatcher () = {
    d_handlers = MyMap.empty;
    d_mutex = Mutex.create ();
  }

  let add_callback ?(kind=Forever) tag f d =
    d.d_handlers <- MyMap.add tag (f, kind) d.d_handlers

  let remove_callback tag d =
    d.d_handlers <- MyMap.remove tag d.d_handlers

  let call_callback d tag s =
    try
      let f, kind = MyMap.find tag d.d_handlers in
        if kind = Once then
          d.d_handlers <- MyMap.remove tag d.d_handlers;
        print_debug "Calling callback for tag: %a@." C.print_tag tag;
        f s
    with
      | Not_found -> print_debug "%a: Received unexpected tag: %a@." C.print_here () C.print_tag tag

  let mk_queue () =
  { q_alive = true;
    q_mutex = Mutex.create ();
    q_queue = [];
    q_queue_filled = Condition.create () }

  let is_empty q =
    Mutex.lock q.q_mutex;
    let b = q.q_queue = [] in
    Mutex.unlock q.q_mutex;
    b

  let await_new_msg q =
    Mutex.lock q.q_mutex;
    if q.q_queue = [] then
      Condition.wait q.q_queue_filled q.q_mutex;
    Mutex.unlock q.q_mutex

  let dispatch_all q d =
    Mutex.lock q.q_mutex;
    let l = List.rev q.q_queue in
    q.q_queue <- [];
    Mutex.unlock q.q_mutex;
    List.iter (fun (tag, msg) -> call_callback d tag msg) l

  let recv_given_msg q tag =
    let rec aux () =
      if q.q_queue = [] then
        Condition.wait q.q_queue_filled q.q_mutex;
      print_debug "Looking for requested tag '%a' @." C.print_tag tag;
      let found, others = List.partition (fun (t,_) -> t = tag) q.q_queue in
      match found with
        | [] ->
            print_debug "Message not there yet@.";
            (* wait for a new msg *)
            Condition.wait q.q_queue_filled q.q_mutex;
            aux ()
        | (_, msg)::_ -> (* found the awaited message *)
            q.q_queue <- others;
            msg
    in
    Mutex.lock q.q_mutex;
    let msg = aux () in
    Mutex.unlock q.q_mutex;
    print_debug "Received the awaited message with tag '%a'@." C.print_tag tag;
    msg

  let recv_n_given_msg q tag n =
    let counter = ref n in
    let rec aux () =
      if q.q_queue = [] then
        Condition.wait q.q_queue_filled q.q_mutex;
      print_debug "Looking for requested tag '%a' @." C.print_tag tag;
      let found, others = List.partition (fun (t,_) -> t = tag) q.q_queue in
      q.q_queue <- others;
      counter := !counter - (List.length found);
      if !counter <> 0 then (
        Condition.wait q.q_queue_filled q.q_mutex;
        aux ()
      )
    in
    Mutex.lock q.q_mutex;
    aux ();
    Mutex.unlock q.q_mutex;
    print_debug "Received the n awaited messages with tag '%a'@." C.print_tag tag


  let stop_receiving q =
    q.q_alive <- false

  let receive q =
    while q.q_alive do
      let tag, msg = C.receive () in
      Mutex.lock q.q_mutex;
     (* let was_empty = q.q_queue = [] in*)
      q.q_queue <- (tag, msg) :: q.q_queue;
      Mutex.unlock q.q_mutex;
(*      if was_empty then *)
      Condition.signal q.q_queue_filled
    done
end
