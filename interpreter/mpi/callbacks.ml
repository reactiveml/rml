
module type S = sig
  type tag
  type msg
  type msg_queue

  type dispatcher

  type callback = msg -> unit

  val add_handler : tag -> callback -> dispatcher -> unit
  val mk_dispatcher : unit -> dispatcher
  val dispatch : dispatcher -> unit

  val is_empty : msg_queue -> bool
  val dispatch_all : msg_queue -> msg list
end

module Make (C : Communication.S) (T : Map.OrderedType) = struct
  module MyMap = Map.Make(struct
    type t = T.t
    let compare = compare
  end)

  type tag = T.t
  type msg = C.msg
  type msg_queue = {
    q_mutex : Mutex.t;
    q_queue_filled : Condition.t;
    mutable q_queue : msg list;
  }

  type callback = C.msg -> unit
  type dispatcher = {
    mutable d_handlers : callback MyMap.t;
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
      let f, kind = MyMap.find k d.d_handlers in
        if kind = Once then
          d.d_handlersMyMap.remove k d.d_handlers;
        f s
    with
      | Not_found -> Format.eprintf "Received unexpected key '%a' @." C.print k

  let mk_queue () =
  { q_mutex = Mutex.create ();
    q_queue = [];
    q_queue_filled = Condition.create () }

  let is_empty q =
    Mutex.lock q.q_mutex;
    let b = q.q_queue = [] in
    Mutex.unlock q.q_mutex;
    b

  let await_new_msg q =
    Mutex.lock q.q_mutex;
    if q.q_queue <> [] then
      Condition.wait q.q_queue_filled;
    Mutex.unlock q.q_mutex

  let dispatch_all q d =
    Mutex.lock q.q_mutex;
    let l = q.q_queue in
    q.q_queue <- [];
    Mutex.unlock q.q_mutex;
    List.iter (fun (tag, msg) -> call_callback d tag msg) l

  let rec receive queue =
    let tag, msg = Communication.receive () in
    Mutex.lock queue.q_mutex;
    q.q_queue <- (tag, msg) :: q.q_queue;
    Mutex.unlock queue.q_mutex;
    receive queue
end
