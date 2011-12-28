
module type S = sig
  type tag
  type msg
  type msg_queue
  type dispatcher

  type callback = msg -> unit
  type callback_kind = Once | Forever

  val mk_queue : unit -> msg_queue
  val dispatch_all : msg_queue -> dispatcher -> unit
  val await_new_msg : msg_queue -> unit

  val add_callback : ?kind:callback_kind -> tag -> callback -> dispatcher -> unit
  val remove_callback : tag -> dispatcher -> unit

  val mk_dispatcher : unit -> dispatcher
  val receive : msg_queue -> unit
end

module Make : functor (C: Communication.S) -> functor (T : Map.OrderedType) ->
  S with type msg = C.msg and type tag = T.t
