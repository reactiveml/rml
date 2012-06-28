
(** Nodes in a given communicator
    are assigned integer ranks 0, 1, \ldots, $N-1$
    where $N$ is the size of the communicator. *)
type rank = int
type tag = int

val any_tag: tag
val any_source: rank

(** [send d dst tag] sends the data [d] with the tag [tag] to the node with rank [dst].
    It will return when the message has been successfully received.
    It does not block the OCaml runtime, so it can safely be used in a thread.
*)
val send: 'a -> rank -> tag -> unit

(** Same as [send] but specialized for integers. *)
val send_int: int -> rank -> tag -> unit

(** [receive src tag] receives a value with tag [tag] coming from [src].
    [any_tag] and [any_source] can be used to match any tag and any sender.
    The call will return when the message has been successfully received.
    It does not block the OCaml runtime, so it can safely be used in a thread.
*)
val receive: rank -> tag -> 'a

(** Same as [receive] but specialized for integers. *)
val receive_int: rank -> tag -> int
(** Same as [receive] but specialized for integers and with an additional
    argument that is a timeout in us. *)
val receive_int_timeout : rank -> tag -> int -> int


(** Number of nodes in the global communicator *)
val communicator_size : int
(** Returns the rank of the current node. *)
val communicator_rank : unit -> rank
