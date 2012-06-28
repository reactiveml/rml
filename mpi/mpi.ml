
(* Initialization *)
exception Error of string

let mpi_error s = raise(Error s)

external init : string array -> unit = "caml_mpi_init"
external is_initialized : unit -> bool = "caml_mpi_initialized"
external finalize : unit -> unit = "caml_mpi_finalize"

let _ =
  if not (is_initialized ()) then (
    Callback.register_exception "Mpi.Error" (Error "");
    init Sys.argv;
    at_exit finalize
  )

(* Ranks and tags*)
type rank = int
type tag = int

external get_any_tag : unit -> int = "caml_mpi_get_any_tag"
external get_any_source : unit -> int = "caml_mpi_get_any_source"

let any_tag = get_any_tag ()
let any_source = get_any_source ()

(* Sending and receiving messages *)

external send_basic: 'a -> Marshal.extern_flags list -> rank -> tag -> unit = "caml_mpi_send"

let send d dst tag =
  send_basic d [Marshal.Closures] dst tag

external send_int: int -> rank -> tag -> unit = "caml_mpi_send_int"

external receive: rank -> tag -> 'a = "caml_mpi_receive"

external receive_int : rank -> tag -> int = "caml_mpi_receive_int"
external receive_int_timeout : rank -> tag -> int -> int = "caml_mpi_receive_int_timeout"

(* Global state *)

external get_comm_world_size : unit -> int = "caml_mpi_get_comm_world_size"

let communicator_size = get_comm_world_size ()

external communicator_rank : unit -> rank = "caml_mpi_comm_rank"
