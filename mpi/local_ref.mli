(** A reference local to a process.

    Each process must call [init idx d] to store a value.
    Calls to [get idx] will then return the value stored in the calling thread,
    even if it was sent trough a marshalled closure.

    For now, only one value an be stored this way, so there is no associated type or value.
*)

external init : int -> 'a -> unit = "caml_mpi_init_local_ref"
external get : int -> 'a = "caml_mpi_get_local_ref"
