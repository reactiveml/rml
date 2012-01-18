(** A reference local to a process.

    Each process must call [init d] to store a value.
    Calls to [get] will then return the value stored in the calling thread,
    even if it was sent trough a marshalled closure.

    For now, only one value an be stored this way, so there is no associated type or value.
*)

external init : 'a -> unit = "caml_mpi_init_local_ref"
external get : unit -> 'a = "caml_mpi_get_local_ref"
