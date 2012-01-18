(** Locality token

    [get_token] returns a valid token, that gets invalidated whenever it is marshalled.
    This should be associated with a mutable value that might be sent to another process,
    to make sure that the value is valid when it is accessed.
*)
type token

external is_valid : token -> bool = "is_valid_locality_token"
external get_token : unit -> token = "get_valid_locality_token"
