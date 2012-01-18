type token

external is_valid : token -> bool = "is_valid_locality_token"
external get_token : unit -> token = "get_valid_locality_token"
