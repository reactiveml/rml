type msg_queue

external mk_queue : unit -> msg_queue = "mlmpi_mk_queue"
external start_receiving : msg_queue -> Mpi.tag -> unit = "mlmpi_start_receiving"
external stop_receiving : msg_queue -> unit = "mlmpi_stop_receiving"

external get : msg_queue -> 'a list = "mlmpi_queue_get"
