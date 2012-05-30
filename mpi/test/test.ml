open Mpi

let msg_tag = 1
let nb_msgs = 100

let test_simple () =
  if Mpi.communicator_rank () = 0 then (
    Mpi.send "Bla" 1 msg_tag
  ) else (
    let s:string = Mpi.receive Mpi.any_source msg_tag in
    Format.printf "Received msg: %s@." s
  )

let test_mpi_queue () =
  if Mpi.communicator_rank () = 0 then (
    for i=1 to nb_msgs do
      Format.printf "Sending message@.";
      Mpi.send "Bla" 1 msg_tag
    done;
    Format.printf "Termintating sender@."
  ) else (
    let queue = Mpi_queue.mk_queue () in
    let msg_left = ref nb_msgs in
    Format.printf "Starting receiving thread@.";
    Mpi_queue.start_receiving queue msg_tag;
    while !msg_left > 0 do
      let l:string list = Mpi_queue.get queue in
      let n = List.length l in
      msg_left := !msg_left - n;
      Format.printf "Received %d messages, %d left@." n !msg_left
    done;
    Format.printf "All messages were received@.";
    Mpi_queue.stop_receiving queue;
    Format.printf "Exit done@.";
    (* Send a msg to self to exit the receiving thread *)
    Mpi.send "Bye" 1 msg_tag;
    Format.printf "Exit done@."
  )

let test_order () =
  if Mpi.communicator_rank () = 0 then (
    for i=1 to 10 do
      Format.printf "Sending message@.";
      Mpi.send i 1 msg_tag
    done;
    Format.printf "Termintating sender@."
  ) else (
    let queue = Mpi_queue.mk_queue () in
    let msg_left = ref 10 in
    let buff = ref [] in
    Format.printf "Starting receiving thread@.";
    Mpi_queue.start_receiving queue msg_tag;
    while !msg_left > 0 do
      let l:int list = Mpi_queue.get queue in
      let n = List.length l in
      buff := l @ !buff;
      msg_left := !msg_left - n;
      Format.printf "Received %d messages, %d left@." n !msg_left;
    done;
    Format.printf "All messages were received@.";
    List.iter print_int (List.rev !buff);
    Mpi_queue.stop_receiving queue;
    Format.printf "Exit done@.";
    (* Send a msg to self to exit the receiving thread *)
    Mpi.send "Bye" 1 msg_tag;
    Format.printf "Exit done@."
  )

let _ =
  test_order ()
