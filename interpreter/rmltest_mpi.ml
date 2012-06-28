open Rmltest
open Runtime_options

module Test (C : Communication.T) = struct
  let next_step = -1
  let end_test = -2

  let checkers = ref []
  let counter = Counter.mk_counter ()

  let receive_act recv recv_timeout s =
    let is_done = ref false in
    while not !is_done do
      (* si on recoit next_step et qu'il manque des messages, attendre un autre message avec un timeout *)
      let r = recv () in
      IFDEF RML_DEBUG THEN
        print_debug "Received action for '%s': %d@." s.s_name r
      ELSE () END;
      if r = next_step then (
        (* wait a little bit for other messages *)
        let rec aux () =
          let r = recv_timeout 100 in
          if r < 0 then ( (* no message received, really do next step *)
            ignore (step_stack s);
            Counter.decr counter
          ) else (
            IFDEF RML_DEBUG THEN
              print_debug "Received action after end_step for '%s': %d@." s.s_name r
            ELSE () END;
            act s r;
            aux ()
          )
        in
        aux ()
      ) else if r = end_test then (
        check_empty s; is_done := true
      ) else
          act s r
    done

  let new_test name b =
    let n = List.length b in
    let s = mk_stack name b n in
    let send, recv, recv_timeout = C.fresh_channel () in
    let send i =
      IFDEF RML_DEBUG THEN print_debug "Acting: %d@." i ELSE () END;
      send i
    in
    let t = Thread.create (receive_act recv recv_timeout) s in
    ignore (step_stack s);
    checkers := (s, send, t) :: !checkers;
    send

  let end_test () =
    List.iter (fun (s, send, _) -> send end_test) !checkers;
    List.iter (fun (_, _, t) -> Thread.join t) !checkers

  let next_step () =
    Counter.set counter (List.length !checkers);
    IFDEF RML_DEBUG THEN print_debug "Sending next step@." ELSE () END;
    List.iter (fun (_, send, _) -> send next_step) !checkers;
    Counter.await_zero counter;
    IFDEF RML_DEBUG THEN print_debug "Checcking if tests failed@." ELSE () END;
    if List.exists (fun (s, _, _) -> s.s_failed) !checkers then (
      if print_status then
        Format.eprintf "Test failed@.";
      raise (Test_failed "")
    );
    if List.for_all (fun (s, _, _) -> not s.s_active) !checkers then (
      if print_status then
        Format.eprintf "All tests OK@.";
      raise Test_success
    )
end
