open Rmltest

module Test (C : Communication.T) = struct
  let next_step = -1
  let end_test = -2

  let checkers = ref []
  let counter = Counter.mk_counter ()

  let receive_act recv s =
    let is_done = ref false in
    while not !is_done do
      let r = recv () in
      if r = next_step then (
        ignore (step_stack s); Counter.decr counter
      ) else if r = end_test then (
        check_empty s; is_done := true
      ) else
          act s r
    done

  let new_test name b =
    let n = List.length b in
    let s = mk_stack name b n in
    let send, recv = C.fresh_channel () in
    let t = Thread.create (receive_act recv) s in
    ignore (step_stack s);
    checkers := (s, send, t) :: !checkers;
    send

  let end_test () =
    List.iter (fun (s, send, _) -> send end_test) !checkers;
    List.iter (fun (_, _, t) -> Thread.join t) !checkers

  let next_step () =
    Counter.set counter (List.length !checkers);
    Format.eprintf "Sending next step@.";
    List.iter (fun (_, send, _) -> send next_step) !checkers;
    Counter.await_zero counter;
    Format.eprintf "Checcking if tests failed@.";
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
