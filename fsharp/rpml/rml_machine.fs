#indent "off"

module Rml_machine

module LcoM = 
  begin
    module I = Lco_ctrl_tree_n.Lco_ctrl_tree_seq_interpreter
    module R = Seq_runtime.SeqRuntime

    let rml_make p =
      let result = ref None in
      let step = I.rml_make R.top_clock_domain result p in
      R.on_current_instant R.top_clock_domain step;
      let react () =
        R.react R.top_clock_domain;
        !result
      in
      react

    let rml_exec p =
      let react = rml_make p in
      let rec exec () =
        match react () with
        | None -> exec()
        | Some v -> v
      in exec ()

    let rml_exec_n p n =
      let react = rml_make p in
      let rec exec n =
        if n > 0 then (
          match react () with
          | None -> exec (n-1)
          | v -> v
        ) else
          None
      in exec n
(*
    let rml_exec_sampling p min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let react = rml_make p in
      let rec exec () =
        let _ = debut := Sys.time() in
        let v = react () in
        let _ =
          fin := Sys.time();
          diff := min -. (!fin -. !debut);
          if !diff > 0.001 then (
            ignore (Unix.setitimer
                      Unix.ITIMER_REAL
                      {Unix.it_interval = 0.0; Unix.it_value = !diff});
            Unix.pause())
          else ();
        in
        match v with
        | None -> exec ()
        | Some v -> v
      in exec ()


    let rml_exec_n_sampling p n min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let instant = ref 0 in
      let react = rml_make p in
      let rec exec n =
        if n > 0 then
          let _ =
            print_string ("************ Instant "^
                          (string_of_int !instant)^
                          " ************");
            print_newline();
            debut := Sys.time();
            incr instant
          in
          let _ = debut := Sys.time() in
          let v = react () in
          let _ =
            fin := Sys.time();
            diff := min -. (!fin -. !debut);
            if !diff > 0.001 then (
              ignore (Unix.setitimer
                        Unix.ITIMER_REAL
                        {Unix.it_interval = 0.0; Unix.it_value = !diff});
              Unix.pause())
            else
              (print_string "Instant ";
               print_int !instant;
               print_string " : depassement = ";
               print_float (-. !diff);
               print_newline());
          in
          match v with
          | None -> exec (n-1)
          | v -> v
        else
          None
      in exec n


    let rml_make_test pl =
      let result = ref None in
      let steps = I.rml_make_n I.R.top_clock_domain result pl in
      List.iter (fun step -> I.R.on_current_instant I.R.top_clock_domain step) steps;
      let react () =
        I.R.react I.R.top_clock_domain;
        Rmltest.step ();
        !result
      in
      react

    let rml_test test_list =
      let mk_test (p, name, expected) =
        let act, n = Rmltest.mk_checker name expected in
        p act
      in
      let pl = List.map mk_test test_list in
      let react = rml_make_test pl in
      let rec exec () =
        Format.printf "@.@.*******************  New step ********************@.@.";
        match react () with
        | None -> exec()
        | Some v -> Rmltest.end_program (); v
      in exec ()
      *)
  end