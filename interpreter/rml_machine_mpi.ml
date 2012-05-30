open Rml_machine
open Runtime_options

module type INTERPRETER =
  sig
    type 'a process
    type ('a, 'b) event

    module R : (sig
      type clock_domain
      type ('a, 'b) event
      type 'a step

      val mk_top_clock_domain : unit -> clock_domain
      val finalize_top_clock_domain : clock_domain -> unit
      val react : clock_domain -> unit
      val on_current_instant : clock_domain -> unit step -> unit

      val is_master : unit -> bool
      val start_slave : unit -> unit
    end)

    val rml_make: R.clock_domain -> 'a option ref -> 'a process -> unit R.step
    val rml_make_n: R.clock_domain -> 'a option ref -> 'a process list -> unit R.step list
  end

module Machine (I : INTERPRETER) =
struct
  module T = Rmltest_mpi.Test(Mpi_communication.Test)

  module MyInterpreter = struct
    type 'a process = 'a I.process

    let rml_make p =
      if I.R.is_master () then (
        let start_t = Unix.gettimeofday () in
        let result = ref None in
        let cd = I.R.mk_top_clock_domain () in
        let step = I.rml_make cd result p in
        I.R.on_current_instant cd step;
        let react () =
          I.R.react cd;
          !result
        in
        let finalize () =
          I.R.finalize_top_clock_domain cd;
          if !Runtime_options.bench_mode then
            let end_t = Unix.gettimeofday () in
            Format.printf "%f@." (end_t -. start_t)
        in
        react, finalize
      ) else
        (fun _ -> print_debug "Launching slave@."; I.R.start_slave (); None), (fun () -> ())

    let rml_make_test test_list =
      if I.R.is_master () then (
        let result = ref None in
        let cd = I.R.mk_top_clock_domain () in
        let mk_test (p, name, expected) =
          let act = T.new_test name expected in
          p act
        in
        let pl = List.map mk_test test_list in
        let steps = I.rml_make_n cd result pl in
        List.iter (fun step -> I.R.on_current_instant cd step) steps;
        let react () =
          print_debug "@.********************* Doing one step@.";
          I.R.react cd;
          T.next_step ();
          !result
        in
        let finalize () =
          T.end_test ();
          I.R.finalize_top_clock_domain cd
        in
        react, finalize
      ) else
        (fun _ -> I.R.start_slave (); None), (fun () -> ())

  end

  include M(MyInterpreter)
end

module Lco_ctrl_tree_mpi_interpreter =
  Lco_ctrl_tree_n.Rml_interpreter(Distributed_runtime.MpiRuntime)

module Lco_ctrl_tree_mpi_buffer_interpreter =
  Lco_ctrl_tree_n.Rml_interpreter(Distributed_runtime.MpiBufferedRuntime)

module Lco_ctrl_tree_mpi_c_interpreter =
  Lco_ctrl_tree_n.Rml_interpreter(Distributed_runtime.MpiCRuntime)
