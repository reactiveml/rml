open Unix
open Filename
open Format
open Bench_version

let verbose = ref false
let no_run = ref false
let nb_mpi_procs = ref 4
let tests = ref []
let use_native_code = ref false

let seq_launcher p = p
let mpi_launcher balancer program_args p =
  mpiexec^" -n "^string_of_int !nb_mpi_procs^" "^p^" -load-balancer "^balancer^program_args
let mpi_one_launcher balancer p =
  mpiexec^" -n 1 "^p^" -load-balancer "^balancer

let rml_backends =
  [
    ("lco", seq_launcher, "Sequential (Lco)");
    ("lco_mpi", mpi_one_launcher "local", "MPI with local load balancer");
 (*   ("lco_mpi", mpi_launcher "user_local" "", "MPI with user annotations and local load balancer");
    ("lco_mpi", mpi_launcher "robin" "", "MPI with round robin load balancer"); *)
    ("lco_mpi", mpi_launcher "user_robin" "", "MPI with user annotations and round robin balancer");
  (* ("lco_mpi_c", mpi_launcher "user_robin" "", "MPI C with user annotations and round robin balancer"); *)
 (*   ("lco_mpi", mpi_launcher "user_robin" " -no-signals-remote ",
      "MPI with user annotations and round robin balancer and no signals remotes");
    ("lco_mpi", mpi_launcher "user_robin" " -no-local-slow-signals ",
      "MPI with user annotations and round robin balancer and no local slow signals");
    ("lco_mpi", mpi_launcher "user_robin" " -no-local-slow-signals -no-signals-remote ",
      "MPI with user annotations and round robin balancer and no local slow signals and no signals remotes"); *)

    ("lco_mpi_new", mpi_one_launcher "local", "MPI New with local load balancer");
    ("lco_mpi_new", mpi_launcher "user_robin" "", "MPI New with user annotations and round robin balancer");

  (*  ("lco_mpi_buffer", mpi_launcher "user_local" "", "MPI+buffering with user annotations and local load balancer");
    ("lco_mpi_buffer", mpi_launcher "robin" "", "MPI+buffering with round robin load balancer");
    ("lco_mpi_buffer", mpi_launcher "user_robin" "", "MPI+buffering with user annotations and round robin balancer") *)
  ]

let all_tests =
  [
    ("balance.rml", "");
    ("cd_creation.rml", "");
    ("cds.rml", "");
    ("cds_pause.rml", "");
    ("collision.rml", " -n 100 ");
    ("collision_nosig.rml", " -n 100 ");
    ("manual.rml", "");
    ("mapreduce.rml", "");
    ("parallel.rml", "");
    ("planets.rml", " -n 100 ");
    ("planets_adapt.rml", " -n 100 ");
    ("planets_fixed_step.rml", " -n 100 ");
    ("planets_pos.rml", " -n 100 ");
    ("planets_pos_sig.rml", " -n 100 ");
  ]


let add_test s =
  tests := s :: !tests

exception Command_failed

let print_status fmt =
  if !verbose then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt

let check_success p s =
  match s with
    | WEXITED 0 -> ()
    | WEXITED i -> Format.eprintf "Command '%s' exited with error code %d@." p i; raise Command_failed
    | WSIGNALED i -> Format.eprintf "Command '%s' was killed by signal %d@." p i; raise Command_failed
    | WSTOPPED i -> Format.eprintf "Command '%s' was stopped by signal %d@." p i; raise Command_failed

let run_prog p = check_success p (system (p^" >/dev/null 2>/dev/null"))

let mk_tags_file backend =
  let oc = open_out "_tags" in
  let ff = formatter_of_out_channel oc in
  fprintf ff "<*.byte>:thread@.";
  fprintf ff "<*.native>:thread@.";
  fprintf ff "<*.rml>: no_clocking, %s@." backend;
  close_out oc

let ocamlbuild args =
  run_prog (ocamlbuild^" "^args)

let run_test args (f, test_arg) =
  print_status "** Test: %s@." f;
  let bin = if !use_native_code then f^".native" else f^".byte" in
  let cmd = args ("./"^bin)^" -bench"^test_arg in
  if !no_run then (
    print_status "* Running '%s'@." cmd
  ) else (
    (*compile*)
    print_status "* Compiling %s@." bin;
    ocamlbuild bin;
    (* run *)
    try
      print_status "* Running '%s'@." cmd;
      let ic = open_process_in cmd in
      let t = float_of_string (input_line ic) in
      check_success cmd (close_process_in ic);
      Format.printf "%s: %f ms@." f (t *. 1000.)
    with
        _ -> Format.eprintf "Test '%s' failed.@." f; raise Command_failed
  )

let do_tests rml_files (backend, args, desc) =
  Format.printf "%s:@." desc;
  ocamlbuild "-clean";
  mk_tags_file backend;
  List.iter (run_test args) rml_files;
  print_status "All tests completed@.@."

let do_all_tests () =
  let rml_files =
    if !tests = [] then
      all_tests
    else
      List.filter (fun (f, _) -> List.mem f !tests) all_tests
  in
  print_status "Doing tests: %a@.@." (fun ff l -> List.iter (fun (s, _) -> fprintf ff "%s " s) l) rml_files;
  Sys.chdir "tests/";
  List.iter (do_tests rml_files) rml_backends

let list_tests () =
  let files = Array.to_list (Sys.readdir (Sys.getcwd ()^"/tests")) in
  let rml_files = List.filter (fun f -> Filename.check_suffix f ".rml") files in
  Format.printf "Available tests: %a@." (fun ff l -> List.iter (fun s -> fprintf ff "%s " s) l) rml_files;
  exit 0

let options =
  ["-v", Arg.Set verbose, " Verbose mode";
   "-no-run", Arg.Set no_run, "Don't run tests for real. Just show commands.";
   "-list", Arg.Unit list_tests, " List available tests";
   "-native", Arg.Set use_native_code, "Use native code";
   "-mpi-n", Arg.Set_int nb_mpi_procs, " Number of MPI processes to run"]
let usage_msg =
"Benchmark program.
  '"^Sys.executable_name^"' runs alls the tests in the tests/ dir.
  You can also specify tests as arguments: '"^Sys.executable_name^" test1.rml test2.rml'

Options are:"


let _ =
  Arg.parse options add_test usage_msg;
  try
    do_all_tests ()
  with
      Command_failed ->
        print_status "Error detected. Cleaning up@.";
        ocamlbuild "-clean";
        exit 2
