open Unix
open Filename
open Format

let verbose = ref false
let nb_mpi_procs = ref 4

exception Command_failed

let print_status fmt =
  if !verbose then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt


let ocamlbuild = "ocamlbuild"
let rpmlc = "../../compiler/rpmlc.byte"

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
  fprintf ff "<*.rml>:%s@." backend;
  close_out oc

let ocamlbuild args =
  run_prog ("ocamlbuild "^args)

let run_test args f =
  print_status "** Test: %s@." f;
  let bin = f^".byte" in
  (*compile*)
  print_status "* Compiling %s@." bin;
  ocamlbuild bin;
  (* run *)
  try
    let cmd = args ("./"^bin)^" -bench" in
    print_status "* Running '%s'@." cmd;
    let ic = open_process_in cmd in
    let t = float_of_string (input_line ic) in
    check_success cmd (close_process_in ic);
    Format.printf "%s: %f ms@." f (t *. 1000.)
  with
      _ -> Format.eprintf "Test '%s' failed.@." f; raise Command_failed

let do_tests (backend, args, desc) =
  Format.printf "%s:@." desc;
  let files = Array.to_list (Sys.readdir (Sys.getcwd ())) in
  let rml_files = List.filter (fun f -> Filename.check_suffix f ".rml") files in
  print_status "Found files: %a@.@." (fun ff l -> List.iter (fun s -> fprintf ff "%s " s) l) rml_files;
  ocamlbuild "-clean";
  mk_tags_file backend;
  List.iter (run_test args) rml_files;
  print_status "All tests completed@.@."

let seq_launcher p = p
let mpi_launcher balancer p =
  "mpiexec -n "^string_of_int !nb_mpi_procs^" "^p^" -load-balancer "^balancer

let rml_backends =
  [
    ("lco", seq_launcher, "Sequential");
    ("lco_mpi", mpi_launcher "local", "MPI with local load balancer");
    ("lco_mpi", mpi_launcher "robin", "MPI with round robin load balancer");
    ("lco_mpi", mpi_launcher "all_remote", "MPI with always remote load balancer");
    ("lco_mpi_buffer", mpi_launcher "local", "MPI+buffering with local load balancer");
    ("lco_mpi_buffer", mpi_launcher "robin", "MPI+buffering with round robin load balancer");
    ("lco_mpi_buffer", mpi_launcher "all_remote", "MPI+buffering with always remote load balancer")
  ]

let options =
  ["-v", Arg.Set verbose, " Verbose mode";
   "-mpi-n", Arg.Set_int nb_mpi_procs, " Number of MPI processes to run"]
let err_msg =
  "Wrong arguments. Expected:"

let _ =
  Arg.parse options ignore err_msg;
  try
    Sys.chdir "tests/";
    List.iter do_tests rml_backends
  with
      Command_failed ->
        print_status "Error detected. Cleaning up@.";
        (*ocamlbuild "-clean";*)
        exit 2
