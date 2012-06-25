(***************************************************************************)
(*                                Lucy-n                                   *)
(*                                                                         *)
(*                                                                         *)
(* Authors: Louis Mandel (louis.mandel@lri.fr)                             *)
(*          Florence Plateau (florence.plateau@lri.fr)                     *)
(*                                                                         *)
(* Creation date: September 2011                                           *)
(*                                                                         *)
(***************************************************************************)

open Run_test_misc

(* Configuration *)

let files, default_tests, exts, verbose =
  let add_elt l v = l := v :: !l in
  let set_run_test_lbl s =
    Hashtbl.remove Run_test_lexer.tbl (!Run_test_lexer.run_test_lbl);
    Run_test_lexer.run_test_lbl := s;
    Hashtbl.add Run_test_lexer.tbl 
      (!Run_test_lexer.run_test_lbl) Run_test_lexer.Id_run_test;
  in
  let files = ref [] in
  let tests = ref [] in
  let add_good compiler = 
    add_elt tests (Good, compiler)
  in
  let add_bad compiler = 
    add_elt tests (Bad (None, ""), compiler)
  in
  let add_warning compiler = 
    add_elt tests (Warning "", compiler)
  in
  let exts = ref [".ls"] in
  let verbose = ref 0 in
  let spec =       
    [ "-test", Arg.String set_run_test_lbl,
      "lbl\t change the label of the test suite to execute (default "^ !Run_test_lexer.run_test_lbl ^")";
      "-good", Arg.String add_good,
      "\"compiler\"\tadd a compilation line that should succed";
      "-bad", Arg.String add_bad,
      "\"compiler\"\tadd a compilation line that should fail";
      "-warning", Arg.String add_warning,
      "\"compiler\"\tadd a compilation line that should produce a warning";
      "-ext", Arg.String (add_elt exts), 
      "\tadd an extension of files to test (default ["^ (String.concat " " !exts) ^"])"; 
      "-verbose", Arg.Set_int verbose, 
      "n\t set the verbosity level";
    ] 
  in
  let usage = 
    "Usage: "^Sys.argv.(0)^" files\n"^
    "Test the compilation of files. The descritption of the test is given in the files.\n"^
    "The syntax of the test descritption is: \n"^
    "  run_test: \n"^
    "   [ Good: \"compiler1\" \n"^
    "     Bad n: \"compiler2\" \n"^
    "     Bad n \"regexp\": \"compiler3\" \n"^
    "     Warning: \"compiler4\" \n"^
    "     Warning \"regexp\": \"compiler5\"  ] \n"^
    "Options:"
  in
  Arg.parse spec (add_elt files) usage; 
  List.rev !files, List.rev !tests, List.rev !exts, !verbose


(* Tests *)

let compile_file file log =
  let tests = 
    let chan = open_in file in
    let buf = Lexing.from_channel chan in
    Run_test_lexer.main [] buf
  in
  let tests = default_tests @ tests in
  if tests = [] then
    Format.eprintf "%s: no compiler@\n@?" file;
  List.fold_left
    (fun acc (kind, compiler) ->
      if List.exists (Filename.check_suffix file) exts then
        compile verbose kind compiler file exts :: acc
      else acc)
    log tests


let () =
  let logs =
    List.rev 
      (List.fold_left 
         (fun acc file -> compile_file file acc)
         [] files)
  in
  (* Results by kind of test *)
  let good_logs = 
    List.filter 
      (fun test -> match test.kind with Good -> true | _ -> false)
      logs
  in
  let bad_logs = 
    List.filter 
      (fun test -> match test.kind with Bad _ -> true | _ -> false)
      logs
  in
  let warning_logs = 
    List.filter 
      (fun test -> match test.kind with Warning _ -> true | _ -> false)
      logs
  in
  report_total ("Total Good: ") (total good_logs);
  report_total ("Total Bad: ") (total bad_logs);
  report_total ("Total Warning: ") (total warning_logs);

  (* Total results *)
  report_total "Total: " (total logs)
