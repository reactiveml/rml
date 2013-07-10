open Compiler_options

(* the main function: parse the command line *)
let parse_cli () =
  begin try
    Arg.parse
      [ "-stdlib", Arg.String set_stdlib, doc_stdlib;
        "-v", Arg.Unit show_v, doc_v;
        "-version", Arg.Unit show_version, doc_version;
        "-where", Arg.Unit show_where, doc_where;
        "-c",Arg.Set no_link, doc_compilation;
        "-I",Arg.String add_include,doc_libraries;
        "-s", Arg.String set_simulation_process, doc_simulation;
        "-t", Arg.String set_test_name, doc_test_name;
        "-i", Arg.Unit set_verbose, doc_verbose;
        "-dtypes", Arg.Unit set_save_types, doc_save_types;
        "-no_loop_warning", Arg.Unit unset_instantaneous_loop_warning, doc_no_loop_warning;
        "-runtime", Arg.String set_runtime, doc_runtime;
        "-interactive", Arg.Unit set_interactive, doc_interactive;
        "-nopervasives", Arg.Unit set_no_pervasives, doc_no_pervasives;
        "-no_nary_opt", Arg.Unit set_no_nary, doc_no_nary;
        "-no_static_opt", Arg.Unit set_no_static, doc_no_static;
        "-no_for_opt", Arg.Unit set_no_for, doc_no_for;
        "-no_const_opt", Arg.Clear const_optimization, doc_no_const_opt;
        "-no-clocking", Arg.Set no_clocking, doc_no_clocking;
        "-row-clocking", Arg.Set use_row_clocking, doc_row_clocking;
        "-no-reactivity", Arg.Set no_reactivity, doc_no_reactivity;
        "-dreactivity", Arg.Set show_reactivity, doc_show_reactivity;
        "-dparse", Arg.Unit set_dparse, doc_dparse;
        "-dtime", Arg.Unit set_dtime, doc_dtime;
        "-dwarn_error", Arg.Set warning_are_errors, doc_warning_are_errors;
        "-dno_clock_effects", Arg.Set no_clock_effects, doc_no_clock_effects;
      ]
      add_to_compile
      errmsg;
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2
  end;
  to_compile := List.rev !to_compile

let _ = parse_cli ()
