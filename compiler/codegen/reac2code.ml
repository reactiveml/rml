open Compiler_options
open Compiler_utils
open Errors

(* back-end *)
let compile_implementation_back_end_buf info_chan module_name rml_table =
  let lco_compile_one_phrase impl =
    (* translation into lco code *)
    let lco_code = Reac2lco.translate_impl_item info_chan impl in
    (* producing caml code *)
    let caml_code = Lco2caml.translate_impl_item info_chan lco_code in
    (* Optimization *)
    let caml_code = silent_pass "Constant propagation"
      !const_optimization Constant_propagation.impl caml_code in

    caml_code
  in

  (* compilation of the whole table *)
  let caml_table = List.map lco_compile_one_phrase rml_table in

  (* emit the caml code *)
  List.map (Print_caml_src.output_impl_decl_string module_name) caml_table

let compile_implementation_back_end info_chan out_chan module_name rml_table =
  let strings = compile_implementation_back_end_buf info_chan module_name rml_table in
  List.iter (output_string out_chan) strings

let compile_implementation_rml_back_end info_chan out_chan module_name rml_table =
  let compile_one_phrase impl = match !Compiler_options.translation with
    | Rml_print -> impl
    | Rpml2Rml-> Rpml2rml.impl_item impl
    | _ -> assert false
  in
  (* compilation of the whole table *)
  let caml_table = List.map compile_one_phrase rml_table in
  (* emit the caml code *)
  List.iter (Print_reac.output_impl_decl out_chan module_name) caml_table


let gen_main_fun out_chan =
  let main =
    try
      Modules.pfind_value_desc
        (Parse_ident.Pident !simulation_process)
    with Modules.Desc_not_found ->
      unbound_main !simulation_process
  in
  let main_id = Ident.name main.Global.gi.Global_ident.id in
  if not (Typing.is_unit_process (Global.ty_info main)) then
    bad_type_main !simulation_process (Global.ty_info main);
  match !translation with
    | Lco_fsharp ->
      output_string out_chan
        ("let _ = Machine.rml_exec Interpreter.rml_make "^main_id^"\n")
    | _ ->
      output_string out_chan
        ("let _ = Machine.rml_exec "^main_id^"\n")

let gen_test_fun out_chan =
  output_string out_chan
    ("let _ = Machine.rml_test "^ !Compiler_options.test_name)

let compile_impl info_chan filename module_name intermediate_code =
  match !translation with
    | Lco ->
        let obj_name = filename ^ ".ml" in
        let out_chan = open_out obj_name in
        output_string out_chan
          ("(* THIS FILE IS GENERATED. *)\n"^
              "(* "^(Array.fold_right (fun s cmd -> s^" "^cmd) Sys.argv " ")^
              "*)\n\n");
        (* selection of the interpreter *)
        output_string out_chan ("open "^ !interpreter_impl ^";;\n");
        if !simulation_process <> "" then
          output_string out_chan ("module Machine = "^ !machine_module ^ "(Interpreter);;\n")
        else
          output_string out_chan "Interpreter.R.init ();;\n";

        (* the implementation *)
        compile_implementation_back_end info_chan out_chan module_name intermediate_code;

        (* main process *)
        if !Compiler_options.test_name <> "" then
          gen_test_fun out_chan
        else if !simulation_process <> "" then
          gen_main_fun out_chan;
        close_out out_chan

    | Lco_fsharp ->
        let obj_name = filename ^ ".ml" in
        let fs_name = filename ^ ".fs" in
        let out_chan = open_out obj_name in
        (* the implementation *)
        compile_implementation_back_end info_chan out_chan module_name intermediate_code;
        (* main process *)
        if !simulation_process <> "" then
          gen_main_fun out_chan;
        close_out out_chan;

        (* Format output for F# *)
        let cmd = "camlp4o pa_o.cmo pr_o.cmo -impl "^obj_name^" -o "^obj_name in
        ignore (Sys.command cmd);

        (* Prepend the beginning of the pile after Camlp4 because it is not valid OCaml *)
        let out_chan = open_out fs_name in
        output_string out_chan
          ("(* THIS FILE IS GENERATED. *)\n"^
              "(* "^(Array.fold_right (fun s cmd -> s^" "^cmd) Sys.argv " ")^
              "*)\n\n");
        (* selection of the interpreter *)
        output_string out_chan "#indent \"off\"\n";
        output_string out_chan "open Caml_compat\n";
        output_string out_chan ("let Interpreter = Machine."^ !interpreter_module ^"\n");
        output_string out_chan ("let Machine = Machine."^ !machine_module ^"\n");
        (* copy the rest of the file *)
        Compiler_utils.output_file out_chan obj_name;
        close_out out_chan;
        Sys.remove obj_name

    | Rml_print | Rpml2Rml ->
        let obj_name = filename ^ "_gen.rml" in
        let out_chan = open_out obj_name in
        output_string out_chan
          ("(* THIS FILE IS GENERATED. *)\n"^
              "(* "^(Array.fold_right (fun s cmd -> s^" "^cmd) Sys.argv " ")^
              "*)\n\n");

        compile_implementation_rml_back_end info_chan out_chan module_name intermediate_code;
        close_out out_chan

(* back-end for interface *)
let compile_interface_back_end info_chan out_chan module_name rml_table =
  let lco_compile_one_phrase intf =
    (* translation into lco code *)
    let lco_code = Reac2lco.translate_intf_item info_chan intf in
    (* producing caml code *)
    let caml_code = Lco2caml.translate_intf_item info_chan lco_code in
    caml_code
  in
  (* compilation of the whole table *)
  let caml_table = List.map lco_compile_one_phrase rml_table in
  (* emit the caml code *)
  List.iter (Print_caml_src.output_intf_decl out_chan module_name) caml_table



let compile_intf info_chan filename module_name intermediate_code =
  let obj_name = filename ^ ".mli" in
  let out_chan = open_out obj_name in
  (* selection of the interpreter *)
  output_string out_chan ("open "^ !interpreter_impl ^";;\n");

  (* the interface *)
  compile_interface_back_end info_chan out_chan module_name intermediate_code;

  close_out out_chan
