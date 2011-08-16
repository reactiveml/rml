open Compiler_options
open Compiler_utils
open Errors

(* back-end *)
let compile_implementation_back_end info_chan out_chan module_name rml_table =
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
  List.iter (Print_caml_src.output_impl_decl out_chan module_name) caml_table


let gen_main_fun out_chan =
  output_string out_chan ("module Rml_machine = Rml_machine.M(Interpreter);;\n");
  let main =
    try
      Modules.pfind_value_desc
        (Parse_ident.Pident !simulation_process)
    with Modules.Desc_not_found ->
      unbound_main !simulation_process
  in
  let main_id = Ident.name main.Global.gi.Global_ident.id in
  if not (Typing.is_unit_process (Global.info main)) then
    bad_type_main !simulation_process (Global.info main);
  match !number_of_instant >= 0, !sampling >= 0.0 with
    | true, true ->
      output_string out_chan
        ("let _ = Rml_machine.rml_exec_n_sampling "^
            main_id^" "^(string_of_int !number_of_instant)^" "^
            (string_of_float !sampling)^"\n")
    | true, false ->
      output_string out_chan
        ("let _ = Rml_machine.rml_exec_n "^
            main_id^" "^(string_of_int !number_of_instant)^"\n")
    | false, true ->
      output_string out_chan
        ("let _ = Rml_machine.rml_exec_sampling "^
            main_id^" "^(string_of_float !sampling)^"\n")
    | false, false ->
      output_string out_chan
        ("let _ = Rml_machine.rml_exec "^main_id^"\n")

let compile_impl info_chan filename module_name intermediate_code =
  let obj_name = filename ^ ".ml" in
  let out_chan = open_out obj_name in
  output_string out_chan
    ("(* THIS FILE IS GENERATED. *)\n"^
        "(* "^(Array.fold_right (fun s cmd -> s^" "^cmd) Sys.argv " ")^
        "*)\n\n");
        (* selection of the interpreter *)
  output_string out_chan ("module Interpreter = "^ !interpreter_module ^"."^ !interpreter_impl^"\n");
  output_string out_chan ("module Machine = Rml_machine.M(Interpreter)\n");

  (* the implementation *)
  compile_implementation_back_end info_chan out_chan module_name intermediate_code;

  (* main process *)
  if !simulation_process <> "" then
    gen_main_fun out_chan;
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
