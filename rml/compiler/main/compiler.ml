(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : compiler.ml                                                *)
(*  Date de creation : 06/05/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from Lucid Synchron                                 *)
(*************************************************************************)

(* $Id$ *)

open Misc
open Errors

(* compiling a file. Two steps. *)
(* - Front-end: - static analysis
                - emission of the intermediate rml code *)
(* - Back-end: - generation of caml code for the whole file *)

(* [info_chan] is the channel where static informations are emitted *)
(* [out_chan] is the channel where the generated code is emitted *)

(* front-end *)
let compile_implementation_front_end info_chan itf impl_list =
  let rml_table = ref [] in
  let compile_one_phrase impl = 
	
    (* static analysis *)
    Static.static info_chan impl;

    (* Display the parse code *)
    if !dparse then
      begin
	Parse_printer.impl_item 0 
	  (Format.formatter_of_out_channel info_chan) impl;
      end;

    (* producing rml code (and openning of modules) *)
    let rml_code = Parse2reac.translate_impl_item info_chan impl in
    rml_table :=  rml_code :: !rml_table;

   (* typing *)
    Typing.type_impl_item info_chan rml_code;

  in
  
  (* compilation of the whole file *)
  List.iter compile_one_phrase impl_list;

  (* write interface *)
  Modules.write_compiled_interface itf;
  close_out itf;

  (* we return the rml code *)
  List.rev !rml_table


(* back-end *)
let compile_implementation_back_end info_chan out_chan module_name rml_table =
  let lk_table = ref [] in
  let lco_table = ref [] in
  let caml_table = ref [] in
  let lk_compile_one_phrase impl = 

    (* translation into lk code *)
    let lk_code = Reac2lk.translate_impl_item info_chan impl in
    lk_table := lk_code :: !lk_table;

    (* producing caml code *)
    let caml_code = Lk2caml.translate_impl_item info_chan lk_code in 
    caml_table := caml_code :: !caml_table;
  in

  let lco_compile_one_phrase impl = 

    (* translation into lco code *)
    let lco_code = Reac2lco.translate_impl_item info_chan impl in
    lco_table := lco_code :: !lco_table;

    (* producing caml code *)
    let caml_code = Lco2caml.translate_impl_item info_chan lco_code in 
    caml_table := caml_code :: !caml_table;
  in

  (* selection of the back-end *)
  let compile_one_phrase =
    match !translation with
    | Lk -> lk_compile_one_phrase
    | Lco -> lco_compile_one_phrase
  in

  (* compilation of the whole table *)
  List.iter compile_one_phrase rml_table;

  (* emit the caml code *)
  List.iter 
    (Print_caml_src.output_impl_decl out_chan module_name) 
    (List.rev !caml_table)


(* the main functions *)
let compile_implementation module_name filename =
  (* input and output files *)
  let source_name = filename ^ ".rml"
  and obj_interf_name = filename ^ ".rzi"
  and obj_name = filename ^ ".ml" 
  and annot_name = filename ^ ".rannot" 
  and module_name = String.capitalize filename  in

  let ic = open_in source_name in
  let itf = open_out_bin obj_interf_name in
  let info_chan = stdout in

  try
    
    (* load predefined base types *)
    Modules.start_compiling_interface module_name;
    Initialization.load_initial_modules ();
    
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source_name;
    
    (* parsing of the file *)
    let decl_list = Parse.implementation lexbuf in
    
    (* front-end *)
    let intermediate_code = compile_implementation_front_end info_chan itf 
	decl_list in

    (* back-end *)
    if not !no_link then
      begin
	let out_chan = open_out obj_name in
        (* selection of the interpreter *)
	output_string out_chan ("open "^ !interpreter_impl ^";;\n");

        (* the implementation *)
	compile_implementation_back_end info_chan out_chan 
	  module_name intermediate_code;

        (* main process *)
	if !simulation_process <> "" then
	  begin
	    let main =
	      try
		Modules.pfind_value_desc 
		  (Parse_ident.Pident !simulation_process)
	      with Modules.Desc_not_found ->
		unbound_main !simulation_process
	    in
	    let main_id = Ident.name main.Global.gi.Global_ident.id in
	    match !number_of_instant >= 0, !sampling >= 0.0 with
	    | true, true -> 
		output_string out_chan
		  ("let _ = "^interpreter_module^".rml_exec_n_sampling "^
		   main_id^" "^(string_of_int !number_of_instant)^" "^
		   (string_of_float !sampling)^"\n")
	    | true, false ->
		output_string out_chan
		  ("let _ = "^interpreter_module^".rml_exec_n "^
		   main_id^" "^(string_of_int !number_of_instant)^"\n")
	    | false, true ->
		output_string out_chan
		  ("let _ = "^interpreter_module^".rml_exec_sampling "^
		   main_id^" "^(string_of_float !sampling)^"\n")
	    | false, false ->
		output_string out_chan
		  ("let _ = "^interpreter_module^".rml_exec "^main_id^"\n")
	  end;
	close_out out_chan
      end;

   (* write types annotation *)
    Stypes.dump annot_name;

    close_in ic;
  with
    x -> 
      Stypes.dump annot_name;
      close_in ic;
      raise x


(* compiling an interface *)
(* front-end *)
let compile_interface_front_end info_chan itf intf_list =
  let rml_table = ref [] in
  let compile_one_phrase phr =
    (* producing rml code (and openning of modules) *)
    let rml_code = Parse2reac.translate_intf_item info_chan phr in
    rml_table := rml_code :: !rml_table;
    
    (* typing *)
    Typing.type_intf_item info_chan rml_code;
  in
  
  (* compilation of the whole file *)
  List.iter compile_one_phrase intf_list;

  (* write interface *)
  Modules.write_compiled_interface itf;
  close_out itf;

  (* we return the rml code *)
  List.rev !rml_table

(* back-end *)
let compile_interface_back_end info_chan out_chan module_name rml_table =
  let lk_table = ref [] in
  let lco_table = ref [] in
  let caml_table = ref [] in
  let lk_compile_one_phrase intf = 
    
    (* translation into lk code *)
    let lk_code = Reac2lk.translate_intf_item info_chan intf in
    lk_table := lk_code :: !lk_table;

    (* producing caml code *)
    let caml_code = Lk2caml.translate_intf_item info_chan lk_code in 
    caml_table := caml_code :: !caml_table;
  in

  let lco_compile_one_phrase intf = 

    (* translation into lco code *)
    let lco_code = Reac2lco.translate_intf_item info_chan intf in
    lco_table := lco_code :: !lco_table;

    (* producing caml code *)
    let caml_code = Lco2caml.translate_intf_item info_chan lco_code in 
    caml_table := caml_code :: !caml_table;
  in

  (* selection of the back-end *)
  let compile_one_phrase =
    match !translation with
    | Lk -> lk_compile_one_phrase
    | Lco -> lco_compile_one_phrase
  in

  (* compilation of the whole table *)
  List.iter compile_one_phrase rml_table;

  (* emit the caml code *)
  List.iter 
    (Print_caml_src.output_intf_decl out_chan module_name) 
    (List.rev !caml_table)


(* the main functions *)
let compile_interface parse module_name filename filename_end =
  (* input and output files *)
  let source_name = filename ^ filename_end
  and obj_interf_name = filename ^ ".rzi"
  and obj_name = filename ^ ".mli" 
  and module_name = String.capitalize filename  in

  let ic = open_in source_name in
  let itf = open_out_bin obj_interf_name in
  let info_chan = stdout in
  
  try
    
    (* load predefined base types *)
    Modules.start_compiling_interface module_name;
    Initialization.load_initial_modules ();
    
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source_name;
    
    (* parsing of the file *)
    let decl_list = parse lexbuf in
    
    (* front-end *)
    let intermediate_code = compile_interface_front_end info_chan itf 
	decl_list in

    (* back-end *)
    if (not !no_link) then
      begin
	let out_chan = open_out obj_name in
        (* selection of the interpreter *)
	output_string out_chan ("open "^ !interpreter_impl ^";;\n");

        (* the interface *)
	compile_interface_back_end info_chan out_chan 
	  module_name intermediate_code;

	close_out out_chan
      end;

    close_in ic;
  with
    x -> 
      close_in ic;
      raise x


(* compiling a scalar interface *)
let compile_scalar_interface module_name filename =
  no_link := true;
  compile_interface Parse.interface module_name filename ".mli"

(* compiling a Reactive ML interface *)
let compile_interface module_name filename = 
  compile_interface Parse.interface module_name filename ".rmli"
