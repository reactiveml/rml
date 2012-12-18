(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the Q Public License  *)
(*  version 1.0.                                                      *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* file: types_printer.ml *)

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* created: 2004-05-25  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Printing a type expression *)

open Format
open Misc
open Compiler_options
open Clocks
open Asttypes
open Ident
open Modules
open Global_ident
open Global

let _print_list f priority sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [ty] ->
	      f priority ty
    | ty::rest ->
	      f priority ty;
	      print_space ();
	      print_string sep;
	      print_space ();
	      printrec rest
  in
  printrec l

(* the long name of an ident is printed *)
(* if it is different from the current module *)
(* or if it is from the standard module *)
let print_qualified_ident q =
  if (compiled_module_name () <> q.qual) &
    (pervasives_module <> q.qual) &
    (!interpreter_module <> q.qual)
  then begin print_string q.qual;print_string "." end;
  print_string (Ident.name q.id)

(* type variables are printed 'a, 'b,... *)
let names ={
  k_clock = new name_assoc_table int_to_alpha;
  k_carrier = new name_assoc_table (fun i -> "c"^string_of_int i);
  k_carrier_row = new name_assoc_table (fun i -> "cr"^string_of_int i);
  k_effect = new name_assoc_table (fun i -> "e"^string_of_int i);
  k_effect_row = new name_assoc_table (fun i -> "er"^string_of_int i);
  k_react = new name_assoc_table (fun i -> "r"^string_of_int i)
}
let skolem_name = new name_assoc_table (fun i -> "c"^string_of_int i)


let print_skolem_name (n, i) =
  print_string "?";
  print_string n;
  print_string (skolem_name#name i)

let rec print priority ty =
  open_box 0;
  begin match ty.desc with
    | Clock_static -> print_string "."
    | Clock_var ->
        print_string "'";
        if ty.level <> generic then print_string "_";
        print_string (names.k_clock#name ty.index)
    | Clock_depend c ->
        print_string "{";
        print_carrier priority c;
        print_string "}"
    | Clock_arrow(ty1, ty2, eff) ->
        if priority >= 1 then print_string "(";
        print 1 ty1;
        print_space ();
        print_string"=>{";
        print_effect_row priority eff;
        print_string "}";
        print_space ();
        print 0 ty2;
        if priority >= 1 then print_string ")"
    | Clock_product(ty_list) ->
        (*if priority >= 2 then*) print_string "(";
        print_clock_list 2 "*" ty_list;
        (*if priority >= 2 then*) print_string ")"
    | Clock_constr(name, p_list) ->
        let p_list = kind_sum_split p_list in
        let n = List.length p_list.k_clock in
        if n > 1 then print_string "(";
        print_clock_list 2 "," p_list.k_clock;
        if n > 1 then print_string ")";
        if p_list.k_clock <> [] then print_space ();
        print_qualified_ident name.gi;
        if p_list.k_carrier <> [] || p_list.k_carrier_row <> [] || p_list.k_effect_row <> [] then (
          print_string "{";
          print_carrier_list 2 "," p_list.k_carrier;
          print_string "|";
          print_carrier_row_list 2 "," p_list.k_carrier_row;
          print_string "|";
          print_effect_list 2 "," p_list.k_effect;
          print_string "}"
        );
        if p_list.k_react <> [] then (
          print_string "[";
          print_react_list 2 ", " p_list.k_react;
          print_string "]"
        )
       (* (match name.ck_info with
          | Some { constr_abbr = Constr_abbrev(_, ck) } -> print_string " ----> "; print priority ck
          | _ -> ()) *)
    | Clock_link(link) ->
        (*print_string "~>";*) print priority link
    | Clock_process (ty, c, eff, r) ->
        print 2 ty;
        print_string " process {";
        print_carrier priority c;
        print_string "||";
        print_effect_row priority eff;
        print_string "}[";
        print_react priority r;
        print_string "]"
    | Clock_forall sch ->
        print_scheme_full priority sch
  end;
  close_box ()

and print_carrier priority car =
  open_box 0;
  begin match car.desc with
    | Carrier_var(s) ->
      print_string "''";
      if car.level <> generic then print_string "_";
      print_string (names.k_carrier#name car.index)
    | Carrier_skolem(n, i) ->
      print_skolem_name (n, i)
    | Carrier_link(link) -> print_carrier priority link
  end;
  close_box ()

and print_carrier_row priority cr =
  open_box 0;
  begin match cr.desc with
    | Carrier_row_var ->
      print_string "'";
      if cr.level <> generic then print_string "_";
      print_string (names.k_carrier_row#name cr.index)
    | Carrier_row_one car -> print_carrier priority car
    | Carrier_row(c, { desc = Carrier_row_var }) ->
      print_carrier_row priority c;
      print_string ";.."
    | Carrier_row(c, { desc = Carrier_row_empty }) ->
      print_carrier_row priority c
    | Carrier_row(c1, c2) ->
      print_carrier_row priority c2;
      print_string ";";
      print_carrier_row priority c1
    | Carrier_row_empty -> print_string "empty"
    | Carrier_row_link(link) -> print_carrier_row priority link
  end;
  close_box ()

and print_effect priority eff =
  open_box 0;
  begin match eff.desc with
    | Effect_var ->
      print_string "''";
      if eff.level <> generic then print_string "_";
      print_string (names.k_effect#name eff.index)
    | Effect_empty -> print_string "0"
    | Effect_sum (eff1, eff2) ->
      print_effect priority eff1;
      print_space ();
      print_string "+";
      print_space ();
      print_effect priority eff2
    | Effect_depend c ->
      print_string "<";
      print_carrier_row priority c;
      print_string ">"
    | Effect_one er ->
      print_string "{";
      print_effect_row priority er;
      print_string "}"
    | Effect_link(link) -> print_effect priority link
  end;
  close_box ()

and print_effect_row priority eff =
  open_box 0;
  begin match eff.desc with
    | Effect_row_var ->
      print_string "'";
      if eff.level <> generic then print_string "_";
      print_string (names.k_effect_row#name eff.index)
    | Effect_row_one car -> print_effect priority car
    | Effect_row(c, { desc = Effect_row_var }) ->
      print_effect_row priority c;
      print_string ";.."
    | Effect_row(c, { desc = Effect_row_empty }) ->
      print_effect_row priority c
    | Effect_row(c1, c2) ->
      print_effect_row priority c2;
      print_string ";";
      print_effect_row priority c1
    | Effect_row_rec (c1, c2) ->
      print_string "(rec ";
      print_effect_row priority c1;
      print_string ".";
      print_effect_row priority c2;
      print_string ")"
    | Effect_row_empty -> print_string "empty"
    | Effect_row_link(link) -> print_effect_row priority link
  end;
  close_box ()

and print_react priority r =
  open_box 0;
  begin match r.desc with
    | React_var ->
        print_string "'";
        if r.level <> generic then print_string "_";
        print_string (names.k_react#name r.index)
    | React_empty -> print_string "0"
    | React_carrier c ->
        print_string "{";
        print_carrier_row priority c;
        print_string "}"
    | React_seq rl ->
        print_string "("; print_react_list 2 "; " rl; print_string ")"
    | React_par rl ->
        print_string "("; print_react_list 2 " || " rl; print_string ")"
    | React_or rl ->
        print_string "("; print_react_list 2 " + " rl; print_string ")"
    | React_rec (b, r1, r2) ->
        print_string "(";
        if b then
          print_string "rec* "
        else
          print_string "rec ";
        print_react priority r1;
        print_string ". ";
        print_react priority r2;
        print_string ")"
    | React_run r1 ->
        print_string "(run ";
        print_react priority r1;
        print_string ")"
    | React_link link -> print_react priority link
  end;
  close_box ()

and print_scheme_full priority { cs_vars = vars; cs_desc = ty } =
  let vars = kind_sum_split vars in
  print_string "s";
  if vars.k_clock <> [] then (
    print_string "forall"; print_space ();
    print_clock_list priority "," vars.k_clock;
    print_string "."
  );
  if vars.k_carrier <> [] then (
    print_string "forall"; print_space ();
    print_carrier_list priority "," vars.k_carrier;
    print_string "."
  );
  if vars.k_carrier_row <> [] then (
    print_string "forall"; print_space ();
    print_carrier_row_list priority "," vars.k_carrier_row;
    print_string "."
  );
  if vars.k_effect <> [] then (
    print_string "forall"; print_space ();
    print_effect_list priority "," vars.k_effect;
    print_string "."
  );
  if vars.k_effect_row <> [] then (
    print_string "forall"; print_space ();
    print_effect_row_list priority "," vars.k_effect_row;
    print_string "."
  );
  print priority ty

and print_clock_list priority sep l = _print_list print  priority sep l
and print_carrier_list priority sep l = _print_list print_carrier priority sep l
and print_carrier_row_list priority sep l = _print_list print_carrier_row priority sep l
and print_effect_list priority sep l = _print_list print_effect priority sep l
and print_effect_row_list priority sep l  = _print_list print_effect_row priority sep l
and print_react_list priority sep l = _print_list print_react priority sep l

and print_param priority =
  kind_fold ~clock:print ~carrier:print_carrier ~carrier_row:print_carrier_row
    ~effect:print_effect ~effect_row:print_effect_row ~react:print_react

let print ty =
  names.k_clock#reset;
  names.k_carrier#reset;
  names.k_carrier_row#reset;
  names.k_effect#reset;
  names.k_effect_row#reset;
  names.k_react#reset;
  print 0 ty
let print_scheme { cs_desc = ty } =
  print ty

let print_value_clock_declaration global =
  let prefix = "val" in
  let name = little_name_of_global global in
  open_box 2;
  print_string prefix;
  print_space ();
  if is_an_infix_or_prefix_operator name
  then begin print_string "( "; print_string name; print_string " )" end
  else print_string name;
  print_space ();
  print_string "::";
  print_space ();
  print_scheme (clock_of_global global);
  print_string "\n";
  close_box ();
  print_flush ()

(* printing type declarations *)
let print_clock_name tc arity =
  let print_one_variable assoc i =
    print_string "'";
    print_string (assoc#name i)
  in
  let print_n_variables assoc n =
    if n = 0 then
      ()
    else if n = 1 then (
      print_one_variable assoc 0;
      print_space ()
    ) else (
      print_string "(";
      print_one_variable assoc 0;
      for i = 1 to n-1 do
        print_string ",";
        print_space ();
        print_one_variable assoc i;
      done;
      print_string ")";
      print_space ()
    )
  in
  print_n_variables names.k_clock arity.k_clock;
  print_string (Ident.name tc.id);
  if arity.k_carrier > 0 || arity.k_carrier_row > 0 || arity.k_effect > 0 then (
    print_string "{";
    print_n_variables names.k_carrier arity.k_carrier;
    print_string "|";
    print_n_variables names.k_carrier_row arity.k_carrier_row;
    print_string "|";
    print_n_variables names.k_effect_row arity.k_effect_row;
    print_string "}"
  );
  if arity.k_react > 0 then (
    print_string "[";
    print_n_variables names.k_react arity.k_react;
    print_string "]"
  )

(* prints one variant *)
let print_one_variant global =
  print_space ();
  print_string "|";
  open_box 3;
  print_space ();
  print_string (little_name_of_global global);
  (* prints the rest if the arity is not null *)
  begin
    match clock_of_constr_arg global with
    | None -> ()
    | Some typ ->
	print_space ();
	print_string "of";
	print_space ();
	print typ
  end;
  close_box ()

(* prints one label *)
let print_one_label global =
  print_space ();
  open_box 2;
  print_string (little_name_of_global global);
  print_string ":";
  print_space ();
  print (clock_of_label_res global);
  close_box ()

let rec print_label_list = function
    [] -> ()
  | h::t ->
      print_one_label h;
      if t <> [] then
	begin
	  print_string "; ";
	  print_label_list t
	end

let print_clock_declaration gl =
  let q = Global.gi gl in
  let { clock_kind = td;
	      clock_arity = ta; } = Global.ck_info gl in
  open_box 2;
  print_clock_name q ta;
  begin match td with
  | Clock_abstract -> ()
  | Clock_variant global_list ->
      print_string " =";
      open_hvbox 0;
      List.iter print_one_variant global_list;
      close_box ()
  | Clock_record global_list ->
      print_string " =";
      print_space ();
      print_string "{";
      open_hvbox 1;
      print_label_list global_list;
      close_box ();
      print_space ();
      print_string "}"
  | Clock_rebind te ->
      print_string " =";
      print_space ();
      print te
  end;
  close_box ()

let print_list_of_clock_declarations global_list =
  let rec printrec global_list =
    match global_list with
      [] -> ()
    | [global] -> print_clock_declaration global
    | global :: global_list ->
	print_clock_declaration global;
	print_space ();
	print_string "and ";
	printrec global_list in
  match global_list with
    [] -> ()
  | global_list ->
      open_vbox 0;
      print_string "type ";
      printrec global_list;
      close_box ();
      print_string "\n";;

(* the main printing functions *)
set_max_boxes max_int

let output oc ty =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print ty;
  print_flush ()

let output_carrier oc c =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print_carrier 0 c;
  print_flush ()

let output_carrier_row oc cr =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print_carrier_row 0 cr;
  print_flush ()

let output_effect oc eff =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print_effect 0 eff;
  print_flush ()

let output_effect_row oc effr =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print_effect_row 0 effr;
  print_flush ()

let output_react oc r =
  set_formatter_out_channel oc;
(*   print_string "  "; *)
  print_react 0 r;
  print_flush ()

let output_value_declaration oc global_list =
  set_formatter_out_channel oc;
  List.iter print_value_clock_declaration global_list

let output_type_declaration oc global_list =
  set_formatter_out_channel oc;
  print_list_of_clock_declarations global_list;
  print_flush ()

let output_exception_declaration oc gl =
  set_formatter_out_channel oc;
  open_box 0;
  print_string "exception ";
  print_space ();
  print_string (little_name_of_global gl);
  (* prints the rest if the arity is not null *)
  begin
    match clock_of_constr_arg gl with
    | None -> ()
    | Some typ ->
	print_space ();
	print_string "of";
	print_space ();
	print typ
  end;
  close_box ();
  print_string "\n";
  print_flush ()

