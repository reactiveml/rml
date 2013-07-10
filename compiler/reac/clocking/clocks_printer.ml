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

let _print_list ff f priority sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [ty] ->
	      f ff priority ty
    | ty::rest ->
	      f ff priority ty;
	      pp_print_space ff ();
	      pp_print_string ff sep;
	      pp_print_space ff ();
	      printrec rest
  in
  printrec l

(* the long name of an ident is printed *)
(* if it is different from the current module *)
(* or if it is from the standard module *)
let print_qualified_ident ff q =
  if (compiled_module_name () <> q.qual) &
    (pervasives_module <> q.qual) &
    (!interpreter_module <> q.qual)
  then begin pp_print_string ff q.qual;pp_print_string ff "." end;
  pp_print_string ff (Ident.name q.id)

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

let react_visited_list, react_visited = mk_visited ()
let eff_visited_list, eff_visited = mk_visited ()

let print_skolem_name ff (n, i) =
  pp_print_string ff "?";
  pp_print_string ff n

let rec print ff priority ty =
  pp_open_box ff 0;
  begin match ty.desc with
    | Clock_var ->
        pp_print_string ff "'";
        if ty.level <> generic then pp_print_string ff "_";
        pp_print_string ff (names.k_clock#name ty.index)
    | Clock_depend cr ->
        pp_print_string ff "{";
        print_carrier_row ff priority cr;
        pp_print_string ff "}"
    | Clock_arrow(ty1, ty2, eff) ->
        if priority >= 1 then pp_print_string ff "(";
        print ff 1 ty1;
        pp_print_space ff ();
        pp_print_string ff "=>{";
        print_effect_row_safe ff priority eff;
        pp_print_string ff "}";
        pp_print_space ff ();
        print ff 0 ty2;
        if priority >= 1 then pp_print_string ff ")"
    | Clock_product(ty_list) ->
        (*if priority >= 2 then*) pp_print_string ff "(";
        print_clock_list ff 2 "*" ty_list;
        (*if priority >= 2 then*) pp_print_string ff ")"
    | Clock_constr(name, p_list) ->
        let p_list = kind_sum_split p_list in
        let n = List.length p_list.k_clock in
        if n > 1 then pp_print_string ff "(";
        print_clock_list ff 2 "," p_list.k_clock;
        if n > 1 then pp_print_string ff ")";
        if p_list.k_clock <> [] then pp_print_space ff ();
        print_qualified_ident ff name.gi;
        if !Compiler_options.use_row_clocking then
          begin
            if p_list.k_carrier <> [] || p_list.k_carrier_row <> [] || p_list.k_effect_row <> [] then (
              pp_print_string ff "{";
              print_carrier_list ff 2 "," p_list.k_carrier;
              pp_print_string ff "|";
              print_carrier_row_list ff 2 "," p_list.k_carrier_row;
              pp_print_string ff "|";
              print_effect_row_safe_list ff 2 "," p_list.k_effect_row;
              pp_print_string ff "}"
            );
          end
        else
          begin
            if p_list.k_carrier <> [] || p_list.k_effect <> [] then (
              pp_print_string ff "{";
              print_carrier_list ff 2 "," p_list.k_carrier;
              pp_print_string ff "|";
              print_effect_list ff 2 "," p_list.k_effect;
              pp_print_string ff "}"
            );
          end;
        if p_list.k_react <> [] then (
          pp_print_string ff "[";
          print_reactivity_list ff 2 ", " p_list.k_react;
          pp_print_string ff "]"
        )
       (* (match name.ck_info with
          | Some { constr_abbr = Constr_abbrev(_, ck) } -> pp_print_string ff " ----> "; print priority ck
          | _ -> ()) *)
    | Clock_link(link) ->
        (*pp_print_string ff "~>";*) print ff priority link
    | Clock_process (ty, c, eff, r) ->
        print ff 2 ty;
        pp_print_string ff " process {";
        print_carrier ff priority c;
        if !Compiler_options.use_row_clocking then
          pp_print_string ff "||"
        else
          pp_print_string ff "|";
        print_effect_row_safe ff priority eff;
        pp_print_string ff "}";
        if not !Compiler_options.no_reactivity && !Compiler_options.show_reactivity then (
          pp_print_string ff "[";
          print_reactivity ff priority r;
          pp_print_string ff "]"
        )
    | Clock_forall sch ->
        print_scheme_full ff priority sch
  end;
  pp_close_box ff ()

and print_carrier ff priority car =
  pp_open_box ff 0;
  begin match car.desc with
    | Carrier_var(s) ->
      if !Compiler_options.use_row_clocking then
        pp_print_string ff "''"
      else
        pp_print_string ff "'";
      if car.level <> generic then pp_print_string ff "_";
      pp_print_string ff (names.k_carrier#name car.index)
    | Carrier_skolem(n, i) ->
      print_skolem_name ff (n, i)
    | Carrier_link(link) -> print_carrier ff priority link
  end;
  pp_close_box ff ()

and print_carrier_row ff priority cr =
  pp_open_box ff 0;
  begin match cr.desc with
    | Carrier_row_var ->
      pp_print_string ff "'";
      if cr.level <> generic then pp_print_string ff "_";
      pp_print_string ff (names.k_carrier_row#name cr.index)
    | Carrier_row_one car -> print_carrier ff priority car
    | Carrier_row(c, { desc = Carrier_row_var }) ->
      print_carrier_row ff priority c;
      pp_print_string ff ";.."
    | Carrier_row(c, { desc = Carrier_row_empty }) ->
      print_carrier_row ff priority c
    | Carrier_row(c1, c2) ->
      print_carrier_row ff priority c2;
      pp_print_string ff ";";
      print_carrier_row ff priority c1
    | Carrier_row_empty -> pp_print_string ff "empty"
    | Carrier_row_link(link) -> print_carrier_row ff priority link
  end;
  pp_close_box ff ()

and print_effect ff priority eff =
  pp_open_box ff 0;
  begin match eff.desc with
    | Effect_var ->
      pp_print_string ff "''";
      if eff.level <> generic then pp_print_string ff "_";
      pp_print_string ff (names.k_effect#name eff.index)
    | Effect_empty -> pp_print_string ff "0"
    | Effect_sum (eff1, eff2) ->
      print_effect ff priority eff1;
      pp_print_space ff ();
      pp_print_string ff "+";
      pp_print_space ff ();
      print_effect ff priority eff2
    | Effect_depend c ->
      if !Compiler_options.use_row_clocking then (
        pp_print_string ff "<";
        print_carrier_row ff priority c;
        pp_print_string ff ">"
      ) else
        print_carrier_row ff priority c
    | Effect_one er ->
      pp_print_string ff "{";
      print_effect_row ff priority er;
      pp_print_string ff "}"
    | Effect_link(link) -> print_effect ff priority link
  end;
  pp_close_box ff ()

and print_effect_row ff priority eff =
  pp_open_box ff 0;
  begin match eff.desc with
    | Effect_row_var ->
      pp_print_string ff "'";
      if eff.level <> generic then pp_print_string ff "_";
      pp_print_string ff (names.k_effect_row#name eff.index)
    | Effect_row_one car -> print_effect ff priority car
    | Effect_row(c, { desc = Effect_row_var }) ->
      print_effect_row ff priority c;
      pp_print_string ff ";.."
  (*  | Effect_row(c, { desc = Effect_row_empty }) ->
      print_effect_row priority c *)
    | Effect_row(c1, c2) ->
      print_effect_row ff priority c1;
      pp_print_string ff ";";
      print_effect_row ff priority c2
    | Effect_row_rec eff1 ->
      if not (eff_visited eff) then (
        pp_print_string ff "(rec '";
        pp_print_string ff (names.k_effect_row#name eff.index);
        pp_print_string ff ".";
        print_effect_row ff priority eff1;
        pp_print_string ff ")"
      ) else (
        pp_print_string ff "'";
        pp_print_string ff (names.k_effect_row#name eff.index);
      )
    | Effect_row_empty -> pp_print_string ff "empty"
    | Effect_row_link(link) -> print_effect_row ff priority link
  end;
  pp_close_box ff ()

and print_effect_row_safe ff priority r =
  eff_visited_list := [];
  print_effect_row ff priority r

and print_react ff priority r =
  pp_open_box ff 0;
  begin match r.desc with
    | React_var ->
        pp_print_string ff "'";
        if r.level <> generic then pp_print_string ff "_";
        pp_print_string ff (names.k_react#name r.index)
    | React_empty -> pp_print_string ff "0"
    | React_carrier c ->
        pp_print_string ff "{";
        print_carrier_row ff priority c;
        pp_print_string ff "}"
    | React_seq rl ->
        pp_print_string ff "("; print_react_list ff 2 "; " rl; pp_print_string ff ")"
    | React_par rl ->
        pp_print_string ff "("; print_react_list ff 2 " || " rl; pp_print_string ff ")"
    | React_or rl ->
        pp_print_string ff "("; print_react_list ff 2 " + " rl; pp_print_string ff ")"
    | React_rec (b, r1) ->
        if not (react_visited r) then (
          pp_print_string ff "(";
          if b then
            pp_print_string ff "rec* "
          else
            pp_print_string ff "rec ";
          pp_print_string ff "'";
          pp_print_string ff (names.k_react#name r.index);
          pp_print_string ff ". ";
          print_react ff priority r1;
          pp_print_string ff ")"
        ) else (
          pp_print_string ff "'";
          pp_print_string ff (names.k_react#name r.index)
        )
    | React_run r1 ->
        pp_print_string ff "(run ";
        print_react ff priority r1;
        pp_print_string ff ")"
    | React_link link -> print_react ff priority link
  end;
  pp_close_box ff ()

and print_reactivity ff priority r =
  react_visited_list := [];
  print_react ff priority r

and print_scheme_full ff priority { cs_vars = vars; cs_desc = ty } =
  let vars = kind_sum_split vars in
  pp_print_string ff "(";
  if vars.k_clock <> [] then (
    pp_print_string ff "forall"; pp_print_space ff ();
    print_clock_list ff priority "," vars.k_clock;
    pp_print_string ff ". "
  );
  if vars.k_carrier <> [] then (
    pp_print_string ff "forall"; pp_print_space ff ();
    print_carrier_list ff priority "," vars.k_carrier;
    pp_print_string ff ". "
  );
  if vars.k_carrier_row <> [] then (
    pp_print_string ff "forall"; pp_print_space ff ();
    print_carrier_row_list ff priority "," vars.k_carrier_row;
    pp_print_string ff ". "
  );
  if vars.k_effect <> [] then (
    pp_print_string ff "forall"; pp_print_space ff ();
    print_effect_list ff priority "," vars.k_effect;
    pp_print_string ff ". "
  );
  if vars.k_effect_row <> [] then (
    pp_print_string ff "forall"; pp_print_space ff ();
    print_effect_row_list ff priority "," vars.k_effect_row;
    pp_print_string ff ". "
  );
  print ff priority ty;
  pp_print_string ff ")"

and print_clock_list ff priority sep l = _print_list ff print priority sep l
and print_carrier_list ff priority sep l = _print_list ff print_carrier priority sep l
and print_carrier_row_list ff priority sep l = _print_list ff print_carrier_row priority sep l
and print_effect_list ff priority sep l = _print_list ff print_effect priority sep l
and print_effect_row_list ff priority sep l  = _print_list ff print_effect_row priority sep l
and print_effect_row_safe_list ff priority sep l  = _print_list ff print_effect_row_safe priority sep l
and print_react_list ff priority sep l = _print_list ff print_react priority sep l
and print_reactivity_list ff priority sep l = _print_list ff print_reactivity priority sep l

and print_param ff priority p =
  let print =
    mk_kind_prod ~clock:(print ff) ~carrier:(print_carrier ff) ~carrier_row:(print_carrier_row ff)
      ~effect:(print_effect ff) ~effect_row:(print_effect_row ff) ~react:(print_reactivity ff)
  in
  ignore (kind_fold print priority p)

let print ff ty =
  names.k_clock#reset;
  names.k_carrier#reset;
  names.k_carrier_row#reset;
  names.k_effect#reset;
  names.k_effect_row#reset;
  names.k_react#reset;
  print ff 0 ty
let print_scheme ff { cs_desc = ty } =
  print ff ty

let print_value_clock_declaration ff global =
  let prefix = "val" in
  let name = little_name_of_global global in
  pp_open_box ff 2;
  pp_print_string ff prefix;
  pp_print_space ff ();
  if is_an_infix_or_prefix_operator name
  then begin pp_print_string ff "( "; pp_print_string ff name; pp_print_string ff " )" end
  else pp_print_string ff name;
  pp_print_space ff ();
  pp_print_string ff "::";
  pp_print_space ff ();
  print_scheme ff (clock_of_global global);
  pp_print_string ff "\n";
  pp_close_box ff ();
  pp_print_flush ff ()

(* printing type declarations *)
let print_clock_name ff tc arity =
  let print_one_variable assoc i =
    pp_print_string ff "'";
    pp_print_string ff (assoc#name i)
  in
  let print_n_variables assoc n =
    if n = 0 then
      ()
    else if n = 1 then (
      print_one_variable assoc 0;
      pp_print_space ff ()
    ) else (
      pp_print_string ff "(";
      print_one_variable assoc 0;
      for i = 1 to n-1 do
        pp_print_string ff ",";
        pp_print_space ff ();
        print_one_variable assoc i;
      done;
      pp_print_string ff ")";
      pp_print_space ff ()
    )
  in
  print_n_variables names.k_clock arity.k_clock;
  pp_print_string ff (Ident.name tc.id);
  if !Compiler_options.use_row_clocking then (
    if arity.k_carrier > 0 || arity.k_carrier_row > 0 || arity.k_effect_row > 0 then (
      pp_print_string ff "{";
      print_n_variables names.k_carrier arity.k_carrier;
      pp_print_string ff "|";
      print_n_variables names.k_carrier_row arity.k_carrier_row;
      pp_print_string ff "|";
      print_n_variables names.k_effect_row arity.k_effect_row;
      pp_print_string ff "}"
    )
  ) else (
    if arity.k_carrier > 0 || arity.k_effect > 0 then (
      pp_print_string ff "{";
      print_n_variables names.k_carrier arity.k_carrier;
      pp_print_string ff "|";
      print_n_variables names.k_effect arity.k_effect;
      pp_print_string ff "}"
    )
  );
  if arity.k_react > 0 && !Compiler_options.show_reactivity then (
    pp_print_string ff "[";
    print_n_variables names.k_react arity.k_react;
    pp_print_string ff "]"
  )

(* prints one variant *)
let print_one_variant ff global =
  pp_print_space ff ();
  pp_print_string ff "|";
  pp_open_box ff 3;
  pp_print_space ff ();
  pp_print_string ff (little_name_of_global global);
  (* prints the rest if the arity is not null *)
  begin
    match clock_of_constr_arg global with
    | None -> ()
    | Some typ ->
	pp_print_space ff ();
	pp_print_string ff "of";
	pp_print_space ff ();
	print ff typ
  end;
  pp_close_box ff ()

(* prints one label *)
let print_one_label ff global =
  pp_print_space ff ();
  pp_open_box ff 2;
  pp_print_string ff (little_name_of_global global);
  pp_print_string ff ":";
  pp_print_space ff ();
  print ff (clock_of_label_res global);
  pp_close_box ff ()

let rec print_label_list ff = function
    [] -> ()
  | h::t ->
      print_one_label ff h;
      if t <> [] then
	begin
	  pp_print_string ff "; ";
	  print_label_list ff t
	end

let print_clock_declaration ff gl =
  let q = Global.gi gl in
  let { clock_kind = td;
	      clock_arity = ta; } = Global.ck_info gl in
  pp_open_box ff 2;
  print_clock_name ff q ta;
  begin match td with
  | Clock_abstract -> ()
  | Clock_variant global_list ->
      pp_print_string ff " =";
      pp_open_hvbox ff 0;
      List.iter (print_one_variant ff) global_list;
      pp_close_box ff ()
  | Clock_record global_list ->
      pp_print_string ff " =";
      pp_print_space ff ();
      pp_print_string ff "{";
      pp_open_hvbox ff 1;
      print_label_list ff global_list;
      pp_close_box ff ();
      pp_print_space ff ();
      pp_print_string ff "}"
  | Clock_rebind te ->
      pp_print_string ff " =";
      pp_print_space ff ();
      print ff te
  end;
  pp_close_box ff ()

let print_list_of_clock_declarations ff global_list =
  let rec printrec global_list =
    match global_list with
      [] -> ()
    | [global] -> print_clock_declaration ff global
    | global :: global_list ->
	print_clock_declaration ff global;
	pp_print_space ff ();
	pp_print_string ff "and ";
	printrec global_list in
  match global_list with
    [] -> ()
  | global_list ->
      pp_open_vbox ff 0;
      pp_print_string ff "type ";
      printrec global_list;
      pp_close_box ff ();
      pp_print_string ff "\n";;

let output ff ty =
  print ff ty;
  pp_print_flush ff ()

let output_carrier ff c =
  print_carrier ff 0 c;
  pp_print_flush ff ()

let output_carrier_row ff cr =
  print_carrier_row ff 0 cr;
  pp_print_flush ff ()

let output_effect ff eff =
  print_effect ff 0 eff;
  pp_print_flush ff ()

let output_effect_row ff effr =
  print_effect_row ff 0 effr;
  pp_print_flush ff ()

let output_react ff r =
  print_reactivity ff 0 r;
  pp_print_flush ff
 ()

let output_param ff p =
  print_param ff 0 p;
  pp_print_flush ff ()

let output_value_declaration ff global_list =
  pp_open_box ff 0;
  List.iter (print_value_clock_declaration ff) global_list;
  pp_close_box ff ()

let output_type_declaration ff global_list =
  pp_open_box ff 0;
  print_list_of_clock_declarations ff global_list;
  pp_close_box ff ();
  pp_print_flush ff ()

let output_exception_declaration ff gl =
  pp_open_box ff 0;
  pp_print_string ff "exception ";
  pp_print_space ff ();
  pp_print_string ff (little_name_of_global gl);
  (* prints the rest if the arity is not null *)
  begin
    match clock_of_constr_arg gl with
    | None -> ()
    | Some typ ->
	pp_print_space ff ();
	pp_print_string ff "of";
	pp_print_space ff ();
	print ff typ
  end;
  pp_close_box ff ();
  pp_print_string ff "\n";
  pp_print_flush ff ()

(* Utils *)
let print_to_string  f x =
  ignore (Format.flush_str_formatter ());
  f Format.str_formatter x;
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

