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
open Def_types
open Asttypes
open Misc
open Ident
open Modules
open Global_ident
open Global

let default_formatter = ref std_formatter

(* the long name of an ident is printed *)
(* if it is different from the current module *)
(* or if it is from the standard module *)
let print_qualified_ident q =
  if (compiled_module_name () <> q.qual) &
    (pervasives_module <> q.qual) &
    (!interpreter_module <> q.qual)
  then begin pp_print_string !default_formatter q.qual;pp_print_string !default_formatter "." end;
  pp_print_string !default_formatter (Ident.name q.id)

(* type variables are printed 'a, 'b,... *)
let type_name = new name_assoc_table int_to_alpha

let rec print priority ty =
  open_box 0;
  begin match ty.type_desc with
    Type_var ->
      pp_print_string !default_formatter "'";
      if ty.type_level <> generic then pp_print_string !default_formatter "_";
      pp_print_string !default_formatter (type_name#name ty.type_index)
  | Type_arrow(ty1, ty2) ->
      if priority >= 1 then pp_print_string !default_formatter "(";
      print 1 ty1;
      pp_print_space !default_formatter ();
      pp_print_string !default_formatter "->";
      pp_print_space !default_formatter ();
      print 0 ty2;
      if priority >= 1 then pp_print_string !default_formatter ")"
  | Type_product(ty_list) ->
      if priority >= 2 then pp_print_string !default_formatter "(";
      print_list 2 "*" ty_list;
      if priority >= 2 then pp_print_string !default_formatter ")"
  | Type_constr(name,ty_list) ->
      let n = List.length ty_list in
      if n > 1 then pp_print_string !default_formatter "(";
      print_list 2 "," ty_list;
      if n > 1 then pp_print_string !default_formatter ")";
      if ty_list <> [] then pp_print_space !default_formatter ();
      print_qualified_ident name.gi
  | Type_link(link) ->
      print priority link
  | Type_process(ty, proc_info) ->
      print 2 ty;
      pp_print_space !default_formatter ();
      pp_print_string !default_formatter "process";
  end;
  pp_close_box !default_formatter ()

and print_proc_info pi = ()
(*   begin match pi.proc_static with *)
(*   | None | Some(Def_static.Dontknow) -> () *)
(*   | Some(Def_static.Instantaneous) -> pp_print_string !default_formatter "-" *)
(*   | Some(Def_static.Noninstantaneous) -> pp_print_string !default_formatter "+" *)
(*   end *)

and print_list priority sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [ty] ->
	print priority ty
    | ty::rest ->
	print priority ty;
	pp_print_space !default_formatter ();
	pp_print_string !default_formatter sep;
	pp_print_space !default_formatter ();
	printrec rest in
  printrec l

let print ty =
  type_name#reset;
  print 0 ty;
  pp_print_flush !default_formatter ()
let print_scheme { ts_desc = ty } = print ty

let print_value_type_declaration global =
  let prefix = "val" in
  let name = little_name_of_global global in
  pp_open_box !default_formatter 2;
  pp_print_string !default_formatter prefix;
  pp_print_space !default_formatter ();
  if is_an_infix_or_prefix_operator name
  then begin pp_print_string !default_formatter "( "; pp_print_string !default_formatter name; pp_print_string !default_formatter " )" end
  else pp_print_string !default_formatter name;
  pp_print_space !default_formatter ();
  pp_print_string !default_formatter ":";
  pp_print_space !default_formatter ();
  print_scheme (type_of_global global);
  pp_print_string !default_formatter "\n";
  pp_close_box !default_formatter ();
  pp_print_flush !default_formatter ()

(* printing type declarations *)
let print_type_name tc ta =
  let print_one_type_variable i =
    pp_print_string !default_formatter "'";
    pp_print_string !default_formatter (int_to_alpha i) in
  let rec printrec n =
    if n >= ta then ()
    else if n = ta - 1 then print_one_type_variable n
    else begin
      print_one_type_variable n;
      pp_print_string !default_formatter ",";
      printrec (n+1)
    end in
  if ta = 0 then () else if ta = 1
  then
    begin
      print_one_type_variable 0;
      pp_print_space !default_formatter ()
    end
  else begin
    pp_print_string !default_formatter "(";
    printrec 0;
    pp_print_string !default_formatter ")";
    pp_print_space !default_formatter ()
  end;
  pp_print_string !default_formatter (Ident.name tc.id)

(* prints one variant *)
let print_one_variant global =
  pp_print_space !default_formatter ();
  pp_print_string !default_formatter "|";
  pp_open_box !default_formatter 3;
  pp_print_space !default_formatter ();
  pp_print_string !default_formatter (little_name_of_global global);
  (* prints the rest if the arity is not null *)
  begin
    match type_of_constr_arg global with
    | None -> ()
    | Some typ ->
	pp_print_space !default_formatter ();
	pp_print_string !default_formatter "of";
	pp_print_space !default_formatter ();
	print typ
  end;
  pp_close_box !default_formatter ()

(* prints one label *)
let print_one_label global =
  pp_print_space !default_formatter ();
  pp_open_box !default_formatter 2;
  pp_print_string !default_formatter (little_name_of_global global);
  pp_print_string !default_formatter ":";
  pp_print_space !default_formatter ();
  print (type_of_label_res global);
  pp_close_box !default_formatter ()

let rec print_label_list = function
    [] -> ()
  | h::t ->
      print_one_label h;
      if t <> [] then
	begin
	  pp_print_string !default_formatter "; ";
	  print_label_list t
	end

let print_type_declaration gl =
  let q = Global.gi gl in
  let { type_kind = td;
	type_arity = ta; } = Global.info gl in
  pp_open_box !default_formatter 2;
  print_type_name q ta;
  begin match td with
  | Type_abstract -> ()
  | Type_variant global_list ->
      pp_print_string !default_formatter " =";
      open_hvbox 0;
      List.iter print_one_variant global_list;
      pp_close_box !default_formatter ()
  | Type_record global_list ->
      pp_print_string !default_formatter " =";
      pp_print_space !default_formatter ();
      pp_print_string !default_formatter "{";
      open_hvbox 1;
      print_label_list global_list;
      pp_close_box !default_formatter ();
      pp_print_space !default_formatter ();
      pp_print_string !default_formatter "}"
  | Type_rebind te ->
      pp_print_string !default_formatter " =";
      pp_print_space !default_formatter ();
      print te
  end;
  pp_close_box !default_formatter ()

let print_list_of_type_declarations global_list =
  let rec printrec global_list =
    match global_list with
      [] -> ()
    | [global] -> print_type_declaration global
    | global :: global_list ->
	print_type_declaration global;
	pp_print_space !default_formatter ();
	pp_print_string !default_formatter "and ";
	printrec global_list in
  match global_list with
    [] -> ()
  | global_list ->
      open_vbox 0;
      pp_print_string !default_formatter "type ";
      printrec global_list;
      pp_close_box !default_formatter ();
      pp_print_string !default_formatter "\n";;

(* the main printing functions *)
let () = set_max_boxes max_int

let output oc ty =
  set_formatter_out_channel oc;
  print ty;
  pp_print_flush !default_formatter ()

let output_value_type_declaration fmt global_list =
  default_formatter := fmt;
  List.iter print_value_type_declaration global_list

let output_type_declaration fmt global_list =
  default_formatter := fmt;
  print_list_of_type_declarations global_list;
  pp_print_flush !default_formatter ()

let output_exception_declaration fmt gl =
  default_formatter := fmt;
  pp_open_box !default_formatter 0;
  pp_print_string !default_formatter "exception ";
  pp_print_space !default_formatter ();
  pp_print_string !default_formatter (little_name_of_global gl);
  (* prints the rest if the arity is not null *)
  begin
    match type_of_constr_arg gl with
    | None -> ()
    | Some typ ->
	pp_print_space !default_formatter ();
	pp_print_string !default_formatter "of";
	pp_print_space !default_formatter ();
	print typ
  end;
  pp_close_box !default_formatter ();
  pp_print_string !default_formatter "\n";
  pp_print_flush !default_formatter ()

