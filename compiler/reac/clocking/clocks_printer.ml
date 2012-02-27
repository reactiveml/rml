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
let type_name = new name_assoc_table int_to_alpha
let carrier_names = new name_assoc_table (fun i -> string_of_int i)

let print_carrier_name n i =
  print_string "_";
  print_string n;
  print_string (carrier_names#name i)
let print_skolem_name (n, i) =
  print_string "?";
  print_carrier_name n i

let rec print priority ty =
  open_box 0;
  begin match ty.desc with
    | Clock_static -> print_string "."
    | Clock_var ->
        print_string "'";
        if ty.level <> generic then print_string "_";
        print_string (type_name#name ty.index)
    | Clock_depend c ->
        print_string "{";
        print_carrier c;
        print_string "}"
    | Clock_arrow(ty1, ty2) ->
        if priority >= 1 then print_string "(";
        print 1 ty1;
        print_space ();
        print_string "->";
        print_space ();
        print 0 ty2;
        if priority >= 1 then print_string ")"
    | Clock_product(ty_list) ->
        (*if priority >= 2 then*) print_string "(";
        print_list 2 "*" ty_list;
        (*if priority >= 2 then*) print_string ")"
    | Clock_constr(name,ty_list) ->
        let n = List.length ty_list in
        if n > 1 then print_string "(";
        print_list 2 "," ty_list;
        if n > 1 then print_string ")";
        if ty_list <> [] then print_space ();
        print_qualified_ident name.gi;
       (* (match name.ck_info with
          | Some { constr_abbr = Constr_abbrev(_, ck) } -> print_string " ----> "; print priority ck
          | _ -> ()) *)
    | Clock_link(link) ->
        (*print_string "~>";*) print priority link
    | Clock_process ty ->
        print 2 ty;
        print_space ();
        print_string "process";
  end;
  close_box ()

and print_carrier car =
  open_box 0;
  begin match car.desc with
    Carrier_var(s) ->
      print_carrier_name s car.index
  | Carrier_skolem(n, i) ->
      print_skolem_name (n, i)
  | Carrier_link(link) -> print_carrier link
  end;
  close_box ()

and print_list priority sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [ty] ->
	      print priority ty
    | ty::rest ->
	      print priority ty;
	      print_space ();
	      print_string sep;
	      print_space ();
	      printrec rest
  in
  printrec l

let print ty =
  type_name#reset;
  print 0 ty
let print_scheme { cs_desc = ty } = print ty

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
let print_clock_name tc ta =
  let print_one_clock_variable i =
    print_string "''";
    print_string (int_to_alpha i) in
  let rec printrec n =
    if n >= ta then ()
    else if n = ta - 1 then print_one_clock_variable n
    else begin
      print_one_clock_variable n;
      print_string ",";
      printrec (n+1)
    end in
  if ta = 0 then () else if ta = 1
  then
    begin
      print_one_clock_variable 0;
      print_space ()
    end
  else begin
    print_string "(";
    printrec 0;
    print_string ")";
    print_space ()
  end;
  print_string (Ident.name tc.id)

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

