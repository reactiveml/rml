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

(* the long name of an ident is printed *)
(* if it is different from the current module *)
(* or if it is from the standard module *)
let print_qualified_ident ff q =
  if (compiled_module_name () <> q.qual) &&
    (pervasives_module <> q.qual) &&
    (!interpreter_module <> q.qual)
  then begin pp_print_string ff q.qual;pp_print_string ff "." end;
  pp_print_string ff (Ident.name q.id)

(* type variables are printed 'a, 'b,... *)
let type_name = new name_assoc_table int_to_alpha
let react_name = new name_assoc_table (fun i -> Format.sprintf "r%i" (i+1))

(** Print reactivity effects **)

(* Find the root of a type, that is the folded version of the type *)
let visited_list, visited = mk_visited ()

type status = NoCycle | Cycle | Rec of reactivity_effect
let max r1 r2 = match r1, r2 with
| Rec _, _ -> r1
| _, Rec _ -> r2
| Cycle, _ | _, Cycle -> Cycle
| NoCycle, NoCycle -> NoCycle

let rec max_list rl =
  List.fold_left max NoCycle rl

let rec find_root check first k =
  if check && k == first then Cycle
  else
    match k.react_desc with
      | React_var | React_pause | React_epsilon -> NoCycle
      | React_seq kl | React_par kl | React_or kl ->
        let rl = List.map (find_root true first) kl in
        max_list rl
      | React_raw (k1, k2) ->
        max (find_root true first k1) (find_root true first k2)
      | React_rec (_, k1) ->
        if visited k then
          NoCycle
        else
          (match find_root true first k1 with
            | NoCycle -> NoCycle
            | Cycle | Rec _ -> Rec k)
      | React_run k -> find_root true first k
      | React_link(link) -> find_root true first link

let find_root k =
  visited_list := [];
  match k.react_desc with
    | React_rec _ -> k
    | _ ->
      match find_root false k k with
        | Rec k1 -> k1
        | Cycle -> assert false
        | NoCycle -> k

let visited_list, visited = mk_visited ()
let rec print_reactivity ff k =
  match k.react_desc with
  | React_link k' -> print_reactivity ff k'
  | React_var ->
      fprintf ff "'%s%s"
        (if k.react_level <> generic then "_" else "")
        (react_name#name k.react_index)
  | React_pause -> fprintf ff "*"
  | React_epsilon -> fprintf ff "0"
  | React_seq l -> fprintf ff "(%a)" (print_reactivity_list "; ") l
  | React_par l -> fprintf ff "(%a)" (print_reactivity_list " || ") l
  | React_or l -> fprintf ff "(%a)" (print_reactivity_list " + ") l
  | React_raw (k1, { react_desc = React_var }) ->
      fprintf ff "(%a + ..)"
        print_reactivity k1
  | React_raw (k1, k2) ->
      fprintf ff "(%a + %a)"
        print_reactivity k1
        print_reactivity k2
  | React_rec (_, k') ->
      if not (visited k) then (
        fprintf ff "(rec '%s. %a)"
          (react_name#name k.react_index)
          print_reactivity k'
      ) else
        fprintf ff "'%s" (react_name#name k.react_index)
  | React_run k' -> fprintf ff "run %a" print_reactivity k'

and print_reactivity_list sep ff l =
  let rec printrec ff l =
    match l with
      [] -> ()
    | [k] ->
	print_reactivity ff k
    | k::rest ->
        fprintf ff "%a%s%a"
	  print_reactivity k
          sep
	  printrec rest in
  printrec ff l

let print_reactivity ff k =
  visited_list := [];
  print_reactivity ff (*(find_root k)*) k


let rec print ff priority ty =
  pp_open_box ff 0;
  begin match ty.type_desc with
    Type_var ->
      pp_print_string ff "'";
      if ty.type_level <> generic then pp_print_string ff "_";
      pp_print_string ff (type_name#name ty.type_index)
  | Type_arrow(ty1, ty2) ->
      if priority >= 1 then pp_print_string ff "(";
      print ff 1 ty1;
      pp_print_space ff ();
      pp_print_string ff "->";
      pp_print_space ff ();
      print ff 0 ty2;
      if priority >= 1 then pp_print_string ff ")"
  | Type_product(ty_list) ->
      if priority >= 2 then pp_print_string ff "(";
      print_list ff 2 " * " ty_list;
      if priority >= 2 then pp_print_string ff ")"
  | Type_constr(name,ty_list) ->
      let n = List.length ty_list in
      if n > 1 then pp_print_string ff "(";
      print_list ff 2 ", " ty_list;
      if n > 1 then pp_print_string ff ")";
      if ty_list <> [] then pp_print_space ff ();
      print_qualified_ident ff name.gi
  | Type_link(link) ->
      print ff priority link
  | Type_process(ty, proc_info) ->
      print ff 2 ty;
      pp_print_space ff ();
      pp_print_string ff "process";
      print_proc_info ff proc_info
  end;
  pp_close_box ff ()

and print_proc_info ff pi =
(*   begin match pi.proc_static with *)
(*   | None | Some(Def_static.Dontknow) -> () *)
(*   | Some(Def_static.Instantaneous) -> pp_print_string ff "-" *)
(*   | Some(Def_static.Noninstantaneous) -> pp_print_string ff "+" *)
(*   end *)
  if !Misc.dreactivity then
    fprintf ff "[%a]" print_reactivity pi.proc_react

and print_list ff priority sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [ty] ->
	print ff priority ty
    | ty::rest ->
	print ff priority ty;
	pp_print_space ff ();
	pp_print_string ff sep;
	pp_print_space ff ();
	printrec rest in
  printrec l

let print ff ty =
  type_name#reset;
  react_name#reset;
  print ff 0 ty;
  pp_print_flush ff ()
let print_scheme ff { ts_desc = ty } = print ff ty

let print_value_type_declaration ff global =
  let prefix = "val" in
  let name = little_name_of_global global in
  pp_open_box ff 2;
  pp_print_string ff prefix;
  pp_print_space ff ();
  if is_an_infix_or_prefix_operator name
  then begin pp_print_string ff "( "; pp_print_string ff name; pp_print_string ff " )" end
  else pp_print_string ff name;
  pp_print_space ff ();
  pp_print_string ff ":";
  pp_print_space ff ();
  print_scheme ff (type_of_global global);
  pp_print_string ff "\n";
  pp_close_box ff ();
  pp_print_flush ff ()

(* printing type declarations *)
let print_type_name ff tc ta =
  let print_one_type_variable i =
    pp_print_string ff "'";
    pp_print_string ff (int_to_alpha i) in
  let rec printrec n =
    if n >= ta then ()
    else if n = ta - 1 then print_one_type_variable n
    else begin
      print_one_type_variable n;
      pp_print_string ff ",";
      printrec (n+1)
    end in
  if ta = 0 then () else if ta = 1
  then
    begin
      print_one_type_variable 0;
      pp_print_space ff ()
    end
  else begin
    pp_print_string ff "(";
    printrec 0;
    pp_print_string ff ")";
    pp_print_space ff ()
  end;
  pp_print_string ff (Ident.name tc.id)

(* prints one variant *)
let print_one_variant ff global =
  pp_print_space ff ();
  pp_print_string ff "|";
  pp_open_box ff 3;
  pp_print_space ff ();
  pp_print_string ff (little_name_of_global global);
  (* prints the rest if the arity is not null *)
  begin
    match type_of_constr_arg global with
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
  print ff (type_of_label_res global);
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

let print_type_declaration ff gl =
  let q = Global.gi gl in
  let { type_kind = td;
	type_arity = ta; } = Global.info gl in
  pp_open_box ff 2;
  print_type_name ff q ta;
  begin match td with
  | Type_abstract -> ()
  | Type_variant global_list ->
      pp_print_string ff " =";
      pp_open_hvbox ff 0;
      List.iter (print_one_variant ff) global_list;
      pp_close_box ff ()
  | Type_record global_list ->
      pp_print_string ff " =";
      pp_print_space ff ();
      pp_print_string ff "{";
      pp_open_hvbox ff 1;
      print_label_list ff global_list;
      pp_close_box ff ();
      pp_print_space ff ();
      pp_print_string ff "}"
  | Type_rebind te ->
      pp_print_string ff " =";
      pp_print_space ff ();
      print ff te
  end;
  pp_close_box ff ()

let print_list_of_type_declarations ff global_list =
  let rec printrec global_list =
    match global_list with
      [] -> ()
    | [global] -> print_type_declaration ff global
    | global :: global_list ->
	print_type_declaration ff global;
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

(* the main printing functions *)

let output ff ty =
  print ff ty;
  pp_print_flush ff ()

let output_value_type_declaration ff global_list =
  pp_open_box ff 0;
  List.iter (print_value_type_declaration ff) global_list;
  pp_close_box ff ()

let output_type_declaration ff global_list =
  pp_open_box ff 0;
  print_list_of_type_declarations ff global_list;
  pp_close_box ff ();
  pp_print_flush ff ()

let output_exception_declaration ff gl =
  pp_open_box ff 0;
  pp_print_string ff "exception ";
  pp_print_space ff ();
  pp_print_string ff (little_name_of_global gl);
  (* prints the rest if the arity is not null *)
  begin
    match type_of_constr_arg gl with
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

