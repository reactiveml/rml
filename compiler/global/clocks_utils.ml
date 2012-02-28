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

(* Warning: *)
(* This file has been done from CamlLight, Lucid Synchrone and the book *)
(* "Le langage Caml" Pierre Weis Xavier Leroy *)

(* created: 2004-05-13  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Basic operations over types *)

open Misc
open Asttypes
open Clocks
open Global

exception Unify
exception Escape of string * int
exception Escape_case of string

(* generating fresh names *)
let names = new Ident.name_generator
let generic_prefix_name = "_c"

(* The current nesting level of lets *)
let current_level = ref 0;;

let reset_type_var () =
  current_level := 0; ()
and push_type_level () =
  incr current_level; ()
and pop_type_level () =
  decr current_level; ()
;;

(* making types *)
let make_clock ck =
  { desc = ck;
    level = generic;
    index = names#name; }

let static = make_clock Clock_static

let depend c =
  make_clock (Clock_depend c)

let product ck_list =
  make_clock (Clock_product(ck_list))

let constr ck_constr ck_list =
  make_clock (Clock_constr(ck_constr, ck_list))

let constr_notabbrev name ck_list =
  make_clock (Clock_constr({ gi = name;
                             ty_info = no_info ();
                             ck_info = Some {constr_abbr = Constr_notabbrev}; },
                          ck_list))

let arrow ck1 ck2 =
  make_clock (Clock_arrow(ck1, ck2))

let rec arrow_list ck_l ck_res =
  match ck_l with
    [] -> ck_res
  | [ck] -> arrow ck ck_res
  | ck :: ck_l -> arrow ck (arrow_list ck_l ck_res)

let process ck =
  make_clock (Clock_process ck)


let make_carrier s =
  { desc = Carrier_var s;
    index = names#name;
    level = !current_level }
let carrier_skolem s i =
  { desc = Carrier_skolem(s, i);
    index = names#name;
    level = !current_level }

let no_clock =
  { desc = Clock_static;
    level = generic;
    index = -1; }
let no_carrier =
  { desc = Carrier_var "";
    index = -1;
    level = generic }
let topck_carrier =
  { desc = Carrier_var "topck";
    index = -2;
    level = generic }

let clock_topck =
  make_clock (Clock_depend topck_carrier)

(* To get fresh type variables *)

let new_clock_var () =
  { desc = Clock_var;
    level = !current_level;
    index = names#name }

let new_generic_clock_var () =
  { desc = Clock_var;
    level = generic;
    index = names#name }

let make_generic_carrier s =
  { desc = Carrier_var s;
    index = names#name;
    level = generic }

let new_generic_carrier_clock_var () =
  { desc = Clock_depend (make_generic_carrier generic_prefix_name);
    level = generic;
    index = names#name }

let new_carrier_clock_var () =
  make_clock (Clock_depend (make_carrier generic_prefix_name))

let rec new_clock_var_list n =
  match n with
    | 0 -> []
    | n -> (new_clock_var ()) :: new_clock_var_list (n - 1)



let forall ck_vars car_vars typ =
  { cs_clock_vars = ck_vars;
    cs_carrier_vars = car_vars;
    cs_desc = typ; }


(* To take the canonical representative of a type.
   We do path compression there. *)

let rec carrier_repr car = match car.desc with
  | Carrier_link c ->
      let c = carrier_repr c in
      car.desc <- Carrier_link c;
      c
  | _ -> car

let rec clock_repr ck = match ck.desc with
  | Clock_link t ->
      let t = clock_repr t in
      ck.desc <- Clock_link t;
      t
  | _ ->
      ck

(* To generalize a type *)

(* generalisation and non generalisation of a type. *)
(* the level of generalised type variables *)
(* is set to [generic] when the flag [is_gen] is true *)
(* and set to [!binding_level] when the flag is false *)
(* returns [generic] when a sub-term can be generalised *)

let list_of_clock_vars = ref []
let list_of_carrier_vars = ref []

let rec gen_clock is_gen ck =
  let ck = clock_repr ck in
  (match ck.desc with
    | Clock_static -> ()
    | Clock_var ->
        if ck.level > !current_level then
          if is_gen then (
            ck.level <- generic;
            list_of_clock_vars := ck :: !list_of_clock_vars
          ) else
            ck.level <- !current_level
    | Clock_depend car ->
        ck.level <- gen_carrier is_gen car
    | Clock_arrow(ck1, ck2) ->
        let level1 = gen_clock is_gen ck1 in
        let level2 = gen_clock is_gen ck2 in
        ck.level <- min level1 level2
    | Clock_product ck_list ->
        ck.level <- gen_clock_list is_gen ck_list
    | Clock_constr(name, ck_list) ->
        ck.level <- gen_clock_list is_gen ck_list
    | Clock_link(link) ->
        ck.level <- gen_clock is_gen link
    | Clock_process body_ck ->
        ck.level <- min generic (gen_clock is_gen body_ck)
  );
  ck.level

and gen_clock_list is_gen ck_list =
  List.fold_left (fun level ck -> min level (gen_clock is_gen ck)) notgeneric ck_list

and gen_carrier is_gen car =
  let car = carrier_repr car in
  (match car.desc with
    | Carrier_var s ->
        if car.level > !current_level then
          if is_gen then (
            car.level <- generic;
            list_of_carrier_vars := car :: !list_of_carrier_vars
          ) else
            car.level <- !current_level
    | Carrier_skolem _ -> ()
    | Carrier_link(link) ->
        car.level <- gen_carrier is_gen link
  );
  car.level


(* main generalisation function *)
let gen ck =
  list_of_clock_vars := [];
  list_of_carrier_vars := [];
  let _ = gen_clock true ck in
  { cs_clock_vars = !list_of_clock_vars;
    cs_carrier_vars = !list_of_carrier_vars;
    cs_desc = ck }

let non_gen ck = ignore (gen_clock false ck)

(* To compute the free type variables in a type *)
let free_clock_vars level ck =
  let fv = ref [] in
  let rec free_vars ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_static | Clock_depend _ -> ()
      | Clock_var ->
          if ck.level >= level then fv := ck :: !fv
      | Clock_arrow(t1,t2) ->
          free_vars t1; free_vars t2
      | Clock_product ck_list ->
          List.iter free_vars ck_list
      | Clock_constr (c, ck_list) ->
          List.iter free_vars ck_list
      | Clock_link link ->
          free_vars link
      | Clock_process ck -> free_vars ck
  in
  free_vars ck;
  !fv

(* save previous configurations *)
class ['a,'b] save =
  object
    val mutable s = ([] : ('a * 'b) list)
    method save value desc = s <- (value, desc) :: s
    method cleanup =
      List.iter (fun (value, desc) -> value.desc <- desc) s;
      s <- []
  end

let clocks = new save
let carriers = new save
let cleanup () =
  clocks#cleanup;
  carriers#cleanup

(* makes a copy of a type *)
let rec copy_clock ck =
  let level = ck.level in
  match ck.desc with
    | Clock_var ->
        if level = generic then
          let v = new_clock_var () in
          ck.desc <- Clock_link(v);
          clocks#save ck Clock_var;
          v
        else
          ck
    | Clock_static -> ck
    | Clock_depend c ->
        if level = generic then
          depend (copy_carrier c)
        else
          ck
    | Clock_link link ->
        if level = generic then
          link
        else
          copy_clock link
    | Clock_arrow (ck1, ck2) ->
        if level = generic then
          arrow (copy_clock ck1) (copy_clock ck2)
        else
          ck
    | Clock_product ck_list ->
        if level = generic then
          product (List.map copy_clock ck_list)
        else
          ck
    | Clock_constr (name, ck_list) ->
        if level = generic then
          constr name (List.map copy_clock ck_list)
        else
          ck
    | Clock_process ck ->
        if level = generic then
          process (copy_clock ck)
        else
          ck

and copy_carrier car =
  let level = car.level in
  match car.desc with
    | Carrier_var s ->
        if level = generic then
          let v = make_carrier s in
          car.desc <- Carrier_link v;
          carriers#save car (Carrier_var s);
          v
        else
          car
  | Carrier_link(link) ->
      if level = generic then
        link
      else
        copy_carrier link
  | Carrier_skolem(s, i) ->
      if level = generic then
        carrier_skolem s i
      else
        car


(* instanciation *)
let instance { cs_desc = ck } =
  let ck_i = copy_clock ck in
  cleanup ();
  ck_i


let instance_and_vars { cs_clock_vars = ck_vars; cs_carrier_vars = car_vars; cs_desc = ck } =
  let ck_i = copy_clock ck in
  let ck_vars = List.map clock_repr ck_vars in
  let car_vars = List.map carrier_repr car_vars in
  cleanup ();
  ck_vars, car_vars, ck_i

let constr_instance { cstr_arg = ck_opt; cstr_res = ck_res; } =
  let ck_opt = opt_map copy_clock ck_opt in
  let ck_res = copy_clock ck_res in
  cleanup ();
  { cstr_arg = ck_opt; cstr_res = ck_res; }

let label_instance { lbl_arg = ck_arg; lbl_res = ck_res; lbl_mut = mut } =
  let ck_arg = copy_clock ck_arg in
  let ck_res = copy_clock ck_res in
  cleanup ();
  { lbl_arg = ck_arg; lbl_res = ck_res; lbl_mut = mut }


(* the occur check *)
let rec occur_check level index ck =
  let rec check ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_var ->
        if ck == index then
          raise Unify
        else if ck.level > level then
          ck.level <- level
      | Clock_static -> ()
      | Clock_depend c -> carrier_occur_check level no_carrier c
      | Clock_arrow (ck1, ck2) -> check ck1; check ck2
      | Clock_product ck_list | Clock_constr(_, ck_list) -> List.iter check ck_list
      | Clock_link link -> check link
      | Clock_process ck -> check ck
  in
  Format.eprintf "Occur_check@.";
  check ck

and carrier_occur_check level index car =
  let rec check car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_var s ->
          Format.eprintf "Var occur check: %s car.level %d, level: %d@." s car.level level;
          if car == index then
            raise Unify
          else if car.level > level then
            car.level <- level
      | Carrier_skolem(s, i) ->
          Format.eprintf "Skolem occur check: %s i:%d  car.level %d, level: %d@." s i car.level level;
          if level < car.level then
            raise (Escape (s, i))
      | Carrier_link link -> check link
  in
  Format.eprintf "Occur_check carrier@.";
  check car


(* type constructor equality *)
let same_type_constr c1 c2 = Global_ident.same c1.gi c2.gi


(* Expansion of an abbreviation *)
let bind_variable ck1 ck2 =
  match ck1.desc, ck2.desc with
    | Clock_var, _ -> ck1.desc <- Clock_link ck2
    | Clock_depend ({ desc = Carrier_var _ } as c1), Clock_depend c2 -> c2.desc <- Carrier_link c1
    | _ -> fatal_error "bind_variable"

let expand_abbrev params body args =
  let params' = List.map copy_clock params
  and body' = copy_clock body in
  cleanup();
  List.iter2 bind_variable params' args;
  body'

(* unification *)
let rec unify expected_ck actual_ck =
 (* Printf.eprintf "Unify '%a' and %a\n" Clocks_printer.output expected_ck  Clocks_printer.output actual_ck; *)
  if expected_ck == actual_ck then ()
  else
    let expected_ck = clock_repr expected_ck in
    let actual_ck = clock_repr actual_ck in
  (*  Printf.eprintf "   After repr:'%a' and %a\n" Clocks_printer.output expected_ck  Clocks_printer.output actual_ck; *)
    if expected_ck == actual_ck then ()
    else
      match expected_ck.desc, actual_ck.desc with
        | Clock_static, Clock_static -> ()
        | Clock_var, _ ->
            occur_check expected_ck.level expected_ck actual_ck;
            expected_ck.desc <- Clock_link actual_ck
        | _, Clock_var ->
            occur_check actual_ck.level actual_ck expected_ck;
            actual_ck.desc <- Clock_link expected_ck
        | Clock_depend c1, Clock_depend c2 -> carrier_unify c1 c2
        | Clock_product ck_l1, Clock_product ck_l2 -> unify_list ck_l1 ck_l2
        | Clock_arrow(ck1, ck2), Clock_arrow(ck3, ck4) ->
            unify ck1 ck3;
            unify ck2 ck4
        | Clock_constr(c1, ck_l1), Clock_constr(c2, ck_l2) when same_type_constr c1 c2 ->
            unify_list ck_l1 ck_l2
        | Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args), _ ->
          (*  Printf.eprintf "Replacing '%a' with '%a'\n"   Clocks_printer.output expected_ck   Clocks_printer.output (expand_abbrev params body args); *)
            unify (expand_abbrev params body args) actual_ck
        | _, Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args) ->
           (* Printf.eprintf "Replacing '%a' with '%a'\n"   Clocks_printer.output actual_ck   Clocks_printer.output (expand_abbrev params body args); *)
            unify expected_ck (expand_abbrev params body args)
        | Clock_process ck1, Clock_process ck2 -> unify ck1 ck2
        | _ ->
            (* Printf.eprintf "Failed to unify '%a' and '%a'\n"  Clocks_printer.output expected_ck  Clocks_printer.output actual_ck; *)
            raise Unify

and unify_list ck_l1 ck_l2 =
  try
    List.iter2 unify ck_l1 ck_l2
  with
    | Invalid_argument _ -> raise Unify

and carrier_unify expected_car actual_car =
  if expected_car == actual_car then ()
  else
    let expected_car = carrier_repr expected_car in
    let actual_car = carrier_repr actual_car in
    if expected_car == actual_car then ()
    else
      match expected_car.desc, actual_car.desc with
        | Carrier_var(s), _ ->
            carrier_occur_check expected_car.level expected_car actual_car;
            expected_car.desc <- Carrier_link actual_car
        | _, Carrier_var(s) ->
            carrier_occur_check actual_car.level actual_car expected_car;
            actual_car.desc <- Carrier_link expected_car
        | Carrier_skolem(_,i), Carrier_skolem(_, j) when i = j -> ()
        | _ -> raise Unify

(* special cases of unification *)
let rec filter_arrow ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_arrow(ck1, ck2) -> ck1, ck2
    | Clock_constr({ck_info = Some { constr_abbr = Constr_abbrev (params, body) }}, args) ->
        filter_arrow (expand_abbrev params body args)
    | _ ->
        let ck1 = new_clock_var () in
        let ck2 = new_clock_var () in
        unify ck (arrow ck1 ck2);
        ck1, ck2

let rec filter_product arity ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_product l ->
        if List.length l = arity then l else raise Unify
    | Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params, body) }}, args) ->
        filter_product arity (expand_abbrev params body args)
    | _ ->
        let ck_list = new_clock_var_list arity in
        unify ck (product ck_list);
        ck_list

let rec filter_depend ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_depend c -> c
    | _ ->
        let c = make_carrier generic_prefix_name in
        unify ck (depend c);
        c


let add_type_description g =
  { gi = g.gi;
    ty_info = None;
    ck_info = g.ck_info }
