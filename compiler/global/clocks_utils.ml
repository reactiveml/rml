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
open Clock_mapfold

exception Unify
exception Escape of string * int
exception Escape_case of string

(* generating fresh names *)
let names = new Ident.name_generator
let generic_prefix_name = "c"
let generic_carrier_row_name = "cr"
let generic_activation_name = "act"
let generic_effect_name = "e"
let generic_effect_row_name = "er"

(* The current nesting level of lets *)
let current_level = ref 0;;

let reset_type_var () =
  current_level := 0; ()
and push_type_level () =
  incr current_level; ()
and pop_type_level () =
  decr current_level; ()


(* making types *)
let make_generic desc =
  { desc = desc;
    level = generic;
    index = names#name; }
let make_local desc =
  { desc = desc;
    level = !current_level;
    index = names#name; }

(* Type variables *)
let new_clock_var () =
  make_local Clock_var
let new_generic_clock_var () =
  make_generic Clock_var

let new_carrier_var s =
  make_local (Carrier_var s)
let new_generic_carrier_var s =
  make_generic (Carrier_var s)

let new_carrier_row_var () =
  make_local Carrier_row_var
let new_generic_carrier_row_var () =
  make_generic Carrier_row_var

let new_effect_var () =
  make_local Effect_var
let new_generic_effect_var () =
  make_generic Effect_var

let new_effect_row_var () =
  make_local Effect_row_var
let new_generic_effect_row_var () =
  make_generic Effect_row_var

let new_react_var () =
  make_local React_var
let new_generic_react_var () =
  make_generic React_var

let new_var k = match k with
  | Kclock _ -> Kclock (new_clock_var ())
  | Kcarrier s -> Kcarrier (new_carrier_var s)
  | Kcarrier_row _ -> Kcarrier_row (new_carrier_row_var ())
  | Keffect _ -> Keffect (new_effect_var ())
  | Keffect_row _ -> Keffect_row (new_effect_row_var ())
  | Kreact _ -> Kreact (new_react_var ())

let new_generic_var k = match k with
  | Kclock _ -> Kclock (new_generic_clock_var ())
  | Kcarrier s -> Kcarrier (new_generic_carrier_var s)
  | Kcarrier_row _ -> Kcarrier_row (new_generic_carrier_row_var ())
  | Keffect _ -> Keffect (new_generic_effect_var ())
  | Keffect_row _ -> Keffect_row (new_generic_effect_row_var ())
  | Kreact _ -> Kreact (new_generic_react_var ())

let rec new_clock_var_list n =
  match n with
    | 0 -> []
    | n -> (new_clock_var ()) :: new_clock_var_list (n - 1)

(* Simple constructors for clocks *)
let static = make_generic Clock_static
let no_clock =
  { desc = Clock_static;
    level = generic;
    index = -1; }

let depend c =
  make_generic (Clock_depend c)

let product ck_list =
  make_generic (Clock_product(ck_list))

let constr ck_constr ck_list =
  make_generic (Clock_constr(ck_constr, ck_list))

let constr_notabbrev name ck_list =
  make_generic (Clock_constr({ gi = name;
                             ty_info = no_info ();
                             ck_info = Some {constr_abbr = Constr_notabbrev}; },
                          ck_list))

let arrow ck1 ck2 eff =
  make_generic (Clock_arrow(ck1, ck2, eff))

let rec arrow_list ck_l eff_l ck_res =
  match ck_l, eff_l with
    [], [] -> ck_res
  | [ck], [eff] -> arrow ck ck_res eff
  | ck :: ck_l, eff::eff_l -> arrow ck (arrow_list ck_l eff_l ck_res) eff
  | _, _ -> invalid_arg "arrow_list"

let process ck activation_car eff r =
  make_generic (Clock_process (ck, activation_car, eff, r))

(* Constructors for carriers *)

let carrier_skolem s i =
  make_local (Carrier_skolem(s, i))

let no_carrier =
  { desc = Carrier_var "";
    index = -1;
    level = generic }
let topck_carrier =
  { desc = Carrier_skolem ("topck", names#name);
    index = -2;
    level = notgeneric }
let clock_topck =
  make_generic (Clock_depend topck_carrier)

(* Constructors for carrier rows *)

let carrier_row_empty =
  { desc = Carrier_row_empty;
    level = generic;
    index = -2 }

let carrier_row cr1 cr2 =
  make_generic (Carrier_row (cr1, cr2))

let carrier_row_one c =
  make_generic (Carrier_row_one c)

let rec carrier_row_list l = match l with
  | [] -> carrier_row_empty
  | [c] -> c
  | c::c_l -> carrier_row c (carrier_row_list c_l)

let carrier_closed_row c =
  carrier_row (carrier_row_one c) carrier_row_empty
let carrier_open_row c =
  carrier_row (carrier_row_one c) (new_carrier_row_var ())

(* Constructors for effects *)

let no_effect =
 { desc = Effect_empty;
   level = generic;
   index = -1; }

let eff_sum eff1 eff2 = match eff1.desc, eff2.desc with
  | Effect_empty, _ -> eff2
  | _, Effect_empty -> eff1
  | _, _ -> make_generic (Effect_sum (eff1, eff2))

let rec eff_sum_list l = match l with
  | [] -> no_effect
  | [eff] -> eff
  | eff::eff_l -> eff_sum eff (eff_sum_list eff_l)

let eff_depend c =
  make_generic (Effect_depend c)

let eff_one er =
  make_generic (Effect_one er)

(* Constructors for effect rows *)

let eff_row er1 er2 =
  make_generic (Effect_row (er1, er2))

let eff_row_one eff =
  make_generic (Effect_row_one eff)

let effect_row_empty =
  { desc = Effect_row_empty;
    level = generic;
    index = -1; }

let eff_open_row eff =
  eff_row (eff_row_one eff) (new_effect_row_var ())
let eff_closed_row eff =
  eff_row (eff_row_one eff) effect_row_empty

let comb_arrow ck1 ck2 =
  arrow ck1 ck2 (eff_closed_row no_effect)

(* Constructors for reactivity effects *)

let no_react =
  { desc = React_empty;
    level = generic;
    index = -1; }

let react_carrier c = make_generic (React_carrier c)

let react_seq r1 r2 = match r1.desc, r2.desc with
  | React_seq rl1, React_seq rl2 -> make_generic (React_seq (rl1@rl2))
  | React_seq rl1, _ -> make_generic (React_seq (rl1 @ [r2]))
  | _, React_seq rl2 -> make_generic (React_seq (r1::rl2))
  | React_empty, _ -> r2
  | _, React_empty -> r1
  | React_carrier c1, React_carrier c2 when c1 == c2 -> r1
  | _, _ -> make_generic (React_seq [r1; r2])

let react_par r1 r2 = match r1.desc, r2.desc with
  | React_par rl1, React_par rl2 -> make_generic (React_par (rl1@rl2))
  | React_par rl1, _ -> make_generic (React_par (r2::rl1))
  | _, React_par rl2 -> make_generic (React_par (r1::rl2))
  | React_empty, _ -> r2
  | _, React_empty -> r1
  | _, _ -> make_generic (React_par [r1; r2])

let react_or r1 r2 =
  let desc = match r1.desc, r2.desc with
    | React_or rl1, React_or rl2 -> React_or (rl1@rl2)
    | React_or rl1, _ -> React_or (r2::rl1)
    | _, React_or rl2 -> React_or (r1::rl2)
    | _, _ -> React_or [r1; r2]
  in
  make_generic desc

let react_loop r =
  let var = new_react_var () in
  let body = react_seq r var in
  make_generic (React_rec (false, var, body))

(* Type schemes *)

let forall vars typ =
  { cs_vars = vars;
    cs_desc = typ; }

let ty_forall params ck =
  make_generic (Clock_forall (forall params ck))

let clock_of_sch sch =
  make_generic (Clock_forall sch)

(* activation clock*)
let activation_carrier = ref topck_carrier
let current_effect = ref no_effect
let current_react = ref no_react

(* To take the canonical representative of a type.
   We do path compression there. *)

let rec carrier_repr car = match car.desc with
  | Carrier_link c ->
      let c = carrier_repr c in
      car.desc <- Carrier_link c;
      c
  | _ -> car

let rec carrier_row_repr car = match car.desc with
  | Carrier_row_link c ->
      let c = carrier_row_repr c in
      car.desc <- Carrier_row_link c;
      c
  | _ -> car

let rec clock_repr ck = match ck.desc with
  | Clock_link t ->
      let t = clock_repr t in
      ck.desc <- Clock_link t;
      t
  | _ ->
      ck

let rec effect_repr eff = match eff.desc with
  | Effect_link t ->
      let t = effect_repr t in
      eff.desc <- Effect_link t;
      t
  | _ ->
      eff

let rec effect_row_repr eff = match eff.desc with
  | Effect_row_link t ->
      let t = effect_row_repr t in
      eff.desc <- Effect_row_link t;
      t
  | _ ->
      eff

let rec react_repr r = match r.desc with
  | React_link t ->
      let t = react_repr t in
      r.desc <- React_link t;
      t
  | _ ->
      r

let repr_record =
  mk_kind_prod ~clock:clock_repr ~carrier:carrier_repr ~carrier_row:carrier_row_repr
    ~effect:effect_repr ~effect_row:effect_row_repr ~react:react_repr

let get_eff er =
  match er.desc with
    | Effect_row_one eff -> eff
    | _ -> Printf.eprintf "Unexpected effect row: %a\n" Clocks_printer.output_effect_row er; assert false

let get_car cr =
  match cr.desc with
    | Carrier_row_one car -> car
    | _ -> Printf.eprintf "Unexpected carrier row: %a\n" Clocks_printer.output_carrier_row cr; assert false

(* simplification of effects *)
let equal_carrier c1 c2 =
  let c1 = carrier_repr c1 in
  let c2 = carrier_repr c2 in
  match c1.desc, c2.desc with
    | Carrier_var _, Carrier_var _ -> c1.index = c2.index
    | Carrier_skolem (_, i1), Carrier_skolem (_, i2) -> i1 = i2
    | _ -> false

(* Compares two effects, but only if they are leaves *)
let equal_effect eff1 eff2 =
  let eff1 = effect_repr eff1 in
  let eff2 = effect_repr eff2 in
  match eff1.desc, eff2.desc with
    | Effect_var, Effect_var -> eff1.index = eff2.index
    | Effect_depend c1, Effect_depend c2 -> equal_carrier (get_car c1) (get_car c2)
    | _ -> false

let simplify_effect eff =
  let rec mem_effect eff l = match l with
    | [] -> false
    | eff2::l -> equal_effect eff eff2 || mem_effect eff l
  in
  let add_to_list x l =
    if mem_effect x l then l else x::l
  in
  let rec clocks_of acc eff = match eff.desc with
    | Effect_empty | Effect_one _ -> acc
    | Effect_var | Effect_depend _ -> add_to_list eff acc
    | Effect_link link -> clocks_of acc link
    | Effect_sum (eff1, eff2) -> clocks_of (clocks_of acc eff2) eff1
  in
  let eff = effect_repr eff in
  let new_eff = eff_sum_list (clocks_of [] eff) in
  new_eff

let simplify_effect eff =
  if !Compiler_options.use_row_clocking then eff (* TODO *) else simplify_effect eff

(*
let rec remove_ck_from_carrier ck car = match car.desc with
  | Carrier_var _ -> car
  | Carrier_skolem _ ->
      if ck.index = car.index then carrier_empty else car
  | Carrier_link link -> remove_ck_from_carrier ck link
*)

let rec remove_var_from_effect index eff = match eff.desc with
  | Effect_var ->
      if eff.index = index then
        no_effect
      else
        eff
  | Effect_empty | Effect_depend _ | Effect_one _ -> eff
  | Effect_link link -> remove_var_from_effect index link
  | Effect_sum (eff1, eff2) ->
      { eff with desc = Effect_sum (remove_var_from_effect index eff1,
                                   remove_var_from_effect index eff2) }
let remove_var_from_effect index eff =
  simplify_effect (remove_var_from_effect index eff)

(* Remove local clock from effect without rows *)
let rec remove_ck_from_effect ck eff = match eff.desc with
  | Effect_depend { desc = Carrier_row_one c } ->
      if (carrier_repr c).desc = ck.desc then
        no_effect
      else
        eff
  | Effect_empty | Effect_var | Effect_one _ | Effect_depend _ -> eff
  | Effect_link link -> remove_ck_from_effect ck link
  | Effect_sum (eff1, eff2) ->
      { eff with desc = Effect_sum (remove_ck_from_effect ck eff1,
                                   remove_ck_from_effect ck eff2) }
let remove_ck_from_effect_simple ck eff =
  simplify_effect (remove_ck_from_effect ck eff)

(* Remove local clock from effect with rows *)
let rec remove_ck_from_carrier_row ck cr = match cr.desc with
  | Carrier_row_empty | Carrier_row_var -> cr
  | Carrier_row_one car ->
    let car = carrier_repr car in
    if ck.index = car.index then carrier_row_empty else cr
  | Carrier_row (cr1, cr2) ->
    { cr with desc = Carrier_row (remove_ck_from_carrier_row ck cr1,
                                   remove_ck_from_carrier_row ck cr2) }
  | Carrier_row_link link -> remove_ck_from_carrier_row ck link

let rec remove_ck_from_effect ck eff = match eff.desc with
  | Effect_empty | Effect_var -> eff
  | Effect_depend cr ->
    { eff with desc = Effect_depend (remove_ck_from_carrier_row ck cr) }
  | Effect_one er ->
    { eff with desc = Effect_one (remove_ck_from_effect_row ck er) }
  | Effect_sum (eff1, eff2) ->
      { eff with desc = Effect_sum (remove_ck_from_effect ck eff1,
                                   remove_ck_from_effect ck eff2) }
  | Effect_link link -> remove_ck_from_effect ck link
(*and remove_ck_from_effect ck eff =
  simplify_effect (remove_ck_from_effect ck eff) *)

and remove_ck_from_effect_row ck eff = match eff.desc with
  | Effect_row_var | Effect_row_empty -> eff
  | Effect_row_one eff1 ->
    { eff with desc = Effect_row_one (remove_ck_from_effect ck eff1) }
  | Effect_row (eff1, eff2) ->
    { eff with desc = Effect_row (remove_ck_from_effect_row ck eff1,
                                  remove_ck_from_effect_row ck eff2) }
  | Effect_row_rec (var, body) ->
    { eff with desc = Effect_row_rec (var, remove_ck_from_effect_row ck body) }
  | Effect_row_link link -> remove_ck_from_effect_row ck link

let remove_ck_from_effect ck eff =
  if !Compiler_options.use_row_clocking then
    remove_ck_from_effect ck eff
  else
    remove_ck_from_effect_simple ck eff

let subst_var_effect_row index subst er =
  let effect_row funs () er = match er.desc with
    | Effect_row_var when er.index = index -> subst, ()
    | _ ->
      let er, _ = Clock_mapfold.effect_row funs () er in
      er, ()
  in
  let funs = { Clock_mapfold.defaults with effect_row = effect_row } in
  fst (Clock_mapfold.effect_row_it funs () er)

let rec remove_local_from_carrier_row level cr = match cr.desc with
  | Carrier_row_empty | Carrier_row_one _ -> cr
  | Carrier_row_var ->
      if cr.level > level then
        carrier_row_empty
      else
        cr
  | Carrier_row (cr1, cr2) ->
    { cr with desc = Carrier_row (remove_local_from_carrier_row level cr1,
                                  remove_local_from_carrier_row level cr2) }
  | Carrier_row_link link -> remove_local_from_carrier_row level link

let rec remove_local_from_effect level bound_vars eff = match eff.desc with
  | Effect_empty | Effect_var -> eff
  | Effect_depend cr ->
    { eff with desc = Effect_depend (remove_local_from_carrier_row level cr) }
  | Effect_one er ->
    { eff with desc = Effect_one (remove_local_from_effect_row level bound_vars er) }
  | Effect_sum (eff1, eff2) ->
    { eff with desc = Effect_sum (remove_local_from_effect level bound_vars eff1,
                                  remove_local_from_effect level bound_vars eff2) }
  | Effect_link link -> remove_local_from_effect level bound_vars link

and remove_local_from_effect_row level bound_vars eff = match eff.desc with
  | Effect_row_empty -> eff
  | Effect_row_var ->
      if eff.level > level && not (List.memq eff bound_vars) then
        effect_row_empty
      else
        eff
  | Effect_row_one eff1 ->
     { eff with desc = Effect_row_one (remove_local_from_effect level bound_vars eff1) }
  | Effect_row (eff1, eff2) ->
    { eff with desc = Effect_row (remove_local_from_effect_row level bound_vars eff1,
                                  remove_local_from_effect_row level bound_vars eff2) }
  | Effect_row_rec (var, body) ->
    { eff with
      desc = Effect_row_rec (var, remove_local_from_effect_row level (var::bound_vars) body) }
  | Effect_row_link link -> remove_local_from_effect_row level bound_vars link

(* Operations on reactivity effects *)
let rec remove_ck_from_react ck r = match r.desc with
  | React_empty | React_var -> r
  | React_carrier cr ->
    let cr = remove_ck_from_carrier_row ck cr in
    (match cr.desc with
      | Carrier_row_empty -> no_react
      | _ -> { r with desc = React_carrier cr })
  | React_seq rl ->
      { r with desc = React_seq (List.map (remove_ck_from_react ck) rl) }
  | React_par rl ->
      { r with desc = React_par (List.map (remove_ck_from_react ck) rl) }
  | React_or rl ->
      { r with desc = React_or (List.map (remove_ck_from_react ck) rl) }
  | React_run r1 ->
      { r with desc = React_run (remove_ck_from_react ck r1) }
  | React_rec (b, r1, r2) ->
      { r with desc = React_rec (b, r1, remove_ck_from_react ck r2) }
  | React_link link -> remove_ck_from_react ck link

let is_not_local_var level bound_vars r = match r.desc with
  | React_var when r.level > level && not (List.memq r bound_vars) -> false
  | _ -> true

let rec remove_local_from_react level bound_vars r = match r.desc with
  | React_empty | React_var | React_carrier _ -> r
  | React_seq rl ->
      { r with desc = React_seq (List.map (remove_local_from_react level bound_vars) rl) }
  | React_par rl ->
      { r with desc = React_par (List.map (remove_local_from_react level bound_vars) rl) }
  | React_or rl ->
      let rl = List.filter (is_not_local_var level bound_vars) rl in
      (match rl with
        | [] -> assert false
        | [r] -> r
        | _ -> { r with desc = React_or (List.map (remove_local_from_react level bound_vars) rl) })
  | React_run r1 ->
      { r with desc = React_run (remove_local_from_react level bound_vars r1) }
  | React_rec (b, r1, r2) ->
      { r with desc = React_rec (b, r1, remove_local_from_react level (r1::bound_vars) r2) }
  | React_link link -> remove_local_from_react level bound_vars link

let subst_var_react index subst r =
  let react_effect funs () r =  match r.desc with
    | React_var when r.index = index -> subst, ()
    | _ -> Clock_mapfold.react_effect funs () r
  in
  let funs = { Clock_mapfold.defaults with react_effect = react_effect } in
  fst (Clock_mapfold.react_effect_it funs () r)

(* To generalize a type *)

(* generalisation and non generalisation of a type. *)
(* the level of generalised type variables *)
(* is set to [generic] when the flag [is_gen] is true *)
(* and set to [!binding_level] when the flag is false *)
(* returns [generic] when a sub-term can be generalised *)

let list_of_vars = ref []

let rec gen_clock is_gen ck =
  let ck = clock_repr ck in
  (match ck.desc with
    | Clock_static -> ()
    | Clock_var ->
        if ck.level > !current_level then
          if is_gen then (
            ck.level <- generic;
            list_of_vars := (Kclock ck) :: !list_of_vars
          ) else
            ck.level <- !current_level
    | Clock_depend car ->
        ck.level <- gen_carrier is_gen car
    | Clock_arrow(ck1, ck2, eff) ->
        let level1 = gen_clock is_gen ck1 in
        let level2 = gen_clock is_gen ck2 in
        let level_eff = gen_effect_row is_gen eff in
        ck.level <- min level1 (min level2 level_eff)
    | Clock_product ck_list ->
        ck.level <- gen_clock_list is_gen ck_list
    | Clock_constr(name, param_list) ->
        ck.level <- gen_param_list is_gen param_list
    | Clock_link(link) ->
        ck.level <- gen_clock is_gen link
    | Clock_process (body_ck, act_car, eff, r) ->
        let level_eff = gen_effect_row is_gen eff in
        let level_r = gen_react is_gen r in
        let level_body = gen_clock is_gen body_ck in
        let level_act = gen_carrier is_gen act_car in
        ck.level <- min (min (min level_r level_eff) level_body) level_act
    | Clock_forall sch -> (* TODO *) ()
  );
  ck.level

and gen_clock_list is_gen ck_list =
  List.fold_left (fun level ck -> min level (gen_clock is_gen ck)) notgeneric ck_list

and gen_carrier is_gen car =
  let car = carrier_repr car in
  (match car.desc with
    | Carrier_var _ ->
        if car.level > !current_level then
          if is_gen then (
            car.level <- generic;
            list_of_vars := (Kcarrier car) :: !list_of_vars
          ) else
            car.level <- !current_level
    | Carrier_skolem _ -> ()
    | Carrier_link(link) -> car.level <- gen_carrier is_gen link
  );
  car.level

and gen_carrier_row is_gen cr =
  let cr = carrier_row_repr cr in
  (match cr.desc with
    | Carrier_row_var ->
      if cr.level > !current_level then
        if is_gen then (
          cr.level <- generic;
          list_of_vars := (Kcarrier_row cr) :: !list_of_vars
        ) else
          cr.level <- !current_level
    | Carrier_row_empty -> ()
    | Carrier_row_one c -> cr.level <- (gen_carrier is_gen c)
    | Carrier_row (cr1, cr2) ->
        cr.level <- min (gen_carrier_row is_gen cr1) (gen_carrier_row is_gen cr2)
    | Carrier_row_link link ->
        cr.level <- gen_carrier_row is_gen link
  );
  cr.level

and gen_effect is_gen eff =
  let eff = effect_repr eff in
  (match eff.desc with
    | Effect_empty -> ()
    | Effect_sum (eff1, eff2) ->
        eff.level <- min (gen_effect is_gen eff1) (gen_effect is_gen eff2)
    | Effect_depend c ->
        eff.level <- gen_carrier_row is_gen c
    | Effect_one er ->
        eff.level <- gen_effect_row is_gen er
    | Effect_var ->
        if eff.level > !current_level then
          if is_gen then (
            eff.level <- generic;
            list_of_vars := (Keffect eff) :: !list_of_vars
          ) else
            eff.level <- !current_level
    | Effect_link(link) ->
        eff.level <- gen_effect is_gen link
  );
  eff.level

and gen_effect_row is_gen eff =
  let eff = effect_row_repr eff in
  (match eff.desc with
    | Effect_row_var ->
      if eff.level > !current_level then
        if is_gen then (
          eff.level <- generic;
          list_of_vars := (Keffect_row eff) :: !list_of_vars
        ) else
          eff.level <- !current_level
    | Effect_row_empty -> ()
    | Effect_row_one eff1 -> eff.level <- gen_effect is_gen eff1
    | Effect_row (eff1, eff2) | Effect_row_rec (eff1, eff2) ->
      eff.level <- min (gen_effect_row is_gen eff1) (gen_effect_row is_gen eff2)
    | Effect_row_link link -> eff.level <- gen_effect_row is_gen link
  );
  eff.level

and gen_react is_gen r =
  let r = react_repr r in
  (match r.desc with
    | React_empty -> ()
    | React_carrier c -> r.level <- gen_carrier_row is_gen c
    | React_seq rl | React_par rl | React_or rl ->
        r.level <- gen_react_list is_gen rl
    | React_rec (_, r1, r2) ->
        r.level <- min (gen_react is_gen r1) (gen_react is_gen r2)
    | React_run r2 ->
        let _ = react_repr r2 in
        r.level <- gen_react is_gen r2
    | React_var ->
        if r.level > !current_level then (
          if is_gen then (
            list_of_vars := (Kreact r) :: !list_of_vars;
            r.level <- generic
          ) else
            r.level <- !current_level
        )
    | React_link link -> r.level <- gen_react is_gen link
  );
  r.level

and gen_react_list is_gen rl =
  List.fold_left (fun level r -> min level (gen_react is_gen r)) notgeneric rl

and gen_param is_gen x =
  let gen_record =
    mk_kind_prod ~clock:gen_clock ~carrier:gen_carrier ~carrier_row:gen_carrier_row
      ~effect:gen_effect ~effect_row:gen_effect_row ~react:gen_react
  in
  kind_fold gen_record is_gen x
and gen_param_list is_gen param_list =
  List.fold_left (fun level p -> min level (gen_param is_gen p)) notgeneric param_list

(* main generalisation function *)
let gen ck =
  (*Printf.eprintf "generalizing %a\n" Clocks_printer.output ck;*)
  list_of_vars:= [];
  let _ = gen_clock true ck in
  { cs_vars = !list_of_vars;
    cs_desc = ck }

let non_gen ck = ignore (gen_clock false ck)

(* To compute the free type variables in a type *)
let free_clock_vars level ck =
  let fv = ref [] in
  let rec free_vars ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_static-> ()
      | Clock_var ->
          if ck.level >= level then fv := (Kclock ck) :: !fv
      | Clock_arrow(t1,t2, eff) ->
          free_vars t1; free_vars t2; free_vars_effect_row eff
      | Clock_product ck_list ->
          List.iter free_vars ck_list
      | Clock_constr (c, param_list) ->
          List.iter free_vars_param param_list
      | Clock_link link ->
          free_vars link
      | Clock_process (ck, act, eff, r) ->
          free_vars ck;
          free_vars_carrier act;
          free_vars_effect_row eff;
          free_vars_react [] r
      | Clock_depend c -> free_vars_carrier c
      | Clock_forall sch -> (*TODO*) ()
  and free_vars_carrier car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_skolem _ -> ()
      | Carrier_var _ ->
          if car.level >= level then fv := (Kcarrier car) :: !fv
      | Carrier_link link -> free_vars_carrier link
  and free_vars_carrier_row cr =
    let cr = carrier_row_repr cr in
    match cr.desc with
      | Carrier_row_empty -> ()
      | Carrier_row_var ->
          if cr.level >= level then fv := (Kcarrier_row cr) :: !fv
      | Carrier_row_one c -> free_vars_carrier c
      | Carrier_row (cr1, cr2) ->
          free_vars_carrier_row cr1; free_vars_carrier_row cr2
      | Carrier_row_link link -> free_vars_carrier_row link
  and free_vars_effect eff =
    let eff = effect_repr eff in
    match eff.desc with
      | Effect_empty -> ()
      | Effect_var ->
          if eff.level >= level then fv := (Keffect eff) :: !fv
      | Effect_sum (eff1, eff2) ->
          free_vars_effect eff1; free_vars_effect eff2
      | Effect_depend c ->
          free_vars_carrier_row c
      | Effect_one er ->
          free_vars_effect_row er
      | Effect_link link -> free_vars_effect link
  and free_vars_effect_row eff =
    let eff = effect_row_repr eff in
    match eff.desc with
      | Effect_row_empty -> ()
      | Effect_row_var ->
          if eff.level >= level then fv := (Keffect_row eff) :: !fv
      | Effect_row_one e -> free_vars_effect e
      | Effect_row (eff1, eff2) | Effect_row_rec (eff1, eff2) ->
          free_vars_effect_row eff1; free_vars_effect_row eff2
      | Effect_row_link link -> free_vars_effect_row link
  and free_vars_react bound r =
    let r = react_repr r in
    match r.desc with
      | React_var ->
          if r.level >= level && (not (List.memq r bound))
          then fv := (Kreact r) :: !fv
      | React_empty -> ()
      | React_carrier c -> free_vars_carrier_row c
      | React_seq rl | React_par rl | React_or rl ->
          List.iter (free_vars_react bound) rl
      | React_rec (_, r1, r2) -> free_vars_react (r1::bound) r2
      | React_run r  | React_link r -> free_vars_react bound r
  and free_vars_param p =
    let f =
      mk_kind_prod ~clock:free_vars ~carrier:free_vars_carrier
        ~carrier_row:free_vars_carrier_row ~effect:free_vars_effect
        ~effect_row:free_vars_effect_row ~react:(free_vars_react [])
    in
    kind_iter f p
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

let saves =
  mk_kind_prod ~clock:(new save) ~carrier:(new save) ~carrier_row:(new save)
    ~effect:(new save) ~effect_row:(new save) ~react:(new save)
let cleanup () =
  saves.k_clock#cleanup;
  saves.k_carrier#cleanup;
  saves.k_carrier_row#cleanup;
  saves.k_effect#cleanup;
  saves.k_effect_row#cleanup;
  saves.k_react#cleanup

(* makes a copy of a type *)
let rec copy_subst_clock m ck =
  let level = ck.level in
  match ck.desc with
    | Clock_var ->
        if level = generic then
          let v = new_clock_var () in
          ck.desc <- Clock_link(v);
          saves.k_clock#save ck Clock_var;
          v
        else
          ck
    | Clock_static -> ck
    | Clock_depend c ->
        if level = generic then
          depend (copy_subst_carrier m c)
        else
          ck
    | Clock_link link ->
        if level = generic then
          link
        else
          copy_subst_clock m link
    | Clock_arrow (ck1, ck2, eff) ->
        if level = generic then
          arrow (copy_subst_clock m ck1) (copy_subst_clock m ck2) (copy_subst_effect_row m eff)
        else
          ck
    | Clock_product ck_list ->
        if level = generic then
          product (List.map (copy_subst_clock m) ck_list)
        else
          ck
    | Clock_constr (name, param_list) ->
        if level = generic then
          constr name (List.map (copy_subst_param m) param_list)
        else
          ck
    | Clock_process (ck1, act_car, eff, r) ->
        if level = generic then
          process (copy_subst_clock m ck1) (copy_subst_carrier m act_car)
            (copy_subst_effect_row m eff) (copy_subst_react m r)
        else
          ck
    | Clock_forall sch ->
      let cs_vars = List.map no_copy_param sch.cs_vars in
      let cs_desc = copy_subst_clock m sch.cs_desc in
      let sch = forall cs_vars cs_desc in
      make_generic (Clock_forall sch)

and copy_subst_carrier m car =
  let level = car.level in
  match car.desc with
    | Carrier_var s ->
        if level = generic then
          (try
            List.assoc car.index m
          with
            | Not_found ->
                let v = new_carrier_var s in
                car.desc <- Carrier_link v;
                saves.k_carrier#save car (Carrier_var s);
                v
          )
        else
          car
  | Carrier_link(link) ->
      if level = generic then
        link
      else
        copy_subst_carrier m link
  | Carrier_skolem(s, i) ->
      if level = generic then
        carrier_skolem s i
      else
        car

and copy_subst_carrier_row m cr =
  let level = cr.level in
  match cr.desc with
    | Carrier_row_empty -> cr
    | Carrier_row_var ->
      if level = generic then
        let v = new_carrier_row_var () in
        cr.desc <- Carrier_row_link v;
        saves.k_carrier_row#save cr Carrier_row_var;
        v
      else
        cr
    | Carrier_row_one c ->
      if level = generic then
        carrier_row_one (copy_subst_carrier m c)
      else
        cr
    | Carrier_row (cr1, cr2) ->
      if level = generic then
        carrier_row (copy_subst_carrier_row m cr1) (copy_subst_carrier_row m cr2)
      else
        cr
    | Carrier_row_link(link) ->
      if level = generic then
        link
      else
        copy_subst_carrier_row m link

and copy_subst_effect m eff =
  let level = eff.level in
  match eff.desc with
    | Effect_empty -> eff
    | Effect_sum (eff1, eff2) ->
        if level = generic then
          eff_sum (copy_subst_effect m eff1) (copy_subst_effect m eff2)
        else
          eff
    | Effect_var ->
        if level = generic then
          let v = new_effect_var () in
          eff.desc <- Effect_link v;
          saves.k_effect#save eff Effect_var;
          v
        else
          eff
    | Effect_link(link) ->
        if level = generic then
          link
        else
          copy_subst_effect m link
    | Effect_depend c ->
        if level = generic then
          eff_depend (copy_subst_carrier_row m c)
        else
          eff
    | Effect_one er ->
        if level = generic then
          eff_one (copy_subst_effect_row m er)
        else
          eff

and copy_subst_effect_row m eff =
  let level = eff.level in
  match eff.desc with
    | Effect_row_empty -> eff
    | Effect_row_var ->
      if level = generic then
        let v = new_effect_row_var () in
        eff.desc <- Effect_row_link v;
        saves.k_effect_row#save eff Effect_row_var;
        v
      else
        eff
    | Effect_row_one eff1 ->
      if level = generic then
        eff_row_one (copy_subst_effect m eff1)
      else
        eff
    | Effect_row (eff1, eff2) ->
      if level = generic then
        eff_row (copy_subst_effect_row m eff1) (copy_subst_effect_row m eff2)
      else
        eff
    | Effect_row_rec (eff1, eff2) ->
      if level = generic then
        make_generic (Effect_row_rec (copy_subst_effect_row m eff1, copy_subst_effect_row m eff2))
      else
        eff
    | Effect_row_link link ->
      if level = generic then
        link
      else
        copy_subst_effect_row m link

and copy_subst_react m r =
  let level = r.level in
  match r.desc with
    | React_empty -> r
    | React_var ->
        if level = generic then
          let v = new_react_var () in
          r.desc <- React_link v;
          saves.k_react#save r React_var;
          v
        else
          r
    | React_carrier c ->
        if level = generic then
          make_generic (React_carrier (copy_subst_carrier_row m c))
        else
          r
    | React_seq rl ->
        if level = generic then
          make_generic (React_seq (List.map (copy_subst_react m) rl))
        else
          r
    | React_par rl ->
        if level = generic then
          make_generic (React_par (List.map (copy_subst_react m) rl))
        else
          r
    | React_or rl ->
        if level = generic then
          make_generic (React_or (List.map (copy_subst_react m) rl))
        else
          r
    | React_rec (b, r1, r2) ->
        if level = generic then
          make_generic (React_rec (b, copy_subst_react m r1, copy_subst_react m r2))
        else
          r
    | React_run r2 ->
        if level = generic then
          make_generic (React_run (copy_subst_react m r2))
        else
          r
    | React_link link ->
        if level = generic then
          link
        else
          copy_subst_react m link

and copy_subst_param m p =
  let copy_subst_record =
    mk_kind_prod ~clock:copy_subst_clock ~carrier:copy_subst_carrier
      ~carrier_row:copy_subst_carrier_row ~effect:copy_subst_effect
      ~effect_row:copy_subst_effect_row ~react:copy_subst_react
  in
  kind_map_env copy_subst_record m p

(* For variables in a Clock_forall, generate a generic variable *)
and no_copy_param p = match p with
  | Kcarrier ({ desc = Carrier_var s } as car) ->
      let v = new_generic_carrier_var s in
      car.desc <- Carrier_link v;
      saves.k_carrier#save car (Carrier_var s);
      Kcarrier v
  | Kcarrier_row ({ desc = Carrier_row_var } as cr) ->
      let v = new_generic_carrier_row_var () in
      cr.desc <- Carrier_row_link v;
      saves.k_carrier_row#save cr Carrier_row_var;
      Kcarrier_row v
  | _ -> p

let copy_clock ck = copy_subst_clock [] ck
let copy_param p = copy_subst_param [] p

(* instanciation *)
let instance { cs_desc = ck } =
  (*Printf.eprintf "before: %a\n" Clocks_printer.output ck;*)
  let ck_i = copy_clock ck in
  cleanup ();
  (*Printf.printf "After: %a\n" Clocks_printer.output ck_i;*)
  ck_i


let instance_and_vars { cs_vars = vars; cs_desc = ck } =
  let ck_i = copy_clock ck in
  let vars = List.map (kind_map repr_record) vars in
  cleanup ();
  vars, ck_i

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

let rec ensure_monotype ck =
  let ck = clock_repr ck in
  match ck.desc with
  | Clock_forall sch -> ensure_monotype (instance sch)
  | _ -> ck

(* the occur check *)
let is_rec = ref false
let rec occur_check level index ck =
  let rec check ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_var ->
          (*Printf.eprintf "Occur_check : level:%d  index:%a   ck.level:%d  ck:%a\n"  level  Clocks_printer.output index  ck.level Clocks_printer.output ck;*)
        if ck.index = index then
          raise Unify
        else if ck.level > level then
          ck.level <- level
      | Clock_static -> ()
      | Clock_depend c -> carrier_occur_check level index c
      | Clock_arrow (ck1, ck2, eff) ->
          check ck1; check ck2;
          effect_row_occur_check level index eff
      | Clock_product ck_list -> List.iter check ck_list
      | Clock_constr(_, param_list) -> List.iter (param_occur_check level index) param_list
      | Clock_link link -> check link
      | Clock_process (ck, act_car, eff, r) ->
          check ck;
          carrier_occur_check level index act_car;
          effect_row_occur_check level index eff;
          react_occur_check level index r
      | Clock_forall sch -> (*TODO*) ()
  in
  (*Printf.eprintf "Occur_check : level:%d   index:%a   ck:%a\n"  level  Clocks_printer.output index  Clocks_printer.output ck;*)
  check ck

and carrier_occur_check level index car =
  let rec check car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_var _ ->
        (*  Format.eprintf "Var occur check: %s car.level %d, level: %d@." s car.level level; *)
          if car.index = index then
            raise Unify
          else if car.level > level then
            car.level <- level
      | Carrier_skolem(s, i) ->
          (*Format.eprintf "Skolem occur check: %s i:%d  car.level %d, level: %d\n" s i car.level level;*)
          if level  < car.level then
            raise (Escape (s, i))
      | Carrier_link link -> check link
  in
  check car

and carrier_row_occur_check level index cr =
  let rec check cr =
    let cr = carrier_row_repr cr in
    match cr.desc with
      | Carrier_row_empty -> ()
      | Carrier_row_var ->
        (*  Format.eprintf "Var occur check: %s car.level %d, level: %d@." s car.level level; *)
        if cr.index = index then
          raise Unify
        else if cr.level > level then
          cr.level <- level
      | Carrier_row(cr1, cr2) -> check cr1; check cr2
      | Carrier_row_one c -> carrier_occur_check level index c
      | Carrier_row_link link -> check link
  in
  check cr

and effect_occur_check level index eff =
  let rec check eff =
    let eff = effect_repr eff in
    match eff.desc with
      | Effect_var ->
          if eff.index = index && !Compiler_options.use_row_clocking then
            raise Unify
          else if eff.level > level then
            eff.level <- level
      | Effect_empty -> ()
      | Effect_depend c -> carrier_row_occur_check level index c
      | Effect_one er -> effect_row_occur_check level index er
      | Effect_sum (eff1, eff2) -> check eff1; check eff2
      | Effect_link link -> check link
  in
  check eff

and effect_row_occur_check level index eff =
  let rec check eff =
    let eff = effect_row_repr eff in
    match eff.desc with
      | Effect_row_var ->
        if eff.index = index then
          is_rec := true
        else if eff.level > level then
          eff.level <- level
      | Effect_row_empty -> ()
      | Effect_row_one eff1 -> effect_occur_check level index eff1
      | Effect_row (eff1, eff2) | Effect_row_rec (eff1, eff2) -> check eff1; check eff2
      | Effect_row_link link -> check link
  in
  check eff

and react_occur_check level index r =
  let rec check r =
    let r = react_repr r in
    match r.desc with
      | React_empty -> ()
      | React_var ->
          if r.index = index then (* go on to fix levels *)
            is_rec := true
          else if r.level > level then
            r.level <- level
      | React_carrier c -> carrier_row_occur_check level index c
      | React_seq rl | React_par rl | React_or rl ->
          List.iter check rl
      | React_run r1 -> check r1
      | React_rec (_, _, r2) -> (* TODO: parcourir r1 ?*)
          check r2
      | React_link link -> check link
  in
  check r

and param_occur_check level index p = match p with
  | Kclock c -> occur_check level index c
  | Kcarrier c -> carrier_occur_check level index c
  | Kcarrier_row c -> carrier_row_occur_check level index c
  | Keffect eff -> effect_occur_check level index eff
  | Keffect_row eff -> effect_row_occur_check level index eff
  | Kreact r -> react_occur_check level index r

let effect_row_occur_check_is_rec level index r =
  is_rec := false;
  effect_row_occur_check level index r;
  !is_rec

let react_occur_check_is_rec level index r =
  is_rec := false;
  ignore (react_occur_check level index r);
  !is_rec

(* the occur check *)
(*
let rec skolem_check skolems ck =
  let rec check ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_var | Clock_static -> ()
      | Clock_depend c -> carrier_skolem_check skolems c
      | Clock_arrow (ck1, ck2, eff) ->
          check ck1; check ck2;
          effect_skolem_check skolems eff
      | Clock_product ck_list -> List.iter check ck_list
      | Clock_constr(_, param_list) -> List.iter (param_skolem_check skolems) param_list
      | Clock_link link -> check link
      | Clock_process (ck, act_car, eff, r) ->
          check ck;
          carrier_skolem_check skolems act_car;
          effect_skolem_check skolems eff;
          react_skolem_check skolems r
      | Clock_forall sch -> (*TODO*) ()
  in
  check ck

and carrier_skolem_check skolems car =
  let rec check car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_var _  -> ()
      | Carrier_skolem(s, i) ->
          if List.mem i skolems then
            raise Unify
      | Carrier_link link -> check link
  in
  check car

and carrier_row_skolem_check skolems car =
  let rec check car =
    let car = carrier_row_repr car in
    match car.desc with
      | Carrier_row_var | Carrier_row_empty -> ()
      | Carrier_row_one c -> carrier_skolem_check skolems c
      | Carrier_row(cr1, cr2) -> check cr1; check cr2
      | Carrier_row_link link -> check link
  in
  check car

and effect_skolem_check skolems eff =
  let rec check eff =
    let eff = effect_repr eff in
    match eff.desc with
      | Effect_var | Effect_empty -> ()
      | Effect_depend c -> carrier_skolem_check skolems c
      | Effect_sum (eff1, eff2) -> check eff1; check eff2
      | Effect_link link -> check link
  in
  check eff

and effect_row_skolem_check skolems eff =
  let rec check eff =
    let eff = effect_repr eff in
    match eff.desc with
      | Effect_row_empty | Effect_row_var -> ()
      | Effect_row_one eff1 -> effect_skolem_check skolems eff1
      | Effect_row (eff1, eff2) -> check eff1; check eff2
      | Effect_row_link link -> check link
  in
  check eff

and react_skolem_check skolems r =
  let rec check r =
    let r = react_repr r in
    match r.desc with
      | React_var | React_empty -> ()
      | React_carrier c -> carrier_skolem_check skolems c
      | React_seq rl | React_par rl | React_or rl ->
          List.iter check rl
      | React_run r1 -> check r1
      | React_rec (_, _, r2) -> check r2
      | React_link link -> check link
  in
  check r


and param_skolem_check skolems =
  clock_param_iter (skolem_check skolems)
    (carrier_skolem_check skolems)
    (effect_skolem_check skolems)
    (react_skolem_check skolems)

*)
let skolem_check skolems ck =
  let carrier_desc funs skolems ckd = match ckd with
    | Carrier_skolem(_, i) ->
      if List.mem i skolems then
        raise Unify;
      ckd, skolems
    | _ -> raise Global_mapfold.Fallback
  in
  let funs = { Clock_mapfold.defaults with carrier_desc = carrier_desc } in
  let _, _ = Clock_mapfold.clock_it funs skolems ck in
  ()

let make_fresh_skolem_subst vars =
  let fresh_skolem (m, skolems) car = match car.desc with
    | Carrier_var _ ->
        let sk = carrier_skolem generic_prefix_name names#name in
        (car.index, sk)::m, sk.index::skolems
    | _ -> assert false
  in
  List.fold_left fresh_skolem ([], []) vars


(* type constructor equality *)
let same_type_constr c1 c2 = Global_ident.same c1.gi c2.gi


(* Expansion of an abbreviation *)
let bind_variable param1 param2 =
  match param1, param2 with
    | Kclock ({ desc  = Clock_var } as ck1), Kclock ck2 ->
        ck1.desc <- Clock_link ck2
    | Kcarrier ({ desc  = Carrier_var _ } as car1), Kcarrier car2 ->
        car1.desc <- Carrier_link car2
    | Kcarrier_row ({ desc  = Carrier_row_var } as car1), Kcarrier_row car2 ->
        car1.desc <- Carrier_row_link car2
    | Keffect ({ desc  = Effect_var } as eff1), Keffect eff2 ->
        eff1.desc <- Effect_link eff2
    | Keffect_row ({ desc  = Effect_row_var } as eff1), Keffect_row eff2 ->
        eff1.desc <- Effect_row_link eff2
    | _ -> fatal_error "bind_variable"

let bind_carrier car1 car2 =
  match car1, car2 with
    | { desc  = Carrier_var _  }, car2 ->
        car1.desc <- Carrier_link car2
    | _ -> raise Unify

let bind_carrier_row car1 car2 =
  match car1, car2 with
    | { desc  = Carrier_row_var }, car2 ->
        car1.desc <- Carrier_row_link car2
    | _ -> raise Unify

let bind_effect eff1 eff2 =
  match eff1, eff2 with
    | { desc  = Effect_var }, eff2 ->
        eff1.desc <- Effect_link eff2
    | _ -> raise Unify

let bind_effect_row eff1 eff2 =
  match eff1, eff2 with
    | { desc  = Effect_row_var }, eff2 ->
        eff1.desc <- Effect_row_link eff2
    | _ -> raise Unify

let bind_react r1 r2 =
  match r1, r2 with
    | { desc  = React_var }, r2 ->
        r1.desc <- React_link r2
    | _ -> raise Unify

let expand_abbrev params body args =
  let params' = List.map copy_param params
  and body' = copy_clock body in
  cleanup();
  List.iter2 bind_variable params' args;
  body'

(* The row variables is the one to the right *)
let rec find_carrier_row_var c =
  let c = carrier_row_repr c in
  match c.desc with
  | Carrier_row (cr1, cr2) ->
      let cr2 = carrier_row_repr cr2 in
      (match cr2.desc with
        | Carrier_row_var | Carrier_row_empty -> cr2, cr1
        | _ ->
            let var, cr2 = find_carrier_row_var cr2 in
            var, carrier_row cr1 cr2)
  | _ -> raise Unify

let rec find_effect_row_var eff =
  let eff = effect_row_repr eff in
  match eff.desc with
  | Effect_row (eff1, eff2) ->
      let eff2 = effect_row_repr eff2 in
      (match eff2.desc with
        | Effect_row_var | Effect_row_empty -> eff2, eff1
        | _ ->
            let var, eff2 = find_effect_row_var eff2 in
            var, eff_row eff1 eff2)
  | _ -> raise Unify

let mk_effect_row_rec rec_var body =
  let new_var = new_effect_row_var () in
  let body = subst_var_effect_row rec_var.index new_var body in
  make_generic (Effect_row_rec (new_var, body))

(* the row variable is the first variable in the + *)
let rec find_react_row_var rl = match rl with
  | [] -> raise Unify
  | r::rl ->
      let r = react_repr r in
      (match r with
        | ({ desc = React_var } as var) -> rl, var
        | { desc = React_or rl' } ->
            let rl', var = find_react_row_var rl' in
            rl'@rl, var
        | _ ->
            let r' = make_generic (React_or (r::rl)) in
            Printf.eprintf "Cannot find row var in %a\n" Clocks_printer.output_react r';
            raise Unify)

(* create the react effect 'rec rec_var. body'*)
let mk_react_rec rec_var body =
  let new_var = new_generic_react_var () in
  match body with
    | { desc = React_or (({ desc = React_var; index = i } as row_var)::rl) }
        when i <> rec_var.index ->
        (* the body is of the form 'phi + body': we generate 'phi + rec rec_var.body' *)
        let body = make_generic (React_or rl) in
        let body = subst_var_react rec_var.index new_var body in
        let new_r = make_generic (React_rec (false, new_var, body)) in
        react_or row_var new_r
    | _ ->
        let body = subst_var_react rec_var.index new_var body in
        make_generic (React_rec (false, new_var, body))

(* unification *)
let rec unify expected_ck actual_ck =
  (*Printf.eprintf "Unify '%a' and %a\n" Clocks_printer.output expected_ck  Clocks_printer.output actual_ck;*)
  if expected_ck == actual_ck then ()
  else
    let expected_ck = clock_repr expected_ck in
    let actual_ck = clock_repr actual_ck in
   (*Printf.eprintf "   After repr:'%a' and %a\n" Clocks_printer.output expected_ck  Clocks_printer.output actual_ck;*)
    if expected_ck == actual_ck then ()
    else
      match expected_ck.desc, actual_ck.desc with
        | Clock_static, Clock_static -> ()
        | Clock_var, _ ->
            occur_check expected_ck.level expected_ck.index actual_ck;
            expected_ck.desc <- Clock_link actual_ck
        | _, Clock_var ->
            occur_check actual_ck.level actual_ck.index expected_ck;
            actual_ck.desc <- Clock_link expected_ck
        | Clock_depend c1, Clock_depend c2 -> carrier_unify c1 c2
        | Clock_product ck_l1, Clock_product ck_l2 -> unify_list ck_l1 ck_l2
        | Clock_arrow(ck1, ck2, eff1), Clock_arrow(ck3, ck4, eff2) ->
            unify ck1 ck3;
            unify ck2 ck4;
            effect_row_unify eff1 eff2
        | Clock_constr(c1, p_l1), Clock_constr(c2, p_l2) when same_type_constr c1 c2 ->
            unify_param_list p_l1 p_l2
        | Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args), _ ->
            unify (expand_abbrev params body args) actual_ck
        | _, Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args) ->
            unify expected_ck (expand_abbrev params body args)
        | Clock_process (ck1, c1, eff1, r1), Clock_process (ck2, c2, eff2, r2) ->
            unify ck1 ck2;
            carrier_unify c1 c2;
            effect_row_unify eff1 eff2;
            react_unify r1 r2
        | Clock_forall { cs_vars = []; cs_desc = expected_ck }, _ ->
            unify expected_ck actual_ck
        | _, Clock_forall { cs_vars = []; cs_desc = actual_ck } ->
            unify expected_ck actual_ck
        | Clock_forall sch1, Clock_forall sch2 ->
            unify_schema sch1 sch2
        | _ ->
             Printf.eprintf "Failed to unify '%a' and '%a'\n"  Clocks_printer.output expected_ck  Clocks_printer.output actual_ck;
            raise Unify

(* TODO: on doit mettre les quantifieurs dans leur ordre d'apparition. Ensuite, on instancie les parametres des deux schemas par les memes skolems frais, on les unifie puis on verifie que les skolems n'ont pas echappe dans l'environnement en verifiant sils apparaissent dans le type des schemas de depart. *)
and unify_schema expected_sch actual_sch =
  Printf.eprintf "expected_sch: %a\nactual_sch: %a\n" Clocks_printer.output expected_sch.cs_desc  Clocks_printer.output actual_sch.cs_desc;
  (* il faut mettre des skolems pour les rows de carrier aussi *)
  let carrier_vars = (kind_sum_split expected_sch.cs_vars).k_carrier in
  let m, skolems = make_fresh_skolem_subst carrier_vars in
  let expected_ty = copy_subst_clock m expected_sch.cs_desc in
  let actual_ty = instance actual_sch in
  Printf.eprintf "expected_ty: %a\nactual_ty: %a\n" Clocks_printer.output expected_ty  Clocks_printer.output actual_ty;
  unify expected_ty actual_ty;
  Printf.eprintf "after expected_ty: %a\nafter actual_ty: %a\n" Clocks_printer.output expected_ty  Clocks_printer.output actual_ty;
  skolem_check skolems actual_sch.cs_desc

and unify_list ck_l1 ck_l2 =
  try
    List.iter2 unify ck_l1 ck_l2
  with
    | Invalid_argument _ -> raise Unify

and carrier_unify expected_car actual_car =
  (*Printf.eprintf "Unify carrier '%a' and %a\n" Clocks_printer.output_carrier expected_car  Clocks_printer.output_carrier actual_car; *)
  if expected_car == actual_car then ()
  else
    let expected_car = carrier_repr expected_car in
    let actual_car = carrier_repr actual_car in
    if expected_car == actual_car then ()
    else
      match expected_car.desc, actual_car.desc with
        | Carrier_var(s), _ ->
            carrier_occur_check expected_car.level expected_car.index actual_car;
            expected_car.desc <- Carrier_link actual_car
        | _, Carrier_var(s) ->
            carrier_occur_check actual_car.level actual_car.index expected_car;
            actual_car.desc <- Carrier_link expected_car
        | Carrier_skolem(_,i), Carrier_skolem(_, j) when i = j -> ()
        | _, _ ->
          Printf.eprintf "Failed to unify carriers '%a' and '%a'\n"  Clocks_printer.output_carrier expected_car  Clocks_printer.output_carrier actual_car;
          raise Unify

and carrier_row_unify expected_cr actual_cr =
  (*Printf.eprintf "Unify carrier rows '%a' and %a\n" Clocks_printer.output_carrier_row expected_cr  Clocks_printer.output_carrier_row actual_cr;*)
  if expected_cr == actual_cr then ()
  else
    let expected_cr = carrier_row_repr expected_cr in
    let actual_cr = carrier_row_repr actual_cr in
    if expected_cr == actual_cr then ()
    else
      match expected_cr.desc, actual_cr.desc with
        (* unification of row types *)
        | Carrier_row_empty, Carrier_row_empty -> ()
        | Carrier_row_one expected_car, Carrier_row_one actual_car ->
            carrier_unify expected_car actual_car
        | Carrier_row_var, _ ->
            carrier_row_occur_check expected_cr.level expected_cr.index actual_cr;
            expected_cr.desc <- Carrier_row_link actual_cr
        | _, Carrier_row_var ->
            carrier_row_occur_check actual_cr.level actual_cr.index expected_cr;
            actual_cr.desc <- Carrier_row_link expected_cr
        | Carrier_row _, Carrier_row _ ->
            let var1, c1 = find_carrier_row_var expected_cr in
            let var2, c2 = find_carrier_row_var actual_cr in
            (match var1.desc, var2.desc with
              | _, _ when var1 == var2 -> carrier_row_unify c1 c2
              | Carrier_row_empty, Carrier_row_empty -> carrier_row_unify c1 c2
              | Carrier_row_empty, _ ->
                  carrier_row_unify var2 carrier_row_empty;
                  carrier_row_unify expected_cr c2
              | _, Carrier_row_empty ->
                  carrier_row_unify var1 carrier_row_empty;
                  carrier_row_unify c1 actual_cr
              | _ , _ ->
                  let var = new_carrier_row_var () in
                  let new_c1 = carrier_row var c1 in
                  let new_c2 = carrier_row var c2 in
                  carrier_row_unify var1 new_c2;
                  carrier_row_unify var2 new_c1
            )
        (* this cases are used when unifying a closed row with on open row.
           Here we unify a row without a var or empty with a single carrier,
           by unifying all carriers in the row with the one carrier *)
        | Carrier_row (cr1, cr2), Carrier_row_one _ ->
            carrier_row_unify cr1 actual_cr;
            carrier_row_unify cr2 actual_cr
        | Carrier_row_one _, Carrier_row (cr1, cr2) ->
            carrier_row_unify expected_cr cr1;
            carrier_row_unify expected_cr cr2
        | Carrier_row_empty, Carrier_row_one _ -> ()
        | Carrier_row_one _, Carrier_row_empty -> ()
        | _, _ ->
          Printf.eprintf "Failed to unify carrier rows '%a' and '%a'\n"  Clocks_printer.output_carrier_row expected_cr  Clocks_printer.output_carrier_row actual_cr;
          raise Unify

and effect_unify expected_eff actual_eff =
  (* Printf.eprintf "Unify effects '%a' and %a\n" Clocks_printer.output_effect expected_eff  Clocks_printer.output_effect actual_eff; *)
  if expected_eff == actual_eff then ()
  else
    let expected_eff = effect_repr expected_eff in
    let actual_eff = effect_repr actual_eff in
  (*  Printf.eprintf "   After repr:'%a' and %a\n" Clocks_printer.output_effect expected_eff  Clocks_printer.output_effect actual_eff; *)
    if expected_eff == actual_eff then ()
    else
      match expected_eff.desc, actual_eff.desc with
        | Effect_empty, Effect_empty -> ()
        | Effect_var, _ ->
            effect_occur_check expected_eff.level expected_eff.index actual_eff;
            if !Compiler_options.use_row_clocking then
              expected_eff.desc <- Effect_link actual_eff
            else
              expected_eff.desc <- Effect_link (remove_var_from_effect expected_eff.index actual_eff)
        | _, Effect_var ->
            effect_occur_check actual_eff.level actual_eff.index expected_eff;
            if !Compiler_options.use_row_clocking then
              actual_eff.desc <- Effect_link expected_eff
            else
              actual_eff.desc <- Effect_link (remove_var_from_effect actual_eff.index expected_eff)
        | Effect_depend c1, Effect_depend c2 -> carrier_row_unify c1 c2
        | Effect_sum (eff1, eff2), Effect_sum (eff3, eff4) ->
            (* only called for identical effects *)
            effect_unify eff1 eff3; effect_unify eff2 eff4
        | (Effect_depend _ | Effect_empty), Effect_sum (eff1, eff2)
          when not !Compiler_options.use_row_clocking ->
            effect_unify expected_eff eff1; effect_unify expected_eff eff2
        | Effect_sum (eff1, eff2), (Effect_depend _ | Effect_empty)
          when not !Compiler_options.use_row_clocking ->
            effect_unify eff1 actual_eff; effect_unify eff2 actual_eff
        (* unifications with the empty effect: all the effects should be the empty effect
           and effect row vars empty *)
        | Effect_empty, Effect_sum (eff1, eff2) when !Compiler_options.use_row_clocking ->
            effect_unify expected_eff eff1; effect_unify expected_eff eff2
        | Effect_sum (eff1, eff2), Effect_empty when !Compiler_options.use_row_clocking ->
            effect_unify expected_eff eff1; effect_unify expected_eff eff2
        | Effect_empty, Effect_one actual_effr when !Compiler_options.use_row_clocking ->
            effect_row_unify (eff_closed_row expected_eff) actual_effr
        | Effect_one expected_effr, Effect_empty when !Compiler_options.use_row_clocking ->
            effect_row_unify expected_effr (eff_closed_row actual_eff)
        | _ ->
             Printf.eprintf "Failed to unify effects '%a' and '%a'\n"  Clocks_printer.output_effect expected_eff  Clocks_printer.output_effect actual_eff;
            raise Unify

and effect_row_unify expected_eff actual_eff =
  (* Printf.eprintf "Unify effects rows '%a' and %a\n" Clocks_printer.output_effect_row expected_eff  Clocks_printer.output_effect_row actual_eff_row *)
  if expected_eff == actual_eff then ()
  else
    let expected_eff = effect_row_repr expected_eff in
    let actual_eff = effect_row_repr actual_eff in
  (*  Printf.eprintf "   After repr:'%a' and %a\n" Clocks_printer.output_effect_row expected_eff  Clocks_printer.output_effect_row actual_eff; *)
    if expected_eff == actual_eff then ()
    else
      match expected_eff.desc, actual_eff.desc with
        (*unification of rows*)
        | Effect_row_empty, Effect_row_empty -> ()
        | Effect_row_one expected_eff, Effect_row_one actual_eff ->
            effect_unify expected_eff actual_eff
        | Effect_row_var, _ ->
            let is_rec =
              effect_row_occur_check_is_rec expected_eff.level
                expected_eff.index actual_eff
            in
            if is_rec then
              let new_eff = mk_effect_row_rec expected_eff actual_eff in
              expected_eff.desc <- Effect_row_link new_eff
            else
              expected_eff.desc <- Effect_row_link actual_eff
        | _, Effect_row_var ->
            let is_rec =
              effect_row_occur_check_is_rec actual_eff.level
                actual_eff.index expected_eff
            in
            if is_rec then
              let new_eff = mk_effect_row_rec actual_eff expected_eff in
              actual_eff.desc <- Effect_row_link new_eff
            else
              actual_eff.desc <- Effect_row_link expected_eff
        | Effect_row _, Effect_row _ ->
            let var1, e1 = find_effect_row_var expected_eff in
            let var2, e2 = find_effect_row_var actual_eff in
            (match var1.desc, var2.desc with
              | _, _ when var1 == var2 -> effect_row_unify e1 e2
              | Effect_row_empty, Effect_row_empty -> effect_row_unify e1 e2
              | Effect_row_empty, _ ->
                  effect_row_unify var2 effect_row_empty;
                  effect_row_unify expected_eff e2
              | _, Effect_row_empty ->
                  effect_row_unify var1 effect_row_empty;
                  effect_row_unify e1 actual_eff
              | _ , _ ->
                let var = new_effect_row_var () in
                let new_e1 = eff_row var e1 in
                let new_e2 = eff_row var e2 in
                effect_row_unify var1 new_e2;
                effect_row_unify var2 new_e1
            )
        (* Not correct in general, but should be correct in our setting *)
        | Effect_row_rec (var1, body1), Effect_row_rec (var2, body2) ->
          effect_row_unify var1 var2;
          effect_row_unify body1 body2
        | Effect_row_rec (_, expected_body), (Effect_row _ | Effect_row_one _) ->
            effect_row_unify expected_body actual_eff
        | (Effect_row _ | Effect_row_one _), Effect_row_rec (_, actual_body) ->
            effect_row_unify expected_eff actual_body
        (* this cases are used when unifying a closed row with on open row.
           Here we unify a row without a var or empty with a single effect,
           by unifying all effects in the row with the one effect *)
        | Effect_row (eff1, eff2), Effect_row_one _ ->
            effect_row_unify eff1 actual_eff;
            effect_row_unify eff2 actual_eff
        | Effect_row_one _, Effect_row (eff1, eff2) ->
            effect_row_unify expected_eff eff1;
            effect_row_unify expected_eff eff2
        | Effect_row_empty, Effect_row_one _ -> ()
        | Effect_row_one _, Effect_row_empty -> ()
        | _, _ ->
          Printf.eprintf "Failed to unify effect rows '%a' and '%a'\n"  Clocks_printer.output_effect_row expected_eff  Clocks_printer.output_effect_row actual_eff;
          raise Unify


and react_unify expected_r actual_r =
  (*Printf.eprintf "Unify reactivities '%a' and '%a'\n"  Clocks_printer.output_react expected_r  Clocks_printer.output_react actual_r;*)
  if !Compiler_options.no_reactivity || expected_r == actual_r then ()
  else
    let expected_r = react_repr expected_r in
    let actual_r = react_repr actual_r in
    if expected_r == actual_r then ()
    else
      match expected_r.desc, actual_r.desc with
        | React_empty, React_empty -> ()
        | React_carrier c1, React_carrier c2 -> carrier_row_unify c1 c2
            (* phi == uphi. r *)
        (*| React_var, React_rec (r2, _) when expected_r.index = r2.index -> ()
        | React_rec(r2, _), React_var when actual_r.index = r2.index -> ()*)
        | React_var, _ ->
            let is_rec = react_occur_check_is_rec expected_r.level expected_r.index actual_r in
            if is_rec then (
              let new_r = mk_react_rec expected_r actual_r in
              expected_r.desc <- React_link new_r
            ) else
              expected_r.desc <- React_link actual_r
        | _, React_var ->
            let is_rec = react_occur_check_is_rec actual_r.level actual_r.index expected_r in
            if is_rec then (
              let new_r = mk_react_rec actual_r expected_r in
              actual_r.desc <- React_link new_r
            ) else
              actual_r.desc <- React_link expected_r
        (* unification of row types *)
        | React_or rl1, React_or rl2 ->
            let rl1, var1 = find_react_row_var rl1 in
            let rl2, var2 = find_react_row_var rl2 in
            if var1.index = var2.index then
              (* this should only be the case if we are unifying two reacts
               that are just copies *)
              List.iter2 react_unify rl1 rl2
            else (
              let var = new_react_var () in
              let new_rl1 = make_generic (React_or (var::rl1)) in
              let new_rl2 = make_generic (React_or (var::rl2)) in
              react_unify var1 new_rl2;
              react_unify var2 new_rl1
            )
        | React_rec (_, var1, r1), React_rec (_, var2, r2) ->
            react_unify var1 var2; react_unify r1 r2
       (* | React_rec (_, expected_r), _ -> react_unify expected_r actual_r
        | _, React_rec (_, actual_r) -> react_unify expected_r actual_r *)
        | _ ->
            Printf.eprintf "Failed to unify reactivities '%a' and '%a'\n"  Clocks_printer.output_react expected_r  Clocks_printer.output_react actual_r;
            raise Unify

and unify_param p1 p2 =
  let unify_record =
      mk_kind_prod ~clock:unify ~carrier:carrier_unify ~carrier_row:carrier_row_unify
    ~effect:effect_unify ~effect_row:effect_row_unify ~react:react_unify
  in
  kind_iter2 unify_record p1 p2

and unify_param_list l1 l2 =
  try
    List.iter2 unify_param l1 l2
  with
    | Invalid_argument _ -> raise Unify

(* special cases of unification *)
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
        let c = new_carrier_var generic_prefix_name in
        unify ck (depend c);
        c

let rec filter_arrow ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_arrow(ck1, ck2, eff) -> ck1, ck2, eff
    | Clock_constr({ck_info = Some { constr_abbr = Constr_abbrev (params, body) }}, args) ->
        filter_arrow (expand_abbrev params body args)
    | _ ->
        let ck1 = new_clock_var () in
        let ck2 = new_clock_var () in
        let eff = new_effect_row_var () in
        unify ck (arrow ck1 ck2 eff);
        ck1, ck2, eff

let add_type_description g =
  { gi = g.gi;
    ty_info = None;
    ck_info = g.ck_info }


let cond_subst_react ck subst r =
  let sub ckz cksub =
    List.iter (fun ck -> ck.desc <- React_empty) ckz;
    List.iter (fun ck -> ck.desc <- React_link subst) cksub
  in
  let rec aux rec_vars (ckz, cksub, b) r = match r.desc with
    | React_empty -> ckz, cksub, b
    | React_carrier ck ->
      if ck.index = r.index then r::ckz, cksub, b else ckz, r::cksub, true
    | React_var ->
      if List.mem r.index rec_vars then
        (sub ckz cksub; [],[],b)
      else
        ckz, cksub, b
    | React_seq rl ->
      let one_step (ckz, cksub, b) r =
        let rec_vars = if b then [] else rec_vars in aux rec_vars (ckz, cksub, b) r
      in
      List.fold_left one_step (ckz, cksub, b) rl
    | React_or rl ->
      let ckz_list, cksub_list, b_list = Misc.split3 (List.map (aux rec_vars (ckz, cksub, b)) rl) in
      List.flatten ckz_list, List.flatten cksub_list, b || List.for_all (fun b -> b) b_list
    | React_par rl ->
      let ckz_list, cksub_list, b_list = Misc.split3 (List.map (aux rec_vars (ckz, cksub, b)) rl) in
      List.flatten ckz_list, List.flatten cksub_list, b || (List.exists (fun b -> b) b_list)
    | React_run r1 -> aux rec_vars (ckz, cksub, b) r1
    | React_rec(_, var, r1) ->  aux (var.index::rec_vars) (ckz, cksub, b) r1
    | React_link r1 -> aux rec_vars (ckz, cksub, b) r1
  in
  let r_copy = copy_subst_react [] r in
  cleanup ();
  let ckz, cksub, _ = aux [] ([], [], false) r_copy in
  sub ckz cksub;
  r_copy
