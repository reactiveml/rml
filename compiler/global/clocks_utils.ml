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
let generic_prefix_name = "c"
let generic_activation_name = "act"
let generic_effect_name = "eff"

(* The current nesting level of lets *)
let current_level = ref 0;;

let reset_type_var () =
  current_level := 0; ()
and push_type_level () =
  incr current_level; ()
and pop_type_level () =
  decr current_level; ()
;;

(* functins on clock params *)

let clock_param_map f_ck f_car f_eff f_react v = match v with
  | Var_clock c -> Var_clock (f_ck c)
  | Var_carrier c -> Var_carrier (f_car c)
  | Var_effect eff -> Var_effect (f_eff eff)
  | Var_react r -> Var_react (f_react r)

let clock_param_iter f_ck f_car f_eff f_react v = match v with
  | Var_clock c -> f_ck c
  | Var_carrier c -> f_car c
  | Var_effect eff -> f_eff eff
  | Var_react r -> f_react r

let clock_param_iter2 f_ck f_car f_eff f_react v1 v2 = match v1, v2 with
  | Var_clock c1, Var_clock c2 -> f_ck c1 c2
  | Var_carrier c1, Var_carrier c2 -> f_car c1 c2
  | Var_effect eff1, Var_effect eff2 -> f_eff eff1 eff2
  | Var_react r1, Var_react r2 -> f_react r1 r2
  | _, _ -> invalid_arg "clock_param_iter"

let clock_param_fold f_ck f_car f_eff f_react acc v = match v with
  | Var_clock c -> f_ck acc c
  | Var_carrier c -> f_car acc c
  | Var_effect eff -> f_eff acc eff
  | Var_react r -> f_react acc r

let expect_clock p = match p with
  | Var_clock ck -> ck
  | _ -> invalid_arg "expect_clock"

let expect_carrier p = match p with
  | Var_carrier car -> car
  | _ -> invalid_arg "expect_carrier"

let expect_effect p = match p with
  | Var_effect eff -> eff
  | _ -> invalid_arg "expect_effect"

let expect_react p = match p with
  | Var_react r -> r
  | _ -> invalid_arg "expect_react"

let params_split l =
  let aux (ck_l, c_l, eff_l, r_l) x = match x with
    | Var_clock ck -> ck::ck_l, c_l, eff_l, r_l
    | Var_carrier c -> ck_l, c::c_l, eff_l, r_l
    | Var_effect eff -> ck_l, c_l, eff::eff_l, r_l
    | Var_react r -> ck_l, c_l, eff_l, r::r_l
  in
  List.fold_left aux ([], [], [], []) l

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

let arrow ck1 ck2 eff =
  make_clock (Clock_arrow(ck1, ck2, eff))

let rec arrow_list ck_l eff_l ck_res =
  match ck_l, eff_l with
    [], [] -> ck_res
  | [ck], [eff] -> arrow ck ck_res eff
  | ck :: ck_l, eff::eff_l -> arrow ck (arrow_list ck_l eff_l ck_res) eff
  | _, _ -> invalid_arg "arrow_list"

let process ck activation_car eff r =
  make_clock (Clock_process (ck, activation_car, eff, r))

let make_carrier s =
  { desc = Carrier_var s;
    index = names#name;
    level = !current_level }
let carrier_skolem s i =
  { desc = Carrier_skolem(s, i);
    index = names#name;
    level = !current_level }

let make_effect eff =
  { desc = eff;
    level = generic;
    index = names#name; }

let eff_sum eff1 eff2 =
  make_effect (Effect_sum (eff1, eff2))

let rec eff_sum_list l = match l with
  | [] -> make_effect Effect_empty
  | [eff] -> eff
  | eff::eff_l -> eff_sum eff (eff_sum_list eff_l)

let eff_depend c =
  make_effect (Effect_depend c)



let no_clock =
  { desc = Clock_static;
    level = generic;
    index = -1; }
let no_carrier =
  { desc = Carrier_var "";
    index = -1;
    level = generic }
let no_effect =
 { desc = Effect_empty;
   level = generic;
   index = -1; }
let topck_carrier =
  { desc = Carrier_skolem ("topck", names#name);
    index = -2;
    level = 0 }

let clock_topck =
  make_clock (Clock_depend topck_carrier)


(* reactivity effects *)

let make_react r =
  { desc = r;
    level = !current_level;
    index = names#name; }

let no_react () =
  { desc = React_empty;
    level = generic;
    index = -1; }
let top_react =
  { desc = React_top;
    level = generic;
    index = -2; }

let react_carrier c = make_react (React_carrier c)

let react_seq r1 r2 = match r1.desc, r2.desc with
  | React_seq rl1, React_seq rl2 -> make_react (React_seq (rl1@rl2))
  | React_seq rl1, _ -> make_react (React_seq (rl1 @ [r2]))
  | _, React_seq rl2 -> make_react (React_seq (r1::rl2))
  | React_empty, _ -> r2
  | _, React_empty -> r1
  | React_carrier c1, React_carrier c2 when c1 == c2 -> r1
  | _, _ -> make_react (React_seq [r1; r2])

let react_par r1 r2 = match r1.desc, r2.desc with
  | React_par rl1, React_par rl2 -> make_react (React_par (rl1@rl2))
  | React_par rl1, _ -> make_react (React_par (r2::rl1))
  | _, React_par rl2 -> make_react (React_par (r1::rl2))
  | React_empty, _ -> r2
  | _, React_empty -> r1
  | _, _ -> make_react (React_par [r1; r2])

let react_or r1 r2 =
  let desc = match r1.desc, r2.desc with
    | React_or rl1, React_or rl2 -> React_or (rl1@rl2)
    | React_or rl1, _ -> React_or (r2::rl1)
    | _, React_or rl2 -> React_or (r1::rl2)
    | _, _ -> React_or [r1; r2]
  in
  make_react desc

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

let new_effect_var () =
  { desc = Effect_var;
    level = !current_level;
    index = names#name }

let new_generic_effect_var () =
  { desc = Effect_var;
    level = generic;
    index = names#name }

let new_react_var () =
  { desc = React_var;
    level = !current_level;
    index = names#name }

let new_generic_react_var () =
  { desc = React_var;
    level = generic;
    index = names#name }

let new_generic_var (v, k) = match k with
    | Ttype_var -> Var_clock (new_generic_clock_var ())
    | Tcarrier_var -> Var_carrier (make_generic_carrier v)
    | Teffect_var -> Var_effect (new_generic_effect_var ())
    | Treact_var -> Var_react (new_generic_react_var ())

let rec new_clock_var_list n =
  match n with
    | 0 -> []
    | n -> (new_clock_var ()) :: new_clock_var_list (n - 1)



let react_loop r =
  let var = new_react_var () in
  let body = react_seq r var in
  make_react (React_rec (false, var, body))


let forall ck_vars car_vars eff_vars react_vars typ =
  { cs_clock_vars = ck_vars;
    cs_carrier_vars = car_vars;
    cs_effect_vars = eff_vars;
    cs_react_vars = react_vars;
    cs_desc = typ; }

let ty_forall params ck =
  let ty_l, c_l, eff_l, r_l = params_split params in
  Misc.assert_empty ty_l;
  make_clock (Clock_forall (forall [] c_l eff_l r_l ck))

let clock_of_sch sch =
  make_clock (Clock_forall sch)

(* activation clock*)
let activation_carrier = ref topck_carrier
let current_effect = ref no_effect
let current_react = ref (no_react ())

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

let rec effect_repr eff = match eff.desc with
  | Effect_link t ->
      let t = effect_repr t in
      eff.desc <- Effect_link t;
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
    | Effect_depend c1, Effect_depend c2 -> equal_carrier c1 c2
    | _ -> false

(*
let rec simplify_effect eff =
  let eff = effect_repr eff in
  match eff.desc with
    | Effect_empty | Effect_var | Effect_link _ | Effect_depend _ -> eff
    | Effect_sum ({ desc = Effect_empty }, eff)
    | Effect_sum (eff, { desc = Effect_empty }) -> simplify_effect eff
    | Effect_sum (eff1, eff2) ->
        { eff with desc = Effect_sum (simplify_effect eff1, simplify_effect eff2) }
        *)


let simplify_effect eff =
  let rec mem_effect eff l = match l with
    | [] -> false
    | eff2::l -> equal_effect eff eff2 || mem_effect eff l
  in
  let add_to_list x l =
    if mem_effect x l then l else x::l
  in
  let rec clocks_of acc eff = match eff.desc with
    | Effect_empty -> acc
    | Effect_var | Effect_depend _ -> add_to_list eff acc
    | Effect_link link -> clocks_of acc link
    | Effect_sum (eff1, eff2) -> clocks_of (clocks_of acc eff2) eff1
  in
  (*Printf.eprintf "Simplify before: %a with index %d\n" Clocks_printer.output_effect eff  eff.index;*)
  let eff = effect_repr eff in
  let new_eff = eff_sum_list (clocks_of [] eff) in
  (*Printf.eprintf "Simplify after: %a with index %d\n" Clocks_printer.output_effect eff  eff.index;*)
  new_eff

(*
let remove_duplicates eff =
  let used_vars = ref [] in
  let used_cars = ref [] in
  let rec aux eff = match eff.desc with
    | Effect_empty -> ()vars, cars)
    | Effect_var ->
        if List.mem eff.index !used_vars then

let remove_empty_effect eff = match eff.desc with
  | Effect_sum( { desc = Effect_empty }, eff2) ->
      eff.desc <- Effect_link eff2
   | Effect_sum(eff1, { desc = Effect_empty }) ->
       eff.desc <- Effect_link eff1
   | _ -> ()

let simplify_effect eff =
  remove_empty_effect eff;
  effect_repr eff
  *)

let rec remove_ck_from_effect ck eff = match eff.desc with
  | Effect_depend c ->
      if (carrier_repr c).desc = ck.desc then
        no_effect
      else
        eff
  | Effect_empty | Effect_var -> eff
  | Effect_link link -> remove_ck_from_effect ck link
  | Effect_sum (eff1, eff2) ->
      { eff with desc = Effect_sum (remove_ck_from_effect ck eff1,
                                   remove_ck_from_effect ck eff2) }
let remove_ck_from_effect ck eff =
  simplify_effect (remove_ck_from_effect ck eff)

let rec remove_var_from_effect index eff = match eff.desc with
  | Effect_var ->
      if eff.index = index then
        no_effect
      else
        eff
  | Effect_empty | Effect_depend _ -> eff
  | Effect_link link -> remove_var_from_effect index link
  | Effect_sum (eff1, eff2) ->
      { eff with desc = Effect_sum (remove_var_from_effect index eff1,
                                   remove_var_from_effect index eff2) }
let remove_var_from_effect index eff =
  simplify_effect (remove_var_from_effect index eff)

(* Operations on reactivity effects *)
let rec remove_ck_from_react ck r = match r.desc with
  | React_empty | React_top | React_var -> r
  | React_carrier c ->
      if (carrier_repr c).desc = ck.desc then
        no_react ()
      else
        r
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

let rec subst_var_react index subst r = match r.desc with
  | React_var when r.index = index -> subst
  | React_empty | React_top | React_var | React_carrier _ -> r
  | React_seq rl ->
      { r with desc = React_seq (List.map (subst_var_react index subst) rl) }
  | React_par rl ->
      { r with desc = React_par (List.map (subst_var_react index subst) rl) }
  | React_or rl ->
      { r with desc = React_or (List.map (subst_var_react index subst) rl) }
  | React_run r1 ->
      { r with desc = React_run (subst_var_react index subst r1) }
  | React_rec (b, r1, r2) ->
      { r with desc = React_rec (b, r1, subst_var_react index subst r2) }
  | React_link link -> subst_var_react index subst link

(* To generalize a type *)

(* generalisation and non generalisation of a type. *)
(* the level of generalised type variables *)
(* is set to [generic] when the flag [is_gen] is true *)
(* and set to [!binding_level] when the flag is false *)
(* returns [generic] when a sub-term can be generalised *)

let list_of_clock_vars = ref []
let list_of_carrier_vars = ref []
let list_of_effect_vars = ref []
let list_of_react_vars = ref []

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
    | Clock_arrow(ck1, ck2, eff) ->
        let level1 = gen_clock is_gen ck1 in
        let level2 = gen_clock is_gen ck2 in
        let level_eff = gen_effect is_gen eff in
        ck.level <- min level1 (min level2 level_eff)
    | Clock_product ck_list ->
        ck.level <- gen_clock_list is_gen ck_list
    | Clock_constr(name, param_list) ->
        ck.level <- gen_param_list is_gen param_list
    | Clock_link(link) ->
        ck.level <- gen_clock is_gen link
    | Clock_process (body_ck, act_car, eff, r) ->
        let level_eff = gen_effect is_gen eff in
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

and gen_effect is_gen eff =
  let eff = effect_repr eff in
  (match eff.desc with
    | Effect_empty -> ()
    | Effect_sum (eff1, eff2) ->
        eff.level <- min (gen_effect is_gen eff1) (gen_effect is_gen eff2)
    | Effect_depend c ->
        eff.level <- gen_carrier is_gen c
    | Effect_var ->
        if eff.level > !current_level then
          if is_gen then (
            eff.level <- generic;
            list_of_effect_vars := eff :: !list_of_effect_vars
          ) else
            eff.level <- !current_level
    | Effect_link(link) ->
        eff.level <- gen_effect is_gen link
  );
  eff.level

and gen_react is_gen r =
  let r = react_repr r in
  if r.level > !current_level then (
    if is_gen then
      r.level <- generic
    else
      r.level <- !current_level
  );
  (match r.desc with
    | React_empty | React_top -> ()
    | React_carrier c -> r.level <- gen_carrier is_gen c
    | React_seq rl | React_par rl | React_or rl ->
        r.level <- gen_react_list is_gen rl
    | React_rec (_, r1, r2) -> (*TODO: generaliser var?*)
        r.level <- min (gen_react is_gen r1) (gen_react is_gen r2)
    | React_run r2 ->
        let _ = react_repr r2 in
        r.level <- gen_react is_gen r2
    | React_var ->
        if r.level > !current_level && is_gen then
          list_of_react_vars := r :: !list_of_react_vars
    | React_link link -> r.level <- gen_react is_gen link
  );
  r.level

and gen_react_list is_gen rl =
  List.fold_left (fun level r -> min level (gen_react is_gen r)) notgeneric rl

and gen_param is_gen x =
  clock_param_iter (gen_clock is_gen) (gen_carrier is_gen)
    (gen_effect is_gen) (gen_react is_gen) x

and gen_param_list is_gen param_list =
  List.fold_left (fun level p -> min level (gen_param is_gen p)) notgeneric param_list

(* main generalisation function *)
let gen ck =
  list_of_clock_vars := [];
  list_of_carrier_vars := [];
  list_of_effect_vars := [];
  list_of_react_vars := [];
  let _ = gen_clock true ck in
  { cs_clock_vars = !list_of_clock_vars;
    cs_carrier_vars = !list_of_carrier_vars;
    cs_effect_vars = !list_of_effect_vars;
    cs_react_vars = !list_of_react_vars;
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
          if ck.level >= level then fv := (Var_clock ck) :: !fv
      | Clock_arrow(t1,t2, eff) ->
          free_vars t1; free_vars t2; free_vars_effect eff
      | Clock_product ck_list ->
          List.iter free_vars ck_list
      | Clock_constr (c, param_list) ->
          List.iter free_vars_param param_list
      | Clock_link link ->
          free_vars link
      | Clock_process (ck, act, eff, r) ->
          free_vars ck;
          free_vars_carrier act;
          free_vars_effect eff;
          free_vars_react r
      | Clock_depend c -> free_vars_carrier c
      | Clock_forall sch -> (*TODO*) ()
  and free_vars_carrier car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_skolem _ -> ()
      | Carrier_var _ ->
          if car.level >= level then fv := (Var_carrier car) :: !fv
      | Carrier_link link -> free_vars_carrier link
  and free_vars_effect eff =
    let eff = effect_repr eff in
    match eff.desc with
      | Effect_empty -> ()
      | Effect_var ->
          if eff.level >= level then fv := (Var_effect eff) :: !fv
      | Effect_sum (eff1, eff2) ->
          free_vars_effect eff1; free_vars_effect eff2
      | Effect_depend c ->
          free_vars_carrier c
      | Effect_link link -> free_vars_effect link
  and free_vars_react r =
    let r = react_repr r in
    match r.desc with
      | React_var ->
          if r.level >= level then fv := (Var_react r) :: !fv
      | React_empty | React_top -> ()
      | React_carrier c -> free_vars_carrier c
      | React_seq rl | React_par rl | React_or rl ->
          List.iter free_vars_react rl
      | React_rec (_, r1, r2) -> free_vars_react r1; free_vars_react r2
      | React_run r  | React_link r -> free_vars_react r
  and free_vars_param p =
    clock_param_iter free_vars free_vars_carrier free_vars_effect free_vars_react p
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
let effects = new save
let reacts = new save
let cleanup () =
  clocks#cleanup;
  carriers#cleanup;
  effects#cleanup;
  reacts#cleanup

(* makes a copy of a type *)
let rec copy_subst_clock m ck =
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
          arrow (copy_subst_clock m ck1) (copy_subst_clock m ck2) (copy_subst_effect m eff)
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
            (copy_subst_effect m eff) (copy_subst_react m r)
        else
          ck
    | Clock_forall sch ->
        let cs_clock_vars = List.map (fun ck -> copy_subst_clock m ck) sch.cs_clock_vars in
        let cs_carrier_vars = List.map (fun ck -> copy_subst_carrier m ck) sch.cs_carrier_vars in
        let cs_effect_vars = List.map (fun ck -> copy_subst_effect m ck) sch.cs_effect_vars in
        let cs_react_vars = List.map (fun ck -> copy_subst_react m ck) sch.cs_react_vars in
        let cs_desc = copy_subst_clock m sch.cs_desc in
        let sch = forall cs_clock_vars cs_carrier_vars cs_effect_vars cs_react_vars cs_desc in
        make_clock (Clock_forall sch)

and copy_subst_carrier m car =
  let level = car.level in
  match car.desc with
    | Carrier_var s ->
        if level = generic then
          (try
            List.assoc car.index m
          with
            | Not_found ->
                let v = make_carrier s in
                car.desc <- Carrier_link v;
                carriers#save car (Carrier_var s);
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
          effects#save eff Effect_var;
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
          eff_depend (copy_subst_carrier m c)
        else
          eff

and copy_subst_react m r =
  let level = r.level in
  match r.desc with
    | React_empty -> no_react ()
    | React_top -> r
    | React_var ->
        if level = generic then
          let v = new_react_var () in
          r.desc <- React_link v;
          reacts#save r React_var;
          v
        else
          r
    | React_carrier c ->
        if level = generic then
          make_react (React_carrier (copy_subst_carrier m c))
        else
          r
    | React_seq rl ->
        if level = generic then
          make_react (React_seq (List.map (copy_subst_react m) rl))
        else
          r
    | React_par rl ->
        if level = generic then
          make_react (React_par (List.map (copy_subst_react m) rl))
        else
          r
    | React_or rl ->
        if level = generic then
          make_react (React_or (List.map (copy_subst_react m) rl))
        else
          r
    | React_rec (b, r1, r2) ->
        if level = generic then
          make_react (React_rec (b, copy_subst_react m r1, copy_subst_react m r2))
        else
          r
    | React_run r2 ->
        if level = generic then
          make_react (React_run (copy_subst_react m r2))
        else
          r
    | React_link link ->
        if level = generic then
          link
        else
          copy_subst_react m link

and copy_subst_param m p = clock_param_map (copy_subst_clock m)
  (copy_subst_carrier m) (copy_subst_effect m) (copy_subst_react m) p

let copy_clock ck = copy_subst_clock [] ck
let copy_param p = copy_subst_param [] p

(* instanciation *)
let instance { cs_desc = ck } =
  let ck_i = copy_clock ck in
  cleanup ();
  ck_i


let instance_and_vars { cs_clock_vars = ck_vars; cs_carrier_vars = car_vars;
                        cs_effect_vars = eff_vars; cs_react_vars = r_vars;
                        cs_desc = ck } =
  let ck_i = copy_clock ck in
  let ck_vars = List.map clock_repr ck_vars in
  let car_vars = List.map carrier_repr car_vars in
  let eff_vars = List.map effect_repr eff_vars in
  let r_vars = List.map react_repr r_vars in
  cleanup ();
  ck_vars, car_vars, eff_vars, r_vars, ck_i

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
let rec occur_check level index ck =
  let rec check ck =
    let ck = clock_repr ck in
    match ck.desc with
      | Clock_var ->
          (*Printf.eprintf "Occur_check : level:%d  index:%a   ck.level:%d  ck:%a\n"  level  Clocks_printer.output index  ck.level Clocks_printer.output ck;*)
        if ck == index then
          raise Unify
        else if ck.level > level then
          ck.level <- level
      | Clock_static -> ()
      | Clock_depend c -> carrier_occur_check level no_carrier c
      | Clock_arrow (ck1, ck2, eff) ->
          check ck1; check ck2;
          effect_occur_check level no_effect eff
      | Clock_product ck_list -> List.iter check ck_list
      | Clock_constr(_, param_list) -> List.iter (param_occur_check level index) param_list
      | Clock_link link -> check link
      | Clock_process (ck, act_car, eff, r) ->
          check ck;
          carrier_occur_check level no_carrier act_car;
          effect_occur_check level no_effect eff;
          react_occur_check level (no_react ()) r
      | Clock_forall sch -> (*TODO*) ()
  in
  (*Printf.eprintf "Occur_check : level:%d   index:%a   ck:%a\n"  level  Clocks_printer.output index  Clocks_printer.output ck;*)
  check ck

and carrier_occur_check level index car =
  let rec check car =
    let car = carrier_repr car in
    match car.desc with
      | Carrier_var s ->
        (*  Format.eprintf "Var occur check: %s car.level %d, level: %d@." s car.level level; *)
          if car == index then
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

and effect_occur_check level index eff =
  let rec check eff =
    let eff = effect_repr eff in
    match eff.desc with
        (* unification of effects is unification of sets so there is no problem
           if the same effect appears on both sides *)
      | Effect_var ->
          if eff.level > level then
            eff.level <- level;
      | Effect_empty -> ()
      | Effect_depend c -> carrier_occur_check level no_carrier c
      | Effect_sum (eff1, eff2) -> check eff1; check eff2
      | Effect_link link -> check link
  in
  check eff

and react_occur_check_is_rec level index r =
  let is_rec = ref false in
  let rec check r =
    let r = react_repr r in
    if r.level > level then
      r.level <- level;
    match r.desc with
      | React_empty | React_top -> ()
      | React_var ->
          if r == index then (* go on to fix levels *)
            is_rec := true
      | React_carrier c -> carrier_occur_check level no_carrier c
      | React_seq rl | React_par rl | React_or rl ->
          List.iter check rl
      | React_run r1 -> check r1
      | React_rec (_, _, r2) -> (* TODO: parcourir r1 ?*)
          check r2
      | React_link link -> check link
  in
  check r;
  !is_rec

and react_occur_check level index r =
  ignore (react_occur_check_is_rec level index r)

and param_occur_check level index =
  clock_param_iter (occur_check level index)
    (carrier_occur_check level no_carrier)
    (effect_occur_check level no_effect)
    (react_occur_check level (no_react ()))

(* the occur check *)
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
      | Carrier_var s -> ()
      | Carrier_skolem(s, i) ->
          if List.mem i skolems then
            raise Unify
      | Carrier_link link -> check link
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

and react_skolem_check skolems r =
  let rec check r =
    let r = react_repr r in
    match r.desc with
      | React_var | React_empty | React_top -> ()
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
    | Var_clock ({ desc  = Clock_var } as ck1), Var_clock ck2 ->
        ck1.desc <- Clock_link ck2
    | Var_carrier ({ desc  = Carrier_var _ } as car1), Var_carrier car2 ->
        car1.desc <- Carrier_link car2
    | Var_effect ({ desc  = Effect_var } as eff1), Var_effect eff2 ->
        eff1.desc <- Effect_link eff2
    | _ -> fatal_error "bind_variable"

let bind_carrier car1 car2 =
  match car1, car2 with
    | { desc  = Carrier_var _ }, car2 ->
        car1.desc <- Carrier_link car2
    | _ -> raise Unify

let bind_effect eff1 eff2 =
  match eff1, eff2 with
    | { desc  = Effect_var _ }, eff2 ->
        eff1.desc <- Effect_link eff2
    | _ -> raise Unify

let bind_react r1 r2 =
  match r1, r2 with
    | { desc  = React_var _ }, r2 ->
        r1.desc <- React_link r2
    | _ -> raise Unify

let expand_abbrev params body args =
  let params' = List.map copy_param params
  and body' = copy_clock body in
  cleanup();
  List.iter2 bind_variable params' args;
  body'

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
            occur_check expected_ck.level expected_ck actual_ck;
            expected_ck.desc <- Clock_link actual_ck
        | _, Clock_var ->
            occur_check actual_ck.level actual_ck expected_ck;
            actual_ck.desc <- Clock_link expected_ck
        | Clock_depend c1, Clock_depend c2 -> carrier_unify c1 c2
        | Clock_product ck_l1, Clock_product ck_l2 -> unify_list ck_l1 ck_l2
        | Clock_arrow(ck1, ck2, eff1), Clock_arrow(ck3, ck4, eff2) ->
            unify ck1 ck3;
            unify ck2 ck4;
            effect_unify eff1 eff2
        | Clock_constr(c1, p_l1), Clock_constr(c2, p_l2) when same_type_constr c1 c2 ->
            unify_param_list p_l1 p_l2
        | Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args), _ ->
          (*  Printf.eprintf "Replacing '%a' with '%a'\n"   Clocks_printer.output expected_ck   Clocks_printer.output (expand_abbrev params body args); *)
            unify (expand_abbrev params body args) actual_ck
        | _, Clock_constr ({ ck_info = Some { constr_abbr = Constr_abbrev(params,body) } }, args) ->
           (* Printf.eprintf "Replacing '%a' with '%a'\n"   Clocks_printer.output actual_ck   Clocks_printer.output (expand_abbrev params body args); *)
            unify expected_ck (expand_abbrev params body args)
        | Clock_process (ck1, c1, eff1, r1), Clock_process (ck2, c2, eff2, r2) ->
            unify ck1 ck2;
            carrier_unify c1 c2;
            effect_unify eff1 eff2;
            react_unify r1 r2
        | Clock_forall { cs_clock_vars = []; cs_carrier_vars = [];
                         cs_effect_vars = []; cs_desc = expected_ck }, _ ->
            unify expected_ck actual_ck
        | _, Clock_forall { cs_clock_vars = []; cs_carrier_vars = [];
                            cs_effect_vars = []; cs_desc = actual_ck } ->
            unify expected_ck actual_ck
        | Clock_forall sch1, Clock_forall sch2 ->
            unify_schema sch1 sch2
        | _ ->
             Printf.eprintf "Failed to unify '%a' and '%a'\n"  Clocks_printer.output expected_ck  Clocks_printer.output actual_ck;
            raise Unify

(* TODO: on doit mettre les quantifieurs dans leur ordre d'apparition. Ensuite, on instancie les parametres des deux schemas par les memes skolems frais, on les unifie puis on verifie que les skolems n'ont pas echappe dans l'environnement en verifiant sils apparaissent dans le type des schemas de depart. *)
and unify_schema expected_sch actual_sch =
  let m, skolems = make_fresh_skolem_subst expected_sch.cs_carrier_vars in
  let expected_ty = copy_subst_clock m expected_sch.cs_desc in
  let actual_ty = instance actual_sch in
  unify expected_ty actual_ty;
  skolem_check skolems actual_sch.cs_desc

and unify_list ck_l1 ck_l2 =
  try
    List.iter2 unify ck_l1 ck_l2
  with
    | Invalid_argument _ -> raise Unify

and carrier_unify expected_car actual_car =
  (*Printf.eprintf "Unify carrier '%a' and %a\n" Clocks_printer.output_carrier expected_car  Clocks_printer.output_carrier actual_car;*)
  if expected_car == actual_car then ()
  else
    let expected_car = carrier_repr expected_car in
    let actual_car = carrier_repr actual_car in
    if expected_car == actual_car then ()
    else
      match expected_car.desc, actual_car.desc with
        | Carrier_var(s), _ ->
  (*Printf.eprintf "Unify carrier next '%a' of level %d and '%a' of level %d\n" Clocks_printer.output_carrier expected_car expected_car.level  Clocks_printer.output_carrier actual_car actual_car.level;*)
            carrier_occur_check expected_car.level expected_car actual_car;
            expected_car.desc <- Carrier_link actual_car
        | _, Carrier_var(s) ->
            carrier_occur_check actual_car.level actual_car expected_car;
            actual_car.desc <- Carrier_link expected_car
        | Carrier_skolem(_,i), Carrier_skolem(_, j) when i = j -> ()
        | _ ->
            Printf.eprintf "Failed to unify carriers '%a' and '%a'\n"  Clocks_printer.output_carrier expected_car  Clocks_printer.output_carrier actual_car;
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
            effect_occur_check expected_eff.level expected_eff actual_eff;
            expected_eff.desc <- Effect_link (remove_var_from_effect expected_eff.index actual_eff)
        | _, Effect_var ->
            effect_occur_check actual_eff.level actual_eff expected_eff;
            actual_eff.desc <- Effect_link (remove_var_from_effect actual_eff.index expected_eff)
        | _, Effect_empty -> ()
        | Effect_depend c1, Effect_depend c2 -> carrier_unify c1 c2
        | (Effect_depend _ | Effect_empty), Effect_sum (eff1, eff2) ->
            effect_unify expected_eff eff1; effect_unify expected_eff eff2
        | Effect_sum (eff1, eff2), (Effect_depend _ | Effect_empty) ->
            effect_unify eff1 actual_eff; effect_unify eff2 actual_eff
        | Effect_sum (eff1, eff2), Effect_sum (eff3, eff4) ->
            effect_unify eff1 eff3; effect_unify eff2 eff4
        | _ ->
             Printf.eprintf "Failed to unify effects '%a' and '%a'\n"  Clocks_printer.output_effect expected_eff  Clocks_printer.output_effect actual_eff;
            raise Unify


and react_unify expected_r actual_r =
  if !Compiler_options.no_reactivity || expected_r == actual_r then ()
  else
    let expected_r = react_repr expected_r in
    let actual_r = react_repr actual_r in
    if expected_r == actual_r then ()
    else
      match expected_r.desc, actual_r.desc with
        | React_empty, React_empty -> ()
        | React_carrier c1, React_carrier c2 when c1 == c2 -> ()
        | React_top, _ -> actual_r.desc <- React_top
        | _, React_top -> expected_r.desc <- React_top
            (* phi == uphi. r *)
        (*| React_var, React_rec (r2, _) when expected_r.index = r2.index -> ()
        | React_rec(r2, _), React_var when actual_r.index = r2.index -> ()*)
        | React_var, _ ->
            let is_rec = react_occur_check_is_rec expected_r.level expected_r actual_r in
            if is_rec then (
              let new_var = new_react_var () in
              let body = subst_var_react expected_r.index new_var actual_r in
              let new_r = make_react (React_rec (false, new_var, body)) in
              expected_r.desc <- React_link new_r
            ) else
              expected_r.desc <- React_link actual_r
        | _, React_var ->
            let is_rec = react_occur_check_is_rec actual_r.level actual_r expected_r in
            if is_rec then (
              let new_var = new_react_var () in
              let body = subst_var_react actual_r.index new_var expected_r in
              let new_r = make_react (React_rec (false, new_var, body)) in
              actual_r.desc <- React_link new_r
            ) else
              actual_r.desc <- React_link expected_r
       (* | React_rec (_, expected_r), _ -> react_unify expected_r actual_r
        | _, React_rec (_, actual_r) -> react_unify expected_r actual_r *)
        | _ ->
            (*Printf.eprintf "Failed to unify reactivities '%a' and '%a'\n"  Clocks_printer.output_react expected_r  Clocks_printer.output_react actual_r; *)
            expected_r.desc <- React_link top_react;
            actual_r.desc <- React_link top_react
            (*raise Unify*)

and unify_param p1 p2 = clock_param_iter2 unify carrier_unify effect_unify react_unify p1 p2

and unify_param_list l1 l2 =
  try
    List.iter2 unify_param l1 l2
  with
    | Invalid_argument _ -> raise Unify

(* special cases of unification *)
let rec filter_arrow ck =
  let ck = clock_repr ck in
  match ck.desc with
    | Clock_arrow(ck1, ck2, eff) -> ck1, ck2, eff
    | Clock_constr({ck_info = Some { constr_abbr = Constr_abbrev (params, body) }}, args) ->
        filter_arrow (expand_abbrev params body args)
    | _ ->
        let ck1 = new_clock_var () in
        let ck2 = new_clock_var () in
        let eff = new_effect_var () in
        unify ck (arrow ck1 ck2 eff);
        ck1, ck2, eff

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
