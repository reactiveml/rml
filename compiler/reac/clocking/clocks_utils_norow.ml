open Asttypes
open Misc
open Clocks
open Clocks_utils

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


let remove_var_from_effect index eff =
  let effect funs () eff = match eff.desc with
     | Effect_var when eff.index = index  -> no_effect, ()
     | _ -> Clock_mapfold.effect funs () eff
  in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.effect = effect } in
  let eff = fst (Clock_mapfold.effect_it funs () eff) in
  simplify_effect eff

(* Remove local clock from effect without rows *)
let remove_ck_from_effect ck eff =
  let effect funs () eff = match eff.desc with
     | Effect_depend { desc = Carrier_row_one c } ->
       let c = carrier_repr c in
       (match c.desc with
         | Carrier_skolem _ when c.index = ck.index -> no_effect, ()
         | _ -> eff, ())
     | _ -> Clock_mapfold.effect funs () eff
  in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.effect = effect } in
  let eff = fst (Clock_mapfold.effect_it funs () eff) in
  simplify_effect eff

let remove_ck_from_react ck r =
  let react_effect funs () r = match r.desc with
     | React_carrier { desc = Carrier_row_one c } ->
       let c = carrier_repr c in
       (match c.desc with
         | Carrier_skolem _ when c.index = ck.index -> no_react, ()
         | _ -> r, ())
     | _ -> Clock_mapfold.react_effect funs () r
  in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.react_effect = react_effect } in
  fst (Clock_mapfold.react_effect_it funs () r)
