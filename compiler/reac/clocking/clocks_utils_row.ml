open Asttypes
open Misc
open Clocks
open Clocks_utils

let simplify_effect eff = eff

(* Effect masking *)
let remove_local_from_effect eff =
  let level = !current_level in
  let effect_row funs () eff = match eff.desc with
    | Effect_row_var -> if eff.level > level then effect_row_empty, () else eff, ()
    | _ -> Clock_mapfold.effect_row funs () eff
  in
  let carrier_row funs () cr = match cr.desc with
    | Carrier_row_var -> if cr.level > level then carrier_row_empty, () else cr, ()
    | _ -> Clock_mapfold.carrier_row funs () cr
  in
  let funs = { Clock_mapfold.defaults with
    Clock_mapfold.effect_row = effect_row; Clock_mapfold.carrier_row = carrier_row } in
  let eff_copy = copy_subst_effect [] eff in
  cleanup ();
  fst (Clock_mapfold.effect_it funs () eff_copy)

(* Remove local clock from effect with rows *)
let remove_ck_from_react, remove_ck_from_effect =
  let carrier_row funs ck cr = match cr.desc with
    | Carrier_row_one car ->
      let car = carrier_repr car in
      if ck.index = car.index then carrier_row_empty, ck else cr, ck
    | _ -> Clock_mapfold.carrier_row funs ck cr
  in
  let react_effect funs ck r = match r.desc with
    | React_carrier cr ->
      let cr, _ = Clock_mapfold.carrier_row_it funs ck cr in
      (match cr.desc with
        | Carrier_row_empty -> no_react, ck
        | _ -> { r with desc = React_carrier cr }, ck)
    | _ -> Clock_mapfold.react_effect funs ck r
  in
  let funs = { Clock_mapfold.defaults with
    Clock_mapfold.react_effect = react_effect; Clock_mapfold.carrier_row = carrier_row } in
  let remove_ck_from_react ck r =
    let r_copy = copy_subst_react [] r in
    cleanup ();
    fst (Clock_mapfold.react_effect_it funs ck r_copy)
  in
  let remove_ck_from_effect ck eff =
    let eff_copy = copy_subst_effect [] eff in
    cleanup ();
    fst (Clock_mapfold.effect_it funs ck eff_copy)
  in
  remove_ck_from_react, remove_ck_from_effect
