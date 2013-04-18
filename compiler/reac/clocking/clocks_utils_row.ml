open Asttypes
open Misc
open Clocks
open Clocks_utils

let simplify_effect eff = eff

(* Remove local clock from effect with rows *)
(*
let visited_list, visited = mk_visited ()
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
  | Effect_row_rec body ->
    if not (visited eff) then
      eff.desc <- Effect_row_rec (remove_ck_from_effect_row ck body);
    eff
  | Effect_row_link link -> remove_ck_from_effect_row ck link

let remove_ck_from_effect ck eff =
  let eff_copy = copy_subst_effect eff in
  cleanup ();
  visited_list := [];
  remove_ck_from_effect ck eff
*)

(*
let visited_list, visited = mk_visited ()

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
  | Effect_row_var -> if eff.level > level then effect_row_empty else eff
  | Effect_row_one eff1 ->
     { eff with desc = Effect_row_one (remove_local_from_effect level bound_vars eff1) }
  | Effect_row (eff1, eff2) ->
    { eff with desc = Effect_row (remove_local_from_effect_row level bound_vars eff1,
                                  remove_local_from_effect_row level bound_vars eff2) }
  | Effect_row_rec body ->
    if not (visited eff) then
      eff.desc = Effect_row_rec (remove_local_from_effect_row level (var::bound_vars) body) }
  | Effect_row_link link -> remove_local_from_effect_row level bound_vars link
*)

let remove_local_from_effect level eff =
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


(*
let visited_list, visited = mk_visited ()
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
  | React_rec (b, r1) ->
      if not (visited r) then
        r.desc <- React_rec (b, remove_ck_from_react ck r1);
      r
  | React_link link -> remove_ck_from_react ck link

let remove_ck_from_react ck r =
  visited_list := [];
  remove_ck_from_react ck r
*)

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
