open Asttypes
open Misc
open Clocks
open Clocks_utils

(* simplification of effects *)
let equal_carrier c1 c2 =
  let c1 = carrier_repr c1 in
  let c2 = carrier_repr c2 in
  if c1 == c2 then true
  else
    match c1.desc, c2.desc with
      | Carrier_var _, Carrier_var _ -> c1.index = c2.index
      | Carrier_skolem (_, i1), Carrier_skolem (_, i2) -> i1 = i2
      | _ -> false

let rec equal_carrier_row cr1 cr2 =
  let cr1 = carrier_row_repr cr1 in
  let cr2 = carrier_row_repr cr2 in
  if cr1 == cr2 then true
  else
    match cr1.desc, cr2.desc with
      | Carrier_row_one c1, Carrier_row_one c2 -> equal_carrier c1 c2
      | Carrier_row(c11, c12), Carrier_row (c21, c22) ->
        equal_carrier_row c11 c21 && equal_carrier_row c12 c22
      | Carrier_row_empty, Carrier_row_empty -> true
      | _ -> false

(* Compares two effects, but only if they are leaves *)
let equal_effect eff1 eff2 =
  let eff1 = effect_repr eff1 in
  let eff2 = effect_repr eff2 in
  if eff1 == eff2 then true
  else
    match eff1.desc, eff2.desc with
      | Effect_var, Effect_var -> eff1.index = eff2.index
      | Effect_depend cr1, Effect_depend cr2 -> equal_carrier_row cr1 cr2
      | _ -> false

(*
let rec simplify_effect eff =
  let rec mem_effect eff l = match l with
    | [] -> false
    | eff2::l -> equal_effect eff eff2 || mem_effect eff l
  in
  let add_to_list x l =
    if mem_effect x l then l else x::l
  in
  let rec clocks_of acc eff =
    let eff = effect_repr eff in
    Printf.eprintf "before: %a  after:%a\n" Clocks_printer.output_effect eff  Clocks_printer.output_effect (try_simplify_effect eff);
    let eff = try_simplify_effect eff in
    match eff.desc with
    | Effect_empty -> acc
    | Effect_var | Effect_depend _ | Effect_one _ -> add_to_list eff acc
    | Effect_link link -> clocks_of acc link
    | Effect_sum (eff1, eff2) -> clocks_of (clocks_of acc eff2) eff1
  in
  let eff = effect_repr eff in
  let new_eff = eff_sum_list (clocks_of [] eff) in
  new_eff

and try_simplify_effect eff = match eff.desc with
  | Effect_one effr ->
    let effr = effect_row_repr effr in
    (match effr.desc with
      | Effect_row (eff1, eff2) ->
        (match (effect_row_repr eff1).desc, (effect_row_repr eff2).desc with
          | Effect_row_one leff, Effect_row_empty -> leff
          | _, _ -> eff)
      | _ -> eff)
  | _ -> eff
      *)


let add_to_list x l =
  let rec mem_effect eff l = match l with
    | [] -> false
    | eff2::l -> equal_effect eff eff2 || mem_effect eff l
  in
  if mem_effect x l then l else x::l

let simplify_effect, simplify_effect_row =
  let effect funs acc eff = match eff.desc with
    | Effect_empty -> eff, acc
    | Effect_var | Effect_depend _ -> eff, add_to_list eff acc
    | Effect_one effr ->
        let effr, _ = funs.Clock_mapfold.effect_row funs [] effr in
        let effr = effect_row_repr effr in
        (match effr.desc with
          | Effect_row (eff1, eff2) ->
              (match (effect_row_repr eff1).desc, (effect_row_repr eff2).desc with
                | Effect_row_one leff, Effect_row_empty -> funs.Clock_mapfold.effect funs acc leff
                | _, _ -> eff, add_to_list eff acc)
          | _ ->  eff, add_to_list eff acc)
    | _ -> Clock_mapfold.effect funs acc eff
  in
  let effect_row funs acc effr = match effr.desc with
    | Effect_row_one eff ->
        let _, clocks = funs.Clock_mapfold.effect funs [] eff in
        { effr with desc = Effect_row_one (eff_sum_list clocks) }, []
    | Effect_row_rec effr2 ->
        (match (effect_row_repr effr2).desc with
          | Effect_row ({ desc = Effect_row_one eff }, { desc = Effect_row_empty }) ->
              (match (effect_repr eff).desc with
                | Effect_one effr3 when (effect_row_repr effr3).index == effr.index ->
                    effr.desc <- Effect_row (eff_row_one no_effect, effect_row_empty);
                    effr, acc
                | _ -> Clock_mapfold.effect_row funs acc effr)
         (* | Effect_row ({ desc = Effect_row_one eff }, ({ desc = Effect_row_var } as var)) ->
              (match (effect_repr eff).desc with
                | Effect_one effr3 when (effect_row_repr effr3).index == effr.index ->
                    effr.desc <- Effect_row (eff_row_one no_effect, var);
                    effr, acc
                | _ -> Clock_mapfold.effect_row funs acc effr) *)
          | _ -> Clock_mapfold.effect_row funs acc effr)
    | _ -> Clock_mapfold.effect_row funs acc effr
  in
  let funs = { Clock_mapfold.defaults with
    Clock_mapfold.effect = effect; Clock_mapfold.effect_row = effect_row } in
  let simplify_effect eff =
    let eff_copy = copy_subst_effect [] eff in
    cleanup ();
    let _, clocks = Clock_mapfold.effect_it funs [] eff_copy in
    eff_sum_list clocks
  in
  let simplify_effect_row effr =
    let effr_copy = copy_subst_effect_row [] effr in
    cleanup ();
    fst (Clock_mapfold.effect_row_it funs [] effr_copy)
  in
  simplify_effect, simplify_effect_row

let simplify_clock ck =
  let effect_row funs () effr = effr.desc <- (simplify_effect_row effr).desc; effr, () in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.effect_row = effect_row } in
  Printf.eprintf "Before simplify: %a\n" Clocks_printer.output ck;
  ignore (Clock_mapfold.clock_it funs () ck);
  Printf.eprintf "After  simplify: %a\n" Clocks_printer.output ck


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
