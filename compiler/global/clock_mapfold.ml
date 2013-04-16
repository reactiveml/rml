open Misc
open Asttypes
open Clocks
open Global_mapfold

type 'a clock_it_funs = {
  clock : 'a clock_it_funs -> 'a -> clock -> clock * 'a;
  clock_desc : 'a clock_it_funs -> 'a -> clock_desc -> clock_desc * 'a;
  carrier : 'a clock_it_funs -> 'a -> carrier -> carrier * 'a;
  carrier_desc : 'a clock_it_funs -> 'a -> carrier_desc -> carrier_desc * 'a;
  carrier_row : 'a clock_it_funs -> 'a -> carrier_row -> carrier_row * 'a;
  carrier_row_desc : 'a clock_it_funs -> 'a -> carrier_row_desc -> carrier_row_desc * 'a;
  effect : 'a clock_it_funs -> 'a -> effect -> effect * 'a;
  effect_desc : 'a clock_it_funs -> 'a -> effect_desc -> effect_desc * 'a;
  effect_row : 'a clock_it_funs -> 'a -> effect_row -> effect_row * 'a;
  effect_row_desc : 'a clock_it_funs -> 'a -> effect_row_desc -> effect_row_desc * 'a;
  react_effect : 'a clock_it_funs -> 'a -> react_effect -> react_effect * 'a;
  react_effect_desc : 'a clock_it_funs -> 'a -> react_effect_desc -> react_effect_desc * 'a;
  clock_param : 'a clock_it_funs -> 'a -> clock_param -> clock_param * 'a;
  clock_scheme : 'a clock_it_funs -> 'a -> clock_scheme -> clock_scheme * 'a;
}

let rec clock_it funs acc ck = funs.clock funs acc ck
and clock funs acc ck =
  let ckd, acc = clock_desc_it funs acc ck.desc in
  { ck with desc = ckd }, acc

and clock_desc_it funs acc ckd =
  try funs.clock_desc funs acc ckd
  with Fallback -> clock_desc funs acc ckd
and clock_desc funs acc ckd = match ckd with
  | Clock_static | Clock_var -> ckd, acc
  | Clock_depend cr ->
    let cr, acc = carrier_row_it funs acc cr in
    Clock_depend cr, acc
  | Clock_arrow (ck1, ck2, eff) ->
    let ck1, acc = clock_it funs acc ck1 in
    let ck2, acc = clock_it funs acc ck2 in
    let eff, acc = effect_row_it funs acc eff in
    Clock_arrow (ck1, ck2, eff), acc
  | Clock_product ck_l ->
    let ck_l, acc = mapfold (clock_it funs) acc ck_l in
    Clock_product ck_l, acc
  | Clock_constr (cstr, p_l) ->
    let p_l, acc = mapfold (clock_param_it funs) acc p_l in
    Clock_constr (cstr, p_l), acc
  | Clock_process (ck, car, eff, r) ->
    let car, acc = carrier_it funs acc car in
    let ck, acc = clock_it funs acc ck in
    let eff, acc = effect_row_it funs acc eff in
    let r, acc = react_effect_it funs acc r in
    Clock_process (ck, car, eff, r), acc
  | Clock_link ck ->
    let ck, acc = clock_it funs acc ck in
    Clock_link ck, acc
  | Clock_forall cs ->
    let cs, acc = clock_scheme_it funs acc cs in
    Clock_forall cs, acc


and carrier_it funs acc car = funs.carrier funs acc car
and carrier funs acc car =
  let card, acc = carrier_desc_it funs acc car.desc in
  { car with desc = card }, acc

and carrier_desc_it funs acc ckd =
  try funs.carrier_desc funs acc ckd
  with Fallback -> carrier_desc funs acc ckd
and carrier_desc funs acc card = match card with
  | Carrier_var _ | Carrier_skolem _  -> card, acc
  | Carrier_link car ->
    let car, acc = carrier_it funs acc car in
    Carrier_link car, acc


and carrier_row_it funs acc cr = funs.carrier_row funs acc cr
and carrier_row funs acc cr =
  let crd, acc = carrier_row_desc_it funs acc cr.desc in
  { cr with desc = crd }, acc

and carrier_row_desc_it funs acc ckd =
  try funs.carrier_row_desc funs acc ckd
  with Fallback -> carrier_row_desc funs acc ckd
and carrier_row_desc funs acc crd = match crd with
  | Carrier_row_empty | Carrier_row_var -> crd, acc
  | Carrier_row_one car ->
    let car, acc = carrier_it funs acc car in
    Carrier_row_one car, acc
  | Carrier_row (cr1, cr2) ->
    let cr1, acc = carrier_row_it funs acc cr1 in
    let cr2, acc = carrier_row_it funs acc cr2 in
    Carrier_row (cr1, cr2), acc
  | Carrier_row_link cr1 ->
    let cr1, acc = carrier_row_it funs acc cr1 in
    Carrier_row_link cr1, acc


and effect_it funs acc eff = funs.effect funs acc eff
and effect funs acc eff =
  let effd, acc = effect_desc_it funs acc eff.desc in
  { eff with desc = effd }, acc

and effect_desc_it funs acc effd =
  try funs.effect_desc funs acc effd
  with Fallback -> effect_desc funs acc effd
and effect_desc funs acc effd = match effd with
  | Effect_empty | Effect_var -> effd, acc
  | Effect_depend car ->
    let car, acc = carrier_row_it funs acc car in
    Effect_depend car, acc
  | Effect_one er ->
    let er, acc = effect_row_it funs acc er in
    Effect_one er, acc
  | Effect_sum (eff1, eff2) ->
    let eff1, acc = effect_it funs acc eff1 in
    let eff2, acc = effect_it funs acc eff2 in
    Effect_sum (eff1, eff2), acc
  | Effect_link eff1 ->
    let eff1, acc = effect_it funs acc eff1 in
    Effect_link eff1, acc


and effect_row_it funs acc eff = funs.effect_row funs acc eff
and effect_row funs acc eff =
  let effd, acc = effect_row_desc_it funs acc eff.desc in
  { eff with desc = effd }, acc

and effect_row_desc_it funs acc effd =
  try funs.effect_row_desc funs acc effd
  with Fallback -> effect_row_desc funs acc effd
and effect_row_desc funs acc effd = match effd with
  | Effect_row_var | Effect_row_empty -> effd, acc
  | Effect_row_one eff ->
    let eff, acc = effect_it funs acc eff in
    Effect_row_one eff, acc
  | Effect_row (eff1, eff2) ->
    let eff1, acc = effect_row_it funs acc eff1 in
    let eff2, acc = effect_row_it funs acc eff2 in
    Effect_row (eff1, eff2), acc
  | Effect_row_rec (eff1, eff2) ->
    let eff1, acc = effect_row_it funs acc eff1 in
    let eff2, acc = effect_row_it funs acc eff2 in
    Effect_row_rec (eff1, eff2), acc
  | Effect_row_link eff ->
    let eff, acc = effect_row_it funs acc eff in
    Effect_row_link eff, acc


and react_effect_it funs acc r = funs.react_effect funs acc r
and react_effect funs acc r =
  let rd, acc = react_effect_desc_it funs acc r.desc in
  { r with desc = rd }, acc

and react_effect_desc_it funs acc rd =
  try funs.react_effect_desc funs acc rd
  with Fallback -> react_effect_desc funs acc rd
and react_effect_desc funs acc rd = match rd with
  | React_var | React_empty -> rd, acc
  | React_carrier car ->
    let car, acc = carrier_row_it funs acc car in
    React_carrier car, acc
  | React_seq r_l ->
    let r_l, acc = mapfold (react_effect_it funs) acc r_l in
    React_seq r_l, acc
  | React_par r_l ->
    let r_l, acc = mapfold (react_effect_it funs) acc r_l in
    React_par r_l, acc
  | React_or r_l ->
    let r_l, acc = mapfold (react_effect_it funs) acc r_l in
    React_or r_l, acc
  | React_rec (b, r1, r2) ->
    let r1, acc = react_effect_it funs acc r1 in
    let r2, acc = react_effect_it funs acc r2 in
    React_rec (b, r1, r2), acc
  | React_run r1 ->
    let r1, acc = react_effect_it funs acc r1 in
    React_run r1, acc
  | React_link r1 ->
    let r1, acc = react_effect_it funs acc r1 in
    React_link r1, acc


and clock_param_it funs acc p =
  try funs.clock_param funs acc p
  with Fallback -> clock_param funs acc p
and clock_param funs acc p = match p with
  | Kclock c ->
    let c, acc = clock_it funs acc c in
    Kclock c, acc
  | Kcarrier c ->
    let c, acc = carrier_it funs acc c in
    Kcarrier c, acc
  | Kcarrier_row c ->
    let c, acc = carrier_row_it funs acc c in
    Kcarrier_row c, acc
  | Keffect eff ->
    let eff, acc = effect_it funs acc eff in
    Keffect eff, acc
  | Keffect_row eff ->
    let eff, acc = effect_row_it funs acc eff in
    Keffect_row eff, acc
  | Kreact r ->
    let r, acc = react_effect_it funs acc r in
    Kreact r, acc


and clock_scheme_it funs acc sch = funs.clock_scheme funs acc sch
and clock_scheme funs acc sch =
  let cs_vars, acc = mapfold (clock_param_it funs) acc sch.cs_vars in
  let cs_desc, acc = clock_it funs acc sch.cs_desc in
  { sch with cs_vars = cs_vars; cs_desc = cs_desc }, acc

let defaults = {
  clock = clock;
  clock_desc = clock_desc;
  carrier = carrier;
  carrier_desc = carrier_desc;
  carrier_row = carrier_row;
  carrier_row_desc = carrier_row_desc;
  effect = effect;
  effect_desc = effect_desc;
  effect_row = effect_row;
  effect_row_desc = effect_row_desc;
  react_effect = react_effect;
  react_effect_desc = react_effect_desc;
  clock_param = clock_param;
  clock_scheme = clock_scheme;
}
