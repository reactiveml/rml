open Misc
open Reac
open Global_mapfold

type 'a reac_it_funs = {
  expression: 'a reac_it_funs -> 'a -> Reac.expression -> Reac.expression * 'a;
  expression_desc: 'a reac_it_funs -> 'a -> Reac.expression_desc -> Reac.expression_desc * 'a;
  event_config: 'a reac_it_funs -> 'a -> Reac.event_config -> Reac.event_config * 'a;
  event_config_desc:
    'a reac_it_funs -> 'a -> Reac.event_config_desc -> Reac.event_config_desc * 'a;
  pattern: 'a reac_it_funs -> 'a -> Reac.pattern -> Reac.pattern * 'a;
  pattern_desc: 'a reac_it_funs -> 'a -> Reac.pattern_desc -> Reac.pattern_desc * 'a;
  varpatt: 'a reac_it_funs -> 'a -> Reac.varpatt -> Reac.varpatt * 'a;
  type_expression:
    'a reac_it_funs -> 'a -> Reac.type_expression -> Reac.type_expression * 'a;
  type_expression_desc:
    'a reac_it_funs -> 'a -> Reac.type_expression_desc -> Reac.type_expression_desc * 'a;
  type_declaration:
    'a reac_it_funs -> 'a -> Reac.type_declaration -> Reac.type_declaration * 'a;
  impl_item: 'a reac_it_funs -> 'a -> Reac.impl_item -> Reac.impl_item * 'a;
  impl_item_desc: 'a reac_it_funs -> 'a -> Reac.impl_desc -> Reac.impl_desc * 'a;
  intf_item: 'a reac_it_funs -> 'a -> Reac.intf_item -> Reac.intf_item * 'a;
  intf_item_desc: 'a reac_it_funs -> 'a -> Reac.intf_desc -> Reac.intf_desc * 'a;
}


let rec expression_it funs acc e = funs.expression funs acc e
and expression funs acc e =
  let ed, acc = expression_desc_it funs acc e.e_desc in
  { e with e_desc = ed }, acc


and expression_desc_it funs acc ed =
  try funs.expression_desc funs acc ed
  with Fallback -> expression_desc funs acc ed
and expression_desc funs acc ed = match ed with
  | Elocal x -> Elocal x, acc
  | Eglobal id -> Eglobal id, acc
  | Econstant c -> Econstant c, acc
  | Elet (rf, p_e_list, e2) ->
    let p_e_list, acc = mapfold (pattern_expression_it funs) acc p_e_list in
    let e2, acc = expression_it funs acc e2 in
    Elet(rf, p_e_list, e2), acc
  | Efunction p_e_list ->
    let p_e_list, acc = mapfold (pattern_expression_it funs) acc p_e_list in
    Efunction p_e_list, acc
  | Eapply (e, e_list) ->
    let e, acc = expression_it funs acc e in
    let e_list, acc = mapfold (expression_it funs) acc e_list in
    Eapply (e, e_list), acc
  | Etuple e_list ->
    let e_list, acc = mapfold (expression_it funs) acc e_list in
    Etuple e_list, acc
  | Econstruct (id, e_opt) ->
    let e_opt, acc = optional_wacc (expression_it funs) acc e_opt in
    Econstruct(id, e_opt), acc
  | Earray e_list ->
    let e_list, acc = mapfold (expression_it funs) acc e_list in
    Earray e_list, acc
  | Erecord f_e_list ->
     let aux acc (f,e) =
       let e, acc = expression_it funs acc e in
       (f,e), acc
     in
    let f_e_list, acc = mapfold aux acc f_e_list in
    Erecord f_e_list, acc
  | Erecord_access (e, f) ->
    let e, acc = expression_it funs acc e in
    Erecord_access (e,f), acc
  | Erecord_update (e1, f, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    Erecord_update (e1, f, e2), acc
  | Econstraint (e, te) ->
    let e, acc = expression_it funs acc e in
    let te, acc = type_expression_it funs acc te in
    Econstraint (e, te), acc
  | Etrywith (e, p_e_list) ->
    let e, acc = expression_it funs acc e in
    let p_e_list, acc = mapfold (pattern_expression_it funs) acc p_e_list in
    Etrywith (e, p_e_list), acc
  | Eassert e ->
    let e, acc = expression_it funs acc e in
    Eassert e, acc
  | Eifthenelse (e1, e2, e3) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    let e3, acc = expression_it funs acc e3 in
    Eifthenelse (e1, e2, e3), acc
  | Ematch (e, p_e_list) ->
    let e, acc = expression_it funs acc e in
    let p_e_list, acc = mapfold (pattern_expression_it funs) acc p_e_list in
    Ematch (e, p_e_list), acc
  | Ewhen_match (e1, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    Ewhen_match (e1, e2), acc
  | Ewhile (e1, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    Ewhile (e1, e2), acc
  | Efor (x, e1, e2, d, e3) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    let e3, acc = expression_it funs acc e3 in
    Efor (x, e1, e2, d, e3), acc
  | Eseq e_list ->
    let e_list, acc = mapfold (expression_it funs) acc e_list in
    Eseq e_list, acc
  | Eprocess e ->
    let e, acc = expression_it funs acc e in
    Eprocess e, acc
  | Epre (k, e) ->
    let e, acc = expression_it funs acc e in
    Epre(k, e), acc
  | Elast e ->
    let e, acc = expression_it funs acc e in
    Elast e, acc
  | Edefault e ->
    let e, acc = expression_it funs acc e in
    Edefault e, acc
  | Enothing -> Enothing, acc
  | Epause boi -> Epause boi, acc
  | Ehalt boi -> Ehalt boi, acc
  | Eemit (e, e_opt) ->
    let e, acc = expression_it funs acc e in
    let e_opt, acc = optional_wacc (expression_it funs) acc e_opt in
    Eemit (e, e_opt), acc
  | Eloop (e_opt, e) ->
    let e_opt, acc = optional_wacc (expression_it funs) acc e_opt in
    let e, acc = expression_it funs acc e in
    Eloop (e_opt, e), acc
  | Efordopar (x, e1, e2, d, e3) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    let e3, acc = expression_it funs acc e3 in
    Efordopar (x, e1, e2, d, e3), acc
  | Epar e_list ->
    let e_list, acc = mapfold (expression_it funs) acc e_list in
    Epar e_list, acc
  | Emerge (e1, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    Emerge (e1, e2), acc
  | Esignal ((x, te_opt), e_e_opt, e) ->
    let aux acc (e1, e2) =
      let e1, acc = expression_it funs acc e1 in
      let e2, acc = expression_it funs acc e2 in
      (e1, e2), acc
    in
    let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
    let e_e_opt, acc = optional_wacc aux acc e_e_opt in
    let e, acc = expression_it funs acc e in
    Esignal ((x, te_opt), e_e_opt, e), acc
  | Erun e ->
    let e, acc = expression_it funs acc e in
    Erun e, acc
  | Euntil (evc, e, p_e_opt) ->
    let evc, acc = event_config_it funs acc evc in
    let e, acc = expression_it funs acc e in
    let p_e_opt, acc = optional_wacc (pattern_expression_it funs) acc p_e_opt in
    Euntil (evc, e, p_e_opt), acc
  | Ewhen (evc, e) ->
    let evc, acc = event_config_it funs acc evc in
    let e, acc = expression_it funs acc e in
    Ewhen (evc, e), acc
  | Econtrol (evc, p_e_opt, e) ->
    let evc, acc = event_config_it funs acc evc in
    let p_e_opt, acc = optional_wacc (pattern_expression_it funs) acc p_e_opt in
    let e, acc = expression_it funs acc e in
    Econtrol (evc, p_e_opt, e), acc
  | Eget (e1, p, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let p, acc = pattern_it funs acc p in
    let e2, acc = expression_it funs acc e2 in
    Eget (e1, p, e2), acc
  | Epresent (evc, e1, e2) ->
    let evc, acc = event_config_it funs acc evc in
    let e1, acc = expression_it funs acc e1 in
    let e2, acc = expression_it funs acc e2 in
    Epresent (evc, e1, e2), acc
  | Eawait (imf, evc) ->
    let evc, acc = event_config_it funs acc evc in
    Eawait (imf, evc), acc
  | Eawait_val (imf, ak, e1, p, e2) ->
    let e1, acc = expression_it funs acc e1 in
    let p, acc = pattern_it funs acc p in
    let e2, acc = expression_it funs acc e2 in
    Eawait_val (imf, ak, e1, p, e2), acc


and pattern_expression_it funs acc (p,e) =
  let p, acc = pattern_it funs acc p in
  let e, acc = expression_it funs acc e in
  (p,e), acc


and event_config_it funs acc e = funs.event_config funs acc e
and event_config funs acc c =
  let cd, acc = event_config_desc_it funs acc c.conf_desc in
  { c with conf_desc = cd }, acc


and event_config_desc_it funs acc ed =
  try funs.event_config_desc funs acc ed
  with Fallback -> event_config_desc funs acc ed
and event_config_desc funs acc ed = match ed with
  | Cpresent e ->
    let e, acc = expression_it funs acc e in
    Cpresent e, acc
  | Cand (evc1, evc2) ->
    let evc1, acc = event_config_it funs acc evc1 in
    let evc2, acc = event_config_it funs acc evc2 in
    Cand(evc1, evc2), acc
  | Cor (evc1, evc2) ->
    let evc1, acc = event_config_it funs acc evc1 in
    let evc2, acc = event_config_it funs acc evc2 in
    Cor(evc1, evc2), acc


and pattern_it funs acc p = funs.pattern funs acc p
and pattern funs acc p =
  let pd, acc = pattern_desc_it funs acc p.patt_desc in
  { p with patt_desc = pd }, acc


and pattern_desc_it funs acc pd =
  try funs.pattern_desc funs acc pd
  with Fallback -> pattern_desc funs acc pd
and pattern_desc funs acc pd = match pd with
  | Pany -> Pany, acc
  | Pvar vp ->
    let vp, acc = varpatt_it funs acc vp in
    Pvar vp, acc
  | Palias (p, vp) ->
    let p, acc = pattern_it funs acc p in
    let vp, acc = varpatt_it funs acc vp in
    Palias (p, vp), acc
  | Pconstant c -> Pconstant c, acc
  | Ptuple p_list ->
    let p_list, acc = mapfold (pattern_it funs) acc p_list in
    Ptuple p_list, acc
  | Pconstruct (id, p_opt) ->
    let p_opt, acc = optional_wacc (pattern_it funs) acc p_opt in
    Pconstruct (id, p_opt), acc
  | Por (p1, p2) ->
    let p1, acc = pattern_it funs acc p1 in
    let p2, acc = pattern_it funs acc p2 in
    Por (p1, p2), acc
  | Precord f_p_list ->
    let aux acc (f,p) =
       let p, acc = pattern_it funs acc p in
       (f,p), acc
    in
    let f_p_list, acc = mapfold aux acc f_p_list in
    Precord f_p_list, acc
  | Parray p_list ->
    let p_list, acc = mapfold (pattern_it funs) acc p_list in
    Parray p_list, acc
  | Pconstraint (p, te) ->
    let p, acc = pattern_it funs acc p in
    let te, acc = type_expression_it funs acc te in
    Pconstraint (p, te), acc


and varpatt_it funs acc vp =
  try funs.varpatt funs acc vp
  with Fallback -> varpatt funs acc vp
and varpatt funs acc vp = vp, acc


and type_expression_it funs acc te = funs.type_expression funs acc te
and type_expression funs acc te =
  let ted, acc = type_expression_desc_it funs acc te.te_desc in
  { te with te_desc = ted }, acc


and type_expression_desc_it funs acc ted =
  try funs.type_expression_desc funs acc ted
  with Fallback -> type_expression_desc funs acc ted
and type_expression_desc funs acc ted = match ted with
  | Tvar s -> Tvar s, acc
  | Tarrow (te1, te2) ->
    let te1, acc = type_expression_it funs acc te1 in
    let te2, acc = type_expression_it funs acc te2 in
    Tarrow (te1, te2), acc
  | Tproduct te_list ->
    let te_list, acc = mapfold (type_expression_it funs) acc te_list in
    Tproduct te_list, acc
  | Tconstr (id, te_list) ->
    let te_list, acc = mapfold (type_expression_it funs) acc te_list in
    Tconstr (id,te_list), acc
  | Tprocess (te, i) ->
    let te, acc = type_expression_it funs acc te in
    Tprocess (te, i), acc


and type_declaration_it funs acc ted =
  try funs.type_declaration funs acc ted
  with Fallback -> type_declaration funs acc ted
and type_declaration funs acc ted = match ted with
 | Tabstract -> Tabstract, acc
  | Trebind te ->
    let te, acc = type_expression_it funs acc te in
    Trebind te, acc
  | Tvariant id_te_opt_list ->
    let aux acc (f,te_opt) =
      let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
       (f,te_opt), acc
    in
    let id_te_opt_list, acc = mapfold aux acc id_te_opt_list in
    Tvariant id_te_opt_list, acc
  | Trecord id_mf_te_list ->
   let aux acc (f, mf, te) =
      let te, acc = type_expression_it funs acc te in
       (f, mf, te), acc
    in
    let id_mf_te_list, acc = mapfold aux acc id_mf_te_list in
    Trecord id_mf_te_list, acc



and impl_item_it funs acc i = funs.impl_item funs acc i
and impl_item funs acc i =
  let id, acc = impl_item_desc_it funs acc i.impl_desc in
  { i with impl_desc = id }, acc


and impl_item_desc_it funs acc id =
  try funs.impl_item_desc funs acc id
  with Fallback -> impl_item_desc funs acc id
and impl_item_desc funs acc id = match id with
  | Iexpr e ->
    let e, acc = expression_it funs acc e in
    Iexpr e, acc
  | Ilet (rf, p_e_list) ->
    let p_e_list, acc = mapfold (pattern_expression_it funs) acc p_e_list in
    Ilet (rf, p_e_list), acc
  | Isignal v_list ->
    let exp_exp_it acc (e1, e2) =
      let e1, acc = expression_it funs acc e1 in
      let e2, acc = expression_it funs acc e2 in
      (e1, e2), acc
    in
    let aux acc ((f,te_opt), e_e_opt) =
      let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
      let e_e_opt, acc = optional_wacc exp_exp_it acc e_e_opt in
      ((f,te_opt), e_e_opt), acc
    in
    let v_list, acc = mapfold aux acc v_list in
    Isignal v_list, acc
  | Itype i_list ->
    let aux acc (id, sl, td) =
      let td, acc = type_declaration_it funs acc td in
      (id, sl, td), acc
    in
    let i_list, acc = mapfold aux acc i_list in
    Itype i_list, acc
  | Iexn (id, te_opt) ->
    let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
    Iexn (id, te_opt), acc
  | Iexn_rebind (id1, id2) -> Iexn_rebind (id1, id2), acc
  | Iopen s -> Iopen s, acc



and intf_item_it funs acc i = funs.intf_item funs acc i
and intf_item funs acc i =
  let id, acc = intf_item_desc_it funs acc i.intf_desc in
  { i with intf_desc = id }, acc


and intf_item_desc_it funs acc id =
  try funs.intf_item_desc funs acc id
  with Fallback -> intf_item_desc funs acc id
and intf_item_desc funs acc id = match id with
  | Dval (id, te) ->
    let te, acc = type_expression_it funs acc te in
    Dval (id, te), acc
  | Dtype i_list ->
    let aux acc (id, sl, td) =
      let td, acc = type_declaration_it funs acc td in
      (id, sl, td), acc
    in
    let i_list, acc = mapfold aux acc i_list in
    Dtype i_list, acc
  | Dexn (id, te_opt) ->
    let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
    Dexn (id, te_opt), acc
  | Dopen s -> Dopen s, acc


let defaults = {
  expression = expression;
  expression_desc = expression_desc;
  event_config = event_config;
  event_config_desc = event_config_desc;
  pattern = pattern;
  pattern_desc = pattern_desc;
  varpatt = varpatt;
  type_expression = type_expression;
  type_expression_desc = type_expression_desc;
  type_declaration = type_declaration;
  impl_item = impl_item;
  impl_item_desc = impl_item_desc;
  intf_item = intf_item;
  intf_item_desc = intf_item_desc;
}

