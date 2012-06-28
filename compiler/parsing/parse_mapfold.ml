open Global_mapfold
open Misc
open Parse_ast

type 'a reac_it_funs = {
  type_expression: 'a reac_it_funs -> 'a
                -> Parse_ast.type_expression -> Parse_ast.type_expression * 'a;
  type_expression_desc: 'a reac_it_funs -> 'a
                -> Parse_ast.type_expression_desc -> Parse_ast.type_expression_desc * 'a;
  carrier_expression: 'a reac_it_funs -> 'a
                -> Parse_ast.carrier_expression -> Parse_ast.carrier_expression * 'a;
  carrier_expression_desc: 'a reac_it_funs -> 'a
                -> Parse_ast.carrier_expression_desc -> Parse_ast.carrier_expression_desc * 'a;
  effect_expression: 'a reac_it_funs -> 'a
                -> Parse_ast.effect_expression -> Parse_ast.effect_expression * 'a;
  effect_expression_desc: 'a reac_it_funs -> 'a
                -> Parse_ast.effect_expression_desc -> Parse_ast.effect_expression_desc * 'a;
  param_expression: 'a reac_it_funs -> 'a
                -> Parse_ast.param_expression -> Parse_ast.param_expression * 'a;
  type_declaration: 'a reac_it_funs -> 'a
                -> Parse_ast.type_declaration -> Parse_ast.type_declaration * 'a;
}

let rec type_expression_it funs acc e = funs.type_expression funs acc e
and type_expression funs acc e =
  let ed, acc = type_expression_desc_it funs acc e.pte_desc in
  { e with pte_desc = ed }, acc

and type_expression_desc_it funs acc ed =
  try funs.type_expression_desc funs acc ed
  with Fallback -> type_expression_desc funs acc ed
and type_expression_desc funs acc ted = match ted with
  | Ptype_var x -> Ptype_var x, acc
  | Ptype_arrow (t1, t2, ee) ->
      let t1, acc = type_expression_it funs acc t1 in
      let t2, acc = type_expression_it funs acc t2 in
      let ee, acc = effect_expression_it funs acc ee in
      Ptype_arrow (t1, t2, ee), acc
  | Ptype_tuple typ_list ->
      let typ_list, acc = mapfold (type_expression_it funs) acc typ_list in
      Ptype_tuple typ_list, acc
  | Ptype_constr (cstr, pe_list) ->
     let pe_list, acc = mapfold (param_expression_it funs) acc pe_list in
     Ptype_constr (cstr, pe_list), acc
  | Ptype_process (t,k,act,ee) ->
      let t, acc = type_expression_it funs acc t in
      let act, acc = carrier_expression_it funs acc act in
      let ee, acc = effect_expression_it funs acc ee in
      Ptype_process (t,k,act,ee), acc
  | Ptype_depend ce ->
      let ce, acc = carrier_expression_it funs acc ce in
      Ptype_depend ce, acc
  | Ptype_forall (pe_list, te) ->
      let pe_list, acc = mapfold (param_expression_it funs) acc pe_list in
      let te, acc = type_expression_it funs acc te in
      Ptype_forall (pe_list, te), acc


and carrier_expression_it funs acc e = funs.carrier_expression funs acc e
and carrier_expression funs acc e =
  let ed, acc = carrier_expression_desc_it funs acc e.pce_desc in
  { e with pce_desc = ed }, acc

and carrier_expression_desc_it funs acc ed =
  try funs.carrier_expression_desc funs acc ed
  with Fallback -> carrier_expression_desc funs acc ed
and carrier_expression_desc funs acc ced = ced, acc


and effect_expression_it funs acc e = funs.effect_expression funs acc e
and effect_expression funs acc e =
  let ed, acc = effect_expression_desc_it funs acc e.pee_desc in
  { e with pee_desc = ed }, acc

and effect_expression_desc_it funs acc ed =
  try funs.effect_expression_desc funs acc ed
  with Fallback -> effect_expression_desc funs acc ed
and effect_expression_desc funs acc eed = match eed with
  | Peff_empty | Peff_var _ | Peff_fresh -> eed, acc
  | Peff_sum (ee1, ee2) ->
      let ee1, acc = effect_expression_it funs acc ee1 in
      let ee2, acc = effect_expression_it funs acc ee2 in
      Peff_sum (ee1, ee2), acc
  | Peff_depend ce ->
      let ce, acc = carrier_expression_it funs acc ce in
      Peff_depend ce, acc


and param_expression_it funs acc ed =
  try funs.param_expression funs acc ed
  with Fallback -> param_expression funs acc ed
and param_expression funs acc pe = match pe with
  | Pptype te ->
      let te, acc = type_expression_it funs acc te in
      Pptype te, acc
  | Ppcarrier ce ->
      let ce, acc = carrier_expression_it funs acc ce in
      Ppcarrier ce, acc
  | Ppeffect ee ->
      let ee, acc = effect_expression_it funs acc ee in
      Ppeffect ee, acc

and type_declaration_it funs acc e = funs.type_declaration funs acc e
and type_declaration funs acc td = match td with
  | Ptype_abstract -> Ptype_abstract, acc
  | Ptype_rebind te ->
      let te, acc = type_expression_it funs acc te in
      Ptype_rebind te, acc
  | Ptype_variant constr_te_list ->
      let aux acc (f,te_opt) =
        let te_opt, acc = optional_wacc (type_expression_it funs) acc te_opt in
        (f,te_opt), acc
      in
      let constr_te_list, acc = mapfold aux acc constr_te_list in
      Ptype_variant constr_te_list, acc
  | Ptype_record id_mf_te_list ->
      let aux acc (f, mf, te) =
        let te, acc = type_expression_it funs acc te in
        (f, mf, te), acc
      in
      let id_mf_te_list, acc = mapfold aux acc id_mf_te_list in
      Ptype_record id_mf_te_list, acc

let defaults = {
  type_expression = type_expression;
  type_expression_desc = type_expression_desc;
  carrier_expression = carrier_expression;
  carrier_expression_desc = carrier_expression_desc;
  effect_expression = effect_expression;
  effect_expression_desc = effect_expression_desc;
  param_expression = param_expression;
  type_declaration = type_declaration;
}