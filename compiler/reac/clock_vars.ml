open Asttypes
open Global
open Clocks
open Parse_ast
open Parse_mapfold
open Binding_errors
open Misc

module Env = Map.Make(struct
  type t = Global_ident.qualified_ident
  let compare = compare
end)

let add_to_list x l =
  if List.mem x l then l else x::l

let do_n f n =
  let rec aux l i = match i with
    | 0 -> l
    | i -> aux ((f ())::l) (i-1)
  in
  aux [] n

let map1n f l =
  let aux acc x = (f x)@acc in
  List.fold_left aux [] l


let mkte d =
  { pte_desc = d; pte_loc = Location.none }
let mkce d =
  { pce_desc = d; pce_loc = Location.none }
let mkee d =
  { pee_desc = d; pee_loc = Location.none }

let mkfresh_type s =
  s, Ttype_var
let mkfresh_car () =
  Clocks_utils.generic_prefix_name^(string_of_int Clocks_utils.names#name), Tcarrier_var
let mkfresh_effect () =
  Clocks_utils.generic_effect_name^(string_of_int Clocks_utils.names#name), Teffect_var

let param_of_var (v, k) = match k with
  | Ttype_var -> Pptype (mkte (Ptype_var v))
  | Tcarrier_var -> Ppcarrier (mkce (Pcar_var v))
  | Teffect_var -> Ppeffect (mkee (Peff_var v))

let arity_of_type_vars typ_vars =
  let aux (nb_ck, nb_car, nb_eff) (_, k) = match k with
    | Ttype_var -> nb_ck + 1, nb_car, nb_eff
    | Tcarrier_var -> nb_ck, nb_car + 1, nb_eff
    | Teffect_var -> nb_ck, nb_car, nb_eff + 1
  in
  List.fold_left aux (0, 0, 0) typ_vars

let all_vars env current_id =
  let rec expand_id force id =
    if not force && id = current_id then
      []
    else
      let vars_list, id_list = Env.find id env in
      let other_vars = map1n (expand_id false) id_list in
      vars_list @ other_vars
  in
  expand_id true current_id

let arity_of_pe_list pe_list =
  let arity_of_pe (nb_ck, nb_car, nb_eff) pe = match pe with
    | Pptype _ -> nb_ck + 1, nb_car, nb_eff
    | Ppcarrier _ -> nb_ck, nb_car + 1, nb_eff
    | Ppeffect _ -> nb_ck, nb_car, nb_eff + 1
  in
  List.fold_left arity_of_pe (0, 0, 0) pe_list

let new_vars (_, found_car, found_eff) (_, nb_car, nb_eff) =
  let new_car_vars = do_n mkfresh_car (nb_car - found_car) in
  let new_eff_vars = do_n mkfresh_effect (nb_eff - found_eff) in
  new_car_vars @ new_eff_vars

let find_new_vars decl_ids td =
  let type_expression funs (vars_list, id_list) te =
    let te, (vars_list, id_list) = Parse_mapfold.type_expression funs (vars_list, id_list) te in
    match te.pte_desc with
      | Ptype_constr (cstr, pe_list) ->
          let gcstr =
            try
              Modules.pfind_type_desc cstr.pident_id
            with
              | Modules.Desc_not_found -> unbound_type_err cstr.pident_id te.pte_loc
          in
          if List.mem gcstr.gi decl_ids then
            te, (vars_list, add_to_list gcstr.gi id_list)
          else if gcstr.gi = Initialization.event_ident
                  || gcstr.gi = Initialization.memory_ident then (
            let (_, ck_arity, _) = arity_of_pe_list pe_list in
            if ck_arity = 0 then (
              let var = mkfresh_car () in
              let te = { te with pte_desc = Ptype_constr(cstr, pe_list @ [param_of_var var]) } in
              te, (var::vars_list, id_list)
            ) else
              te, (vars_list, id_list)
          ) else (
            (* check if the identifier is defined*)
            (* check the arity of parameters in the source *)
            let ck_info = match gcstr.ck_info with None -> assert false | Some i -> i in
            let found_arity = arity_of_pe_list pe_list in
            if found_arity <> ck_info.clock_def_arity then
              constr_wrong_arity_err cstr.pident_id
                found_arity ck_info.clock_def_arity te.pte_loc;
            (* find added parameters *)
            let new_vars_list = new_vars found_arity ck_info.clock_arity in
            let new_pe = List.map param_of_var new_vars_list in
            let te = { te with pte_desc = Ptype_constr (cstr, pe_list@new_pe) } in
            te, (new_vars_list@vars_list, id_list)
          )

      | _ -> te, (vars_list, id_list)
  in
  let carrier_expression_desc funs (vars_list, id_list) ced = match ced with
    | Pcar_fresh ->
        let v, k = mkfresh_car () in
        Pcar_var v, ((v, k)::vars_list, id_list)
    | _ -> raise Global_mapfold.Fallback
  in
  let effect_expression_desc funs (vars_list, id_list) eed = match eed with
    | Peff_fresh ->
        let v, k = mkfresh_effect () in
        Peff_var v, ((v, k)::vars_list, id_list)
    | _ -> raise Global_mapfold.Fallback
  in
  let funs = { Parse_mapfold.defaults with
    type_expression = type_expression;
    carrier_expression_desc = carrier_expression_desc;
    effect_expression_desc = effect_expression_desc }
  in
  Parse_mapfold.type_declaration_it funs ([], []) td


let add_missing_args env td =
  let type_expression_desc funs acc ted =
    let ted, acc = Parse_mapfold.type_expression_desc funs acc ted in
    match ted with
      | Ptype_constr (cstr, pe_list) ->
          let gcstr = Modules.pfind_type_desc cstr.pident_id in
          if Env.mem gcstr.gi env then
            let new_vars = all_vars env gcstr.gi in
            let new_params = List.map param_of_var new_vars in
            Ptype_constr (cstr, pe_list@new_params), acc
          else
            Ptype_constr (cstr, pe_list), acc
      | _ -> ted, acc
  in
  let funs = { Parse_mapfold.defaults with type_expression_desc = type_expression_desc } in
  let td, _ = Parse_mapfold.type_declaration_it funs () td in
  td

let add_missing_vars l =
  if !Compiler_options.no_clocking then
    l
  else (
    (* find missing vars and add new vars *)
    let decl_ids = List.map (fun (gl, _, _) -> gl.gi) l in
    let add_type_decl env (gl, vars, td) =
      let td, (vars_list, id_list) = find_new_vars decl_ids td in
      let env = Env.add gl.gi (vars_list, id_list) env in
      (gl, vars, td), env
    in
    let l, env = mapfold add_type_decl Env.empty l in
    (* set the correct list of vars and arity for each declared type *)
    (* put the correct value for recursively defined constructors *)
    let set_params env (gl, params, td) =
      let params = params @ (all_vars env gl.gi) in
      let info = match gl.ck_info with
        | None -> assert false
        | Some info -> { info with clock_arity = arity_of_type_vars params }
      in
      gl.ck_info <- Some info;
      (gl, params, add_missing_args env td)
    in
    List.map (set_params env) l
  )

let var_of_param vars pe = match pe with
  | Pptype { pte_desc = Ptype_var s } -> (s, Ttype_var) :: vars
  | Ppcarrier { pce_desc = Pcar_var s } -> (s, Tcarrier_var) :: vars
  | Ppeffect { pee_desc = Peff_var s } -> (s, Teffect_var) :: vars
  | _ -> vars

(* Bound locally unbounded vars in the type expression. For instance,
   (x: 'a -> 'b) is transformed into (x: exists 'a, 'b. 'a -> 'b

   (This is different from the interpretation of annotations in OCaml where
   variables are bound existentially at the nearest toplevel let) *)
let bind_annot_vars te =
  let type_expression_desc funs (bound_vars, new_vars) ted = match ted with
    | Ptype_var s ->
        if not (List.mem (s, Ttype_var) bound_vars) then
          let v = mkfresh_type s in
          ted, (v::bound_vars, v::new_vars)
        else
          ted, (bound_vars, new_vars)
    | Ptype_forall (params, te) ->
        let bound_vars = List.fold_left var_of_param bound_vars params in
        let _, acc = type_expression_it funs (bound_vars, new_vars) te in
        ted, acc
    | _ -> raise Global_mapfold.Fallback
  in
  let carrier_expression_desc funs (bound_vars, new_vars) ced = match ced with
    | Pcar_var s ->
        if not (List.mem (s, Tcarrier_var) bound_vars) then
          let v = mkfresh_car () in
          ced, (v::bound_vars, v::new_vars)
        else
          ced, (bound_vars, new_vars)
    | _ -> raise Global_mapfold.Fallback
  in
  let effect_expression_desc funs (bound_vars, new_vars) eed = match eed with
    | Peff_var s ->
        if not (List.mem (s, Teffect_var) bound_vars) then
          let v = mkfresh_effect () in
          eed, (v::bound_vars, v::new_vars)
        else
          eed, (bound_vars, new_vars)
    | _ -> raise Global_mapfold.Fallback
  in
  let funs = { Parse_mapfold.defaults with
    type_expression_desc = type_expression_desc;
    carrier_expression_desc = carrier_expression_desc;
    effect_expression_desc = effect_expression_desc }
  in
  let _, (_, params) = Parse_mapfold.type_expression_it funs ([], []) te in
  let params = List.map param_of_var params in
  mkte (Ptype_some (params, te))
