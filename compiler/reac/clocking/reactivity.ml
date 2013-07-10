open Asttypes
open Clocks
open Reac
open Misc
open Clocks_utils
open Clocks_utils_row

exception Bad_recursion

let instantaneous_loop_err e r =
  if !Compiler_options.show_reactivity then
    Format.eprintf
      "%aWarning: This expression may be an instantaneous loop\n\
         (effect: %a).\n"
      Location.print e.e_loc
      Clocks_printer.output_react r
  else
    Format.eprintf
      "%aWarning: This expression may be an instantaneous loop.\n"
      Location.print e.e_loc;
  if !Compiler_options.warning_are_errors then raise Error

let instantaneous_rec_err e ck =
  if !Compiler_options.show_reactivity then
    Format.eprintf
      "%aWarning: This definition may produce an instantaneous recursion\n\
         (clock: %a).\n"
      Location.print e.e_loc
      Clocks_printer.output ck
  else
    Format.eprintf
      "%aWarning: This definition may produce an instantaneous recursion.\n"
      Location.print e.e_loc;
  if !Compiler_options.warning_are_errors then raise Error

let instantaneous_run_err e r =
  if !Compiler_options.show_reactivity then
    Format.eprintf
      "%aWarning: This expression may produce an instantaneous recursion\n\
         (effect: %a).\n"
      Location.print e.e_loc
      Clocks_printer.output_react r
  else
    Format.eprintf
      "%aWarning: This expression may produce an instantaneous recursion.\n"
      Location.print e.e_loc;
  if !Compiler_options.warning_are_errors then raise Error

let nonreactive_domain_err e r =
  if !Compiler_options.show_reactivity then
    Format.eprintf
      "%aWarning: This reactive domain may not be reactive\n\
         (effect: %a).\n"
      Location.print e.e_loc
      Clocks_printer.output_react r
  else
    Format.eprintf
      "%aWarning: This reactive domain may not be reactive.\n"
      Location.print e.e_loc;
  if !Compiler_options.warning_are_errors then raise Error


module IntSet =
  Set.Make (struct
    type t = int
    let compare = Pervasives.compare
  end)

(* Operations on reactivity effects *)

(* effect masking *)
let visited_list, visited = mk_visited ()

let is_not_local_var level r = match r.desc with
  | React_var when r.level > level -> false
  | _ -> true

let rec remove_local_from_react level r = match r.desc with
  | React_empty | React_var | React_carrier _ -> r
  | React_seq rl ->
      { r with desc = React_seq (List.map (remove_local_from_react level) rl) }
  | React_par rl ->
      { r with desc = React_par (List.map (remove_local_from_react level) rl) }
  | React_or rl ->
      let rl = List.filter (is_not_local_var level) rl in
      (match rl with
        | [] -> assert false
        | [r] -> r
        | _ -> { r with desc = React_or (List.map (remove_local_from_react level) rl) })
  | React_run r1 ->
      { r with desc = React_run (remove_local_from_react level r1) }
  | React_rec (b, r1) ->
      if not (visited r) then
        r.desc <- React_rec (b, remove_local_from_react level r1);
      r
  | React_link link ->
      { r with desc = React_link (remove_local_from_react level link) }

let remove_local_from_react r =
  visited_list := [];
  let r_copy = copy_subst_react [] r in
  cleanup ();
  remove_local_from_react !current_level r_copy


let subst_var_react index subst r =
  let react_effect funs () r =  match r.desc with
    | React_var when r.index = index -> subst, ()
    | _ -> Clock_mapfold.react_effect funs () r
  in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.react_effect = react_effect } in
  let r_copy = copy_subst_react [] r in
  cleanup ();
  fst (Clock_mapfold.react_effect_it funs () r_copy)


let visited_list, visited = mk_visited ()
let cond_subst_react ck subst r =
  let sub ckz cksub =
    List.iter (fun ck -> ck.desc <- React_empty) ckz;
    List.iter (fun ck -> ck.desc <- React_link subst) cksub
  in
  let rec aux env (ckz, cksub, b) r = match r.desc with
    | React_empty | React_var -> ckz, cksub, b
    | React_carrier ck ->
      if ck.index = r.index then r::ckz, cksub, b else ckz, r::cksub, true
    | React_seq rl ->
      let one_step (ckz, cksub, b) r =
        let env = if b then IntSet.empty else env in aux env (ckz, cksub, b) r
      in
      List.fold_left one_step (ckz, cksub, b) rl
    | React_or rl ->
      let ckz_list, cksub_list, b_list = Misc.split3 (List.map (aux env (ckz, cksub, b)) rl) in
      List.flatten ckz_list, List.flatten cksub_list, b || List.for_all (fun b -> b) b_list
    | React_par rl ->
      let ckz_list, cksub_list, b_list = Misc.split3 (List.map (aux env (ckz, cksub, b)) rl) in
      List.flatten ckz_list, List.flatten cksub_list, b || (List.exists (fun b -> b) b_list)
    | React_run r1 -> aux env (ckz, cksub, b) r1
    | React_rec(_, r1) ->
      if not (visited r) then
        let env = IntSet.add r.index env in
        aux env (ckz, cksub, b) r1
      else
        (if IntSet.mem r.index env then
            (sub ckz cksub; [],[],b)
         else
            ckz, cksub, b)
    | React_link r1 -> aux env (ckz, cksub, b) r1
  in
  let r_copy = copy_subst_react [] r in
  cleanup ();
  let ckz, cksub, _ = aux IntSet.empty ([], [], false) r_copy in
  sub ckz cksub;
  r_copy


let visited_list, visited = mk_visited ()
let rec is_not_instantaneous r = match r.desc with
  | React_empty -> false
  | React_carrier _ -> true
  | React_var -> true
  | React_seq rl | React_par rl -> List.exists is_not_instantaneous rl
  | React_or rl -> List.for_all is_not_instantaneous rl
  | React_rec (_, r1) ->
      if not (visited r) then
        is_not_instantaneous r1
      else
        true
  | React_run r | React_link r -> is_not_instantaneous r

let is_not_instantaneous r =
  visited_list := [];
  is_not_instantaneous r


let visited_list, visited = mk_visited ()

let rec check_react env r = match r.desc with
  | React_var | React_empty | React_carrier _ -> ()
  | React_seq rl ->
    let check_one env r =
      check_react env r;
      if is_not_instantaneous r then IntSet.empty else env
    in
    ignore (List.fold_left check_one env rl)
  | React_or rl | React_par rl ->
      List.iter (check_react env) rl
  | React_rec (checked, r1) ->
      if not (visited r) then (
        if checked then
          check_react env r1
        else (
          try
            check_react (IntSet.add r.index env) r1
          with
            | Bad_recursion -> r.desc <- React_rec (true, r1); raise Bad_recursion)
      ) else
        if IntSet.mem r.index env then raise Bad_recursion
  | React_run r1 | React_link r1 -> check_react env r1

let check_react r =
  try
    visited_list := [];
    check_react IntSet.empty r; false
  with
    | Bad_recursion -> true

let check_clock ck =
  let react_effect funs b r =
    let is_bad = check_react r in
    r, b or is_bad
  in
  let funs = { Clock_mapfold.defaults with Clock_mapfold.react_effect = react_effect } in
  let _, b  = Clock_mapfold.clock_it funs false ck in
  b

let check_exp e =  match e.e_desc with
  | Erun _ -> if check_react e.e_react then instantaneous_run_err e e.e_react
  | Eloop _ -> if check_react e.e_react then instantaneous_loop_err e e.e_react
  | Enewclock (_, _, None, _) -> (* only check domains without by *)
      if check_react e.e_react then nonreactive_domain_err e e.e_react
  | _ -> ()

let expression funs acc e =
  let e, _ = Reac_mapfold.expression funs acc e in
  check_exp e;
  e, acc

let impl _ impl =
  let funs = { Reac_mapfold.defaults with Reac_mapfold.expression = expression } in
  let _ = Reac_mapfold.impl_item_it funs () impl in
  impl
