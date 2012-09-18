open Asttypes
open Clocks
open Reac
open Misc

exception Bad_recursion

let instantaneous_loop_err e r =
  Printf.eprintf
    "%aWarning: This loop might be instantaneous\n\
    (effect: %a).\n"
    Location.print_oc e.e_loc
    Clocks_printer.output_react r;
  if !Compiler_options.warning_are_errors then raise Error

(*
let instantaneous_rec_err e ck =
  Printf.eprintf
    "%aWarning: This recursive process might be instantaneous\n\
    (clock: %a).\n"
    Location.print_oc e.e_loc
    Clocks_printer.output ck;
  if !Compiler_options.warning_are_errors then raise Error
*)

let instantaneous_run_err e r =
  Printf.eprintf
    "%aWarning: This process might be instantaneous\n\
    (effect: %a).\n"
    Location.print_oc e.e_loc
    Clocks_printer.output_react r;
  if !Compiler_options.warning_are_errors then raise Error

let nonreactive_domain_err e r =
  Printf.eprintf
    "%aWarning: This reactive domain might not be reactive\n\
    (effect: %a).\n"
    Location.print_oc e.e_loc
    Clocks_printer.output_react r;
  if !Compiler_options.warning_are_errors then raise Error

let rec is_not_instantaneous index r = match r.desc with
  | React_top -> raise Bad_recursion
  | React_empty -> false
  | React_carrier _ -> true
  | React_var ->
      if r.index = index then
        raise Bad_recursion
      else
        true
  | React_seq rl -> is_not_instantaneous_list index rl
  | React_or rl -> List.for_all (is_not_instantaneous index) rl
  | React_par rl -> (* like a for_all but without bypass *)
      let rec aux acc r =
        is_not_instantaneous index r || acc
      in
      List.fold_left aux false rl
  | React_rec _ -> true (* otherwise there would be an error *)
  | React_run r | React_link r -> is_not_instantaneous index r

and is_not_instantaneous_list index rl = match rl with
  | [] -> false
  | r::rl ->
      is_not_instantaneous index r || is_not_instantaneous_list index rl

(*
let is_bad_rec loc index r =
  try
    let _ = is_bad_rec index r in false
  with
    | Bad_recursion -> true
*)

let rec check_clock ck = match ck.desc with
  | Clock_static | Clock_var | Clock_depend _ -> ()
  | Clock_arrow (ck1, ck2, _) -> check_clock ck1; check_clock ck2
  | Clock_product ck_list -> List.iter check_clock ck_list
  | Clock_constr (_, p_list) -> List.iter check_param p_list
  | Clock_process (ck1, _, _, r) -> check_clock ck1; check_react r
  | Clock_link ck -> check_clock ck
  | Clock_forall { cs_desc = ck } -> check_clock ck

and check_param p = match p with
  | Var_clock ck -> check_clock ck
  | _ -> ()

and check_react r = match r.desc with
  | React_var | React_top | React_empty | React_carrier _ -> ()
  | React_seq rl | React_or  rl | React_par rl ->
      List.iter check_react rl
  | React_rec (var, r1) ->
      (* first check the body *)
      check_react r1;
      (* then check if this recursion is ok *)
      ignore (is_not_instantaneous var.index r1)
  | React_run r1 | React_link r1 -> check_react r1

let check_react r =
  try
    check_react r; false
  with
      Bad_recursion -> true

let check_clock ck =
  try
    check_clock ck; false
  with
    | Bad_recursion -> true

let expression funs acc e =
  let e, _ = Reac_mapfold.expression funs acc e in
  let _ =
    match e.e_desc with
      | Erun _ -> if check_react e.e_react then instantaneous_run_err e e.e_react
      | Eloop _ -> if check_react e.e_react then instantaneous_loop_err e e.e_react
     (* | Elet (Recursive, p_e_list, _) ->
          let check_exp (p, e) =
            if check_clock e.e_clock then instantaneous_rec_err e e.e_clock
          in
          List.iter check_exp p_e_list *)
      | Enewclock _ -> Printf.printf "Domain: %a\n" Clocks_printer.output_react e.e_react; if check_react e.e_react then nonreactive_domain_err e e.e_react
      | _ -> ()
  in
  e, acc

let impl _ impl =
  let funs = { Reac_mapfold.defaults with Reac_mapfold.expression = expression } in
  let _ = Reac_mapfold.impl_item_it funs () impl in
  impl
