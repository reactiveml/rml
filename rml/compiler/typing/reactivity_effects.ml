(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the Q Public License  *)
(*  version 1.0.                                                      *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* file: reactivity_effects.ml *)

open Asttypes
open Reac_ast
open Def_types

exception React_Unify


(* Warnings *)
let rec_warning expr k =
  Printf.eprintf "%aWarning: This expression may produce an instantaneous recursion (%s).\n"
    Location.print_oc expr.expr_loc
    (Types_printer.print_to_string Types_printer.print_reactivity k)

let loop_warning expr =
  Printf.eprintf "%aWarning: This expression may be an instantaneous loop.\n"
    Location.print_oc expr.expr_loc

(* generating fresh names *)
let names = new Ident.name_generator

(* The current nesting level of lets *)
let reactivity_current_level = ref 0;;


(* making reactivity effects *)
let make_react k =
  { react_desc = k;
    react_level = generic;
    react_index = names#name; }

let react_pause () =
  make_react React_pause

let react_epsilon () =
  make_react React_epsilon

let rec react_seq kl =
  match kl with
  | [] -> react_epsilon ()
  | [k]  -> k
  | kl -> make_react (React_seq kl)

let react_par kl =
  match kl with
  | [] -> react_epsilon ()
  | [k]  -> k
  | kl -> make_react (React_par kl)

let react_or kl =
  match kl with
  | [] -> react_pause ()
  | [k]  -> k
  | kl -> make_react (React_or kl)

let react_raw k1 k2 =
  make_react (React_raw (k1, k2))

let react_rec var k =
  make_react (React_rec (var, k))

let react_run k =
  make_react (React_run k)

let no_react =
  { react_desc = React_or [];
    react_level = generic;
    react_index = -1; }

(* To get fresh type variables *)

let new_react_var () =
  { react_desc = React_var;
    react_level = !reactivity_current_level;
    react_index = names#name }

let rec new_react_var_list n =
  match n with
    0 -> []
  | n -> (new_react_var ()) :: new_react_var_list (n - 1)



(* type manipulation *)

let rec subst_react_var old_var new_var k =
  match k.react_desc with
  | React_var ->
      if old_var.react_index = k.react_index then new_var
      else k
  | React_pause -> k
  | React_epsilon -> k
  | React_seq l ->
      let l = List.map (subst_react_var old_var new_var) l in
      { k with react_desc = React_seq l; }
  | React_par l ->
      let l = List.map (subst_react_var old_var new_var) l in
      { k with react_desc = React_par l; }
  | React_or l ->
      let l = List.map (subst_react_var old_var new_var) l in
      { k with react_desc = React_or l; }
  | React_raw (k1, k2) ->
      let k1 = subst_react_var old_var new_var k1 in
      let k2 = subst_react_var old_var new_var k2 in
      { k with react_desc = React_raw (k1, k2)}
  | React_rec (x, k') ->
      assert (x.react_index <> old_var.react_index);
      { k with react_desc = React_rec (x, subst_react_var old_var new_var k'); }
  | React_run k' ->
      { k with react_desc = React_run (subst_react_var old_var new_var k'); }
  | React_link k' ->
      { k with react_desc = React_link (subst_react_var old_var new_var k'); }

let rec split_raw k =
  match k.react_desc with
  | React_var -> [], k
  | React_raw (k1, k2) ->
      let k2', var = split_raw k2 in
      k1 :: k2', var
  | React_link k -> split_raw k
  | React_pause -> assert false
  | React_epsilon -> assert false
  | React_seq _ -> assert false
  | React_par _ -> assert false
  | React_or _  -> assert false
  | React_rec (_, _) -> assert false
  | React_run _ -> assert false



(* makes a copy of a type *)

let sr = ref []
let save_react v = sr := v :: !sr
let cleanup_react () =
  List.iter (fun k -> k.react_desc <- React_var) !sr;
  sr := []


let rec copy_react k =
  let level = k.react_level in
  match k.react_desc with
  | React_var ->
      if level = generic
      then
	let v = new_react_var () in
	k.react_desc <- React_link(v);
	save_react k;
	v
      else k
  | React_link(link) ->
      if level = generic
      then link
      else copy_react link
  | React_pause ->
      if level = generic
      then
        react_pause()
      else
	k
  | React_epsilon ->
      if level = generic
      then
        react_epsilon()
      else
	k
  | React_seq kl ->
      if level = generic
      then
        react_seq (List.map (fun k -> copy_react k) kl)
      else
	k
  | React_par kl ->
      if level = generic
      then
        react_par (List.map (fun k -> copy_react k) kl)
      else
	k
  | React_or kl ->
      if level = generic
      then
        react_or (List.map (fun k -> copy_react k) kl)
      else
	k
  | React_raw (k1, k2) ->
      if level = generic
      then
        react_raw (copy_react k1) (copy_react k2)
      else
	k
  | React_rec (x, k) ->
      if level = generic
      then
        let x = copy_react x in
        let k = copy_react k in
        react_rec x k
      else
	k
  | React_run k ->
      if level = generic
      then
        react_run (copy_react k)
      else
	k


(* To take the canonical representative of a type.
   We do path compression there. *)

let rec react_effect_repr k =
  match k.react_desc with
  | React_link k' ->
      let k' = react_effect_repr k' in
      k.react_desc <- React_link k';
      k'
  | _ ->
      k

let react_simplify =
  let rec simplify k =
    match k.react_desc with
    | React_var -> k
    | React_pause -> k
    | React_epsilon -> k
    | React_seq kl ->
        begin match simplify_seq (List.map simplify kl) [] with
        | [] -> { k with react_desc = React_epsilon; }
        | [ k' ] -> k'
        | kl -> { k with react_desc = React_seq kl; }
        end
    | React_par kl ->
        begin match simplify_par (List.map simplify kl) [] with
        | [] -> { k with react_desc = React_epsilon; }
        | [ k' ] -> k'
        | kl -> { k with react_desc = React_par kl; }
        end
    | React_or kl ->
        begin match simplify_or (List.map simplify kl) [] with
        | [] -> { k with react_desc = React_pause; }
        | [ k' ] -> k'
        | kl -> { k with react_desc = React_or kl; }
        end
    | React_raw (k1, k2) ->
        let k2', var = split_raw k2 in
        let k1' =
          simplify { k with react_desc = React_or (k1 :: k2'); }
        in
        { k with react_desc = React_raw (k1', var) }
    | React_rec (k1, k2) -> { k with react_desc = React_rec (k1, simplify k2) }
    | React_run k_body ->
        let k_body = simplify k_body in
        begin match k_body.react_desc with
        (* | React_pause -> { k with react_desc = React_pause } *)
        (* | React_epsilon -> { k with react_desc = React_epsilon } *)
        | _ -> { k with react_desc = React_run k_body }
        end
    | React_link k -> simplify k
  and simplify_seq kl acc =
    match kl with
    | [] -> List.rev acc
    | k' :: kl ->
        begin match k'.react_desc with
        (* | React_pause -> List.rev_append acc [k'] *)
        | React_epsilon -> simplify_seq kl acc
        | React_seq kl' -> simplify_seq (kl' @ kl) acc
        | React_var
        | React_pause
        | React_par _
        | React_or _
        | React_raw _
        | React_rec (_, _)
        | React_run _ -> simplify_seq kl (k' :: acc)
        | React_link k -> simplify_seq (k :: kl) acc
        end
  and simplify_par kl acc =
    match kl with
    | [] -> List.rev acc
    | k' :: kl ->
        begin match k'.react_desc with
        | React_epsilon -> simplify_par kl acc
        | React_par kl' -> simplify_par (kl' @ kl) acc
        | React_var
        | React_pause
        | React_seq _
        | React_or _
        | React_raw _
        | React_rec (_, _)
        | React_run _ -> simplify_par kl (k' :: acc)
        | React_link k -> simplify_par (k :: kl) acc
        end
  and simplify_or kl acc =
    match kl with
    | [] -> List.rev acc
    | k' :: kl ->
        begin match k'.react_desc with
        | React_pause -> simplify_or kl acc
        | React_or kl' -> simplify_or (kl' @ kl) acc
        | React_raw (k1, k2) ->
            let k2', var = split_raw k2 in
            let k1k2' =
              begin match simplify_or (k1 :: k2' @ kl) acc with
              | [] -> { k1 with react_desc = React_pause; }
              | [ k'' ] -> k''
              | kl' -> { k1 with react_desc = React_or kl'; }
              end
            in
            [ { k' with react_desc = React_raw (k1k2', var) } ]
        | React_var
        | React_epsilon
        | React_seq _
        | React_par _
        | React_rec (_, _)
        | React_run _ -> simplify_or kl (k' :: acc)
        | React_link k -> simplify_or (k :: kl) acc
        end
  in
  fun k ->
    simplify (react_effect_repr k)


let react_equal =
  let rec react_equal k1 k2 =
    match k1.react_desc, k2.react_desc with
    | React_link k1, _ -> react_equal k1 k2
    | _, React_link k2 -> react_equal k1 k2
    | React_var, React_var -> k1.react_index = k2.react_index
    | React_pause, React_pause -> true
    | React_epsilon, React_epsilon -> true
    | React_seq kl1, React_seq kl2 -> react_equal_list kl1 kl2
    | React_par kl1, React_par kl2 -> react_equal_list kl1 kl2
    | React_or kl1, React_or kl2 -> react_equal_list kl1 kl2
    | React_raw (k1_1, k2_1), React_raw (k1_2, k2_2) ->
        react_equal k1_1 k1_2 && react_equal k2_1 k2_2
    | React_rec (v1, k1), React_rec (v2, k2) ->
        let v = new_react_var () in
        v1.react_desc <- React_link v;
        v2.react_desc <- React_link v;
        react_equal k1 k2
    | React_run k1, React_run k2 -> react_equal k1 k2
    | _ -> false

  and react_equal_list kl1 kl2 =
    match kl1, kl2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | k1 :: kl1, k2 :: kl2 -> react_equal k1 k2 && react_equal_list kl1 kl2

  in
  fun k1 k2 ->
    react_equal
      (react_simplify (copy_react k1))
      (react_simplify (copy_react k2))


(* non instantaneity of behaviors *)
let rec non_instantaneous k =
  match k.react_desc with
  | React_var -> true
  | React_pause -> true
  | React_epsilon -> false
  | React_seq kl -> List.exists non_instantaneous kl
  | React_par kl -> List.exists non_instantaneous kl
  | React_or kl -> List.for_all non_instantaneous kl
  | React_raw (k1, k2) -> non_instantaneous k1 && non_instantaneous k2
  | React_rec (_, k) -> non_instantaneous k
  | React_run k -> non_instantaneous k
  | React_link l -> non_instantaneous k


(* correct behaviors *)
let well_formed =
  let module Env =
    Set.Make (struct
      type t = int
      let compare = Pervasives.compare
    end)
  in
  let rec well_formed env k =
  match k.react_desc with
  | React_var -> not (Env.mem k.react_index env)
  | React_pause -> true
  | React_epsilon -> true
  | React_seq kl ->
      let b, _ =
        List.fold_left
          (fun (b, env) k ->
            let b = b && well_formed env k in
            let env = if non_instantaneous k then Env.empty else env in
            (b, env))
          (true, env)
          kl
      in
      b
  | React_par kl -> List.for_all (well_formed env) kl
  | React_or kl -> List.for_all (well_formed env) kl
  | React_raw (k1, k2) -> well_formed env k1 && well_formed env k2
  | React_rec (x, k_body) ->
      let env =
        match x.react_desc with
        | React_var -> Env.add x.react_index env
        | _ -> assert false
      in
      well_formed env k_body
  | React_run k_body -> well_formed env k_body
  | React_link k_body -> well_formed env k_body
  in
  fun k ->
    well_formed Env.empty k


(* To generalize a type *)

(* let list_of_react_vars = ref [] *)

let rec gen_react is_gen k =
  let k = react_effect_repr k in
  begin match k.react_desc with
  | React_var
  | React_pause
  | React_epsilon ->
      if k.react_level > !reactivity_current_level
      then if is_gen
      then (k.react_level <- generic;
	    (* list_of_react_vars := k :: !list_of_react_vars *))
      else k.react_level <- !reactivity_current_level
  | React_seq kl
  | React_par kl
  | React_or kl ->
      k.react_level <-
        List.fold_left (fun level k -> min level (gen_react is_gen k))
          notgeneric kl
  | React_raw (k1, k2) ->
      k.react_level <-
        min generic
          (min (gen_react is_gen k1) (gen_react is_gen k2))
  | React_run k_body ->
      k.react_level <- min generic (gen_react is_gen k_body)
  | React_rec (var, k_body) ->
      k.react_level <-
        min generic
          (min (gen_react is_gen var) (gen_react is_gen k_body))
  | React_link(link) ->
      k.react_level <- gen_react is_gen link
  end;
  k.react_level


(* the occur check *)
let rec occur_check_react level index k =
  let rec check k =
    let k = react_effect_repr k in
    match k.react_desc with
    | React_var ->
        if k.react_level > level then k.react_level <- level;
        k == index
    | React_pause -> false
    | React_epsilon -> false
    | React_seq l -> List.fold_left (fun acc k -> check k or acc) false l
    | React_par l -> List.fold_left (fun acc k -> check k or acc) false l
    | React_or l -> List.fold_left (fun acc k -> check k or acc) false l
    | React_raw (k1, k2) -> check k1 or check k2
    | React_rec (_, k') -> check k'
    | React_run k' -> check k'
    | React_link link -> check link
  in
  check k


(* unification *)
let rec unify_react_effect expected_k actual_k =
  if expected_k == actual_k then ()
  else
    let expected_k = react_effect_repr expected_k in
    let actual_k = react_effect_repr actual_k in
    if expected_k == actual_k then ()
    else
      match expected_k.react_desc, actual_k.react_desc with
      | React_var, _ ->
          if occur_check_react expected_k.react_level expected_k actual_k then
            let phi = new_react_var() in
            let kl, var = split_raw actual_k in
            (* let kl, var = [actual_k], new_react_var() in *)
            let k = react_or kl in
            let rec_phi =
              React_rec (phi, subst_react_var expected_k phi k)
            in
            let raw =
              React_raw ({ expected_k with react_desc = rec_phi; },
                         var)
            in
            expected_k.react_desc <- raw
          else
	    expected_k.react_desc <- React_link(actual_k)
      | _, React_var ->
	  if occur_check_react actual_k.react_level actual_k expected_k then
            let phi = new_react_var() in
            let kl, var = split_raw expected_k in
            (* let kl, var = [expected_k], new_react_var() in *)
            let k = react_or kl in
            let rec_phi =
              React_rec (phi, subst_react_var expected_k phi k)
            in
            let raw =
              React_raw ({ expected_k with react_desc = rec_phi; },
                         var)
            in
            actual_k.react_desc <- (* rec_phi *) raw
          else
	    actual_k.react_desc <- React_link(expected_k)
      | React_raw (k1_1, k2_1), React_raw (k1_2, k2_2) ->
          let kl1, v1 = split_raw k2_1 in
          let kl2, v2 = split_raw k2_2 in
          let k1_1' = react_or (k1_1 :: kl1) in
          let k1_2' = react_or (k1_2 :: kl2) in
          if react_equal k1_1' k1_2' then unify_react_effect v1 v2
          else begin
            let var = new_react_var () in
            let new_k1_1 = react_raw k1_1' var in
            let new_k1_2 = react_raw k1_2' var in
            unify_react_effect v1 new_k1_2;
            unify_react_effect v2 new_k1_1
          end
      (* | React_or kl1, React_or kl2 -> *)
      (*     let kl1, v1 =  try find_row_var kl1 with Not_found -> raise Unify in *)
      (*     let kl2, v2 =  try find_row_var kl2 with Not_found -> raise Unify in *)
      (*     let var = new_react_var () in *)
      (*     let new_kl1 = make_react (React_or (var::kl1)) in *)
      (*     let new_kl2 = make_react (React_or (var::kl2)) in *)
      (*     unify_react_effect v1 new_kl2; *)
      (*     unify_react_effect v2 new_kl1 *)
      (* | React_pause, React_pause -> () *)
      (* | React_epsilon, React_epsilon -> () *)
      (* | React_seq l1, React_seq l2 -> *)
      (*     List.iter2 unify_react_effect l1 l2 *)
      (* | React_par l1, React_par l2 -> *)
      (*     List.iter2 unify_react_effect l1 l2 *)
      (* | React_or l1, React_or l2 -> *)
      (*     List.iter2 unify_react_effect l1 l2 *)
      (* | React_rec _, React_rec _ -> assert false *)
      (* | React_run k1, React_run k2 -> unify_react_effect k1 k2 *)
      | _ ->
          Format.eprintf "Failed to unify reactivities '%a' and '%a'\n"
            Types_printer.print_reactivity expected_k
            Types_printer.print_reactivity actual_k;
          raise React_Unify (* ne devrait pas arriver *)

(* Reactivity effects *)
let check_epsilon k =
  match k.react_desc with
  | React_epsilon -> ()
  | _ -> () (* XXX TODO XXX *)
