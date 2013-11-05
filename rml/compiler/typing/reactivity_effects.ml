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

open Misc
open Asttypes
open Reac_ast
open Def_types

exception React_Unify

(* generating fresh names *)
let names = new Ident.name_generator

(* The current nesting level of lets *)
let reactivity_current_level = ref 0;;


(* making reactivity effects *)
let make_react k =
  { react_desc = k;
    react_level = generic;
    react_index = names#name; }

let make_local_react k =
  { react_desc = k;
    react_level = !reactivity_current_level;
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

let new_generic_react_var () =
  { react_desc = React_var;
    react_level = generic;
    react_index = names#name }

let react_rec b k =
  { react_desc = React_rec (b, k);
    react_level = generic;
    react_index = names#name }

let react_loop k =
  let v = make_local_react React_epsilon in
  let rk = react_rec false (react_seq [ k; react_run v ]) in
  v.react_desc <- React_link rk;
  rk

let rec new_react_var_list n =
  match n with
    0 -> []
  | n -> (new_react_var ()) :: new_react_var_list (n - 1)


(* type manipulation *)
let visited_list, visited = mk_visited ()
let rec remove_local_react_var level k =
  match k.react_desc with
  | React_var ->
      if k.react_level > level
      then react_pause ()
      else k
  | React_pause -> k
  | React_epsilon -> k
  | React_seq l ->
      let l = List.map (remove_local_react_var level) l in
      { k with react_desc = React_seq l; }
  | React_par l ->
      let l = List.map (remove_local_react_var level) l in
      { k with react_desc = React_par l; }
  | React_or l ->
      let l = List.map (remove_local_react_var level) l in
      { k with react_desc = React_or l; }
  | React_raw (k1, k2) ->
      let k1 = remove_local_react_var level k1 in
      let k2 = remove_local_react_var level k2 in
      { k with react_desc = React_raw (k1, k2)}
  | React_rec (b, k1) ->
      if not (visited k) then (
        let desc = React_rec (b, remove_local_react_var level k1) in
        k.react_desc <- desc
      );
      k
  | React_run k' ->
      let desc = React_run (remove_local_react_var level k') in
      { k with react_desc = desc; }
  | React_link k' ->
      let desc = React_link (remove_local_react_var level k') in
      { k with react_desc = desc; }

let remove_local_react_var r =
  visited_list := [];
  remove_local_react_var !reactivity_current_level r

let rec split_raw k =
  match k.react_desc with
  | React_var -> [], Some k
  | React_raw (k1, k2) ->
      let k2', var = split_raw k2 in
      k1 :: k2', var
  | React_rec (_, k1) -> split_raw k1
  | React_link k -> split_raw k
  | React_pause -> [k], None
  | React_epsilon -> assert false
  | React_seq _ -> assert false
  | React_par _ -> assert false
  | React_or _  -> assert false
  | React_run _ -> assert false



(* makes a copy of a type *)

let sr = ref []
let save_react k = sr := (k, k.react_desc) :: !sr
let cleanup_react () =
  List.iter (fun (v, d) -> v.react_desc <- d) !sr;
  sr := []

let rec copy_react k =
  let level = k.react_level in
  match k.react_desc with
  | React_var ->
      if level = generic
      then
	let v = new_react_var () in
	save_react k;
	k.react_desc <- React_link(v);
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
  | React_rec (b, k1) ->
      let v = make_local_react React_epsilon in
      save_react k;
      k.react_desc <- React_link v;
      let v2 = react_rec b (copy_react k1) in
      v.react_desc <- React_link v2;
      v
  | React_run k1 ->
      if level = generic
      then
        react_run (copy_react k1)
      else
	k

let copy_react k =
  copy_react k


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


(* To compute the free type variables in a type *)
let visited_list, visited = mk_visited ()

let free_react_vars level k =
  let fv = ref [] in
  let rec free_vars k =
    let k = react_effect_repr k in
    match k.react_desc with
    | React_var ->
      if k.react_level >= level
      then fv := k :: !fv
    | React_pause -> ()
    | React_epsilon -> ()
    | React_seq kl -> List.iter (fun k -> free_vars k) kl
    | React_par kl -> List.iter (fun k -> free_vars k) kl
    | React_or kl -> List.iter (fun k -> free_vars k) kl
    | React_raw (k1, k2) -> free_vars k1; free_vars k2
    | React_rec (_, k1) ->
       if not (visited k) then free_vars k1
    | React_run k -> free_vars k
    | React_link(link) -> free_vars link
  in
  visited_list := [];
  free_vars k;
  !fv

let visited_list, visited = mk_visited ()
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
        begin match simplify_par (List.map simplify kl) [] false with
        | [] -> { k with react_desc = React_epsilon; }
        | [ k' ] -> k'
        | kl -> { k with react_desc = React_par kl; }
        end
    | React_or kl ->
        begin match simplify_or (List.map simplify kl) [] false with
        | [] -> { k with react_desc = React_pause; }
        | [ k' ] -> k'
        | kl -> { k with react_desc = React_or kl; }
        end
    | React_raw(k1, { react_desc = React_pause }) ->
        simplify k1
    | React_raw (k1, k2) ->
        { k with react_desc = React_raw(simplify k1, simplify k2) }
        (* Ne marche plus avec les comportements recursifs *)
        (*
        let kl, var_opt = split_raw k in
        let k' =
          simplify (react_or kl)
        in
        begin match var_opt with
        | None -> k'
        | Some var -> { k with react_desc = React_raw (k', var) }
        end*)
    | React_rec (b, k1) ->
        if not (visited k) then
          k.react_desc <- React_rec (b, simplify k1);
        k
    | React_run k_body ->
        let k_body = simplify k_body in
        begin match k_body.react_desc with
        | React_pause -> { k with react_desc = React_pause }
        | React_epsilon -> { k with react_desc = React_epsilon }
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
  and simplify_par kl acc pause =
    match kl with
    | [] -> List.rev acc
    | k' :: kl ->
        begin match k'.react_desc with
        | React_epsilon -> simplify_par kl acc pause
        | React_par kl' -> simplify_par (kl' @ kl) acc pause
        | React_pause ->
            if pause then simplify_par kl acc true
            else simplify_par kl (k' :: acc) true
        | React_var
        | React_seq _
        | React_or _
        | React_raw _
        | React_rec (_, _)
        | React_run _ -> simplify_par kl (k' :: acc) pause
        | React_link k -> simplify_par (k :: kl) acc pause
        end
  and simplify_or kl acc epsilon =
    match kl with
    | [] -> List.rev acc
    | k' :: kl ->
        begin match k'.react_desc with
        | React_pause -> simplify_or kl acc epsilon
        | React_or kl' -> simplify_or (kl' @ kl) acc epsilon
        | React_raw (k1, k2) ->
            let k2', var_opt = split_raw k2 in
            let k1k2' =
              begin match simplify_or (k1 :: k2' @ kl) acc epsilon with
              | [] -> { k1 with react_desc = React_pause; }
              | [ k'' ] -> k''
              | kl' -> { k1 with react_desc = React_or kl'; }
              end
            in
            begin match var_opt with
            | None -> [ k1k2' ]
            | Some var -> [ { k' with react_desc = React_raw (k1k2', var) } ]
            end
        | React_epsilon ->
            if epsilon then simplify_or kl acc true
            else simplify_or kl (k' :: acc) true
        | React_var
        | React_seq _
        | React_par _
        | React_rec (_, _)
        | React_run _ -> simplify_or kl (k' :: acc) epsilon
        | React_link k -> simplify_or (k :: kl) acc epsilon
        end
  in
  fun k ->
    visited_list := [];
    if !Misc.reactivity_simplify then simplify (react_effect_repr k)
    else react_effect_repr k

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
    | React_rec (_, _), React_rec (_, _) ->
        k1 == k2 (* TODO? *)
       (* let v = new_react_var () in
        v1.react_desc <- React_link v;
        v2.react_desc <- React_link v;
        react_equal k1 k2 *)
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



(* the occur check *)
let visited_list, visited = mk_visited ()
let rec occur_check_react level index k =
  let rec check k =
    let k = react_effect_repr k in
    match k.react_desc with
    | React_var ->
        if k.react_level > level then k.react_level <- level;
        k == index
    | React_pause -> false
    | React_epsilon -> false
    | React_seq l -> List.exists check l
    | React_par l ->  List.exists check l
    | React_or l ->  List.exists check l
    | React_raw (k1, k2) -> check k1 || check k2
    | React_rec (_, k') ->
        if not (visited k) then check k'
        else false
    | React_run k' -> check k'
    | React_link link -> check link
  in
  visited_list := [];
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
            let v = react_rec false actual_k in
            expected_k.react_desc <- React_link v
          else
            expected_k.react_desc <- React_link actual_k
      | _, React_var ->
          if occur_check_react actual_k.react_level actual_k expected_k then
            let v = react_rec false expected_k in
            actual_k.react_desc <- React_link v
          else
            actual_k.react_desc <- React_link expected_k
      | React_rec(_, expected_k), _ -> unify_react_effect expected_k actual_k
      | _, React_rec (_, actual_k) -> unify_react_effect expected_k actual_k
      | React_raw (k1_1, k2_1), React_raw (k1_2, k2_2) ->
          let kl1, v1 =
            match split_raw k2_1 with
            | _, None -> assert false
            | kl1, Some v1 -> kl1, v1
          in
          let kl2, v2 =
            match split_raw k2_2 with
            | _, None -> assert false
            | kl2, Some v2 -> kl2, v2
          in
          if react_equal v1 v2 then ()
          else begin
            let k1_1' = react_or (k1_1 :: kl1) in
            let k1_2' = react_or (k1_2 :: kl2) in
            (*if react_equal k1_1' k1_2' then unify_react_effect v1 v2
              else begin*)
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

