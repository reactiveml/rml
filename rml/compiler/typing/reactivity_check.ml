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

(* Warnings *)
let rec_warning expr k =
  if !Misc.reactivity_warning then (
    Format.fprintf !err_fmt
      "%aWarning: This expression may produce an instantaneous recursion.@."
      Location.print expr.expr_loc
  )
(* (Types_printer.print_to_string Types_printer.print_reactivity k) *)

let loop_warning expr k =
  if !Misc.reactivity_warning then (
    Format.fprintf !err_fmt
      "%aWarning: This expression may be an instantaneous loop.@."
      Location.print expr.expr_loc
  )
(* (Types_printer.print_to_string Types_printer.print_reactivity k) *)

(* non instantaneity of behaviors *)
let visited_list, visited = mk_visited ()

let rec non_instantaneous k =
  match k.react_desc with
  | React_var -> true
  | React_pause -> true
  | React_epsilon -> false
  | React_seq kl -> List.exists non_instantaneous kl
  | React_par kl -> List.exists non_instantaneous kl
  | React_or kl -> List.for_all non_instantaneous kl
  | React_raw (k1, k2) -> non_instantaneous k1 && non_instantaneous k2
  | React_rec (_, k_body) ->
      if not (visited k) then
        non_instantaneous k_body (*or not (well_formed k)*)
      else
        true
  | React_run k_body -> non_instantaneous k_body
  | React_link k_body -> non_instantaneous k_body

let non_instantaneous k =
  visited_list := [];
  non_instantaneous k

(* correct behaviors *)
let visited_list, visited = mk_visited ()

let well_formed =
  let module Env =
    Set.Make (struct
      type t = int
      let compare = Pervasives.compare
    end)
  in
  let rec well_formed env k =
    match k.react_desc with
    | React_var -> true
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
    | React_rec (checked, k_body) ->
        if not (visited k) then (
          if checked then well_formed env k_body
          else
            let env = Env.add k.react_index env in
            let b = well_formed env k_body in
            (* if not b then k.react_desc <- React_rec (true, x, k_body); *)
            b
        ) else
          not (Env.mem k.react_index env)
    | React_run k_body -> well_formed env k_body
    | React_link k_body -> well_formed env k_body
  in
  fun k ->
    visited_list := [];
    well_formed Env.empty k


let rec check_expr_one expr =
  let k = expr.expr_reactivity_effect in
  begin match expr.expr_desc with
  | Rexpr_loop _ ->
      (* if not (well_formed k) then *)
      (*   loop_warning expr k *)
      begin match k.react_desc with
      | React_rec (false, k_body) ->
          if not (well_formed k) then begin
            loop_warning expr k;
            k.react_desc <- React_rec (true, k_body)
          end
      | React_rec (true, k_body) -> ()
      | _ -> assert false
      end
  | Rexpr_while _ ->
      begin match k.react_desc with
      | React_or [ { react_desc = React_epsilon }; k' ] ->
          begin match k'.react_desc with
          | React_rec (false, k_body) ->
              if not (well_formed k) then begin
                loop_warning expr k;
                k.react_desc <- React_rec (true, k_body)
              end
          | React_rec (true, k_body) -> ()
          | _ -> assert false
          end
      | React_epsilon -> ()
      | _ -> assert false
      end
  | Rexpr_run _ ->
      if not (well_formed k) then begin
        rec_warning expr k
      end
  | Rexpr_local _
  | Rexpr_global _
  | Rexpr_constant _
  | Rexpr_let (_, _, _)
  | Rexpr_function _| Rexpr_apply (_, _)| Rexpr_tuple _| Rexpr_construct (_, _)
  | Rexpr_array _
  | Rexpr_record _
  | Rexpr_record_access (_, _)
  | Rexpr_record_with (_, _)
  | Rexpr_record_update (_, _, _)
  | Rexpr_constraint (_, _)
  | Rexpr_trywith (_, _)
  | Rexpr_assert _
  | Rexpr_ifthenelse (_, _, _)
  | Rexpr_match (_, _)
  | Rexpr_for (_, _, _, _, _)
  | Rexpr_seq _
  | Rexpr_process _
  | Rexpr_pre (_, _)
  | Rexpr_last _
  | Rexpr_default _
  | Rexpr_nothing
  | Rexpr_pause _
  | Rexpr_halt _
  | Rexpr_emit (_, _)
  | Rexpr_fordopar (_, _, _, _, _)
  | Rexpr_par _
  | Rexpr_merge (_, _)
  | Rexpr_signal (_, _, _)
  | Rexpr_until (_, _)
  | Rexpr_when (_, _)
  | Rexpr_control (_, _, _)
  | Rexpr_get (_, _, _)
  | Rexpr_present (_, _, _)
  | Rexpr_await (_, _)
  | Rexpr_await_val (_, _, _, _, _) -> ()
  end;
  expr

let check_expr expr =
  ignore (Reac2reac.expr_map check_expr_one expr)

let check impl =
  ignore (Reac2reac.impl_map check_expr_one impl)
