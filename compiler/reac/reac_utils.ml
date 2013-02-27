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

(* file: reac_misc.ml *)
(* created: 2005-05-05  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Functions on Reac AST *)

open Asttypes
open Reac
open Types
open Types_utils
open Clocks
open Clocks_utils

let make_expr_all e typ ck react static reactivity loc =
  { e_desc = e;
    e_loc = loc;
    e_type = typ;
    e_clock = ck;
    e_react = react;
    e_static = static;
    e_reactivity = reactivity; }

let make_expr e loc =
  { e_desc = e;
    e_loc = loc;
    e_type = no_type_expression;
    e_clock = no_clock;
    e_react = no_react;
    e_static = Static.Dynamic Static.Dontknow;
    e_reactivity = []; }

let make_patt p loc =
  { patt_desc = p;
    patt_loc = loc;
    patt_type = no_type_expression;
    patt_clock = no_clock }

let make_conf c loc =
  { conf_desc = c;
    conf_loc = loc; }

let make_te t loc =
  { te_desc = t;
    te_loc = loc; }

let make_ce t loc =
  { ce_desc = t;
    ce_loc = loc; }

let make_cer t loc =
  { cer_desc = t;
    cer_loc = loc; }

let make_ee t loc =
  { ee_desc = t;
    ee_loc = loc; }

let make_eer t loc =
  { eer_desc = t;
    eer_loc = loc; }

let make_impl it loc =
  { impl_desc = it;
    impl_loc = loc; }

let make_intf it loc =
  { intf_desc = it;
    intf_loc = loc; }


let string_of_varpatt x =
  begin match x with
  | Vlocal id -> Ident.unique_name id
  | Vglobal x -> Global.little_name_of_global x
  end


(* Compute the list of variables introduce in a pattern *)
let rec vars_of_patt p =
  match p.patt_desc with
  | Pany -> []

  | Pvar x -> [x]

  | Palias (patt,x) -> x :: (vars_of_patt patt)

  | Pconstant _ -> []

  | Ptuple patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list

  | Pconstruct (_, None) -> []

  | Pconstruct (_, Some patt) -> vars_of_patt patt

  | Por (patt1, patt2) -> (vars_of_patt patt1) @ (vars_of_patt patt2)

  | Precord label_patt_list ->
      List.fold_left
        (fun vars (_,patt) -> (vars_of_patt patt)@vars)
        [] label_patt_list

  | Parray patt_list ->
      List.fold_left (fun vars patt -> (vars_of_patt patt)@vars) [] patt_list

  | Pconstraint (patt, _) -> vars_of_patt patt


(* Checks that a variable is not in a variables list *)
let rec is_free x vars =
  begin match vars with
  | [] -> true
  | x' :: vars' ->
      begin match x, x' with
      | Vlocal id1, Vlocal id2 ->
          if Ident.same id1 id2 then
            false
          else
            is_free x vars'
      | Vglobal gl1, Vglobal gl2 ->
          if Global_ident.same gl1.Global.gi gl2.Global.gi then
            false
          else
            is_free x vars'
      | _ ->
          is_free x vars'
      end
  end

(* Compute the list of free variables of an expression *)
let expr_free_vars e =
  let fv = ref [] in
  let rec expr_free_vars vars expr =
    begin match expr.e_desc with
    | Elocal x ->
        if is_free (Vlocal x) vars then
          fv := (Vlocal x) :: !fv

    | Eglobal x ->
        if is_free (Vglobal x) vars then
          fv := (Vglobal x) :: !fv

    | Econstant _ -> ()

    | Elet (rec_flag, patt_expr_list, expr) ->
        let vars' =
          List.fold_left
            (fun vars' (p, _) -> (vars_of_patt p) @ vars')
            vars
            patt_expr_list
        in
        if rec_flag = Recursive then
          List.iter
            (fun (_, e) -> expr_free_vars vars' e)
            patt_expr_list
        else
          List.iter
            (fun (_, e) -> expr_free_vars vars e)
            patt_expr_list;
        expr_free_vars vars' expr

    | Efunction patt_expr_list ->
        List.iter
          (fun (p,e) ->
            let vars' = (vars_of_patt p) @ vars in
            expr_free_vars vars' e)
          patt_expr_list

    | Eapply (e, expr_list) ->
        expr_free_vars vars e;
        List.iter (expr_free_vars vars) expr_list

    | Etuple expr_list ->
        List.iter (expr_free_vars vars) expr_list

    | Econstruct (const, None) -> ()

    | Econstruct (const, Some e) ->
        expr_free_vars vars e

    | Earray expr_list ->
        List.iter (expr_free_vars vars) expr_list

    | Erecord lbl_expr_list ->
        List.iter (fun (_,e) -> expr_free_vars vars e) lbl_expr_list

    | Erecord_access (e, lbl) ->
        expr_free_vars vars e

    | Erecord_with (e, lbl_expr_list) ->
        expr_free_vars vars e;
        List.iter (fun (_,e) -> expr_free_vars vars e) lbl_expr_list

    | Erecord_update (e1, lbl, e2) ->
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Econstraint (e, ty) ->
        expr_free_vars vars e

    | Etrywith (e, patt_expr_list) ->
        expr_free_vars vars e;
        List.iter
          (fun (p,e) ->
            let vars' = (vars_of_patt p) @ vars in
            expr_free_vars vars' e)
          patt_expr_list

    | Eassert e ->
        expr_free_vars vars e

    | Eifthenelse(e,e1,e2) ->
        expr_free_vars vars e;
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Ematch (e, patt_expr_list) ->
        expr_free_vars vars e;
        List.iter
          (fun (p,e) ->
            let vars' = (vars_of_patt p) @ vars in
            expr_free_vars vars' e)
          patt_expr_list

    | Ewhen_match (e1,e2) ->
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Ewhile (e1,e2) ->
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Efor (ident, e1, e2, direction_flag, e) ->
        let vars' = (Vlocal ident) :: vars in
        expr_free_vars vars' e1;
        expr_free_vars vars' e2;
        expr_free_vars vars' e

    | Eseq e_list ->
        List.iter (expr_free_vars vars) e_list

    | Eprocess e ->
        expr_free_vars vars e

    | Epre (pre_kind, e) ->
        expr_free_vars vars e

    | Elast e ->
        expr_free_vars vars e

    | Edefault e ->
        expr_free_vars vars e

    | Enothing -> ()

    | Epause (_, _, ck_e) ->
      (match ck_e with
        | CkExpr e1 -> expr_free_vars vars e1
        | _ -> ())

    | Ehalt _ -> ()

    | Eemit (e, None) ->
        expr_free_vars vars e

    | Eemit (e1, Some e2) ->
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Eloop (n_opt, e) ->
        Misc.opt_iter (expr_free_vars vars) n_opt;
        expr_free_vars vars e

    | Efordopar (ident, e1, e2, direction_flag, e) ->
        let vars' = (Vlocal ident) :: vars in
        expr_free_vars vars' e1;
        expr_free_vars vars' e2;
        expr_free_vars vars' e

    | Epar e_list ->
        List.iter (expr_free_vars vars) e_list

    | Emerge (e1, e2) ->
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Esignal ((ident, tyexpr_opt), ck, r, comb, e) ->
        let vars' = (Vlocal ident) :: vars in
        (match ck with
          | CkExpr e1 -> expr_free_vars vars' e1
          | _ -> ());
        (match r with
          | CkExpr e1 -> expr_free_vars vars' e1
          | _ -> ());
        (match comb with
          | None -> ()
          | Some (e1, e2) -> expr_free_vars vars' e1; expr_free_vars vars' e2);
        expr_free_vars vars' e

    | Erun e ->
        expr_free_vars vars e

    | Euntil (config, e, None) ->
        config_free_vars vars config;
        expr_free_vars vars e
    | Euntil (config, e, Some(p,e1)) ->
        config_free_vars vars config;
        expr_free_vars vars e;
        let vars' = (vars_of_patt p) @ vars in
        expr_free_vars vars' e1

    | Ewhen (config, e) ->
        config_free_vars vars config;
        expr_free_vars vars e

    | Econtrol (config, None, e) ->
        config_free_vars vars config;
        expr_free_vars vars e
    | Econtrol (config, Some(p,e1), e) ->
        config_free_vars vars config;
        expr_free_vars vars e;
        let vars' = (vars_of_patt p) @ vars in
        expr_free_vars vars' e1

    | Eget (e,patt,e1) ->
        expr_free_vars vars e;
        let vars' = (vars_of_patt patt) @ vars in
        expr_free_vars vars' e1

    | Epresent (config, e1, e2) ->
        config_free_vars vars config;
        expr_free_vars vars e1;
        expr_free_vars vars e2

    | Eawait (immediate_flag, config) ->
        config_free_vars vars config

    | Eawait_val (immediate, kind, e, patt, e1) ->
        expr_free_vars vars e;
        let vars' = (vars_of_patt patt) @ vars in
        expr_free_vars vars' e1

    | Enewclock (id, sch, period, e1) ->
        let vars' = (Vlocal id) :: vars in
        Misc.opt_iter (expr_free_vars vars') sch;
        Misc.opt_iter (expr_free_vars vars') period;
        expr_free_vars vars' e1

    | Epauseclock e -> expr_free_vars vars e

    | Etopck -> ()

    | Ememory(s, ck, v, e) ->
        let vars' = (Vlocal s) :: vars in
        (match ck with
          | CkExpr e1 -> expr_free_vars vars e1
          | _ -> ());
        expr_free_vars vars v;
        expr_free_vars vars' e

    | Elast_mem e ->
        expr_free_vars vars e
    | Eupdate (s, e) | Eset_mem (s, e) ->
        expr_free_vars vars s;
        expr_free_vars vars e
    | Eawait_new (s, patt, e) ->
        expr_free_vars vars s;
        let vars' = (vars_of_patt patt) @ vars in
        expr_free_vars vars' e
    end

  and config_free_vars vars config =
    match config.conf_desc with
    | Cpresent e ->
        expr_free_vars vars e

    | Cand (c1, c2) ->
        config_free_vars vars c1;
        config_free_vars vars c2

    | Cor (c1, c2) ->
        config_free_vars vars c1;
        config_free_vars vars c2
  in
  expr_free_vars [] e;
  !fv
