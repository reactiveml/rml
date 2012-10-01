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

open Usages

let type_of_usage = function
  | Affine -> Initialization.type_affine
  | Neutral -> Initialization.type_neutral
  | Zero_n -> Initialization.type_zero_n
  | Zero_1 -> Initialization.type_zero_1
  | Var_n -> Initialization.type_uvar_n
  | Var_1 -> Initialization.type_uvar_1
  | Var -> Def_types.new_var ()

let usage_of_type ty =
  let ty = Static.get_type ty in
  if Initialization.is_affine ty then
    Affine
  else if Initialization.is_neutral ty then
    Neutral
  else if Initialization.is_zero_1 ty then
    Zero_1
  else if Initialization.is_zero_n ty then
    Zero_n
  else if Initialization.is_uvar_n ty then
    Var_n
  else if Initialization.is_uvar_1 ty then
    Var_1
  else
    Var

let mk_t loc u_emit u_get =
  mk_su
    loc
    (usage_of_type u_emit)
    (usage_of_type u_get)

exception Unify of Usages.usage * Usages.usage

let unify ty1 ty2 =
  let unify_ty ty1 ty2 =
    ty1.Def_types.type_desc <- ty2.Def_types.type_desc
  in
  let u1 = usage_of_type ty1
  and u2 = usage_of_type ty2 in
  if u1 != u2 then
    match u1, u2 with
    | Var, _ -> unify_ty ty1 ty2
    | _, Var -> unify_ty ty2 ty1

    | Var_1, (Zero_1 | Affine) -> unify_ty ty1 ty2
    | (Zero_1 | Affine), Var_1-> unify_ty ty2 ty1

    | Var_n, (Zero_n | Neutral) -> unify_ty ty1 ty2
    | (Zero_n | Neutral), Var_n-> unify_ty ty2 ty1

    | Zero_1, Affine -> ()
    | Zero_n, Neutral -> ()
    | _                   -> raise (Unify (u1, u2))
