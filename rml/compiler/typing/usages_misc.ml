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

let string_of_usage = function
  | Affine -> "1"
  | Neutral -> "∞"
  | Zero -> "0"
  | Var -> "_"

let type_of_usage = function
  | Affine -> Initialization.type_affine
  | Neutral -> Initialization.type_neutral
  | Zero -> Initialization.type_zero
  | Var -> Def_types.new_var ()

let string_of_signal_usage su =
  let _, u1, u2 = Usages.km_su su in
  Printf.sprintf "(%s,%s)"
    (string_of_usage u1)
    (string_of_usage u2)

let usage_of_type ty =
  let ty = Static.get_type ty in
  if Initialization.is_affine ty then
    Affine
  else if Initialization.is_neutral ty then
    Neutral
  else if Initialization.is_zero ty then
    Zero
  else
    Var

let mk_t loc u_emit u_get =
  mk_su
    loc
    (usage_of_type u_emit)
    (usage_of_type u_get)

exception Unify

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
    | Zero, u1 | u1, Zero -> ()
    | _                   -> raise Unify

module Table = struct

  module M = Map.Make(Ident)

  type key = M.key
  type 'a t = 'a M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem = M.mem
  let find = M.find

  let add k loc ty_emit ty_get m =
    let u_emit = usage_of_type ty_emit in
    let u_get = usage_of_type ty_get in
    let su = Usages.mk_su loc u_emit u_get in
    M.add k su m

  let singleton k loc ty_emit ty_get =
    add k loc ty_emit ty_get empty

  let merge t1 t2 =
    M.fold (fun k u1 t ->
      try
        let u2 = M.find k t in
        M.add k (add_s u1 u2) t
      with Not_found ->
        M.add k u1 t
    )
    t1
    t2

  let filter m l =
    M.fold
      (fun k u t ->
        if List.mem k l then
          t
        else
          M.add k u t
      )
      m
      empty

  let rec flatten = function
    | [] -> empty
    | a::l -> merge a (flatten l)

  let apply u m =
    M.map (Usages.add_s u) m

  let update_loc m loc =
    M.map (Usages.update_loc loc) m

  let print t =
    if not (M.is_empty t) then begin
      Printf.printf "  ";
      M.iter (fun k v ->
        Printf.printf "%s:%s;"
          (Ident.unique_name k)
          (string_of_signal_usage v)
      )
      t;
      Printf.printf "\n%!"
    end

  let print_l l =
    List.iter print l

end
