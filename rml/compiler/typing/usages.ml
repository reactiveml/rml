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

type 'a loc = {
  mutable loc : Location.t;
  node : 'a
}

type usage =
  | Affine
  | Neutral
  | Zero_n
  | Zero_1
  | Var

let string_of_usage = function
  | Affine -> "1"
  | Neutral -> "∞"
  | Zero_n -> "0_N"
  | Zero_1 -> "0_1"
  | Var -> "_"

let desc_of_usage = function
  | Affine -> "affine"
  | Neutral -> "neutral"
  | Zero_n -> "zero"
  | Zero_1 -> "zero1"
  | Var -> "var"

type signal_usage = (usage * usage) loc

let string_of_signal_usage su =
  let u1, u2 = su.node in
  Printf.sprintf "(%s,%s)"
    (string_of_usage u1)
    (string_of_usage u2)

let mk_loc loc node = {
  loc = loc;
  node = node;
}

let mk_su loc su1 su2 =
  mk_loc
    loc
    (su1, su2)

let km_su su =
  let loc = su.loc in
  let emit, get = su.node in
  loc, emit, get

let km_su_loc su =
  let loc, emit, get = km_su su in
  mk_loc loc emit, mk_loc loc get

let km_s { node = (a,b) ; _ } = a,b

let send_u loc affine =
  let u_emit = if affine then Affine else Neutral in
  mk_loc loc (u_emit, Var)

let await_u loc affine =
  let u_get = if affine then Affine else Neutral in
  mk_loc loc (Var, u_get)

exception Forbidden_usage of Location.t * Location.t
exception Forbidden_signal_usage of signal_usage * signal_usage

let add_u u1 u2 = match u1.node, u2.node with
  | Var, u | u, Var -> u
  | Zero_1, Zero_1 -> Zero_1
  | Zero_1, Affine | Affine, Zero_1 -> Affine
  | Zero_n, Zero_n -> Zero_n
  | Zero_n, Neutral | Neutral, Zero_n -> Neutral
  | Neutral, Neutral -> Neutral
  | _ -> raise (Forbidden_usage (u1.loc, u2.loc))

let max_u u1 u2 = match u1, u2 with
  | Affine, _ | _, Affine -> Affine
  | Neutral, _ | _, Neutral -> Neutral
  | Zero_1, _ | _, Zero_1 -> Zero_1
  | Zero_n, _ | _, Zero_n -> Zero_n
  | _ -> u1

let compare u1 u2 =
  if u1 = u2
  then 0
  else let u_max = max_u u1 u2 in
       if u_max = u1
       then 1
       else -1

let best_loc l1 l2 =
  if l1 = Location.none
  then l2
  else l1

let update_loc loc su =
  su.loc <- loc; su

let add_s u v =
  let u1, u2 = km_su_loc u in
  let v1, v2 = km_su_loc v in
  let loc = best_loc u.loc v.loc in
  try
    let r1 = add_u u1 v1 in
    let r2 = add_u u2 v2 in
    mk_su loc r1 r2
  with Forbidden_usage _ ->
    raise (Forbidden_signal_usage (u, v))

let max_s u v =
  let u1, u2 = km_su_loc u in
  let v1, v2 = km_su_loc v in
  let loc = best_loc u.loc v.loc in
  mk_su
    loc
    (max_u u1.node v1.node)
    (max_u u2.node v2.node)

let mk_null =
  mk_loc Location.none (Var, Var)

let compatible_usage su1 su2 =
  let compatible_usage u1 u2 = match u1, u2 with
    | _ when u1 = u2 -> true
    | Var, _
    | _, Var
    | Zero_n, Neutral
    | Neutral, Zero_n
    | Zero_1, Affine
    | Affine, Zero_1
    | Neutral, Neutral
    | Affine, Affine -> true
    | _ -> false in
  let su1_emit, su1_get = su1.node
  and su2_emit, su2_get = su2.node in
  compatible_usage su1_emit su2_emit && compatible_usage su1_get su2_get
