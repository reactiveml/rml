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
  | Zero

type signal_usage = (usage * usage) loc

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
  mk_loc
    loc
    ((if affine then Affine else Neutral),
     Zero
    )

let await_u loc affine =
  mk_loc
    loc
    (Zero,
     if affine then Affine else Neutral
    )

exception Forbidden_usage of Location.t * Location.t

let add_u u1 u2 = match u1.node, u2.node with
  | Zero, u | u, Zero -> u
  | Neutral, Neutral -> Neutral
  | _ -> raise (Forbidden_usage (u1.loc, u2.loc))

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
  mk_su
    loc
    (add_u u1 v1)
    (add_u u2 v2)

let mk_zero =
  mk_loc Location.none (Zero, Zero)

let compatible_usage su1 su2 =
  let compatible_usage u1 u2 = match u1, u2 with
    | Zero, _
    | _, Zero
    | Neutral, Neutral
    | Affine, Affine -> true
    | _ -> false in
  let su1_emit, su1_get = su1.node
  and su2_emit, su2_get = su2.node in
  compatible_usage su1_emit su2_emit && compatible_usage su1_get su2_get
