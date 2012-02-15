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

type usage =
  | Affine
  | Neutral
  | Zero

type signal_usage = usage * usage

exception Forbidden_usage

let add_u u1 u2 = match u1, u2 with
  | Zero, u | u, Zero -> u
  | Neutral, Neutral -> Neutral
  | _ -> raise Forbidden_usage

let add_s (u1, u2) (v1, v2) =
  add_u u1 u2,
  add_u v1 v2

let addable u1 u2 =
  try
    ignore (add_u u1 u2);
    true
  with Forbidden_usage -> false

let usage_of_type ty =
  let ty = Static.get_type ty in
  if ty = Initialization.type_affine then
    Affine
  else if ty = Initialization.type_neutral then
    Neutral
  else
    Zero

let add_t ty1 ty2 =
  let u1 = usage_of_type ty1 in
  let u2 = usage_of_type ty2 in
  add_u u1 u2

let string_of_usage = function
  | Affine -> "1"
  | neutral -> "_"
  (* | Zero -> "0" *)

let string_of_signal_usage (u1,u2) =
  Printf.sprintf "(%s,%s)"
    (string_of_usage u1)
    (string_of_usage u2)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module Table = struct

  module M = Map.Make(Int)

  type key = M.key
  type 'a t = 'a M.t

  let empty = M.empty
  let mem = M.mem
  let find = M.find

  let add k ty_emit ty_get m =
    let u_emit = usage_of_type ty_emit in
    let u_get = usage_of_type ty_get in
    M.add k (u_emit, u_get) m

  let singleton k ty_emit ty_get =
    add k ty_emit ty_get empty

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

  let rec flatten = function
    | [] -> empty
    | a::l -> merge a (flatten l)

  let print t =
    if not (M.is_empty t) then begin
      M.iter (fun k v ->
        Printf.printf "%d:%s;" k (string_of_signal_usage v)
      )
        t;
      Printf.printf "\n%!"
    end

end
