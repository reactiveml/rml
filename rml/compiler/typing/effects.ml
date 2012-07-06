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

module M = Map.Make(Ident)

type key = M.key
type effects = signal_usage M.t

let empty = M.empty
let is_empty = M.is_empty
let mem = M.mem
let find = M.find

let add k loc u_emit u_get m =
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
