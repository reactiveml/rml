(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : asttypes.mli                                               *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : inspired by OCaml                                         *)
(*************************************************************************)

(* $Id: asttypes.mli,v 1.1.1.1 2005/01/23 17:55:36 mandel Exp $ *)


(* Types used in the a.s.t. *)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type immediate_flag = Immediate | Nonimmediate

type mutable_flag = Mutable | Immutable

type signal_kind = In | Out | Inout

type await_kind = All | One

type pre_kind = Status | Value

type immediate =
  | Const_unit
  | Const_bool of bool
  | Const_int of int
  | Const_float of float
  | Const_char of char
  | Const_string of string
