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

(* file: symbol_table.ml *)
(* created: 2004-04-27  *)
(* author: Louis Mandel *)

(* $Id$ *)

(* Implementation of the symbol table *)

(* The signature of identifiers *)

module type Ident_type =
  sig
    type t
    val compare: t -> t -> int
    val name: t -> string
  end

module type S = functor (Ident: Ident_type) ->
  sig
    type 'a t
    val empty: 'a t
    val add: Ident.t -> 'a -> 'a t -> 'a t
    val find: Ident.t -> 'a t -> 'a
    val mem: Ident.t -> 'a t -> bool
    val append: 'a t -> 'a t -> 'a t
    val iter: (Ident.t -> 'a -> unit) -> 'a t -> unit
    val fold: (Ident.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map: ('a -> 'b) -> 'a t -> 'b t
  end

module Make = functor (Ident: Ident_type) ->
  struct
    module OrderIdent =
      struct
	type t = Ident.t
	let compare = Ident.compare
      end

    module BinSearch = Map.Make(Ident)

    type 'a t = 'a BinSearch.t

    let empty = BinSearch.empty

    let add = BinSearch.add

    let find = BinSearch.find

    let mem = BinSearch.mem

    let append tbl1 tbl2 =
      BinSearch.fold (fun k x tbl -> add k x tbl) tbl1 tbl2

    let iter = BinSearch.iter

    let fold = BinSearch.fold

    let map = BinSearch.map

  end
