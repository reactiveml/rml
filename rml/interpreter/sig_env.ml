(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2005-08-28  *)
(* file: sig_env.ml *)

module type S =
  sig
    type ('a, 'b) t

    val create: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) t
    val create_memory: 'b -> ('a -> 'b -> 'b) -> ('a, 'b) t
    val status: ('a, 'b) t -> bool
    val value: ('a, 'b) t -> 'b
    val pre_status: ('a, 'b) t -> bool
    val pre_value: ('a, 'b) t -> 'b
    val last: ('a, 'b) t -> 'b
    val default: ('a, 'b) t -> 'b
    val one: ('a, 'a list) t -> 'a

    val emit: ('a, 'b) t -> 'a -> unit

    val next: unit -> unit

  end

module Record  (*: S*)  =
  struct

    type kind = Default | Memory

    type ('a, 'b) t =
	{ mutable status: int;
	  mutable value: 'b;
	  mutable pre_status: int;
	  mutable last: 'b;
	  default: 'b;
	  combine: ('a -> 'b -> 'b);
          kind: kind; }

    let instant = ref 0
    let absent = -2

    let create default combine =
      { status = absent;
	value = default;
	pre_status = absent;
	last = default;
	default = default;
	combine = combine;
        kind = Default; }

    let create_memory default combine =
      { status = absent;
        value = default;
        pre_status = absent;
        last = default;
        default = default;
        combine = combine;
        kind = Memory; }

(* -------------------------- Access functions -------------------------- *)
    let default n = n.default
    let status n = n.status = !instant

    let value n = n.value

    let pre_status n =
      if n.status = !instant
      then n.pre_status = !instant - 1
      else n.status = !instant - 1

    let last n =
      if n.status = !instant
      then n.last
      else n.value

    let pre_value n =
      if n.status = !instant
      then
	if n.pre_status = !instant - 1
	then n.last
	else n.default
      else
	if n.status = !instant - 1
	then n.value
	else n.default

    let one n =
      match n.value with
      | x :: _ -> x
      | _ -> assert false

(***************************************)
(* emit                                *)
(***************************************)
    let emit n v =
      if n.status <> !instant
      then
	(n.pre_status <- n.status;
	 n.last <- n.value;
	 n.status <- !instant;
         begin match n.kind with
         | Default -> n.value <- n.combine v n.default
         | Memory -> n.value <- n.combine v n.value
         end)
      else
	n.value <- n.combine v n.value

(***************************************)
(* next                                *)
(***************************************)
    let next () = incr instant

  end


module Class : S =
  struct

    let to_update_list = ref []

    class virtual pur_event =
      object
	val mutable status = false
	val mutable pre_status = false
	val mutable to_update = false

	method status = status
	method pre_status = pre_status

	method virtual update : bool

      end

    class ['a, 'b] valued_event((default: 'b), (combine: ('a -> 'b -> 'b))) =
      object (self)
	inherit pur_event

	val mutable value = default
	val mutable last = default
	val default = default
	val combine = combine

	method value = value
	method last = last
	method default = default

	method emit v =
	  if not to_update then
	    begin
	      to_update <- true;
	      to_update_list := (self :> pur_event) :: !to_update_list
	    end;
	  status <- true;
	  value <- combine v value


        (* maj de l'etat de l'objet si necessaire et revoie "true"
	    s'il doit etre mis a jour à l'instant suivant *)
	method update =
	  if to_update then
	    begin
	      if status then
		begin
		  last <- value;
		  value <- default
		end;
	      pre_status <- status;
	      status <- false;
	      to_update <- pre_status;
	      pre_status
	    end
	  else
	    false
      end

    class ['a, 'b] memory_event((default: 'b), (combine: ('a -> 'b -> 'b))) =
      object (self)
        inherit ['a, 'b] valued_event (default, combine)

        method update =
          if to_update then
            begin
              if status then
                begin
                  last <- value
                end;
              pre_status <- status;
              status <- false;
              to_update <- pre_status;
              pre_status
            end
          else
            false
      end

    type ('a,'b) t =
	< status : bool;
          value : 'b;
          pre_status : bool;
          last : 'b;
          default : 'b;
          emit : 'a -> unit;
          update : bool; >

    let create default combine =
      ((new valued_event (default, combine)) :> ('a, 'b) t)

    let create_memory default combine =
      ((new memory_event (default, combine)) :> ('a, 'b) t)


(* -------------------------- Access functions -------------------------- *)
    let default n = n#default
    let status n = n#status

    let value n = n#value

    let pre_status n = n#pre_status

    let pre_value n = if n#pre_status then n#last else n#default

    let last n = n#last

    let one n =
      match n#value with
      | x :: _ -> x
      | _ -> assert false

(***************************************)
(* emit                                *)
(***************************************)
    let emit n v =
      n#emit v

(***************************************)
(* next                                *)
(***************************************)
    let next () =
      let next_to_update_list = ref [] in
      List.iter
	(fun n ->
	  if n#update then
	    next_to_update_list := n :: !next_to_update_list)
	!to_update_list;
      to_update_list := !next_to_update_list

  end


module Hashtbl (*: S*) =
  struct
    type ('a, 'b) t = string

    type ('a, 'b) event_struct =
	{ mutable status: int;
	  mutable value: 'b;
	  mutable pre_status: int;
	  mutable last: 'b;
	  mutable default: 'b;
	  combine: ('a -> 'b -> 'b); }
    type alpha and beta

    let env = Hashtbl.create 42

    let instant = ref 0
    let absent = -2
    let gensym =
      let cpt = ref 0 in
      fun () ->
	incr cpt;
	"signal_"^(string_of_int !cpt)

    let create default combine =
      let evt_id = gensym () in
      let evt_struct =
	{ status = absent;
	  value = default;
	  pre_status = absent;
	  last = default;
	  default = default;
	  combine = combine; }
      in
      Hashtbl.add env evt_id
	(Obj.magic evt_struct: (alpha, beta) event_struct);
      evt_id

(* -------------------------- Access functions -------------------------- *)
    let default n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      evt_struct.default

    let status n =
      let evt_struct = Hashtbl.find env n in
      evt_struct.status = !instant

    let value n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      evt_struct.value

    let pre_status n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      if evt_struct.status = !instant
      then evt_struct.pre_status = !instant - 1
      else evt_struct.status = !instant - 1

    let last n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      if evt_struct.status = !instant
      then evt_struct.last
      else evt_struct.value

    let pre_value n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      if evt_struct.status = !instant
      then
	if evt_struct.pre_status = !instant - 1
	then evt_struct.last
	else evt_struct.default
      else
	if evt_struct.status = !instant - 1
	then evt_struct.value
	else evt_struct.default

    let one n =
      let evt_struct = Obj.magic Hashtbl.find env n in
      match evt_struct.value with
      | x :: _ -> x
      | _ -> assert false

(***************************************)
(* emit                                *)
(***************************************)
    let emit n v =
      let n = Obj.magic (Hashtbl.find env n) in
      if n.status <> !instant
      then
	(n.pre_status <- n.status;
	 n.last <- n.value;
	 n.status <- !instant;
	 n.value <- n.combine v n.default)
      else
	n.value <- n.combine v n.value

(***************************************)
(* next                                *)
(***************************************)
    let next () = incr instant

  end


