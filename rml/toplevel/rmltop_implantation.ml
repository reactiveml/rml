(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 25/10/2005                                      *)
(* Fichier : rmltop_implantation.ml                                   *)
(*                                                                    *)
(**********************************************************************)

module Sig_env (* : S *) =
  struct 
    type ('a, 'b) t =
	{ mutable status: int;
	  mutable value: 'b;
	  mutable pre_status: int;
	  mutable last: 'b;
	  mutable default: 'b;
	  combine: ('a -> 'b -> 'b); }

    let instant = ref 0
    let absent = -2

    let create default combine =
      { status = absent; 
	value = default;
	pre_status = absent; 
	last = default;
	default = default;
	combine = combine; } 

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
	 n.value <- n.combine v n.default)
      else
	n.value <- n.combine v n.value

(***************************************)
(* next                                *)
(***************************************)
    let next () = incr instant

  end

module Machine_controler_machine = Lco_ctrl_tree.Rml_interpreter(Sig_env)
