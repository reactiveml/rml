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

(* file: annot.ml *)

(* Warning: *)
(* This file is based on the original version of stypes.ml *)
(* from the Objective Caml 3.07 distribution, INRIA        *)

(* first modification: 2004-08-17 *)
(* modified by: Louis Mandel      *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(* Recording and dumping (partial) type information *)

(*
  We record all types in a list as they are created.
  This means we can dump type information even if type inference fails,
  which is extremely important, since type information is most
  interesting in case of errors.
*)

open Lexing;;
open Location;;
open Reac_ast;;

type type_info =
    Ti_patt of pattern
  | Ti_expr of expression

let get_location ti =
  match ti with
    Ti_patt p -> p.patt_loc
  | Ti_expr e -> e.expr_loc

module Annot(T: sig
  type t
  val get_type: type_info -> t
  val output: out_channel -> t -> unit
end) =
  struct

    let type_info = ref ([] : type_info list)
    let phrases = ref ([] : Location.t list)

    let record ti =
      if !Misc.save_types && not (get_location ti).Location.loc_ghost then
	type_info := ti :: !type_info


    let record_phrase loc =
      if !Misc.save_types then phrases := loc :: !phrases


    (* comparison order:
       the intervals are sorted by order of increasing upper bound
       same upper bound -> sorted by decreasing lower bound
     *)
    let cmp_loc_inner_first loc1 loc2 =
      match compare loc1.loc_end.pos_cnum loc2.loc_end.pos_cnum with
      | 0 -> compare loc2.loc_start.pos_cnum loc1.loc_start.pos_cnum
      | x -> x


    let cmp_ti_inner_first ti1 ti2 =
      cmp_loc_inner_first (get_location ti1) (get_location ti2)


    let print_position pp pos =
      Format.fprintf pp "%S %d %d %d"
	pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum


    let sort_filter_phrases () =
      let ph = List.sort (fun x y -> cmp_loc_inner_first y x) !phrases in
      let rec loop accu cur l =
	match l with
	| [] -> accu
	| loc :: t ->
	    if cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum
		&& cur.loc_end.pos_cnum >= loc.loc_end.pos_cnum
	    then loop accu cur t
	    else loop (loc :: accu) loc t
      in
      phrases := loop [] Location.none ph

(*
let rec printtyp_reset_maybe loc =
  match !phrases with
  | cur :: t when cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum ->
     Printtyp.reset ();
     phrases := t;
     printtyp_reset_maybe loc;
  | _ -> ()
;;
*)

(* The format of the annotation file is documented in emacs/caml-types.el. *)

    let print_info oc pp ti =
      match ti with
      | Ti_patt {patt_loc = loc;}
      | Ti_expr {expr_loc = loc;} ->
	  let typ = T.get_type ti in
	  print_position pp loc.loc_start;
	  Format.fprintf pp " ";
	  print_position pp loc.loc_end;
	  Format.fprintf pp "@.type(@.  ";
(*
      printtyp_reset_maybe loc;
      Printtyp.mark_loops typ;
      Printtyp.type_expr pp typ;
*)
	  T.output oc typ;
	  Format.fprintf pp "@.)@."


    let get_info () =
      let info = List.fast_sort cmp_ti_inner_first !type_info in
      type_info := [];
      info


    let dump filename =
      if !Misc.save_types then begin
	let info = get_info () in
	let oc = open_out filename in
	let pp = Format.formatter_of_out_channel oc in
	sort_filter_phrases ();
	List.iter (print_info oc pp) info;
	phrases := [];
      end else begin
	type_info := [];
      end

  end

module Stypes =
  Annot(struct
    type t = Def_types.type_expression

    let get_type ti =
      begin match ti with
      | Ti_patt {patt_type = typ;}
      | Ti_expr {expr_type = typ;} -> typ
      end

(*     let output = Types_printer.output *)
    let output oc ty =
      let pp = Format.formatter_of_out_channel oc in
      Format.fprintf pp "  %a@?"
        Types_printer.print ty

  end)

module Sstatic =
  Annot(struct
    type t =
        Def_static.static * (varpatt * int) list * Def_types.reactivity_effect

    let get_type ti =
      begin match ti with
      | Ti_patt _ -> (Def_static.Static, [], Reactivity_effects.no_react)
      | Ti_expr {expr_static = typ;
                 expr_reactivity = pi;
                 expr_reactivity_effect = k; } -> (typ, pi, k)
      end

    let output oc (k, pi, r) =
      Static_printer.output oc k;
      Printf.fprintf oc " / %s" (Instantaneous_loop.Env.string_of_t pi);
      Printf.fprintf oc " / %s"
          (Types_printer.print_to_string Types_printer.print_reactivity r);
      flush oc

  end)
