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

open Compiler_options
open Asttypes

exception Error

exception Internal of Location.t * string

exception Cannot_find_file of string

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Error

let not_yet_implemented msg =
  prerr_string ">> Not yet implemented: "; prerr_endline msg; raise Error


let print_DEBUG msg =
  prerr_string ">> DEBUG: "; prerr_endline msg


(* File operations *)

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

let find_in_path filename =
  if Sys.file_exists filename then
    filename
  else if not(Filename.is_implicit filename) then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = Filename.concat a filename in
          if Sys.file_exists b then b else find rest
    in find !load_path

let clock_map f x = match x with
  | CkTop -> CkTop
  | CkLocal -> CkLocal
  | CkExpr e -> CkExpr (f e)

let opt_map f = function
    Some x -> Some (f x)
  | None -> None

let opt_iter f = function
    Some x -> f x
  | None -> ()

let mapfold f acc l =
  let l,acc = List.fold_left
                (fun (l,acc) e -> let e,acc = f acc e in e::l, acc)
                ([],acc) l in
  List.rev l, acc

let optional_wacc f acc = function
  | None -> None, acc
  | Some x -> let x, acc = f acc x in Some x, acc

let assert_empty l = match l with
  | [] -> ()
  | _ -> fatal_error "assert_empty"

(* association table with memoization *)
class name_assoc_table f =
  object
    val mutable counter = 0
    val mutable assoc_table: (int * string) list = []
    method name var =
      try
        List.assq var assoc_table
      with
        not_found ->
          let n = f counter in
          counter <- counter + 1;
          assoc_table <- (var,n) :: assoc_table;
          n
    method reset =
      counter <- 0;
      assoc_table <- []
  end

(* converting integers into variable names *)
(* variables are printed 'a, 'b *)
let int_to_letter bound i =
  if i < 26
  then String.make 1 (Char.chr (i+bound))
  else String.make 1 (Char.chr ((i mod 26) + bound)) ^ string_of_int (i/26)

let int_to_alpha i = int_to_letter 97 i

(* for infix operators, print parenthesis around *)
let is_an_infix_or_prefix_operator op =
  if op = "" then false
  else
    let c = String.get op 0 in
    not (((c >= 'a') & (c <= 'z')) or ((c >= 'A') & (c <= 'Z')))


(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s


module Diagnostic =
  struct
    open Format

    let diag_list = ref []
    let global_time = ref 0.0

    let round f = float(truncate(100.0 *. f)) /. 100.0

    let print oc =
      (* prints the diagnostic given by one entry *)
      let print_entry (name, time) =
        let average = 100.0 *. !time /. !global_time in
        print_tab ();
        print_string name;
        print_string ":";
        print_tab ();
        print_float (round !time);
        print_string "s";
        print_tab ();
        print_float (round average);
        print_string "%" in

      (* prints a diagnostic of the execution *)
      set_max_boxes max_int;
      set_formatter_out_channel oc;

      open_tbox ();
      set_tab ();
      print_tbreak 30 0;
      set_tab ();
      print_tbreak 30 0;
      set_tab ();
      print_string "\n";
      print_string "====================================\
                    ==============================\n";
      print_string "        Summary of execution time\n";
      List.iter print_entry (List.rev !diag_list);
      print_string "\n";
      print_entry ("Total", global_time);
      print_string "\n";
      print_string "====================================\
                    ==============================\n";
      close_tbox ();
      print_flush ()
  end

(* every step of the compiler takes its own timer *)
module Timer =
  functor (Name: sig val name: string end) ->
  struct
    open Diagnostic

    let exec_time = ref 0.0
    let accumulated_time = ref 0.0
    let start_time = ref 0.0;;

    (* add the entry to the list *)
    diag_list := (Name.name, accumulated_time) :: !diag_list

    (* start counting *)
    let start () = start_time := Sys.time ()

    (* counting *)
    let time () =
      let t = Sys.time () in
      exec_time := t -. !start_time;
      accumulated_time := !accumulated_time +. !exec_time;
      global_time := !global_time +. !exec_time
  end

(* Operations on kind_sum and kind_prod *)

let mk_kind_prod ~clock:ck ~carrier:c ~carrier_row:cr ~effect:e ~effect_row:er ~react:r =
  { k_clock = ck; k_carrier = c; k_carrier_row = cr;
    k_effect = e; k_effect_row = er; k_react = r }

let kind_prod_empty =
  mk_kind_prod ~clock:[] ~carrier:[] ~carrier_row:[] ~effect:[] ~effect_row:[] ~react:[]

let kind_map f v = match v with
  | Kclock c -> Kclock (f.k_clock c)
  | Kcarrier c -> Kcarrier (f.k_carrier c)
  | Kcarrier_row c -> Kcarrier_row (f.k_carrier_row c)
  | Keffect eff -> Keffect (f.k_effect eff)
  | Keffect_row eff -> Keffect_row (f.k_effect_row eff)
  | Kreact r -> Kreact (f.k_react r)

let kind_map_env f env v = match v with
  | Kclock c -> Kclock (f.k_clock env c)
  | Kcarrier c -> Kcarrier (f.k_carrier env c)
  | Kcarrier_row c -> Kcarrier_row (f.k_carrier_row env c)
  | Keffect eff -> Keffect (f.k_effect env eff)
  | Keffect_row eff -> Keffect_row (f.k_effect_row env eff)
  | Kreact r -> Kreact (f.k_react env r)

let kind_iter f v = match v with
  | Kclock c -> f.k_clock c
  | Kcarrier c -> f.k_carrier c
  | Kcarrier_row c -> f.k_carrier_row c
  | Keffect eff -> f.k_effect eff
  | Keffect_row eff -> f.k_effect_row eff
  | Kreact r -> f.k_react r

let kind_iter2 f v1 v2 = match v1, v2 with
  | Kclock c1, Kclock c2 -> f.k_clock c1 c2
  | Kcarrier c1, Kcarrier c2 -> f.k_carrier c1 c2
  | Kcarrier_row c1, Kcarrier_row c2 -> f.k_carrier_row c1 c2
  | Keffect eff1, Keffect eff2 -> f.k_effect eff1 eff2
  | Keffect_row eff1, Keffect_row eff2 -> f.k_effect_row eff1 eff2
  | Kreact r1, Kreact r2 -> f.k_react r1 r2
  | _, _ -> invalid_arg "kind_iter2"

let kind_fold f acc v = match v with
  | Kclock c -> f.k_clock acc c
  | Kcarrier c -> f.k_carrier acc c
  | Kcarrier_row c -> f.k_carrier_row acc c
  | Keffect eff -> f.k_effect acc eff
  | Keffect_row eff -> f.k_effect_row acc eff
  | Kreact r -> f.k_react acc r

let kind_fold2 f acc1 acc2 v = match v with
  | Kclock c -> f.k_clock acc1 acc2 c
  | Kcarrier c -> f.k_carrier acc1 acc2 c
  | Kcarrier_row c -> f.k_carrier_row acc1 acc2 c
  | Keffect eff -> f.k_effect acc1 acc2 eff
  | Keffect_row eff -> f.k_effect_row acc1 acc2 eff
  | Kreact r -> f.k_react acc1 acc2 r

let expect_clock v = match v with
  | Kclock c -> c
  | _ -> invalid_arg "expect_clock"

let expect_carrier v = match v with
  | Kcarrier c -> c
  | _ -> invalid_arg "expect_carrier"

let expect_carrier_row v = match v with
  | Kcarrier_row c -> c
  | _ -> invalid_arg "expect_carrier_row"

let expect_effect v = match v with
  | Keffect c -> c
  | _ -> invalid_arg "expect_effect"

let expect_effect_row v = match v with
  | Keffect_row c -> c
  | _ -> invalid_arg "expect_effect_row"

let expect_react v = match v with
  | Kreact c -> c
  | _ -> invalid_arg "expect_react"

let kind_sum_split l =
  let aux acc x = match x with
    | Kclock ck -> { acc with k_clock = ck::acc.k_clock }
    | Kcarrier c -> { acc with k_carrier = c::acc.k_carrier }
    | Kcarrier_row c -> { acc with k_carrier_row = c::acc.k_carrier_row }
    | Keffect eff -> { acc with k_effect = eff::acc.k_effect }
    | Keffect_row eff -> { acc with k_effect_row = eff::acc.k_effect_row }
    | Kreact r -> { acc with k_react = r::acc.k_react }
  in
  List.fold_left aux kind_prod_empty l

let filter_types l =
  let l = List.filter (function Kclock _ -> true | _ -> false) l in
  List.map (function Kclock s -> s | _ -> assert false) l

let zero_arity =
  mk_kind_prod ~clock:0 ~carrier:0 ~carrier_row:0
    ~effect:0 ~effect_row:0 ~react:0

let list_arity l =
  let add_arity ar k = match k with
    | Kclock _ -> { ar with k_clock = ar.k_clock + 1 }
    | Kcarrier _ -> { ar with k_carrier = ar.k_carrier + 1 }
    | Kcarrier_row _ -> { ar with k_carrier_row = ar.k_carrier_row + 1 }
    | Keffect _ -> { ar with k_effect = ar.k_effect + 1 }
    | Keffect_row _ -> { ar with k_effect_row = ar.k_effect_row + 1 }
    | Kreact _ -> { ar with k_react = ar.k_react + 1 }
  in
  List.fold_left add_arity zero_arity l
