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
