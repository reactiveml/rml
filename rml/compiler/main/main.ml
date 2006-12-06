(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : main.ml                                                    *)
(*  Date de creation : 06/05/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from Lucid Synchron                                 *)
(*************************************************************************)

(* $Id: main.ml,v 1.2 2005/04/30 16:49:15 mandel Exp $ *)

open Misc
open Modules
open Compiler

(* list of object files passed on the command line *)
let object_files = ref [] 

let compile file =
  if Filename.check_suffix file ".rml"
  then
    let filename = Filename.chop_suffix file ".rml" in
    let modname = Filename.basename filename in
    compile_implementation (String.capitalize modname) filename;
    object_files := modname::!object_files
  else if Filename.check_suffix file ".rmli"
  then 
    let filename = Filename.chop_suffix file ".rmli" in
    compile_interface (String.capitalize (Filename.basename filename)) filename
  else if Filename.check_suffix file ".mli"
  then
    let filename = Filename.chop_suffix file ".mli" in
    compile_scalar_interface 
      (String.capitalize (Filename.basename filename)) filename
  else 
    raise (Arg.Bad ("don't know what to do with " ^ file))


let main () = 
  try
    List.iter compile !to_compile
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2
;;

Printexc.catch main (); 
(* this is strange, but is required to avoid a bug in ocaml 3.04 *)
Format.set_formatter_out_channel stdout; 
if !interactive then Interactive.compile ();
if !dtime then Diagnostic.print stderr;
exit 0;;

