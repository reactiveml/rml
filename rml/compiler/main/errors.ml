(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : errors.ml                                                  *)
(*  Date de creation : 08/05/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : Taken from OCaml                                          *)
(*************************************************************************)

(* $Id$ *)

open Format

let report_error ppf exn =
  let report ppf = function
    | Lexer.Error(err, loc) ->
	Location.print ppf loc;
	Lexer.report_error ppf err
    | Syntaxerr.Error err ->
	Syntaxerr.report_error ppf err

    | Misc.Error -> ()
(*	fprintf ppf "@.Error." *)
    | Misc.Internal (loc,msg) ->
	if loc = Location.none 
      	then fprintf ppf "@.Internal error: %s. \nPlease report it." msg
	else 
	  fprintf ppf "@.%aInternal error: %s. \nPlease report it." 
	    Location.print loc msg
    | Warnings.Errors (n) ->
	fprintf ppf "@.Error: %d error-enabled warnings occurred." n
    | x -> fprintf ppf "@]"; raise x 
  in
  fprintf ppf "@[%a@]@." report exn

let unbound_main main =
  eprintf "The process %s is unbound" main;
  raise Misc.Error

