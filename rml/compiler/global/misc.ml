(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : misc.ml                                                    *)
(*  Date de creation : 02/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* version of the compiler *)
let version = "1.02+1"

exception Error

exception Internal of Location.t * string

exception Cannot_find_file of string

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Error

let not_yet_implemented msg =
  prerr_string ">> Not yet implemented: "; prerr_endline msg; raise Error


(* standard module *)
let pervasives_module = "Rml_pervasives"
let interpreter_module = "Rml_interpreter"
let interpreter_intf = ref "Lco_interpreter"
let interpreter_impl = ref "Lco_ctrl_tree"

let standard_lib = STDLIB 

(* interpreter *)
let set_interpreter_intf s = interpreter_intf := s
let set_interpreter_impl s = interpreter_impl := s

(* different translations *)
type translations = Lk | Lco

let translation = ref Lco

let set_translation t = translation := t

(* load paths *)
let load_path = ref ([] : string list)

(* no link *)
let no_link = ref false

(* simulation process *)
let simulation_process = ref ""

(* number_of_instant to execute *)
let number_of_instant = ref (-1)

(* samplin rate *)
let sampling = ref (-. 1.0)

(* verbose *)
let print_type = ref false
let save_types = ref false

(* dparse *)
let dparse = ref false

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

let opt_map f = function
    Some x -> Some (f x)
  | None -> None

let opt_iter f = function
    Some x -> f x
  | None -> ()

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
