open Format
open Asttypes
open Global_ident
open Global
open Compiler_options

(** Generic printing of a list.
    This function seems to appear in several places... *)
let print_list print print_sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [x] ->
        print x
    | x::l ->
        open_box 0;
        print x;
        print_sep ();
        print_space ();
        printrec l;
        close_box () in
  printrec l

(** Prints an immediate. A patch is needed on float number for
    [ocaml] < 3.05. *)
let print_immediate i =
  match i with
  | Const_unit -> print_string "()"
  | Const_bool(b) -> print_string (if b then "true" else "false")
  | Const_int(i) ->
      if i < 0 then
        (print_string "(";
         print_int i;
         print_string ")")
      else print_int i
  | Const_float(f) ->
      if f < 0.0 then
        (print_string "(";
         print_float f;
         print_string ")")
      else print_float f
        (* patch because "x.0" is printed "x" by C for caml < 3.05 *)
        (* if (fst (modf f)) = 0.0 then print_string ".0" *)
  | Const_char(c) -> print_char '\''; print_char c; print_char '\''
  | Const_string(s) -> print_string ("\""^(String.escaped s)^"\"")

(** Prints a name. Infix chars are surrounded by parenthesis *)
let print_name s =
  let c = String.get s 0 in
  let s =
    if s = "or" or s = "mod" or s = "lxor" or s = "lnot" or s = "lsl"
        or s = "lsr" or s = "asr"
    then "(" ^ s ^ ")"
    else
      if c >= 'a' & c <= 'z' or c >= 'A' & c <= 'Z' or c = '_'
      then s
      else if c = '*' then "( " ^ s ^ " )"
        else
        "(" ^ s ^ ")"
  in
  print_string s


(** Prints pervasives values *)
let print_pervasives n =
  match n with
  | "int" | "char" | "string" | "float" | "bool" | "unit" | "exn" |
    "array" | "list" | "option" | "int32" | "int64" | "nativeint" |
    "format4" | "lazy_t" |
    "[]" | "::" |
    "None" | "Some" |
    "Match_failure" | "Assert_failure" | "Invalid_argument" | "Failure" |
    "Not_found" | "Out_of_memory" | "Stack_overflow" | "Sys_error" |
    "End_of_file" | "Division_by_zero" | "Sys_blocked_io" |
    "Undefined_recursive_module" ->
      print_name n
  | _ ->
      print_string "Pervasives";
      print_string ".";
      print_name n


let current_module = ref ""

(** Prints a global name *)
let print_global ({ gi = {qual=q; id=n} } as gl) =
  if gl.gi = Initialization.event_ident then (
    (match !Compiler_options.translation with
    | Compiler_options.Lco_fsharp -> print_string "Runtime.REvent"
    | _ -> print_string "Interpreter.event")
  ) else if gl.gi = Initialization.clock_ident then
    print_string "Interpreter.clock_expr"
  else if q = pervasives_module then
    (* special case for values imported from the standard library *)
    print_pervasives (Ident.name n)
  else if q = rml_pervasives_module then
    (match Ident.name n with
      | "clock_of" -> print_string "Interpreter.rml_clock"
      | n -> print_name n)
  else if q = !current_module then
    print_name (Ident.name n)
  else
    begin
      print_string q;
      print_string ".";
      print_name (Ident.name n)
    end

(** Prints a type variables *)
let print_type_var s = print_string ("'"^s)
let print_any_var k = match k with
  | Kclock s -> print_type_var s
  | _ -> assert false
