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

(* file: print_caml_src.ml *)

(* Warning: *)
(* This file is based on the original version of print_caml_src.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6           *)

(* first modification: 2004-05-04  *)
(* modified by: Louis Mandel *)

(* $Id$ *)

(** Printing [Caml] code *)

open Misc
open Format
open Caml_ast
open Asttypes
open Global
open Global_ident


let current_module = ref ""
let formatter = ref std_formatter

(** Generic printing of a list.
    This function seems to appear in several places... *)
let print_list  print print_sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [x] ->
	print x
    | x::l ->
	pp_open_box !formatter 0;
	print x;
	print_sep ();
	pp_print_space !formatter ();
	printrec l;
	pp_close_box !formatter () in
  printrec l

(** Prints an immediate. A patch is needed on float number for
    [ocaml] < 3.05. *)
let print_immediate i =
  match i with
  | Const_unit -> pp_print_string !formatter "()"
  | Const_bool(b) -> pp_print_string !formatter  (if b then "true" else "false")
  | Const_int(i) ->
      if i <= 0 then
	(pp_print_string !formatter "(";
	 pp_print_int !formatter i;
	 pp_print_string !formatter ")")
      else pp_print_int !formatter i
  | Const_float(f) ->
      if f <= 0.0 then
	(pp_print_string !formatter "(";
	 pp_print_float !formatter f;
	 pp_print_string !formatter ")")
      else pp_print_float !formatter f
	(* patch because "x.0" is printed "x" by C for caml < 3.05 *)
	(* if (fst (modf f)) = 0.0 then print_string ".0" *)
  | Const_char(c) -> pp_print_char !formatter '\''; pp_print_char !formatter c; pp_print_char !formatter '\''
  | Const_string(s) -> pp_print_string !formatter ("\""^(String.escaped s)^"\"")

(** Prints a name. Infix chars are surrounded by parenthesis *)
let print_name s =
  let c = String.get s 0 in
  let s =
    if s = "or" || s = "mod" || s = "lxor" || s = "lsl"
	|| s = "lsr" || s = "asr" || s = "land" || s = "lor"
    then "(" ^ s ^ ")"
    else
      if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c = '_'
      then s
      else if c = '*' then "( " ^ s ^ " )"
	else
	"(" ^ s ^ ")"
  in
  pp_print_string !formatter s

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
  | "or" ->
      pp_print_string !formatter  "Pervasives";
      pp_print_string !formatter  ".";
      pp_print_string !formatter  "(||)"
  | _ ->
      pp_print_string !formatter  "Pervasives";
      pp_print_string !formatter  ".";
      print_name n


(** Prints a global name *)
let print_global ({ gi = {qual=q; id=n} } as gl) =
  if gl.gi = Initialization.event_ident then
    (* special case for event type *)
    begin
      pp_print_string !formatter !interpreter_module;
      pp_print_string !formatter ".";
      print_name (Ident.name n)
    end
  else if q = pervasives_module then
    (* special case for values imported from the standard library *)
    print_pervasives (Ident.name n)
  else if q = !current_module || q = "" then
    print_name (Ident.name n)
  else
    begin
      pp_print_string !formatter q;
      pp_print_string !formatter ".";
      print_name (Ident.name n)
    end

(** Prints a type variables *)
let print_type_var s = pp_print_string !formatter  ("'"^s)


let priority exp =
  match exp with
    (Cexpr_let _ | Cexpr_function _ | Cexpr_fun _ | Cexpr_match _
  | Cexpr_trywith _ ) -> 0
  | Cexpr_seq _  -> 1
  | Cexpr_ifthenelse _ -> 2
  | Cexpr_record_update _ -> 3
  | (Cexpr_record _  | Cexpr_construct (_,None) | Cexpr_local _
  | Cexpr_global _ | Cexpr_constant _ | Cexpr_array _  | Cexpr_constraint _ )
    -> 5
  | (Cexpr_record_access _ | Cexpr_tuple _) -> 6
  | _ -> 4

let priority_pattern p =
  match p with
    (Cpatt_construct _ | Cpatt_constant _ | Cpatt_var _
  | Cpatt_tuple _ | Cpatt_record _) -> 2
  | _ -> 1

let priority_te t =
  match t with
  | Ctype_arrow _ -> 0
  | Ctype_product _ -> 1
  | _ -> 2

(** Emission of code *)
let rec print pri e =
  pp_open_box !formatter 2;
  let pri_e = priority e.cexpr_desc in
  if pri > pri_e then pp_print_string !formatter  "(";
  begin match e.cexpr_desc with
    Cexpr_constant(im) -> print_immediate im
  | Cexpr_global(gl) -> print_global gl
  | Cexpr_local(s) -> print_name (Ident.unique_name s)
  | Cexpr_construct(gl,None) -> print_global gl
  | Cexpr_construct(gl,Some expr) when (Ident.name gl.gi.id = "::") ->
      begin
	match expr.cexpr_desc with
	| Cexpr_tuple [e1;e2] ->
	    print (1 + pri_e) e1;
	    pp_print_space !formatter ();
	    pp_print_string !formatter "::";
	    pp_print_space !formatter ();
	    print (1 + pri_e) e2
	| _ -> raise (Internal (e.cexpr_loc, "Print_caml_src: ::"))
      end
  | Cexpr_construct(gl,Some e1) ->
      print_global gl;
      pp_print_space !formatter ();
      print (1 + pri_e) e1
  | Cexpr_apply(f,l) ->
      print pri_e f;
      pp_print_space !formatter ();
      print_list (print (pri_e + 1)) (fun () -> ()) l
  | Cexpr_function patt_expr_list ->
      pp_print_string !formatter "function";
      pp_print_space !formatter ();
      List.iter
	(fun patt_expr ->
	  pp_print_string !formatter "| ";
	  print_patt_when_opt_expr
            (fun () -> pp_print_string !formatter " ->"; pp_print_space !formatter ()) patt_expr)
	patt_expr_list
  | Cexpr_fun(param_list,e1) ->
      pp_print_string !formatter "fun";
      pp_print_space !formatter ();
      print_list (print_pattern 0) (fun () -> ()) param_list;
      pp_print_space !formatter ();
      pp_print_string !formatter "->";
      pp_print_space !formatter ();
      print 0 e1
  | Cexpr_let(flag, [patt, { cexpr_desc = Cexpr_fun (param_list, e1) }], e) ->
      pp_print_string !formatter (if flag = Recursive then "let rec " else "let ");
      print_pattern 0 patt;
      pp_print_space !formatter ();
      print_list (print_pattern 0) (fun () -> ()) param_list;
      pp_print_space !formatter ();
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      print 0 e1;
      pp_print_space !formatter ();
      pp_print_string !formatter "in";
      pp_print_space !formatter ();
      print 0 e
  | Cexpr_let(flag, l, e) ->
      pp_print_string !formatter (if flag = Recursive then "let rec " else "let ");
      print_list (print_patt_expr
		    (fun () ->
		      pp_print_space !formatter ();
		      pp_print_string !formatter "=";
		      pp_print_space !formatter ()))
	(fun () ->
	  pp_print_space !formatter ();
	  pp_print_string !formatter "and")
        l;
      pp_print_space !formatter ();
      pp_print_string !formatter "in";
      pp_print_space !formatter ();
      print 0 e
  | Cexpr_ifthenelse(e1,e2,e3) ->
      pp_print_string !formatter "if";
      pp_print_space !formatter ();
      print (pri_e - 1) e1;
      pp_print_space !formatter ();
      pp_print_string !formatter "then";
      pp_print_space !formatter ();
      print pri_e e2;
      pp_print_space !formatter ();
      pp_print_string !formatter "else";
      pp_print_space !formatter ();
      print pri_e e3
  | Cexpr_tuple(l) ->
      pp_print_string !formatter "(";
      print_list (print (pri_e - 1)) (fun () -> pp_print_string !formatter ",") l;
      pp_print_string !formatter ")"
  | Cexpr_record(l) ->
      pp_print_string !formatter "{";
      print_list (fun (gl, e) -> print_global gl;
                                 pp_print_string !formatter "=";
	                         print (pri_e + 1) e)
                 (fun () -> pp_print_string !formatter ";") l;
      pp_print_string !formatter "}"
  | Cexpr_record_access(e, gl) ->
      print pri_e e;
      pp_print_string !formatter ".";
      print_global gl
  | Cexpr_record_with(e, l) ->
      pp_print_string !formatter "{";
      pp_print_string !formatter "(";
      print (pri_e - 1) e;
      pp_print_string !formatter ")";
      pp_print_space !formatter ();
      pp_print_string !formatter "with";
      pp_print_space !formatter ();
      print_list (fun (gl, e) -> print_global gl;
                                 pp_print_string !formatter "=";
	                         print (pri_e + 1) e)
                 (fun () -> pp_print_string !formatter ";") l;
      pp_print_string !formatter "}"
  | Cexpr_record_update(e1, gl, e2) ->
      print (pri_e + 2) e1;
      pp_print_string !formatter ".";
      print_global gl;
      pp_print_space !formatter ();
      pp_print_string !formatter "<-";
      pp_print_space !formatter ();
      print pri_e e2;
  | Cexpr_array(l) ->
      pp_print_string !formatter "[|";
      print_list (print (pri_e - 1)) (fun () -> pp_print_string !formatter ";") l;
      pp_print_string !formatter "|]"
  | Cexpr_match(e,l) ->
      pp_close_box !formatter ();
      pp_open_box !formatter 0;
      pp_print_string !formatter "match ";
      print 0 e;
      pp_print_string !formatter " with";
      pp_print_space !formatter ();
      List.iter
	(fun pat_expr ->
	  pp_print_string !formatter "| ";
	  print_patt_when_opt_expr
            (fun () -> pp_print_string !formatter " ->"; pp_print_space !formatter ()) pat_expr)
	l
  | Cexpr_seq (e1,e2) ->
      print pri_e e1;
      pp_print_string !formatter ";";
      pp_print_space !formatter ();
      print pri_e e2;
  | Cexpr_trywith (e,l) ->
      pp_close_box !formatter ();
      pp_open_box !formatter 0;
      pp_print_string !formatter "try ";
      print 0 e;
      pp_print_string !formatter " with";
      pp_print_space !formatter ();
      List.iter
	(fun pat_expr ->
	  pp_print_string !formatter "| ";
	  print_patt_when_opt_expr
            (fun () -> pp_print_string !formatter " ->"; pp_print_space !formatter ()) pat_expr)
	l
  | Cexpr_assert e ->
      pp_print_string !formatter "assert";
      pp_print_space !formatter ();
      print (pri_e + 1) e
  | Cexpr_while (e1,e2) ->
      pp_print_string !formatter "while";
      pp_print_space !formatter ();
      print (pri_e - 1) e1;
      pp_print_space !formatter ();
      pp_print_string !formatter "do";
      pp_print_space !formatter ();
      print (pri_e - 1) e2;
      pp_print_space !formatter ();
      pp_print_string !formatter "done";
  | Cexpr_for (i,e1,e2,flag,e3) ->
      pp_print_string !formatter "for";
      pp_print_space !formatter ();
      print_name (Ident.unique_name i);
      pp_print_string !formatter " = ";
      print (pri_e - 1) e1;
      pp_print_space !formatter ();
      pp_print_string !formatter (if flag = Upto then "to" else "downto");
      pp_print_space !formatter ();
      print (pri_e - 1) e2;
      pp_print_space !formatter ();
      pp_print_string !formatter "do";
      pp_print_space !formatter ();
      print (pri_e - 1) e3;
      pp_print_space !formatter ();
      pp_print_string !formatter "done";
  | Cexpr_constraint (e,typ) ->
      pp_print_string !formatter "(";
      print (pri_e - 1) e;
      pp_print_string !formatter ":";
      pp_print_space !formatter ();
      print_te 0 typ;
      pp_print_string !formatter ")";
  end;
  if pri > pri_e then pp_print_string !formatter ")";
  pp_close_box !formatter ()

and print_te pri typ =
  pp_open_box !formatter 2;
  let pri_e = priority_te typ.cte_desc in
  if pri > pri_e then pp_print_string !formatter "(";
  begin match typ.cte_desc with
  | Ctype_any -> pp_print_string !formatter "_"
  | Ctype_var s -> print_type_var s
  | Ctype_arrow (t1,t2) ->
      pp_print_string !formatter "(";
      print_te (pri_e + 1) t1;
      pp_print_space !formatter ();
      pp_print_string !formatter "->";
      pp_print_space !formatter ();
      print_te pri_e t2;
      pp_print_string !formatter ")";
  | Ctype_product l ->
      pp_print_string !formatter "(";
      print_list (print_te (pri_e - 1)) (fun () -> pp_print_string !formatter " *") l;
      pp_print_string !formatter ")"
  | Ctype_constr (n,[]) ->
      print_global n
  | Ctype_constr (n,l) ->
      pp_print_string !formatter "(";
      print_list (print_te (pri_e - 1)) (fun () -> pp_print_string !formatter ",") l;
      pp_print_string !formatter ")";
      pp_print_space !formatter ();
      print_global n
  end;
  if pri > pri_e then pp_print_string !formatter ")";
  pp_close_box !formatter ()

and print_type_decl typ =
  pp_open_box !formatter 2;
  match typ with
  | Ctype_abstract -> ()
  | Ctype_rebind t ->
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      print_te 0 t
  | Ctype_variant l ->
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      print_list
	(fun (c,typ_opt) ->
	  print_global c;
	  begin
	    match typ_opt with
	    | None -> ()
	    | Some typ ->
		pp_print_space !formatter ();
		pp_print_string !formatter "of";
		pp_print_space !formatter ();
		print_te 2 typ
	  end)
	(fun () -> pp_print_space !formatter (); pp_print_string !formatter "| ")
	l
  | Ctype_record l ->
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      pp_print_string !formatter "{";
      pp_print_space !formatter ();
      print_list
	(fun (lab,flag,typ) ->
	  if flag = Mutable then pp_print_string !formatter "mutable";
	  pp_print_space !formatter ();
	  print_global lab;
	  pp_print_string !formatter ":";
	  pp_print_space !formatter ();
	  print_te 0 typ;)
	(fun () -> pp_print_space !formatter (); pp_print_string !formatter "; ")
	l;
      pp_print_string !formatter "}"


and print_pattern pri pat =
  pp_open_box !formatter 2;
  let pri_e = priority_pattern pat.cpatt_desc in
  if pri > pri_e then pp_print_string !formatter "(";
  begin match pat.cpatt_desc with
    Cpatt_constant(i) -> print_immediate i
  | Cpatt_var(Cvarpatt_local v) ->
      print_name (Ident.unique_name v)
  | Cpatt_var(Cvarpatt_global gl) ->
      print_global gl
  | Cpatt_construct(gl, None) -> print_global gl
  | Cpatt_construct(gl,Some patt) when (Ident.name gl.gi.id = "::") ->
      begin
	match patt.cpatt_desc with
	| Cpatt_tuple [p1;p2] ->
	    print_pattern (1 + pri_e) p1;
	    pp_print_space !formatter ();
	    pp_print_string !formatter "::";
	    pp_print_space !formatter ();
	    print_pattern (1 + pri_e) p2
	| _ -> raise (Internal (patt.cpatt_loc, "Print_caml_src: ::"))
      end
  | Cpatt_construct(gl, Some pat) ->
      print_global gl;
      pp_print_space !formatter ();
      print_pattern 3 pat
  | Cpatt_tuple(pat_list) ->
      pp_print_string !formatter "(";
      print_list (print_pattern (pri_e - 1))
        (fun () -> pp_print_string !formatter ",") pat_list;
      pp_print_string !formatter ")"
  | Cpatt_record(l) ->
      pp_print_string !formatter "{";
      print_list
	(fun (gl, pat) ->
	  print_global gl;
          pp_print_string !formatter "=";
	  print_pattern (pri_e - 1) pat)
        (fun () -> pp_print_string !formatter ";") l;
      pp_print_string !formatter "}"
  | Cpatt_or(pat1, pat2) ->
      pp_print_string !formatter "(";
      print_pattern pri_e pat1;
      pp_print_string !formatter "|";
      print_pattern pri_e pat2;
      pp_print_string !formatter ")"
  | Cpatt_alias(pat, s) ->
      pp_print_string !formatter "(";
      print_pattern pri_e pat;
      pp_print_space !formatter ();
      pp_print_string !formatter "as";
      pp_print_space !formatter ();
      begin
	match s with
	| Cvarpatt_local id -> print_name (Ident.unique_name id)
	| Cvarpatt_global gl -> print_global gl
      end;
      pp_print_string !formatter ")"
  | Cpatt_any -> pp_print_string !formatter "_"
  | Cpatt_array l ->
      pp_print_string !formatter "[|";
      print_list (print_pattern (pri_e - 1)) (fun () -> pp_print_string !formatter ";") l;
      pp_print_string !formatter "|]"
  | Cpatt_constraint (pat, typ) ->
      pp_print_string !formatter "(";
      print_pattern 0 pat;
      pp_print_string !formatter ":";
      pp_print_space !formatter ();
      print_te 0 typ;
      pp_print_string !formatter ")"
  end;
  if pri > pri_e then pp_print_string !formatter ")";
  pp_close_box !formatter ()

and print_patt_expr print_sep (pat, expr) =
  pp_open_box !formatter 2;
  print_pattern 0 pat;
  print_sep ();
  print 1 expr;
  pp_close_box !formatter ();
  pp_print_space !formatter ()

and print_patt_when_opt_expr print_sep (pat, when_opt, expr) =
  pp_open_box !formatter 2;
  print_pattern 0 pat;
  pp_print_space !formatter ();
  begin match when_opt with
  | None -> ()
  | Some e1 ->
      pp_print_string !formatter "when";
      pp_print_space !formatter ();
      print 1 e1;
  end;
  print_sep ();
  print 1 expr;
  pp_close_box !formatter ();
  pp_print_space !formatter ()

let print_impl_item item =
  match item.cimpl_desc with
  | Cimpl_expr e ->
      pp_open_box !formatter 2;
      print 0 e;
      pp_print_string !formatter ";;";
      pp_close_box !formatter ()
  | Cimpl_let(flag, [patt, { cexpr_desc = Cexpr_fun (param_list, e1) }]) ->
      pp_print_string !formatter (if flag = Recursive then "let rec " else "let ");
      print_pattern 0 patt;
      pp_print_space !formatter ();
      print_list (print_pattern 0) (fun () -> ()) param_list;
      pp_print_space !formatter ();
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      print 0 e1;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_let(flag, l) ->
      pp_print_string !formatter (if flag = Recursive then "let rec " else "let ");
      print_list (print_patt_expr
		    (fun () ->
		      pp_print_space !formatter ();
		      pp_print_string !formatter "=";
		      pp_print_space !formatter ();))
	(fun () ->
	  pp_print_space !formatter ();
	  pp_print_string !formatter "and")
        l;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_type l ->
      pp_print_string !formatter "type ";
      print_list
	(fun (n,param_list,typ) ->
	  begin
	    match param_list with
	    | [] -> ()
	    | [s] -> print_type_var s;
	    | l ->
		pp_print_string !formatter "(";
		print_list print_type_var
		  (fun () -> pp_print_string !formatter ","; pp_print_space !formatter ())
		  l;
		pp_print_string !formatter ")";
	  end;
	  pp_print_space !formatter ();
	  print_global n;
	  pp_print_space !formatter ();
	  print_type_decl typ)
	(fun () ->
	  pp_print_space !formatter ();
	  pp_print_string !formatter "and")
        l;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_exn (n,None) ->
      pp_print_string !formatter "exception ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_exn (n, Some typ) ->
      pp_print_string !formatter "exception ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter "of";
      pp_print_space !formatter ();
      print_te 0 typ;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_exn_rebind (n,gl) ->
      pp_print_string !formatter "exception ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter "=";
      pp_print_space !formatter ();
      print_global gl;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cimpl_open s ->
      pp_print_string !formatter "open ";
      pp_print_string !formatter s;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"

let print_intf_item item =
  match item.cintf_desc with
  | Cintf_val (n, typ) ->
      pp_print_string !formatter "val ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter ":";
      pp_print_space !formatter ();
      print_te 0 typ;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"

  | Cintf_type l ->
      pp_print_string !formatter "type ";
      print_list
	(fun (n,param_list,typ) ->
	  begin
	    match param_list with
	    | [] -> ()
	    | [s] -> print_type_var s;
	    | l ->
		pp_print_string !formatter "(";
		print_list print_type_var
		  (fun () -> pp_print_string !formatter ","; pp_print_space !formatter ())
		  l;
		pp_print_string !formatter ")";
	  end;
	  pp_print_space !formatter ();
	  print_global n;
	  pp_print_space !formatter ();
	  print_type_decl typ)
	(fun () ->
	  pp_print_space !formatter ();
	  pp_print_string !formatter "and")
        l;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cintf_exn (n,None) ->
      pp_print_string !formatter "exception ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cintf_exn (n, Some typ) ->
      pp_print_string !formatter "exception ";
      print_global n;
      pp_print_space !formatter ();
      pp_print_string !formatter "of";
      pp_print_space !formatter ();
      print_te 0 typ;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
  | Cintf_open s ->
      pp_print_string !formatter "open ";
      pp_print_string !formatter s;
      pp_print_space !formatter ();
      pp_print_string !formatter ";;"
;;

(* the main function *)
set_max_boxes max_int ;;

let output_impl_decl_fmt fmt nl module_name decl =
  current_module := module_name;
  formatter := fmt;
  if nl then pp_force_newline !formatter ();
  print_impl_item decl;
  pp_print_string !formatter "\n";
  pp_print_flush !formatter ()

let output_impl_decl oc module_name decl =
  output_impl_decl_fmt (formatter_of_out_channel oc) true module_name decl

let output_impl_decl_string module_name decl =
  output_impl_decl_fmt str_formatter false module_name decl;
  flush_str_formatter ()

let output_intf_decl oc module_name decl =
  current_module := module_name;
  formatter := formatter_of_out_channel oc;
  pp_force_newline !formatter ();
  print_intf_item decl;
  pp_print_string !formatter "\n\n";
  pp_print_flush !formatter ()
