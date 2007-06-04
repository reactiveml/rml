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
  | Const_string(s) -> print_string ("\""^s^"\"")
	
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
      else
	if c = '*' then "( " ^ s ^ " )" 
	else "(" ^ s ^ ")" 
  in
  print_string s

(** Prints a global name *)
let print_global ({ gi = {qual=q; id=n} } as gl) =
  if gl.gi = Initialization.event_ident then 
    (* special case for event type *)
    begin
      print_string !interpreter_module;
      print_string ".";
      print_name (Ident.name n)
    end    
  else if q = pervasives_module then
    (* special case for values imported from the standard library *)
    print_name (Ident.name n)
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
  open_box 2;
  let pri_e = priority e.cexpr_desc in
  if pri > pri_e then print_string "(";
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
	    print_space ();
	    print_string "::";
	    print_space ();
	    print (1 + pri_e) e2
	| _ -> raise (Internal (e.cexpr_loc, "Print_caml_src: ::"))
      end
  | Cexpr_construct(gl,Some e1) ->
      print_global gl;
      print_space ();
      print (1 + pri_e) e1 
  | Cexpr_apply(f,l) ->
      print pri_e f;
      print_space ();
      print_list (print (pri_e + 1)) (fun () -> ()) l
  | Cexpr_function patt_expr_list ->
      print_string "function";
      print_space ();
      List.iter
	(fun patt_expr -> 
	  print_string "| ";
	  print_patt_expr 
            (fun () -> print_string " ->"; print_space ()) patt_expr)
	patt_expr_list
  | Cexpr_fun(param_list,e1) ->
      print_string "fun";
      print_space ();
      print_list (print_pattern 0) (fun () -> ()) param_list;
      print_space ();
      print_string "->";
      print_space ();
      print 0 e1
  | Cexpr_let(flag, l, e) ->
      print_string (if flag = Recursive then "let rec " else "let ");
      print_list (print_patt_expr 
		    (fun () -> 
		      print_space();
		      print_string "=";
		      print_space ()))
	(fun () -> 
	  print_space ();
	  print_string "and")
        l;
      print_space ();
      print_string "in";
      print_space ();
      print 0 e
  | Cexpr_ifthenelse(e1,e2,e3) -> 
      print_string "if";
      print_space ();
      print (pri_e - 1) e1;
      print_space ();
      print_string "then";
      print_space ();
      print pri_e e2;
      print_space ();
      print_string "else";
      print_space ();
      print pri_e e3
  | Cexpr_tuple(l) ->
      print_string "(";
      print_list (print (pri_e - 1)) (fun () -> print_string ",") l;
      print_string ")"
  | Cexpr_record(l) ->
      print_string "{";
      print_list (fun (gl, e) -> print_global gl;
                                 print_string "=";
	                         print (pri_e + 1) e)
                 (fun () -> print_string ";") l;
      print_string "}"
  | Cexpr_record_access(e, gl) ->
      print pri_e e;
      print_string ".";
      print_global gl
  | Cexpr_record_update(e1, gl, e2) ->
      print pri_e e1;
      print_string ".";
      print_global gl;
      print_space ();
      print_string "<-";
      print_space ();
      print pri_e e2;
  | Cexpr_array(l) ->
      print_string "[|";
      print_list (print (pri_e - 1)) (fun () -> print_string ";") l;
      print_string "|]"
  | Cexpr_match(e,l) ->
      close_box ();
      open_box 0;
      print_string "match ";
      print 0 e;
      print_string " with";
      print_space ();
      List.iter
	(fun pat_expr -> 
	  print_string "| ";
	  print_patt_expr 
            (fun () -> print_string " ->"; print_space ()) pat_expr)
	l
  | Cexpr_seq (e1,e2) -> 
      print pri_e e1;
      print_string ";";
      print_space ();
      print pri_e e2;
  | Cexpr_trywith (e,l) ->
      close_box ();
      open_box 0;
      print_string "try ";
      print 0 e;
      print_string " with";
      print_space ();
      List.iter
	(fun pat_expr -> 
	  print_string "| ";
	  print_patt_expr 
            (fun () -> print_string " ->"; print_space ()) pat_expr)
	l
  | Cexpr_assert e ->
      print_string "assert";
      print_space ();
      print pri_e e
  | Cexpr_while (e1,e2) ->
      print_string "while";
      print_space ();
      print (pri_e - 1) e1;
      print_space ();
      print_string "do";
      print_space ();
      print (pri_e - 1) e2;
      print_space ();
      print_string "done";
  | Cexpr_for (i,e1,e2,flag,e3) ->
      print_string "for";
      print_space ();
      print_name (Ident.unique_name i);
      print_string " = ";
      print (pri_e - 1) e1;
      print_space ();
      print_string (if flag = Upto then "to" else "downto");
      print_space ();
      print (pri_e - 1) e2;
      print_space ();
      print_string "do";
      print_space ();
      print (pri_e - 1) e3;
      print_space ();
      print_string "done";
  | Cexpr_constraint (e,typ) ->
      print_string "(";
      print (pri_e - 1) e;
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_string ")";
  | Cexpr_when_match _ ->
      raise (Internal (e.cexpr_loc, "Print_caml_src.print Cexpr_when_match"))
  end;
  if pri > pri_e then print_string ")";
  close_box()

and print_te pri typ =
  open_box 2;
  let pri_e = priority_te typ.cte_desc in
  if pri > pri_e then print_string "(";
  begin match typ.cte_desc with
  | Ctype_any -> print_string "_"
  | Ctype_var s -> print_type_var s
  | Ctype_arrow (t1,t2) ->
      print_te (pri_e + 1) t1;
      print_space ();
      print_string "->";
      print_space ();
      print_te pri_e t2;
  | Ctype_product l ->
      print_string "(";
      print_list (print_te (pri_e - 1)) (fun () -> print_string " *") l;
      print_string ")"
  | Ctype_constr (n,[]) ->
      print_global n
  | Ctype_constr (n,l) ->
      print_string "(";
      print_list (print_te (pri_e - 1)) (fun () -> print_string ",") l;
      print_string ")";
      print_space ();
      print_global n
  end;
  if pri > pri_e then print_string ")";
  close_box()

and print_type_decl typ =
  open_box 2;
  match typ with
  | Ctype_abstract -> ()
  | Ctype_rebind t -> 
      print_string "=";
      print_space ();
      print_te 0 t
  | Ctype_variant l ->
      print_string "=";
      print_space ();
      print_list 
	(fun (c,typ_opt) ->
	  print_global c;
	  begin
	    match typ_opt with
	    | None -> ()
	    | Some typ -> 
		print_space ();
		print_string "of";
		print_space ();
		print_te 2 typ
	  end)
	(fun () -> print_space (); print_string "| ")
	l
  | Ctype_record l ->
      print_string "=";
      print_space ();
      print_string "{";
      print_space ();
      print_list
	(fun (lab,flag,typ) ->
	  if flag = Mutable then print_string "mutable";
	  print_space ();
	  print_global lab;
	  print_string ":";
	  print_space ();
	  print_te 0 typ;) 
	(fun () -> print_space (); print_string "; ")
	l;
      print_string "}"


and print_pattern pri pat =
  open_box 2;
  let pri_e = priority_pattern pat.cpatt_desc in
  if pri > pri_e then print_string "(";
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
	    print_space ();
	    print_string "::";
	    print_space ();
	    print_pattern (1 + pri_e) p2
	| _ -> raise (Internal (patt.cpatt_loc, "Print_caml_src: ::"))
      end
  | Cpatt_construct(gl, Some pat) ->
      print_global gl;
      print_space ();
      print_pattern 2 pat
  | Cpatt_tuple(pat_list) ->
      print_string "(";
      print_list (print_pattern (pri_e - 1))
        (fun () -> print_string ",") pat_list;
      print_string ")"
  | Cpatt_record(l) ->
      print_string "{";
      print_list 
	(fun (gl, pat) -> 
	  print_global gl;
          print_string "=";
	  print_pattern (pri_e - 1) pat)
        (fun () -> print_string ";") l;
      print_string "}"
  | Cpatt_or(pat1, pat2) ->
      print_pattern pri_e pat1;
      print_string "|";
      print_pattern pri_e pat2
  | Cpatt_alias(pat, s) ->
      print_pattern pri_e pat;
      print_space ();
      print_string "as";
      print_space ();
      begin 
	match s with
	| Cvarpatt_local id -> print_name (Ident.unique_name id)
	| Cvarpatt_global gl -> print_global gl
      end
  | Cpatt_any -> print_string "_"
  | Cpatt_array l ->
      print_string "[|";
      print_list (print_pattern (pri_e - 1)) (fun () -> print_string ";") l;
      print_string "|]"
  | Cpatt_constraint (pat, typ) ->
      print_string "(";
      print_pattern 0 pat;
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_string ")"
  end;
  if pri > pri_e then print_string ")";
  close_box ()  

and print_patt_expr print_sep (pat, expr) =
  open_box 2;
  print_pattern 0 pat;
  match expr.cexpr_desc with
  | Cexpr_when_match(e1,e2) ->
      print_space ();
      print_string "when";
      print_space ();
      print 1 e1;
      print_sep ();
      print 1 e2;
      close_box ();
      print_space ()
  | _ -> 
      print_sep ();
      print 1 expr;
      close_box ();
      print_space ()

let print_impl_item item =
  match item.cimpl_desc with
  | Cimpl_expr e ->
      open_box 2;
      print 0 e;
      print_string ";;";
      close_box ()
  | Cimpl_let(flag, l) ->
      print_string (if flag = Recursive then "let rec " else "let ");
      print_list (print_patt_expr 
		    (fun () -> 
		      print_space ();
		      print_string "=";
		      print_space ();))
	(fun () -> 
	  print_space ();
	  print_string "and")
        l;
      print_space ();
      print_string ";;"
  | Cimpl_type l ->
      print_string "type ";
      print_list 
	(fun (n,param_list,typ) ->
	  begin
	    match param_list with
	    | [] -> ()
	    | [s] -> print_type_var s;
	    | l -> 
		print_string "(";
		print_list print_type_var 
		  (fun () -> print_string ","; print_space ())
		  l;
		print_string ")";
	  end;
	  print_space ();
	  print_global n;
	  print_space ();
	  print_type_decl typ)
	(fun () -> 
	  print_space ();
	  print_string "and")
        l;
      print_space ();
      print_string ";;"
  | Cimpl_exn (n,None) -> 
      print_string "exception ";
      print_global n;
      print_space ();
      print_string ";;"
  | Cimpl_exn (n, Some typ) -> 
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "of";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"
  | Cimpl_exn_rebind (n,gl) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "=";
      print_space ();
      print_global gl;
      print_space ();
      print_string ";;"
  | Cimpl_open s ->
      print_string "open ";
      print_string s;
      print_space ();
      print_string ";;"
;;

let print_intf_item item =
  match item.cintf_desc with
  | Cintf_val (n, typ) ->
      print_string "val ";
      print_global n;
      print_space ();
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"

  | Cintf_type l ->
      print_string "type ";
      print_list 
	(fun (n,param_list,typ) ->
	  begin
	    match param_list with
	    | [] -> ()
	    | [s] -> print_type_var s;
	    | l -> 
		print_string "(";
		print_list print_type_var 
		  (fun () -> print_string ","; print_space ())
		  l;
		print_string ")";
	  end;
	  print_space ();
	  print_global n;
	  print_space ();
	  print_type_decl typ)
	(fun () -> 
	  print_space ();
	  print_string "and")
        l;
      print_space ();
      print_string ";;"
  | Cintf_exn (n,None) -> 
      print_string "exception ";
      print_global n;
      print_space ();
      print_string ";;"
  | Cintf_exn (n, Some typ) -> 
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "of";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"
  | Cintf_open s ->
      print_string "open ";
      print_string s;
      print_space ();
      print_string ";;"
;;

(* the main function *)
set_max_boxes max_int ;;

let output_impl_decl oc module_name decl =
  current_module := module_name;
  set_formatter_out_channel oc;
  force_newline ();
  print_impl_item decl;
  print_string "\n\n";
  print_flush ()

let output_intf_decl oc module_name decl =
  current_module := module_name;
  set_formatter_out_channel oc;
  force_newline ();
  print_intf_item decl;
  print_string "\n\n";
  print_flush ()
