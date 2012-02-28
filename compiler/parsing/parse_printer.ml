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

(* file: parse_printer.ml *)

(* Warning: *)
(* This file is based on the original version of printast.ml *)
(* from the Objective Caml 3.07 distribution, INRIA          *)

(* first modification: 2004-05-12 *)
(* modified by: Louis Mandel *)


(* $Id$ *)

open Format
open Lexing
open Parse_ast
open Asttypes
open Location

let fmt_position f l =
  if l.pos_fname = "" && l.pos_lnum = 1
  then fprintf f "%d" l.pos_cnum
  else if l.pos_lnum = -1
  then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)
;;

let fmt_location f loc =
  fprintf f "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost";
;;

let rec fmt_parseident_aux f x =
  match x with
  | Parse_ident.Pident (s) -> fprintf f "%s" s;
  | Parse_ident.Pdot (m, s) -> fprintf f "%s.%s" m s;
;;

let fmt_parseident f x = fprintf f "\"%a\"" fmt_parseident_aux x;;

let fmt_ident_aux f x = fmt_parseident_aux f x.pident_id;;

let fmt_ident f x = fmt_parseident f x.pident_id;;

let fmt_simple f x = fprintf f "%s" x.psimple_id;;

let fmt_constant f x =
  match x with
  | Const_unit -> fprintf f "Const_unit";
  | Const_bool (b) -> fprintf f "Const_bool %s" (string_of_bool b);
  | Const_int (i) -> fprintf f "Const_int %d" i;
  | Const_char (c) -> fprintf f "Const_char %02x" (Char.code c);
  | Const_string (s) -> fprintf f "Const_string %S" s;
  | Const_float (fl) -> fprintf f "Const_float %f" fl;
;;

let fmt_mutable_flag f x =
  match x with
  | Immutable -> fprintf f "Immutable";
  | Mutable -> fprintf f "Mutable";
;;

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> fprintf f "Nonrec";
  | Recursive -> fprintf f "Rec";
;;

let fmt_direction_flag f x =
  match x with
  | Upto -> fprintf f "Up";
  | Downto -> fprintf f "Down";
;;

let fmt_immediate_flag f x =
  match x with
  | Immediate -> fprintf f "Immediate"
  | Nonimmediate -> fprintf f "Nonimmediate"
;;

let fmt_pre_kind f x =
  match x with
  | Status -> fprintf f "Status"
  | Value -> fprintf f "Value"
;;

let fmt_await_kind_flag f x =
  match x with
  | All -> fprintf f "All"
  | One -> fprintf f "One"
;;

let line i f s (*...*) =
  fprintf f "%s" (String.make (2*i) ' ');
  fprintf f s (*...*)
;;

let list i f ppf l = List.iter (f i ppf) l;;

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x;
;;

let parseident i ppf li = line i ppf "%a\n" fmt_parseident li;;
let ident i ppf li = line i ppf "%a\n" fmt_ident li;;
let simple i ppf li = line i ppf "%a\n" fmt_simple li;;
let string i ppf s = line i ppf "\"%s\"\n" s;;
let bool i ppf x = line i ppf "%s\n" (string_of_bool x);;
let label i ppf x = line i ppf "label=\"%s\"\n" x;;

let rec type_expression i ppf x =
  line i ppf "type_expression %a\n" fmt_location x.pte_loc;
  let i = i+1 in
  match x.pte_desc with
  | Ptype_var (s, _) -> line i ppf "Ptype_var \'%s\n" s;
  | Ptype_arrow (ct1, ct2) ->
      line i ppf "Ptype_arrow\n";
      type_expression i ppf ct1;
      type_expression i ppf ct2;
  | Ptype_tuple l ->
      line i ppf "Ptype_tuple\n";
      list i type_expression ppf l;
  | Ptype_constr (pi, l) ->
      line i ppf "Ptype_constr %a\n" fmt_ident pi;
      list i type_expression ppf l
  | Ptype_process (ct,k,_) ->
      line i ppf "Ptype_process(%s)\n"
	(Static.string_of_instantaneous k);
      type_expression i ppf ct
;;

let rec pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.ppatt_loc;
  let i = i+1 in
  match x.ppatt_desc with
  | Ppatt_any -> line i ppf "Ppatt_any\n";
  | Ppatt_var (s) -> line i ppf "Ppatt_var \"%a\"\n" fmt_simple s;
  | Ppatt_alias (p, s) ->
      line i ppf "Ppatt_alias \"%a\"\n" fmt_simple s;
      pattern i ppf p;
  | Ppatt_constant (c) -> line i ppf "Ppatt_constant %a\n" fmt_constant c;
  | Ppatt_tuple (l) ->
      line i ppf "Ppatt_tuple\n";
      list i pattern ppf l;
  | Ppatt_construct (li, po) ->
      line i ppf "Ppatt_construct %a\n" fmt_ident li;
      option i pattern ppf po;
  | Ppatt_record (l) ->
      line i ppf "Ppatt_record\n";
      list i ident_x_pattern ppf l;
  | Ppatt_array (l) ->
      line i ppf "Ppatt_array\n";
      list i pattern ppf l;
  | Ppatt_or (p1, p2) ->
      line i ppf "Ppatt_or\n";
      pattern i ppf p1;
      pattern i ppf p2;
  | Ppatt_constraint (p, ct) ->
      line i ppf "Ppatt_constraint\n";
      pattern i ppf p;
      type_expression i ppf ct

and ident_x_pattern i ppf (li, p) =
  line i ppf "%a\n" fmt_ident li;
  pattern (i+1) ppf p
;;

let rec expression i ppf x =
  line i ppf "expr %a\n"
    fmt_location x.pexpr_loc;
  let i = i+1 in
  match x.pexpr_desc with
  | Pexpr_ident (id) -> line i ppf "Pexpr_ident %a\n" fmt_ident id;
  | Pexpr_constant (c) -> line i ppf "Pexpr_constant %a\n" fmt_constant c;
  | Pexpr_let (rf, l, e) ->
      line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def ppf l;
      expression i ppf e;
  | Pexpr_function (l) ->
      line i ppf "Pexpr_function\n";
      list i pattern_x_expression_case ppf l;
  | Pexpr_apply (e, l) ->
      line i ppf "Pexp_apply\n";
      expression i ppf e;
      list i expression_arg ppf l;
  | Pexpr_match (e, l) ->
      line i ppf "Pexpr_match\n";
      expression i ppf e;
      list i pattern_x_expression_case ppf l;
  | Pexpr_trywith (e, l) ->
      line i ppf "Pexpr_trywith\n";
      expression i ppf e;
      list i pattern_x_expression_case ppf l;
  | Pexpr_tuple (l) ->
      line i ppf "Pexpr_tuple\n";
      list i expression ppf l;
  | Pexpr_construct (li, eo) ->
      line i ppf "Pexpr_construct %a\n" fmt_ident li;
      option i expression ppf eo;
  | Pexpr_record (l) ->
      line i ppf "Pexpr_record\n";
      list i ident_x_expression ppf l;
  | Pexpr_record_access (e, li) ->
      line i ppf "Pexpr_record_access\n";
      expression i ppf e;
      ident i ppf li;
  | Pexpr_record_update (e1, li, e2) ->
      line i ppf "Pexpr_record_update\n";
      expression i ppf e1;
      ident i ppf li;
      expression i ppf e2;
  | Pexpr_array (l) ->
      line i ppf "Pexpr_array\n";
      list i expression ppf l;
  | Pexpr_ifthenelse (e1, e2, eo) ->
      line i ppf "Pexpr_ifthenelse\n";
      expression i ppf e1;
      expression i ppf e2;
      option i expression ppf eo;
  | Pexpr_seq (e1, e2) ->
      line i ppf "Pexpr_seq\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_while (e1, e2) ->
      line i ppf "Pexpr_while\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_for (s, e1, e2, df, e3) ->
      line i ppf "Pexpr_for \"%a\" %a\n" fmt_simple s fmt_direction_flag df;
      expression i ppf e1;
      expression i ppf e2;
      expression i ppf e3;
  | Pexpr_fordopar (s, e1, e2, df, e3) ->
      line i ppf "Pexpr_fordopar \"%a\" %a\n"
	fmt_simple s fmt_direction_flag df;
      expression i ppf e1;
      expression i ppf e2;
      expression i ppf e3;
  | Pexpr_constraint (e, te) ->
      line i ppf "Pexpr_constraint\n";
      expression i ppf e;
      type_expression i ppf te;
  | Pexpr_when_match (e1, e2) ->
      line i ppf "Pexpr_when_match\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_assert (e) ->
      line i ppf "Pexpr_assert";
      expression i ppf e;
  | Pexpr_nothing ->
      line i ppf "Pexpr_nothing\n";
  | Pexpr_pause e ->
      line i ppf "Pexpr_pause\n";
      clock_expr false i ppf e
  | Pexpr_halt ->
      line i ppf "Pexpr_halt\n";
  | Pexpr_emit (e) ->
      line i ppf "Pexpr_emit";
      expression i ppf e;
  | Pexpr_emit_val (e1,e2) ->
      line i ppf "Pexpr_emit_val\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_loop (e) ->
      line i ppf "Pexpr_loop\n";
      expression i ppf e;
  | Pexpr_par (e1, e2) ->
      line i ppf "Pexpr_par\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_merge (e1, e2) ->
      line i ppf "Pexpr_merge\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_signal (l, (ck, r), eeo, e) ->
      line i ppf "Pexpr_signal\n";
      clock_expr true i ppf ck;
      clock_expr true i ppf r;
      list i string_x_type_expression_option ppf l;
      option i expression_x_expression ppf eeo;
      expression i ppf e;
  | Pexpr_process (e) ->
      line i ppf "Pexpr_process\n";
      expression i ppf e;
  | Pexpr_run (e) ->
      line i ppf "Pexpr_run\n";
      expression i ppf e;
  | Pexpr_until (s,e, patt_expr_opt) ->
      line i ppf "Pexpr_until\n";
      expression i ppf s;
      expression i ppf e;
      option i pattern_x_expression_case ppf patt_expr_opt
  | Pexpr_when (s,e) ->
      line i ppf "Pexpr_when\n";
      expression i ppf s;
      expression i ppf e;
  | Pexpr_control (s, peo, e) ->
      line i ppf "Pexpr_control\n";
      expression i ppf s;
      option i pattern_x_expression_case ppf peo;
      expression i ppf e;
  | Pexpr_get (e) ->
      line i ppf "Pexpr_values\n";
      expression i ppf e;
  | Pexpr_present (s,e1,e2) ->
      line i ppf "Pexpr_present\n";
      expression i ppf s;
      expression i ppf e1;
      expression i ppf e2;
  | Pexpr_await (imf,s) ->
      line i ppf "Pexpr_await %a\n" fmt_immediate_flag imf;
      expression i ppf s;
  | Pexpr_await_val (flag1, flag2, s, p, e) ->
      line i ppf "Pexpr_await_val %a %a\n"
	fmt_immediate_flag flag1 fmt_await_kind_flag flag2;
      expression i ppf s;
      pattern_x_expression_def i ppf (p,e);
  | Pexpr_pre (k, s) ->
      line i ppf "Pexpr_pre %a\n" fmt_pre_kind k;
      expression i ppf s;
  | Pexpr_last s ->
      line i ppf "Pexpr_last\n";
      expression i ppf s;
  | Pexpr_default s ->
      line i ppf "Pexpr_default\n";
      expression i ppf s;
  | Pexpr_newclock (id, _, e) ->
      line i ppf "Pexpr_newclock %a\n" fmt_simple id;
      expression i ppf e
  | Pconf_present (e) ->
      line i ppf "Pconf_present\n";
      expression i ppf e
  | Pconf_and (e1,e2) ->
      line i ppf "Pconf_and\n";
      expression i ppf e1;
      expression i ppf e2
  | Pconf_or (e1,e2) ->
      line i ppf "Pconf_or\n";
      expression i ppf e1;
      expression i ppf e2

and clock_expr with_at i ppf e = match e with
  | CkLocal -> ()
  | CkTop ->
    if with_at then
      line i ppf "at topck\n"
    else
      line i ppf "topck\n"
  | CkExpr e1 ->
    if with_at then
      line i ppf "at ";
    expression i ppf e1

and pattern_x_expression_case i ppf (p, e) =
  line i ppf "<case>\n";
  pattern (i+1) ppf  p;
  expression (i+1) ppf e;

and pattern_x_expression_def i ppf (p, e) =
  line i ppf "<def>\n";
  pattern (i+1) ppf p;
  expression (i+1) ppf e;

and expression_arg  i ppf e =
  line i ppf "<arg>\n";
  expression (i+1) ppf e;

and ident_x_expression i ppf (li, e) =
  line i ppf "%a\n" fmt_ident li;
  expression (i+1) ppf e;

and string_x_type_expression_option i ppf (s, teo) =
  line i ppf "%a\n" fmt_simple s;
  option (i+1) type_expression ppf teo;

and expression_x_expression i ppf (e1, e2) =
  expression i ppf e1;
  expression i ppf e2
;;

let rec type_declaration i ppf x =
  line i ppf "type_declaration \n";
  let i = i+1 in
  match x with
  | Ptype_abstract ->
      line i ppf "Ptype_abstract\n";
  | Ptype_rebind (te) ->
      line i ppf "Ptype_rebind\n";
      type_expression i ppf te;
  | Ptype_variant (l) ->
      line i ppf "Ptype_variant\n";
      list (i+1) string_x_type_expression_option ppf l;
  | Ptype_record (l) ->
      line i ppf "Ptype_record\n";
      list (i+1) string_x_mutable_flag_x_type_expression ppf l;

and string_x_mutable_flag_x_type_expression i ppf (s, mf, te) =
  line i ppf "\"%a\" %a\n" fmt_simple s fmt_mutable_flag mf;
  type_expression (i+1) ppf te

and string_x_type_expression_option i ppf (s, teo) =
  line i ppf "%a\n" fmt_simple s;
  option (i+1) type_expression ppf teo;
;;


let rec impl_item i ppf x =
  line i ppf "impl_item %a\n" fmt_location x.pimpl_loc;
  let i = i+1 in
  match x.pimpl_desc with
  | Pimpl_expr (e) ->
      line i ppf "Pimpl_expr\n";
      expression i ppf e;
  | Pimpl_let (rf, l) ->
      line i ppf "Pimpl_let %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def ppf l;
  | Pimpl_signal (l, eeo) ->
      line i ppf "Pimpl_signal\n";
      list i string_x_type_expression_option ppf l;
      option i expression_x_expression ppf eeo;
  | Pimpl_type (l) ->
      line i ppf "Pimpl_type\n";
      list i string_x_string_list_x_type_expression_def ppf l;
  | Pimpl_exn (s,teo) ->
      line i ppf "Pimpl_exn %a\n" fmt_simple s;
      option i type_expression ppf teo;
  | Pimpl_exn_rebind (s,id) ->
      line i ppf "Pimpl_exn_rebind %a\n" fmt_simple s;
      ident i ppf id;
  | Pimpl_open (s) ->
      line i ppf "Pimpl_open %s\n" s;
  | Pimpl_lucky (id, in_ty_list, out_ty_list, files) ->
      line i ppf "Pimpl_lucky ... A FAIRE ...\n";


and string_x_string_list_x_type_expression_def i ppf (s,l,td) =
  line i ppf "<def> %a\n" fmt_simple s;
  list (i+1) type_var ppf l;
  type_declaration (i+1) ppf td

and type_var i ppf (x, k) = match k with
  | Ttype_var -> line i ppf "\'%s" x
  | Tcarrier_var -> line i ppf "\''%s" x
;;

