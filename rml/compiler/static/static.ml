(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : static.ml                                                  *)
(*  Date de creation : 25/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*************************************************************************)

(* $Id$ *)

(* Set the Static/Dynamique status in parse_ast *)

open Parse_ast
open Def_static
open Static_errors

(* For debug *)
let string_of_static typ = 
  match typ with
  | Static -> "Static"
  | Dynamic -> "Dynamic"

let id x = x

let unify typ1 typ2 = 
  match typ1, typ2 with
  | Static, Static -> Static
  | _ -> Dynamic

let static_expr_list f filter ctx l =
  List.fold_left 
    (fun typ x -> unify typ (f ctx (filter x))) 
    Static l

let rec static_expr ctx e =
  let t = 
    match e.pexpr_desc with
    | Pexpr_ident x -> Static

    | Pexpr_constant im -> Static

    | Pexpr_let (_, [_, { pexpr_desc = Pexpr_get s }], e1) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr Process e1 in
	    Dynamic
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e
    | Pexpr_let (_, patt_expr_list, e1) ->
	if static_expr_list static_expr snd ML patt_expr_list = Static
	then static_expr ctx e1 
	else expr_wrong_static_err e

    | Pexpr_function patt_expr_list ->
	if static_expr_list static_expr snd ML patt_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_apply (e1, expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr id ML expr_list in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_tuple expr_list ->
	if static_expr_list static_expr id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_construct (_, None) -> Static
    | Pexpr_construct (_, Some e1) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_array expr_list ->
	if static_expr_list static_expr id ML expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_record ide_expr_list ->
	if static_expr_list static_expr snd ML ide_expr_list = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_record_access (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_record_update (e1, _, e2) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_constraint (e1, _) ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_trywith (e1, patt_expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr snd ML patt_expr_list in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_assert e1 ->
	if static_expr ML e1 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_ifthenelse (e1, e2, None) ->
	if static_expr ML e1 = Static
	then 
	  static_expr ctx e2 
	else expr_wrong_static_err e
    | Pexpr_ifthenelse (e1, e2, Some e3) ->
	if static_expr ML e1 = Static
	then 
	  let typ2 = static_expr ctx e2 in
	  let typ3 = static_expr ctx e3 in
	  unify typ2 typ3
	else expr_wrong_static_err e

    | Pexpr_match (e1, patt_expr_list) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr_list static_expr snd ctx patt_expr_list in
	unify typ1 typ2

    | Pexpr_when_match (e1,e2) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if unify typ1 typ2 = Static
	then Static
	else expr_wrong_static_err e

    | Pexpr_while (e1,e2) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ctx e2 in
	unify typ1 typ2

    | Pexpr_for (_, e1, e2, _, e3) ->
	let typ1 = static_expr ML e1 in
	let typ2 = static_expr ML e2 in
	if unify typ1 typ2 = Static
	then static_expr ctx e3
	else expr_wrong_static_err e

    | Pexpr_seq (e1,e2) ->
	let typ1 = static_expr ctx e1 in
	let typ2 = static_expr ctx e2 in
	unify typ1 typ2

    | Pexpr_nothing ->
	if ctx = Process
	then Dynamic
	else expr_wrong_static_err e

    | Pexpr_pause ->
	if ctx = Process
	then Dynamic
	else expr_wrong_static_err e

    | Pexpr_emit s ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then Dynamic
	  else expr_wrong_static_err s
	else expr_wrong_static_err e

    | Pexpr_emit_val (s, e1) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then
	    if static_expr ML e1 = Static
	    then Dynamic
	    else expr_wrong_static_err e1
	  else expr_wrong_static_err s
	else expr_wrong_static_err e

    | Pexpr_loop e1 ->
	if ctx = Process
	then 
	  let typ1 = static_expr Process e1 in
	  Dynamic
	else
	  expr_wrong_static_err e

    | Pexpr_par (e1,e2) ->
	if ctx = Process
	then 
	  let typ1 = static_expr ctx e1 in
	  let typ2 = static_expr ctx e2 in
	  Dynamic
	else
	  expr_wrong_static_err e

    | Pexpr_merge (e1,e2) ->
	if ctx = Process
	then 
	  let typ1 = static_expr ctx e1 in
	  let typ2 = static_expr ctx e2 in
	  Dynamic
	else
	  expr_wrong_static_err e

    | Pexpr_signal (_, None, p) ->
	if ctx = Process
	then 
	  let typ1 = static_expr Process p in
	  Dynamic
	else
	  expr_wrong_static_err e

    | Pexpr_signal (_, Some(e1,e2), p) ->
	if ctx = Process
	then 
	  let typ1 = static_expr ML e1 in
	  let typ2 = static_expr ML e2 in
	  let typ3 = static_expr Process p in
	  if unify typ1 typ2 = Static
	  then Dynamic
	  else expr_wrong_static_err e
	else expr_wrong_static_err e

    | Pexpr_process (p) ->
	let typ = static_expr Process p in
	Static

    | Pexpr_run (e1) ->
	if static_expr ML e1 = Static
	then Dynamic
	else expr_wrong_static_err e

    | Pexpr_until (s, p) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr Process p in
	    Dynamic
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e

    | Pexpr_when (s, p) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr Process p in
	    Dynamic
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e

    | Pexpr_control (s, p) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr Process p in
	    Dynamic
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e
    | Pexpr_present (s, p1, p2) ->
	if ctx = Process
	then
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr ctx p1 in
	    let typ2 = static_expr ctx p2 in
	    Dynamic
	  else expr_wrong_static_err s
 	else
	  expr_wrong_static_err e
    | Pexpr_await (_, s) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then Dynamic
	  else expr_wrong_static_err s
	else expr_wrong_static_err e
    | Pexpr_await_val (_, _, s, _, p) ->
	if ctx = Process
	then 
	  if static_expr ML s = Static
	  then 
	    let typ1 = static_expr Process p in
	    Dynamic
	  else expr_wrong_static_err s
	else
	  expr_wrong_static_err e
    | Pexpr_pre (_, s) ->
	if static_expr ML s = Static
	then Static
 	else expr_wrong_static_err s
    | Pexpr_get _ -> 
	raise (Misc.Internal (e.pexpr_loc ,"Static.static_expr: Pexpr_values"))
  in 
  e.pexpr_static <- t;
  t

let static info_chan impl =
  let typ =
    match impl.pimpl_desc with
    | Pimpl_expr e -> static_expr ML e
    | Pimpl_let (_, l) -> 
	static_expr_list static_expr snd ML l
    | Pimpl_signal (s, Some(e1,e2)) ->
	if (static_expr ML e1) <> Static 
	then expr_wrong_static_err e1
	else 
	  if (static_expr ML e2) <> Static 
	  then expr_wrong_static_err e2
	  else Static
    | _ -> Static
  in
  if typ = Dynamic then impl_wrong_static_err impl

