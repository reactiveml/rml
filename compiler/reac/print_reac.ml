open Format
open Misc
open Reac
open Global
open Global_ident
open Print_global
open Asttypes

let priority exp =
  match exp with
    (Elet _ | Esignal _ | Efunction _  | Ematch _
  | Etrywith _  | Epar _) -> 0
  | Eseq _  -> 1
  | Eifthenelse _ -> 2
  | (Erecord _  | Econstruct (_,None) | Elocal _
  | Eglobal _ | Econstant _ | Earray _  | Econstraint _ )
    -> 5
  | (Erecord_access _ | Etuple _) -> 6
  | Erun _ -> 7
  | _ -> 4

let priority_pattern p =
  match p with
  | Ptuple _ -> 3
  | (Pconstruct _ | Pconstant _ | Pvar _
  | Precord _) -> 2
  | _ -> 1

let priority_te t =
  match t with
  | Tarrow _ -> 0
  | Tproduct _ -> 1
  | _ -> 2

let rec print pri e =
  open_box 2;
  let pri_e = priority e.e_desc in
  if pri > pri_e then print_string "(";
  begin match e.e_desc with
  | Elocal s -> print_name (Ident.unique_name s)
  | Eglobal gl -> print_global gl
  | Econstant im -> print_immediate im
  | Elet (flag, l, e) ->
      open_box 2;
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
  | Efunction patt_expr_list ->
      open_box 2;
      print_string "function";
      print_space ();
      List.iter
        (fun patt_expr ->
          print_string "| ";
          print_patt_expr
            (fun () -> print_string " ->"; print_space ()) patt_expr)
        patt_expr_list
  | Eapply (f,l) ->
      print pri_e f;
      print_space ();
      print_list (print (pri_e + 1)) (fun () -> ()) l
  | Etuple l ->
      print_string "(";
      print_list (print (pri_e - 1)) (fun () -> print_string ",") l;
      print_string ")"
  | Econstruct (gl, None) -> print_global gl
  | Econstruct(gl,Some expr) when (Ident.name gl.gi.id = "::") ->
      begin
        match expr.e_desc with
          | Etuple [e1;e2] ->
              print (1 + pri_e) e1;
              print_space ();
              print_string "::";
              print_space ();
              print (1 + pri_e) e2
          | _ -> raise (Internal (e.e_loc, "Print_reac: ::"))
      end
  | Econstruct(gl,Some e1) ->
      print_global gl;
      print_space ();
      print (1 + pri_e) e1
  | Earray l ->
      print_string "[|";
      print_list (print (pri_e - 1)) (fun () -> print_string ";") l;
      print_string "|]"
  | Erecord l ->
      print_string "{";
      print_list (fun (gl, e) -> print_global gl;
                                 print_string "=";
                                 print (pri_e + 1) e)
                 (fun () -> print_string ";") l;
      print_string "}"
  | Erecord_access (e, gl) ->
      print pri_e e;
      print_string ".";
      print_global gl
  | Erecord_update (e1, gl, e2) ->
      print (pri_e + 2) e1;
      print_string ".";
      print_global gl;
      print_space ();
      print_string "<-";
      print_space ();
      print pri_e e2
  | Econstraint (e,typ) ->
      print_string "(";
      print (pri_e - 1) e;
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_string ")";
  | Etrywith (e,l) ->
      close_box ();
      open_box 0;
      print_string "try ";
      print 0 e;
      print_string " with";
      open_box 2;
      print_space ();
      List.iter
              (fun pat_expr ->
                print_string "| ";
                print_patt_expr
            (fun () -> print_string " ->"; print_space ()) pat_expr)
              l
  | Eassert e ->
      print_string "assert";
      print_space ();
      print (pri_e + 1) e
  | Eifthenelse (e1,e2,e3) ->
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
  | Ematch (e,l) ->
      close_box ();
      open_box 0;
      print_string "match ";
      print 0 e;
      print_string " with";
      print_space ();
      open_box 2;
      List.iter
        (fun pat_expr ->
          print_string "| ";
          print_patt_expr
            (fun () -> print_string " ->"; print_space ()) pat_expr)
        l
  | Ewhen_match _ -> assert false
  | Ewhile (e1,e2) ->
      print_string "while";
      print_space ();
      print (pri_e - 1) e1;
      print_space ();
      print_string "do";
      print_space ();
      print (pri_e - 1) e2;
      print_space ();
      print_string "done";
  | Efor (i,e1,e2,flag,e3) ->
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
  | Eseq (e1::el) ->
      print pri_e e1;
      List.iter (fun e -> print_string ";"; print_space ();
                          print pri_e e) el
  | Eseq _ -> assert false
  | Eprocess e1 ->
      print_string "process";
      print_space ();
      print (pri_e - 1) e1
  | Epre (k, e1) ->
      let op = match k with Status -> "pre" | Value -> "pre?" in
      print_string op;
      print_space ();
      print (pri_e - 1) e1
  | Elast e1 ->
      print_string "last?";
      print_space ();
      print (pri_e - 1) e1
  | Edefault e1 ->
      print_string "default";
      print_space ();
      print (pri_e - 1) e1
  | Enothing ->
      print_string "nothing"
  | Epause (_, CkLocal) ->
      print_string "pause"
  | Epause (_, CkTop) ->
      print_string "pause topck"
  | Epause (_, CkExpr e1) ->
      print_string "pause";
      print_space ();
      print (pri_e - 1) e1
  | Ehalt _ ->
      print_string "halt"
  | Eemit (e1, None) ->
      print_string "emit";
      print_space ();
      print (pri_e - 1) e1
  | Eemit (e1, Some e2) ->
      print_string "emit";
      print_space ();
      print (pri_e - 1) e1;
      print_space ();
      print (pri_e - 1) e2;
  | Eloop (_, e1) ->
      close_box ();
      open_box 0;
      print_string "loop";
      print_space ();
      print pri_e e1;
      print_space ();
      print_string "end";
  | Efordopar (i,e1,e2,flag,e3) ->
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
      print_string "dopar";
      print_space ();
      print (pri_e - 1) e3;
      print_space ();
      print_string "done";
  | Epar (e::el) ->
      print pri_e e;
      List.iter (fun e -> print_space (); print_string "||"; print_space ();
                          print pri_e e) el
  | Epar [] -> ()
  | Emerge _ -> assert false
  | Esignal ((s, _), ck, _, comb, e1) ->
      print_string "signal ";
      print_name (Ident.unique_name s);
      (match ck with
        | CkLocal -> ()
        | CkTop -> print_string " at topck"
        | CkExpr e2 -> print_string " at "; print pri_e e2);
      (match comb with
        | None -> ()
        | Some (ed, eg) ->
            print_string " default "; print pri_e ed;
            print_string " gather "; print pri_e eg);
      print_string " in";
      print_space ();
      print pri_e e1
  | Erun e1 ->
      print_string "run";
      print_space ();
      print (pri_e - 1) e1
  | Euntil (conf, e1, None) ->
      close_box ();
      open_box 0;
      print_string "do";
      print_space ();
      print pri_e e1;
      print_space ();
      print_string "until ";
      print_event_config conf;
      print_string " done"
  | Euntil (conf, e1, Some (p, e2)) ->
      close_box ();
      open_box 0;
      print_string "do";
      print_space ();
      print pri_e e1;
      print_space ();
      print_string "until ";
      print_signal_patt_expr (fun () -> print_string "->") p e2;
      print_string " done"
  | Ewhen (conf, e1) ->
      close_box ();
      open_box 0;
      print_string "do";
      print_space ();
      print pri_e e1;
      print_space ();
      print_string "when ";
      print_event_config conf;
  | Econtrol (conf, None, e1) -> assert false
  | Econtrol (conf, Some (p, e2), e1) -> assert false
  | Eget _ -> assert false
  | Epresent (conf, e1, e2) ->
      print_string "present";
      print_space ();
      print_event_config conf;
      print_space ();
      print_string "then";
      print_space ();
      print pri_e e1;
      print_space ();
      print_string "else";
      print_space ();
      print pri_e e2;
  | Eawait (k, conf) ->
      let op = match k with Immediate -> "await immediate" | Nonimmediate -> "await" in
      print_string op;
      print_space ();
      print_event_config conf
  | Eawait_val (k, ak, e1, p, e2) ->
      let op =
        match k, ak with
          | Immediate, One -> "await immediate one"
          | Nonimmediate, One -> "await one"
          | Nonimmediate, all -> "await"
          | _, _ -> assert false
      in
      print_string op;
      print_space ();
      print 0 e1;
      print_signal_patt_expr (fun () -> print_string " in ") p e2
  (*reparml related expressions*)
  | Enewclock (s, _, period, e1) ->
      print_string "domain(";
      print_name (Ident.unique_name s);
      print_string ") do";
      print_space ();
      print pri_e e1;
      print_space ();
      (match period with
        | None -> ()
        | Some e3 -> print_string "by "; print 0 e; print_space ());
      print_string "done"
  | Epauseclock _ -> assert false
  | Etopck -> print_string "topck"
  (*memory*)
  | Ememory _
  | Elast_mem _
  | Eupdate _
  | Eset_mem _
  | Eawait_new _ -> assert false
  end;
  if pri > pri_e then print_string ")";
  close_box()

and print_event_config conf =
  match conf.conf_desc with
    | Cpresent e -> print 0 e
    | Cand _ | Cor _ -> assert false

and print_te pri typ =
  open_box 2;
  let pri_e = priority_te typ.te_desc in
  if pri > pri_e then print_string "(";
  begin match typ.te_desc with
  | Tvar s -> print_type_var s
  | Tarrow (t1, t2, _) ->
      print_string "(";
      print_te (pri_e + 1) t1;
      print_space ();
      print_string "->";
      print_space ();
      print_te pri_e t2;
      print_string ")";
  | Tprocess (t1, _, _, _) ->
      print_te (pri_e - 1) t1;
      print_string " process"
  | Tproduct l ->
      print_string "(";
      print_list (print_te (pri_e - 1)) (fun () -> print_string " *") l;
      print_string ")"
  | Tconstr (n,[]) ->
      print_global n
  | Tconstr (n,l) ->
      print_string "(";
      print_list (print_pe (pri_e - 1)) (fun () -> print_string ",") l;
      print_string ")";
      print_space ();
      print_global n
  | _ -> assert false
  end;
  if pri > pri_e then print_string ")";
  close_box()


and print_pe pri pe = match pe with
  | Ptype te -> print_te pri te
  | _ -> assert false

and print_type_decl typ =
  open_box 2;
  match typ with
  | Tabstract -> ()
  | Trebind t ->
      print_string "=";
      print_space ();
      print_te 0 t
  | Tvariant l ->
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
  | Trecord l ->
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
  let pri_e = priority_pattern pat.patt_desc in
  if pri > pri_e then print_string "(";
  begin match pat.patt_desc with
    Pconstant(i) -> print_immediate i
  | Pvar(Vlocal v) ->
      print_name (Ident.unique_name v)
  | Pvar(Vglobal gl) ->
      print_global gl
  | Pconstruct(gl, None) -> print_global gl
  | Pconstruct(gl,Some patt) when (Ident.name gl.gi.id = "::") ->
      begin
        match patt.patt_desc with
        | Ptuple [p1;p2] ->
            print_pattern (1 + pri_e) p1;
            print_space ();
            print_string "::";
            print_space ();
            print_pattern (1 + pri_e) p2
        | _ -> raise (Internal (patt.patt_loc, "Print_reac: ::"))
      end
  | Pconstruct(gl, Some pat) ->
      print_global gl;
      print_space ();
      print_pattern 2 pat
  | Ptuple(pat_list) ->
      print_string "(";
      print_list (print_pattern (pri_e - 1))
        (fun () -> print_string ",") pat_list;
      print_string ")"
  | Precord(l) ->
      print_string "{";
      print_list
        (fun (gl, pat) ->
          print_global gl;
          print_string "=";
          print_pattern (pri_e - 1) pat)
        (fun () -> print_string ";") l;
      print_string "}"
  | Por(pat1, pat2) ->
      print_pattern pri_e pat1;
      print_string "|";
      print_pattern pri_e pat2
  | Palias(pat, s) ->
      print_string "(";
      print_pattern pri_e pat;
      print_space ();
      print_string "as";
      print_space ();
      begin
        match s with
        | Vlocal id -> print_name (Ident.unique_name id)
        | Vglobal gl -> print_global gl
      end;
      print_string ")"
  | Pany -> print_string "_"
  | Parray l ->
      print_string "[|";
      print_list (print_pattern (pri_e - 1)) (fun () -> print_string ";") l;
      print_string "|]"
  | Pconstraint (pat, typ) ->
      print_string "(";
      print_pattern 0 pat;
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_string ")"
  end;
  if pri > pri_e then print_string ")";
  close_box ()

and find_args args e = match e.e_desc with
  | Eprocess e1 -> (List.rev args), true, e1
  | Efunction [pat, e] -> find_args (pat::args) e
  | _ -> List.rev args, false, e

and print_patt_expr print_sep (pat, expr) =
  let args, is_process, e2 = find_args [] expr in
  if is_process then print_string "process ";
  print_pattern 0 pat;
  match expr.e_desc with
    | Ewhen_match(e1,e2) ->
        print_space ();
        print_string "when";
        print_space ();
        print 1 e1;
        print_sep ();
        print 0 e2;
        close_box ();
        print_space ()
    | _ ->
        print_space ();
        List.iter (fun pat -> print_pattern 0 pat; print_space ()) args;
        print_sep ();
        print 0 e2;
        close_box ();
        print_space ()

and print_signal_patt_expr print_sep p e2 =
  print_string "(";
  print_pattern 0 p;
  print_string ") ";
  begin match e2.e_desc with
    | Ewhen_match(e1,e2) ->
        print_space ();
        print_string "when";
        print_space ();
        print 1 e1;
        print_sep ();
        print 0 e2;
        close_box ();
        print_space ()
    | _ ->
        print_sep ();
        print 0 e2;
        close_box ();
        print_space ()
  end


let print_impl_item item =
  match item.impl_desc with
  | Iexpr e ->
      open_box 2;
      print 0 e;
      print_newline();
      print_string ";;";
      close_box ()
  | Ilet(flag, l) ->
      open_box 2;
      print_string (if flag = Recursive then "let rec " else "let ");
      print_list (print_patt_expr
                    (fun () ->
                      print_space ();
                      print_string "=";
                      print_break 2 1;))
        (fun () ->
          print_space ();
          print_string "and")
        l;
      print_space ();
      print_string ";;"
  | Itype l ->
      print_string "type ";
      print_list
        (fun (n,param_list,typ) ->
          begin
            match param_list with
            | [] -> ()
            | [(s, _)] -> print_type_var s;
            | l ->
                print_string "(";
                print_list print_any_var
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
  | Iexn (n,None) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string ";;"
  | Iexn (n, Some typ) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "of";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"
  | Iexn_rebind (n,gl) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "=";
      print_space ();
      print_global gl;
      print_space ();
      print_string ";;"
  | Iopen s ->
      print_string "open ";
      print_string s;
      print_space ();
      print_string ";;"
  | Imemory _ -> assert false
  | Isignal l ->
      print_list
        (fun ((n, _), comb) ->
          begin
            print_string "signal ";
            print_global n;
            (match comb with
              | None -> ()
              | Some (ed, eg) ->
                  print_string " default "; print 0 ed;
                  print_string " gather "; print 0 eg);
          end
        )
        (fun () -> print_string ";;"; print_space ())
        l

let print_intf_item item =
  match item.intf_desc with
  | Dval (n, typ) ->
      print_string "val ";
      print_global n;
      print_space ();
      print_string ":";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"

  | Dtype l ->
      print_string "type ";
      print_list
        (fun (n,param_list,typ) ->
          begin
            match param_list with
            | [] -> ()
            | [(s, _)] -> print_type_var s;
            | l ->
                print_string "(";
                print_list print_any_var
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
  | Dexn (n,None) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string ";;"
  | Dexn (n, Some typ) ->
      print_string "exception ";
      print_global n;
      print_space ();
      print_string "of";
      print_space ();
      print_te 0 typ;
      print_space ();
      print_string ";;"
  | Dopen s ->
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
