open Global
open Global_ident
open Asttypes
open Reac
open Reac_utils
open Global_mapfold
open Reac_mapfold

let make_expr e = make_expr e Location.none
let make_patt e = make_patt e Location.none
let make_conf e = make_conf e Location.none
let make_domain_var s =
  Global.only_ident { qual = "Domain";
                      id = Ident.create Ident.gen_var s Ident.Internal }

let topck = make_expr (Eglobal (make_domain_var "top_ck"))
let topck_domain = make_expr (Eglobal (make_domain_var "topck_domain"))
let unit_expr = make_expr (Econstant Const_unit)

let make_instruction s =
  make_expr (Eglobal (make_domain_var s))

let make_label s =
  Global.only_ident { qual = "Domain";
                      id = Ident.create Ident.gen_label s Ident.Internal }

let expr_of_clock_expr act_ck ce = match ce.e_desc with
  | Ebase -> act_ck
  | Etopck -> topck
  | _ -> ce

let tr_conf label conf = match conf.conf_desc with
  | Cpresent e ->
      { conf with conf_desc = Cpresent (make_expr (Erecord_access (e, label))) }
  | _ -> assert false

let expression_desc funs act_ck ed = match ed with
    (*
        pause ck
        --->
        run (pause_ck ck)
    *)
  | Epause (_, Strong, ck) ->
      let ck_expr = expr_of_clock_expr act_ck ck in
      let e = make_expr (Eapply (make_instruction "pause_ck", [ck_expr])) in
      Erun e, act_ck

  (*
    domain(ck) do p done
    --->
    let domain_ck, ck = mk_domain parent period in
    run domain_ck || run (await_step ck); p; emit ck.finished
  *)
  | Enewclock (id, _, period, e) ->
      let ck = make_expr (Elocal id) in
      let domain_ck_id = Ident.create Ident.gen_var ("domain_"^id.Ident.name) Ident.Val_RML in
      let domain_ck = make_expr (Elocal domain_ck_id) in

      let run_ck = make_expr (Erun domain_ck) in
      let await_ck =
        make_expr (Erun (make_expr (Eapply (make_instruction "await_step", [ck]))))
      in
      let emit_finished =
        make_expr (Eemit (make_expr (Erecord_access (ck, make_label "finished")), None))
      in
      let e, acc = Reac_mapfold.expression_it funs ck e in
      let new_body =
        make_expr (Epar [run_ck; make_expr (Eseq [await_ck; e; emit_finished])]) in

      let period = match period with
        | None -> make_expr (Econstruct (Initialization.none_constr_desc, None))
        | Some e -> make_expr (Econstruct (Initialization.some_constr_desc, Some e))
      in
      let mk_domain =
        make_expr (Eapply (make_instruction "mk_domain",
                          [make_expr (Econstant (Const_string (id.Ident.name)));
                           make_expr (Econstruct (Initialization.some_constr_desc, Some act_ck));
                           period]))
      in
      let pat =
        make_patt (Ptuple [make_patt (Pvar (Vlocal domain_ck_id));
                           make_patt (Pvar (Vlocal id))]) in
      Elet (Nonrecursive, [pat, mk_domain], new_body), act_ck

  (*
    signal s default d gather g in e
    --->
    let hold, s = mk_signal d g act_ck in
    signal body_done in
    do run hold until body_done || e; emit done

    TODO: ce n'est pas correct si le signal est declare dans une structure de controle
    car le processus hold doit etre actif a tous les instants
  *)
  | Esignal ((id, _), ck, _, comb, e) ->
      let hold_id = Ident.create Ident.gen_var ("hold_"^id.Ident.name) Ident.Val_RML in
      let hold = make_expr (Elocal hold_id) in

      let clock = expr_of_clock_expr act_ck ck in
      let mk_signal =
        match comb with
          | None -> make_expr (Eapply (make_instruction "mk_signal_default", [clock]))
          | Some (ed, eg) -> make_expr (Eapply (make_instruction "mk_signal", [ed; eg; clock]))
      in
      let pat =
        make_patt (Ptuple [make_patt (Pvar (Vlocal hold_id));
                           make_patt (Pvar (Vlocal id))]) in


      let body_done_id = Ident.create Ident.gen_var ("body_done_"^id.Ident.name) Ident.Val_RML in
      let body_done = make_expr (Elocal body_done_id) in
      let run_hold =
        make_expr (Euntil (make_conf (Cpresent body_done), make_expr (Erun hold), None))
      in
      let emit_done = make_expr (Eemit (body_done, None)) in
      let e, _ = Reac_mapfold.expression_it funs act_ck e in
      let body = make_expr (Epar [run_hold; make_expr (Eseq [e; emit_done])]) in
      let body = make_expr (Esignal ((body_done_id, None), make_expr Ebase,
                                     make_expr Ebase, None, body)) in
      Elet (Nonrecursive, [pat, mk_signal], body), act_ck

  | Eemit (e1, None) ->
      let e1, _ = Reac_mapfold.expression_it funs act_ck e1 in
      Eapply (make_instruction "emit_v", [e1; unit_expr]), act_ck

  | Eemit (e1, Some e2) ->
      let e1, _ = Reac_mapfold.expression_it funs act_ck e1 in
      let e2, _ = Reac_mapfold.expression_it funs act_ck e2 in
      Eapply (make_instruction "emit_v", [e1; e2]), act_ck

  (*
    await s(v) in p
    --->
    let v = run (await_sig s act_ck) in p
  *)
  | Eawait_val (Nonimmediate, All, e1, p, e2) ->
      let e1, _ = Reac_mapfold.expression_it funs act_ck e1 in
      let e2, _ = Reac_mapfold.expression_it funs act_ck e2 in
      let run_await =
        make_expr (Erun (make_expr (Eapply (make_instruction "await_all", [e1; act_ck]))))
      in
      Elet(Nonrecursive, [p, run_await], e2), act_ck

  (*
    do p until s
    --->
    do
      p
    until s.s_await done;
    run (await_step act_ck)
  *)
  | Euntil(conf, e, None) ->
      let conf = tr_conf (make_label "s_await") conf in
      let e, acc = Reac_mapfold.expression_it funs act_ck e in
      let await_ck =
        make_expr (Erun (make_expr (Eapply (make_instruction "await_step", [act_ck]))))
      in
      Eseq [make_expr (Euntil (conf, e, None)); await_ck], act_ck

  (*
    do p when s
    --->
    do
      p
    when s.s_emit done
  *)
  | Ewhen(conf, e) ->
      let conf = tr_conf (make_label "s_emit") conf in
      let e, acc = Reac_mapfold.expression_it funs act_ck e in
      Ewhen(conf, e), act_ck

  (*
     process e
     ---->
     fun act_ck -> process e
  *)
  | Eprocess e ->
      let local_ck_id = Ident.create Ident.gen_var "ck" Ident.Val_RML in
      let local_ck = make_expr (Elocal local_ck_id) in
      let e, acc = Reac_mapfold.expression_it funs local_ck e in
      Efunction ([make_patt (Pvar (Vlocal local_ck_id)), make_expr (Eprocess e)]), act_ck
  (*
     run e
     ---->
     run (e act_ck)
  *)
  | Erun e ->
      Erun (make_expr (Eapply (e, [act_ck]))), act_ck

  | _ -> raise Fallback

let impl_item_desc funs act_ck impl = match impl with
    (*
      let process main =
        run topck_domain
        ||
        run (await_step topck); p; emit topck.finished
    *)
  | Ilet(flag, [({ patt_desc = Pvar (Vglobal vd) } as pat), ({ e_desc = Eprocess e } as expr)])
      when vd.gi.id.Ident.name = !Compiler_options.simulation_process  ->
      let e, acc = Reac_mapfold.expression_it funs act_ck e in
      let run_topck = make_expr (Erun topck_domain) in
      let await_topck =
        make_expr (Erun (make_expr (Eapply (make_instruction "await_step", [topck]))))
      in
      let emit_finished =
        make_expr (Eemit (make_expr (Erecord_access (topck, make_label "finished")), None))
      in
      let new_body = make_expr (Eseq [await_topck; e; emit_finished]) in
      let e = { expr with e_desc = Eprocess (make_expr (Epar [run_topck; new_body])) } in
      let impl = Ilet (flag, [pat, e]) in
      impl, act_ck
  | _ -> raise Fallback

let impl_item impl =
  let funs = { defaults with expression_desc = expression_desc; impl_item_desc = impl_item_desc } in
  let impl, _ = Reac_mapfold.impl_item_it funs topck impl in
  impl
