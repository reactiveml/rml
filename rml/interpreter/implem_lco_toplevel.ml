(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2013-09-08  *)
(* file: lco/implem_lco_toplevel.ml *)


module Lco_toplevel = Lco_ctrl_tree.Rml_interpreter(Sig_env.Record)

module Rml_machine = struct
  module Interpretor = Lco_toplevel

  let react =
    let react =
      Interpretor.rml_make_exec_process (fun () -> Interpretor.rml_halt)
    in
    fun l ->
      match react l with
      | None -> ()
      | Some () -> assert false


  let sampling_hook min () = ()
  let n_hook n () = ()
  let debug_hook () = ()

  let rml_exec boi_hook p =
    let res = ref None in
    let p' () =
      Interpretor.rml_def_dyn
        (Interpretor.rml_run (fun () -> p))
        (fun v -> Interpretor.rml_compute (fun () -> res := Some v))
    in
    let rec exec () =
      List.iter (fun hook -> hook ()) boi_hook;
      react [];
      match !res with
      | None -> exec ()
      | Some v -> v
    in
    react [p'];
    match !res with
    | None -> exec ()
    | Some v -> v

end
