(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * (C) 2011 Jérôme Vouillon Laboratoire PPS - CNRS Université Paris Diderot
 * (C) 2011 Cagdas Bozman - OCamlPro SAS
 * (C) 2012 Mehdi Dogguy - ParKas Team, DI, ENS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

(****)

class type global_data = object
  method toc : (string * string) list Js.readonly_prop
  method compile : (string -> string) Js.writeonly_prop
end

external global_data : unit -> global_data Js.t = "caml_get_global_data"

let g = global_data ()

let _ =
  let toc = g##toc in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Buffer.contents b
  in
  g##compile <- compile; (*XXX HACK!*)

module Html = Dom_html

let s = ""
let debug_mode = ref false

let doc = Html.document
let window = Html.window
let loc = Js.Unsafe.variable "location"

let registered_buttons = ref []

let button_type = Js.string "button"
let text_button txt action =
  let b = Dom_html.createButton ~_type:button_type doc in
  let id = "button"^txt in
  b##innerHTML <- Js.string txt;
  b##id <- Js.string id;
  registered_buttons := (id, txt) :: !registered_buttons;
  b##className <- Js.string "btn";
  b##onclick <- Dom_html.handler (fun _ -> action b; Js._true);
  b

let at_bol = ref true
let consume_nl = ref false

let input = ref []
let output = ref []

let rec refill_lexbuf s p ppf buffer len =
  match !input with
    | '\000' :: tail ->
      input := tail;
      refill_lexbuf s p ppf buffer len
    | c :: tail ->
      input := tail;
      output := c :: !output;
      buffer.[0] <- c;
      1
    | [] ->
      if !consume_nl then begin
        let l = String.length s in
        if (!p < l && s.[!p] = '\n') then
          incr p
        else if (!p + 1 < l && s.[!p] = '\r' && s.[!p + 1] = '\n') then
          p := !p + 2;
        consume_nl := false
      end;
      if !p = String.length s then begin
        output := '\000' :: !output;
        0
      end else begin
        let c = s.[!p] in
        incr p;
        buffer.[0] <- c;
        if !at_bol then Format.fprintf ppf "> ";
        at_bol := (c = '\n');
        if c = '\n' then
          Format.fprintf ppf "@."
        else
          Format.fprintf ppf "%c" c;
        output := c :: !output;
        1
      end

let parse ppf s =
  let lb =
    if !debug_mode then
      Lexing.from_function (refill_lexbuf s (ref 0) ppf)
    else
      Lexing.from_string s
  in
  Rmltop_lexer.phrase lb

let start ppf =
  Format.fprintf ppf "        ReactiveML (version %s)@.@." (Rmlcompiler.Version.version);
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "";
  Rmlcompiler.Misc.interactive := true;
  Rmlcompiler.Misc.print_type := true;
  Rmlcompiler.Misc.err_fmt := ppf;
  Rmlcompiler.Misc.std_fmt := ppf;
  Rmlcompiler.Configure.configure ();

  let _ = Rmltop_library.eval_command ppf false "open Implem;;" in
  let _ = Rmltop_library.eval_command ppf false "let controller_react =
    Rmltop_implem.Machine_controler_machine.rml_make Rmltop_controller.controller;;" in
  let _ = Rmltop_library.eval ppf (parse ppf) "open Pervasives;;" in
  ()

let ensure_at_bol ppf =
  if not !at_bol then begin
    Format.fprintf ppf "@.";
    consume_nl := true; at_bol := true
  end

let get_element_by_id id =
  Js.Opt.get (doc##getElementById (Js.string id))
    (fun () -> assert false)

let set_by_id id s =
    let container = get_element_by_id id in
    container##innerHTML <- Js.string s

let set_container_by_id id s =
  try
    set_by_id id s
  with _ -> ()

let update_prompt prompt =
  set_container_by_id "sharp" prompt

(* Some useful functions to handle cookies *)
let find_in good_input input =
  try
    let len = String.length good_input in
    for i = 0 to String.length input - len  do
      if String.sub input i len = good_input then
        raise Exit
    done;
    false
  with Exit -> true

let get_cookie () =
  let reg = Regexp.regexp ";" in
  Regexp.split reg (Js.to_string doc##cookie)

let set_cookie key value =
  let today = jsnew Js.date_now () in
  let expire_time = today##setTime
    ((Js.to_float today##getTime()) *. 60. *. 60. *. 24. *. 365.) in
  doc##cookie <- Js.string (Printf.sprintf "%s=%s;expires=%f" key value
                              (Js.to_float expire_time))


let get_by_id id =
  let container = get_element_by_id id in
  Js.to_string container##innerHTML

let get_by_name id =
  let container =
    List.hd (Dom.list_of_nodeList (doc##getElementsByTagName (Js.string id)))
  in
  Js.to_string container##innerHTML

exception End_of_input

let string_of_char_list list =
  let len = List.length list in
  let s = String.create len in
  let rec iter s i list =
    match list with
        [] -> s
      | c :: tail ->
        s.[i] <- c;
        iter s (i+1) tail
  in
  iter s 0 list

let loop s ppf =
  let s =
    begin
      let need_terminator = ref true in
      for i = 0 to String.length s - 2 do
        if s.[i] = ';' && s.[i+1] = ';' then need_terminator := false;
      done;
      output := [];
      if !need_terminator then s ^ ";;" else s
    end
  in
  try
    Format.fprintf ppf "@[#@ %s@]@." s;
    output := [];
    ensure_at_bol ppf;
    Rmltop_library.eval ppf (parse ppf) s;
    ensure_at_bol ppf;
  with
    | End_of_input | Not_found ->
      match !output with
        | [] | [ '\000' ] ->
          output := []; update_prompt "\n#"
        | _ ->
          ()

let append_children id list =
  let ele = get_element_by_id id in
  List.iter (fun w -> Dom.appendChild ele w) list

let run _ =
  let top = get_element_by_id "toplevel"  in
  let output_area = get_element_by_id "output" in
  let ppf =
    let b = Buffer.create 80 in
    Format.make_formatter
      (fun s i l ->
        Buffer.add_substring b s i l)
      (fun _ ->
        let text = Buffer.contents b in
        let () = Buffer.clear b in
        Dom.appendChild output_area
          (doc##createTextNode(Js.string text))
      )
  in
  let textbox = Html.createTextarea doc in
  textbox##value <- Js.string "";
  textbox##id <- Js.string "rmlconsole";
  Dom.appendChild top textbox;
  textbox##focus();
  textbox##select();
  let container = get_element_by_id "toplevel-container" in
  container##onclick <- Dom_html.handler (fun _ ->
    textbox##focus();  textbox##select();  Js._true);
  let history = ref [] in
  let history_bckwrd = ref [] in
  let history_frwrd = ref [] in
  let execute () =
    let s = Js.to_string textbox##value in
    if s <> "" then
      begin
        history := Js.string s :: !history;
      end;
    history_bckwrd := !history;
    history_frwrd := [];
    textbox##value <- Js.string "";
    (try loop s ppf with _ -> ());
    textbox##focus();
    container##scrollTop <- container##scrollHeight;
  in

  let tbox_init_size = textbox##style##height in
  Html.document##onkeydown <-
    (Html.handler
       (fun e -> match e##keyCode with
         | 13 -> (* ENTER key *)
           let keyEv = match Js.Opt.to_option (Html.CoerceTo.keyboardEvent e) with
             | None   -> assert false
             | Some t -> t in
           (* Special handling of ctrl key *)
           if keyEv##ctrlKey = Js._true then
             textbox##value <- Js.string ((Js.to_string textbox##value) ^ "\n");
           if keyEv##ctrlKey = Js._true || keyEv##shiftKey = Js._true then
             let rows_height = textbox##scrollHeight / (textbox##rows + 1) in
             let h = string_of_int (rows_height * (textbox##rows + 1) + 20) ^ "px" in
             textbox##style##height <- Js.string h;
             Js._true
           else begin
             execute ();
             textbox##style##height <- tbox_init_size;
             textbox##value <- Js.string "";
             Js._false
           end
	 | 38 -> (* UP ARROW key *) begin
	   match !history_bckwrd with
	     | s :: l ->
	       let str = Js.to_string textbox##value in
	       history_frwrd := Js.string str :: !history_frwrd;
	       textbox##value <- s;
	       history_bckwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | 40 -> (* DOWN ARROW key *) begin
	   match !history_frwrd with
	     | s :: l ->
	       let str = Js.to_string textbox##value in
	       history_bckwrd := Js.string str :: !history_bckwrd;
	       textbox##value <- s;
	       history_frwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | _ -> Js._true));
  let debug_button_text () =
    if !debug_mode then
      "Debug (on)"
    else
      "Debug (off)"
  in
  let debug_button = text_button (debug_button_text ()) (fun b ->
    debug_mode := not !debug_mode;
    b##innerHTML <- Js.string (debug_button_text ())
  ) in
  let step_button = text_button "Step" (fun b ->
    Rmltop_global.set_step 1
  ) in
  let suspend_button = text_button "Suspend" (fun b ->
    Rmltop_global.set_suspend ()
  ) in
  let resume_button = text_button "Resume" (fun b ->
    Rmltop_global.set_resume ()
  ) in
  let send_button = text_button "Send" (fun _ -> execute ()) in
  let save_button =  text_button "Save" (fun _ ->
    let content = Js.to_string output_area##innerHTML in
    let l = Regexp.split (Regexp.regexp ("\n")) content in
    let content =
      Js.string (
        let l = List.filter (fun x ->
          try x.[0] = '#' with _ -> false) l in
        let l = List.map  (fun x -> String.sub x 2 ((String.length x) - 2)) l in
        String.concat "\n" l)
    in
    let uriContent =
      Js.string ("data:text/x-ocaml," ^
                    (Js.to_string (Js.encodeURI content))) in
    ignore (window##open_(uriContent, Js.string "Try ReactiveML", Js.null));
    window##close ()
  )
  in

  append_children "buttons" [
    debug_button;
    send_button;
    save_button;
    suspend_button;
    step_button;
    resume_button;
  ];

  output_area##scrollTop <- output_area##scrollHeight;
  start ppf;

  Rmltop_global.enter_step_by_step_mode :=
    (fun () ->
      step_button##disabled <- Js._false;
      resume_button##disabled <- Js._false;
    );

  Rmltop_global.exit_step_by_step_mode :=
    (fun () ->
      step_button##disabled <- Js._true;
      resume_button##disabled <- Js._true;
    );

  !Rmltop_global.exit_step_by_step_mode ();

  let (>>=) = Lwt.bind in
  let rec exec_machine_controller () =
    Lwt_js.sleep !Rmltop_global.sampling >>= fun () ->
      let _ = Rmltop_library.eval_command ppf false "controller_react ();;" in
      exec_machine_controller () in
  let _ = exec_machine_controller () in

  Js._false

let _ =
  try
    ignore (run ());
  with e ->
    window##alert (Js.string
                     (Printf.sprintf "exception %s during init."
                        (Printexc.to_string e)))
