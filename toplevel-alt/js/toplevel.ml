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

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (<|) e l = List.iter (fun c -> Dom.appendChild e c) l; e

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
  registered_buttons := (id, b) :: !registered_buttons;
  b##className <- Js.string "btn";
  b##onclick <- Dom_html.handler (fun _ -> action b; Js._true);
  b

let set_action b action =
  b##onclick <- Dom_html.handler (fun _ -> action b; Js._true)

let start ppf =
  Format.fprintf ppf "        ReactiveML (version %s)@.@." (Rmlcompiler.Version.version);
  Rmltop_alt_core.init ();
  Rmlcompiler.Misc.err_fmt := ppf;
  Rmlcompiler.Misc.std_fmt := ppf;
  Rmlcompiler.Configure.configure ();
  Rmlcompiler.Configure.set_runtime "Lco_toplevel_alt";

  let _ = Rmltop_alt_core.eval_ocaml_phrase ppf false "open Implem_lco_ctrl_tree_record;;" in
  let _ = Rmltop_alt_core.eval ppf "open Pervasives;;" in
  ()

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

let get_by_id id =
  let container = get_element_by_id id in
  Js.to_string container##innerHTML

let get_by_name id =
  let container =
    List.hd (Dom.list_of_nodeList (doc##getElementsByTagName (Js.string id)))
  in
  Js.to_string container##innerHTML

let cur_lesson = ref 0
let cur_step = ref 1

let parse_lessons_index content =
  let lessons = Regexp.split (Regexp.regexp "[ \t]*\n+[ \t]*") content in
  let lessons = List.fold_left
    (fun lessons line ->
      if String.length line > 0 && line.[0] <> '#' then
        let lesson = Regexp.split (Regexp.regexp "[ \t]*;[ \t]*") line in
        match lesson with
          | path::title::steps::descr::[] ->
              (path, title, int_of_string steps, descr) :: lessons
          | _ -> lessons
      else
        lessons
    )
    []
    (List.rev lessons) in
  Array.of_list lessons

let get_lessons () =
  XmlHttpRequest.get "lessons/index.json" >|=
    (fun frame ->
      let content = frame.XmlHttpRequest.content in
      parse_lessons_index content
    )

let info_msg msg =
  let textbox = get_element_by_id "info" in
  textbox##innerHTML <- Js.string msg

let update_info name lesson lessons step steps =
  let msg = Printf.sprintf
    "<h3>Lesson %d/%d: %s <small>(step %d/%d)</small></h3>"
    lesson
    lessons
    name
    step
    steps
  in
  info_msg msg

let safe_state_buttons lessons steps =
  let b_next_lesson = List.assoc "button>>" !registered_buttons in
  let b_prev_lesson = List.assoc "button<<" !registered_buttons in
  let b_next_step = List.assoc "button>" !registered_buttons in
  let b_prev_step = List.assoc "button<" !registered_buttons in
  b_next_lesson##disabled <- Js.bool (!cur_lesson = lessons);
  b_prev_lesson##disabled <- Js.bool (!cur_lesson <= 1);
  b_next_step##disabled <- Js.bool (!cur_step = steps);
  b_prev_step##disabled <- Js.bool (!cur_step <= 1)

let load_lesson_step lessons =
  let len = Array.length lessons in
  let path, name, steps, _ = Array.get lessons (!cur_lesson - 1) in
  safe_state_buttons len steps;
  update_info name !cur_lesson len !cur_step steps;
  let lesson_url =
    Printf.sprintf
      "lessons/%s/step%d.html"
      path
      !cur_step
  in
  XmlHttpRequest.get lesson_url >|=
      (fun frame ->
        frame.XmlHttpRequest.content
      )

let load_next_lesson lessons =
  let len = Array.length lessons in
  if !cur_lesson < len then begin
    cur_step := 1;
    incr cur_lesson;
    load_lesson_step lessons;
  end
  else
    Lwt.return "No more lessons found!"

let load_previous_lesson lessons =
  if !cur_lesson > 0 then begin
    cur_step := 1;
    decr cur_lesson;
    load_lesson_step lessons;
  end
  else
    Lwt.return "No previous lessons found!"

let load_next_step lessons =
  let path, _, steps, _ = Array.get lessons (!cur_lesson - 1) in
  if !cur_step < steps then begin
    incr cur_step;
    load_lesson_step lessons;
  end
  else
    Lwt.return "No more steps found!"

let load_previous_step lessons =
  if !cur_step > 1 then begin
    decr cur_step;
    load_lesson_step lessons;
  end
  else
    Lwt.return "No previous steps found!"

let load_tutorial_toc lessons =
  XmlHttpRequest.get "lessons/index.html" >|=
    (fun frame ->
      frame.XmlHttpRequest.content
    )


let extract_escaped_and_kill html i =
  let len = String.length html in
  let rec iter html i len =
    if i = len then i else
      match html.[i] with
          ';' -> i+1
        | _ -> iter html (i+1) len
  in
  let end_pos = iter html (i+1) len in
  let s = String.sub html i (end_pos - i) in
  for j = i to end_pos - 1 do
    html.[j] <- '\000'
  done;
  s

let text_of_html html =
  let b = Buffer.create (String.length html) in
  for i = 0 to String.length html - 1 do
    match html.[i] with
        '&' ->
          begin
            match extract_escaped_and_kill html i with
              | "&gt;" -> Buffer.add_char b '>'
              | "&lt;" -> Buffer.add_char b '<'
              | "&amp;" -> Buffer.add_char b '&'
              | _ -> ()
          end
      | '\000' -> ()
      | c -> Buffer.add_char b c
  done;
  Buffer.contents b

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
  if String.length s <> 0 then begin
    let s = Rmltop_alt_core.add_terminator s in
    Format.fprintf ppf "@[#@ %s@]@." s;
    Rmltop_alt_core.eval ppf s
  end

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
  let rmlconsole = Html.createTextarea doc in
  rmlconsole##value <- Js.string "";
  rmlconsole##id <- Js.string "rmlconsole";
  Dom.appendChild top rmlconsole;
  rmlconsole##focus();
  rmlconsole##select();
  let container = get_element_by_id "toplevel-container" in
  container##onclick <- Dom_html.handler (fun _ ->
    rmlconsole##focus();  rmlconsole##select();  Js._true);
  let history = ref [] in
  let history_bckwrd = ref [] in
  let history_frwrd = ref [] in
  let debug_button = text_button (Rmltop_alt_core.debug_status ()) (fun b ->
    Rmltop_alt_core.toggle_debug ppf;
    b##innerHTML <- Js.string (Rmltop_alt_core.debug_status ())
  ) in
  let rec make_code_clickable () =
    List.iter (fun code ->
      if Js.to_bool (code##classList##contains (Js.string "code")) then begin
        let html =  code##innerHTML in
        let txt = text_of_html (Js.to_string html) in
        code##title <- Js.string "Click here to execute this code";
        code##onclick <- Html.handler (fun _ ->
          rmlconsole##value <- Js.string txt;
          execute ();
          Js._true)
      end
      )
      (Dom.list_of_nodeList (doc##getElementsByTagName(Js.string "pre")))
  and execute () =
    let s = Js.to_string rmlconsole##value in
    if s <> "" then
      begin
        history := Js.string s :: !history;
      end;
    history_bckwrd := !history;
    history_frwrd := [];
    rmlconsole##value <- Js.string "";
    (try loop s ppf with _ -> ());
    make_code_clickable ();
    debug_button##innerHTML <- Js.string (Rmltop_alt_core.debug_status ());
    rmlconsole##focus();
    container##scrollTop <- container##scrollHeight;
  in

  (* Start drag and drop part *)
  let ev = DragnDrop.init () in
  (* Customize dropable part *)
  ev.DragnDrop.ondrop <- (fun e  ->
     container##className <- Js.string "";
    let file =
      match Js.Opt.to_option (e##dataTransfer##files##item(0)) with
        | None -> assert false
        | Some file -> file in
    let reader = jsnew File.fileReader () in
    reader##onload <- Dom.handler
      (fun _ ->
        Firebug.console##log (Js.string "starting eval phase.");
        let s =
          match Js.Opt.to_option (File.CoerceTo.string (reader##result)) with
            | None -> assert false
            | Some str -> str
        in
        let ss = Rmltop_alt_core.split (Js.to_string s) in
        List.iter begin fun s ->
            let s = Rmltop_alt_core.add_terminator s in
            rmlconsole##value <- Js.string s;
            execute ();
            rmlconsole##value <- Js.string "";
          end
          ss;

        Js._false);
    reader##onerror <- Dom.handler
      (fun _ ->
        let error_code = reader##error##code in
        let error_msg =
          Printf.sprintf "Drag and drop failed [error code %d]."
            error_code
        in
        Firebug.console##log (Js.string error_msg);
        (* Firebug.console##log e; *)
        window##alert (Js.string error_msg);
        Js._true);
    reader##readAsText ((file :> (File.blob Js.t)));
    Js._false);
  DragnDrop.make ~events:ev container;
  (* End of Drag and drop part *)

  (* Load and execute a file *)
  let execute_file path =
    XmlHttpRequest.get path >|=
    (fun frame ->
      let content = frame.XmlHttpRequest.content in
      let phrases = Rmltop_alt_core.split content in
      List.iter (fun s ->
        let s = Rmltop_alt_core.add_terminator s in
        rmlconsole##value <- Js.string s;
        execute ();
        rmlconsole##value <- Js.string "")
        phrases)
  in
  let make_execute_button () =
    List.iter
      (fun code_file ->
        if Js.to_bool (code_file##classList##contains (Js.string "code-file"))
        then begin
          let html = code_file##innerHTML in
          code_file##innerHTML <- (Js.string "");
          let path = text_of_html (Js.to_string html) in
          let button =
            text_button ("Try "^(Filename.basename path))
              (fun b -> Lwt.ignore_result (execute_file path))
          in
          button##className <- Js.string "btn btn-primary";
          Dom.appendChild code_file button
        end)
      (Dom.list_of_nodeList (doc##getElementsByTagName (Js.string "a")))
  in

  let tbox_init_size = rmlconsole##style##height in
  Html.document##onkeydown <-
    (Html.handler
       (fun e -> match e##keyCode with
         | 13 -> (* ENTER key *)
           let keyEv = match Js.Opt.to_option (Html.CoerceTo.keyboardEvent e) with
             | None   -> assert false
             | Some t -> t in
           (* Special handling of ctrl key *)
           if keyEv##ctrlKey = Js._true then
             rmlconsole##value <- Js.string ((Js.to_string rmlconsole##value) ^ "\n");
           if keyEv##ctrlKey = Js._true || keyEv##shiftKey = Js._true then
             let rows_height = rmlconsole##scrollHeight / (rmlconsole##rows + 1) in
             let h = string_of_int (rows_height * (rmlconsole##rows + 1) + 20) ^ "px" in
             rmlconsole##style##height <- Js.string h;
             Js._true
           else begin
             execute ();
             rmlconsole##style##height <- tbox_init_size;
             rmlconsole##value <- Js.string "";
             Js._false
           end
	 | 38 -> (* UP ARROW key *) begin
	   match !history_bckwrd with
	     | s :: l ->
	       let str = Js.to_string rmlconsole##value in
	       history_frwrd := Js.string str :: !history_frwrd;
	       rmlconsole##value <- s;
	       history_bckwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | 40 -> (* DOWN ARROW key *) begin
	   match !history_frwrd with
	     | s :: l ->
	       let str = Js.to_string rmlconsole##value in
	       history_bckwrd := Js.string str :: !history_bckwrd;
	       rmlconsole##value <- s;
	       history_frwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | _ -> Js._true));
  let tutorial_div = get_element_by_id "tutorial" in
  let tutorial_action load_page lessons _ =
    load_page lessons >|=
    (fun content ->
      tutorial_div##innerHTML <- Js.string content;
        make_code_clickable ();
        make_execute_button ();
        Lwt.return ()
    ) in
  let make_toc
      previous_lesson
      previous_step
      load_next_lesson
      lessons
      next_lesson
      next_step =
    match Js.Opt.to_option doc##getElementById (Js.string "lessons-toc") with
    | None -> assert false
    | Some elt ->
        let make_item i (path, title, steps, descr) =
          let li = doc##createElement (Js.string "li") in
          let a = doc##createElement (Js.string "a") in
          Dom.appendChild li a;
          let text = Format.sprintf "Lesson %d: %s" (i+1) title in
          a##innerHTML <- Js.string text;
          a##onclick <-
            (Dom_html.handler (fun _ ->
              cur_lesson := i;
              Lwt.ignore_result
                (tutorial_action load_next_lesson lessons next_lesson >>=
                 (fun _ ->
                   let intro = get_element_by_id "intro"
                   and sidebar = get_element_by_id "sidebar" in
                   Lwt.return (Dom.removeChild sidebar intro)
                 )
                );
              append_children "navbar" [
                previous_lesson;
                previous_step;
                next_step;
                next_lesson
              ];
              Js._true));
          let p = doc##createElement (Js.string "p") in
          p##innerHTML <- Js.string descr;
          Dom.appendChild li p;
          li
        in
        let items =
          Array.mapi make_item lessons
        in
        let make_list items =
          doc##createElement (Js.string "ul") <| items
        in
        let list = make_list (Array.to_list items) in
        Dom.appendChild elt list

        (* elt##innerHTML <- (Js.string "coucou") *)
  in
  let tutorial_toc previous_lesson previous_step lessons next_lesson next_step =
    Lwt.ignore_result
      (load_tutorial_toc lessons >|=
        (fun content ->
          tutorial_div##innerHTML <- Js.string content;
          make_toc
            previous_lesson
            previous_step
            load_next_lesson
            lessons
            next_lesson
            next_step;
          Lwt.return ()
        ) >>=
          (fun _ ->
            let intro = get_element_by_id "intro"
            and sidebar = get_element_by_id "sidebar" in
            Lwt.return (Dom.removeChild sidebar intro)
      ))
  in
  let start_tutorial_button = text_button "Start tutorial" (fun b ->
    get_lessons () >|= fun lessons -> (
      b##disabled <- Js._true;
      let previous_lesson = text_button "<<"
        (tutorial_action load_previous_lesson lessons)
      in
      let next_lesson = text_button ">>"
        (tutorial_action load_next_lesson lessons)
      in
      let previous_step = text_button "<"
        (tutorial_action load_previous_step lessons)
      in
      let next_step = text_button ">"
        (tutorial_action load_next_step lessons)
      in

      previous_step##title <- Js.string "Go back to previous step";
      previous_lesson##title <- Js.string "Go back to previous lesson";
      next_step##title <- Js.string "Go to next step";
      next_lesson##title <- Js.string "Go to next lesson";

      tutorial_toc previous_lesson previous_step lessons next_lesson next_step

    );
    Js._true
  ) in
  start_tutorial_button##className <- Js.string "btn btn-large btn-primary";

  let step_button = text_button "Step" (fun b ->
    Rmltop_alt_global.set_step 1
  ) in
  let suspend_button = text_button "Suspend" (fun b ->
    Rmltop_alt_global.set_suspend ()
  ) in
  let resume_button = text_button "Resume" (fun b ->
    Rmltop_alt_global.set_resume ()
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
  let reset_button = text_button "Reset" (fun _ ->
    window##location##reload ()
  )
  in

  make_code_clickable ();
  make_execute_button ();

  append_children "buttons-left" [
    suspend_button;
    step_button;
    resume_button;
  ];
  append_children "buttons-right" [
  (*   debug_button; *)
  (*   (\* send_button; *\) *)
  (*   (\* save_button; *\) *)
    reset_button;
  ];

  append_children "tutorial" [
    start_tutorial_button;
  ];


  output_area##scrollTop <- output_area##scrollHeight;
  start ppf;

  Rmltop_alt_global.enter_step_by_step_mode :=
    (fun () ->
      step_button##disabled <- Js._false;
      resume_button##disabled <- Js._false;
    );

  Rmltop_alt_global.exit_step_by_step_mode :=
    (fun () ->
      step_button##disabled <- Js._true;
      resume_button##disabled <- Js._true;
    );

  !Rmltop_alt_global.exit_step_by_step_mode ();

  let rec exec_machine_controller () =
    Lwt_js.sleep !Rmltop_alt_global.sampling >>= fun () ->
      let _ = Rmltop_alt_core.controller_react () in
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
