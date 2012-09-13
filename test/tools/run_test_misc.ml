(***************************************************************************)
(*                                Lucy-n                                   *)
(*                                                                         *)
(*                                                                         *)
(* Authors: Louis Mandel (louis.mandel@lri.fr)                             *)
(*          Florence Plateau (florence.plateau@lri.fr)                     *)
(*                                                                         *)
(* Creation date: September 2011                                           *)
(*                                                                         *)
(***************************************************************************)


type kind =
  | Good
  | Bad of int option * regexp
  | Warning of regexp

and regexp = string

type test =
    { kind: kind;
      compiler: string;
      file: string;
      cmd: string;
      out: string;
      err: string;
      status: int; }

type ok =
  | OK of ok_diagnostic list
  | KO of ko_diagnostic list
and ok_diagnostic =
  | Bad_retrun_code
  | Bad_message
  | Warning_instead_of_succes
  | Failure_instead_of_warning
and ko_diagnostic =
  | Success_instead_of_failure
  | Warning_instead_of_failure
  | Failure_instead_of_success
  | Success_instead_of_warning

(* Utility functions *)
let is_ok ok =
  match ok with
  | OK _ -> true
  | _ -> false

let is_ko ok =
  match ok with
  | KO _ -> true
  | _ -> false


(* Analysis *)

let is_warning test =
  if test.status = 0 then
    let ch = open_in test.err in
    let b =
      try let _ = input_char ch in true
      with End_of_file -> false
    in
    close_in ch;
    b
  else
    false

let check_msg msg test =
  let is_matching reg line =
    try ignore (Str.search_forward reg line 0); true
    with Not_found -> false
  in
  let reg = Str.regexp msg in
  let ch = open_in test.err in
  let b =
    try
      while not (is_matching reg (input_line ch)) do () done;
      true
    with End_of_file -> false
  in
  close_in ch;
  b

let ok_of_test test =
  match test.kind, test.status with
  | Good, 0 ->
      if is_warning test then
        OK [ Warning_instead_of_succes ]
      else
        OK []
  | Good, n ->
      KO [ Failure_instead_of_success ]
  | Bad _, 0 ->
      if is_warning test then
        KO [ Warning_instead_of_failure ]
      else
        KO [ Success_instead_of_failure ]
  | Bad (code, msg) , n ->
      let diag =
        let diag = [] in
        let diag =
          match code with
          | Some n' when n <> n' -> Bad_retrun_code :: diag
          | _ -> diag
        in
        if check_msg msg test then diag else Bad_message :: diag
      in
      OK diag
  | Warning msg, 0 ->
      if is_warning test then
        if check_msg msg test then
          OK []
        else
          OK [ Bad_message ]
      else
        KO [ Success_instead_of_warning ]
  | Warning _, n ->
      OK [ Failure_instead_of_warning ]

let total log =
  List.fold_left
    (fun (nb_ok, nb_bof, nb_ko) test ->
      match ok_of_test test with
      | OK [] -> nb_ok + 1, nb_bof, nb_ko
      | OK _ -> nb_ok, nb_bof + 1, nb_ko
      | KO _ -> nb_ok, nb_bof, nb_ko + 1)
    (0, 0, 0)
    log


(* Output *)

let cat file =
  let c = open_in file in
  try
    while true do
      let l = input_line c in
      Format.printf "%s\n" l
    done
  with End_of_file ->
    Format.printf "@?";
    close_in c

let green s = "\027[32m"^s^"\027[0m"
let red s = "\027[31m"^s^"\027[0m"
let orange s = "\027[34m"^s^"\027[0m"

let string_of_ok_diagnostic diag =
  match diag with
  | Bad_retrun_code -> "bad return code"
  | Bad_message -> "bad error message"
  | Failure_instead_of_warning -> "failure instead of warning"
  | Warning_instead_of_succes -> "unexpected warning"

let string_of_ok_diagnostics diag_l =
  String.concat " and " (List.map string_of_ok_diagnostic diag_l)

let string_of_ok ok =
  match ok with
  | OK [] -> green "OK"
  | OK diag_l -> orange ("Bof (" ^ (string_of_ok_diagnostics diag_l)^")")
  | KO [ Success_instead_of_failure ] -> red "KO"
  | KO [ Warning_instead_of_failure ] -> red "KO (warning instead of failure)"
  | KO [ Failure_instead_of_success ] -> red "KO"
  | KO [ Success_instead_of_warning ] -> red "KO"
  | KO _ -> assert false

let short_string_of_kind k =
  match k with
  | Good -> "Good: "
  | Bad (_, _) -> "Bad: "
  | Warning _ -> "Warning: "

let string_of_kind k =
  match k with
  | Good -> "Good: "
  | Bad (None, msg) -> Format.sprintf "Bad \"%s\": " msg
  | Bad (Some n, msg) -> Format.sprintf "Bad %i \"%s\": " n msg
  | Warning msg -> Format.sprintf "Warning \"%s\": " msg

let report_test verbose test =
  let ok = ok_of_test test in
  if verbose >=3 then begin
    Format.printf "%s@\n" test.cmd
  end;
  Format.printf "%s (%s%s):\t%s@\n@?"
    test.file
    (if verbose >= 1 then
       string_of_kind test.kind
     else short_string_of_kind test.kind)
    test.compiler
    (string_of_ok ok);
  if (verbose >= 1 && is_ko ok) || verbose >= 2 then begin
    (* cat test.out; *)
    cat test.err
  end

let report_total msg (nb_ok, nb_bof, nb_ko) =
  let nb_test = nb_ok + nb_ko + nb_bof in
  Format.printf "%s%i/%i" msg (nb_ok + nb_bof) nb_test;
  if nb_bof = 0 then
    Format.printf "@\n"
  else
    Format.printf " with %i unexpected behaviors@\n" nb_bof


(* Compilation *)
let compile verbose kind compiler file exts =
  let ext =
    try List.find (Filename.check_suffix file) exts
    with Not_found -> assert false
  in
  let basename = Filename.chop_suffix (Filename.basename file) ext in
  let tempname = Filename.temp_file basename "" in
  let out = tempname ^ ".out" in
  let err = tempname ^ ".err" in
  let cmd =
    Format.sprintf "%s %s > %s 2> %s" compiler file out err
  in
  let status = Sys.command cmd in
  let res =
    { kind = kind;
      compiler = compiler;
      file = file;
      cmd = cmd;
      out = out;
      err = err;
      status = status }
  in
  report_test verbose res;
  res

