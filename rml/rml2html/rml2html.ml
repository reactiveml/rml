open Str

(* ************************* words ****************************** *)

type keyword_kind =
  | Directive
  | Definition
  | Control
  | Blocking
  | Builtin
  | Other

let keyword_table = ((Hashtbl.create 149) : (string, keyword_kind) Hashtbl.t);;

List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok) 
  [ "and", Definition;
    "as", Definition;
    "assert", Directive;
    "await", Definition;
    "begin", Blocking;
    "class", Definition;
    "constraint", Definition;
    "control", Control;
    "default", Definition;
    "do", Control;
    "done", Control;
    "downto", Control;
    "else", Control;
    "emit", Builtin;
    "end", Blocking;
    "exception", Definition;
    "external", Definition;
    "false", Other;
    "for", Control;
    "fun", Definition;
    "function", Definition;
    "functor", Definition;
    "gather", Definition;
    "if", Control;
    "immediate", Definition;
    "in", Definition;
    "include", Directive;
    "inherit", Definition;
    "initializer", Definition;
    "inout", Other;
    "lazy", Control;
    "let", Definition;
    "loop", Blocking;
    "match", Control;
    "method", Definition;
    "module", Definition;
    "mutable", Definition;
    "new", Control;
    "nothing", Builtin;
    "object", Blocking;
    "of", Definition;
    "one", Definition;
    "open", Directive;
    "or", Control;
    "out", Other;
(*  "parser", PARSER; *)
    "pause", Builtin;
    "pre", Builtin;
    "present", Control;
    "private", Definition;
    "process", Definition;
    "rec", Definition;
    "run", Builtin;
    "sig", Blocking;
    "signal", Definition;
    "struct", Blocking;
    "then", Control;
    "to", Control;
    "true", Other;
    "try", Control;
    "type", Definition;
    "until", Control;
    "val", Definition;
    "virtual", Definition;
    "when", Control;
    "while", Control;
    "with", Control;
    "mod", Other;
    "land", Other;
    "lor", Other;
    "lxor", Other;
    "lsl", Other;
    "lsr", Other;
    "asr", Other;
  ]

let treat_keyword kind word =
  match kind with
  | Directive -> "<font color=\"cc9900\">" ^ word ^ "</font>"
  | Definition -> "<font color=\"green\">" ^ word ^ "</font>"
  | Control -> "<font color=\"77aaaa\">" ^ word ^ "</font>"
  | Blocking -> "<font color=\"990099\">" ^ word ^ "</font>"
  | Builtin -> "<font color=\"magenta\">" ^ word ^ "</font>"
  | Other -> word

let treat_word word = 
  match String.get word 0 with
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' 
  | 'K' | 'L' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' 
  | 'V' | 'W' | 'X' | 'Y' | 'Z' 
    -> "<font color=\"0033cc\">" ^ word ^ "</font>"
  | _ ->
      try 
	let kind = Hashtbl.find keyword_table word in
	treat_keyword kind word
      with
      | Not_found ->
	  (* !!!!!!!!!!!!!!!!!!!! A FAIRE !!!!!!!!!!!!!!!!!!!!!!! *)
	  (* commentaires *)
	  word
  

(* ************************* separators ****************************** *)

let treat_separator sep =
  match sep with
  | "->" -> "<font color=\"77aaaa\">-&gt;</font>"
  | "|" ->  "<font color=\"77aaaa\">|</font>"


let treat_line =
  let f elem = 
    match elem with
    | Text word -> Text (treat_word word)
    | Delim sep -> Delim (treat_separator sep)
  in
  List.map f 
