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

(* Syntaxe :
   run_test:
     [ Good: "compiler1" 
       Bad n: "compiler2"  
       Bad n "regexp": "compiler3" 
       Warning: "compiler4"
       Warning "regexp": "compiler5"  ]
*)

{

let debug msg =
  Format.eprintf "%s@." msg

open Run_test_misc

exception Error of string

type ident =
  | Ident of string
  | Id_run_test
  | Id_good
  | Id_bad
  | Id_warning

let run_test_lbl = ref "run_test"

let tbl = Hashtbl.create 7 
let () =
  List.iter (fun (s,id) -> Hashtbl.add tbl s id)
    [ !run_test_lbl, Id_run_test;
      "good", Id_good;
      "bad", Id_bad;
      "warning", Id_warning ]

let ident_of_string =
  (fun s ->
    try Hashtbl.find tbl (String.lowercase s) 
    with Not_found -> Ident s)
    


}

let space = [' ''\t''\n']
let not_space = [^' ''\t']
let ident = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9''-''\'''$''#''@''!']*
let int = ['0'-'9']+

rule main acc = parse
    (ident as id) space* ':' space* '['
      { match ident_of_string id with
        | Id_run_test -> 
            main (get_tests acc lexbuf) lexbuf
        | _ -> main acc lexbuf }
  | eof
      { List.rev acc }
  | _ 
      { main acc lexbuf }

and get_tests acc = parse
    space
      { get_tests acc lexbuf }
  | ']' 
      { acc }
  | (ident as id) 
      { let req = 
          match ident_of_string id with
          | Id_good -> good lexbuf
          | Id_bad -> bad lexbuf
          | Id_warning -> warning lexbuf
          | _ -> 
              raise (Error ("unexpected \'"^id^"\': test description expected"))
        in
        let compiler = get_compiler lexbuf in
        get_tests ((req, compiler) :: acc) lexbuf
      }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf "unexpected \'%c\': unterminated list of tests" c
        in
        raise (Error msg) }

and good = parse 
    space
      { good lexbuf }
  | ':'
      { Good }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf "unexpected \'%c\': \':\' expected after \'good\'" c
        in
        raise (Error msg) }

and bad = parse
    space* 
      { bad lexbuf }
  | ':'
      { Bad (None, "") }
  | (int as code) space* ':'
      { Bad (Some (int_of_string code), "") }
  | (int as code) space* "\""
      { let msg = 
          let buf = Buffer.create 512 in
          string buf lexbuf;
          Buffer.contents buf
        in
        column lexbuf;
        Bad (Some (int_of_string code), msg) }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf "unexpected \'%c\': \':\' or \'code:\' or \'code \"regexp\":\' expected after \'bad\'" c
        in
        raise (Error msg) }

and warning = parse
    space
      { warning lexbuf }
  | ':'
      { Warning "" }
  | "\""
      { let msg = 
          let buf = Buffer.create 512 in
          string buf lexbuf;
          Buffer.contents buf
        in
        column lexbuf;
        Warning msg }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf 
            "unexpected \'%c\': \':\' or \'code:\' or \'code \"regexp\":\' expected after \'warning\'" 
            c
        in
        raise (Error msg) }

and get_compiler = parse 
    space* "\""
      { let compiler = 
          let buf = Buffer.create 512 in
          string buf lexbuf;
          Buffer.contents buf
        in
        compiler }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf "unexpected \'%c\': \'\"compiler\"\' expected" c
        in
        raise (Error msg) }


and string buf = parse
  | "\"" { () }
  | '\\' 'n' 
      { Buffer.add_string buf "\\n";
        string buf lexbuf }
  | '\\' '\\' 
      { Buffer.add_string buf "\\\\";
        string buf lexbuf }
  | '\\' '"' 
      { Buffer.add_string buf "\\\"";
        string buf lexbuf }
  | [^ '\\' '"' '\n']+ 
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        string buf lexbuf }
  | '\\' 
      { raise (Error "illegal escape character in string") }
  | '\n' | eof
      { raise (Error "unterminated string") }
  | _ 
      { raise (Error ("illegal character: " ^ Lexing.lexeme lexbuf)) }

and column = parse
    space
      { column lexbuf }
  | ':'
      { () }
  | eof
      { raise (Error "unexpected end of file")}
  | (_ as c)
      { let msg = 
          Format.sprintf "unexpected \'%c\': \':\' expected" c
        in
        raise (Error msg) }

{

}
