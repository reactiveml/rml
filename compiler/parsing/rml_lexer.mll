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

(* file: lexer.mll *)

(* Warning: *)
(* This file is based on the original version of lexer.mll *)
(* from the Objective Caml 3.07 distribution, INRIA        *)

(* first modification: 2004-05-05 *)
(* modified by: Louis Mandel      *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The lexer definition *)

{
open Lexing
open Rml_misc
open Rml_parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
  | Keyword_as_label of string
;;

exception Error of error * Location.t;;

(* The table of keywords *)

let keyword_table = ((Hashtbl.create 149) : (string, token) Hashtbl.t);;

List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok)
  [ "and", AND;
    "as", AS;
    "assert", ASSERT;
    "await", AWAIT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "control", CONTROL;
    "default", DEFAULT;
    "do", DO;
    "done", DONE;
    "dopar", DOPAR;
    "downto", DOWNTO;
    "else", ELSE;
    "emit", EMIT;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "gather", GATHER;
    "halt", HALT;
    "if", IF;
    "immediate", IMMEDIATE;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
(*    "inout", INOUT;*)
    "last", LAST;
    "lazy", LAZY;
    "let", LET;
    "loop", LOOP;
    "match", MATCH;
    "memory", MEMORY;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nothing", NOTHING;
    "object", OBJECT;
    "of", OF;
    "one", ONE;
    "open", OPEN;
    "or", OR;
(*    "out", OUT;*)
(*  "parser", PARSER; *)
    "pause", PAUSE;
    "pre", PRE;
    "present", PRESENT;
    "private", PRIVATE;
    "proc", PROC;
    "process", PROCESS;
    "rec", REC;
    "run", RUN;
    "sig", SIG;
    "signal", SIGNAL;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "until", UNTIL;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
  ]

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
      string_buff := new_buff
  end;
  Bytes.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  Bytes.to_string s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;

(* To translate escape sequences *)

let char_for_backslash =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | "MacOS" ->
      begin function
      | 'n' -> '\013'
      | 'r' -> '\010'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | x -> fatal_error "Lexer: unknown system type"

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) && not (in_comment ())
  then raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                    Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
;;

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "_"
      { UNDERSCORE }
  | "~"  { TILDE }
  | "~" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, Location.curr lexbuf));
        LABEL name }
  | "?"  { QUESTION }
  | "??" { QUESTIONQUESTION }
  | "?" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, Location.curr lexbuf));
        OPTLABEL name }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            LIDENT s }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | int_literal
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | float_literal
      { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | int_literal "l"
      { let s = Lexing.lexeme lexbuf in
        INT32 (Int32.of_string(String.sub s 0 (String.length s - 1))) }
  | int_literal "L"
      { let s = Lexing.lexeme lexbuf in
        INT64 (Int64.of_string(String.sub s 0 (String.length s - 1))) }
  | int_literal "n"
      { let s = Lexing.lexeme lexbuf in
        NATIVEINT
          (Nativeint.of_string(String.sub s 0 (String.length s - 1))) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r'] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }
  | "(*"
      { comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        token lexbuf }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc (Warnings.Comment "the start of a comment");
        comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        token lexbuf
      }
  | "*)"
      { let loc = Location.curr lexbuf in
        let warn = Warnings.Comment "not the end of a comment" in
        Location.prerr_warning loc warn;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "\\/"{ BACKSLASHSLASH }
  | "/\\"{ SLASHBACKSLASH }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | "|>" { BARGRATER }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }

  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  | eof     { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      }

and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [x] -> comment_start_loc := [];
        | _ :: l -> comment_start_loc := l;
                    comment lexbuf;
       }
  | "\""
      { reset_string_buffer();
        string_start_loc := Location.curr lexbuf;
        begin try string lexbuf
        with Error (Unterminated_string, _) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ -> comment_start_loc := [];
                        raise (Error (Unterminated_string_in_comment, loc))
        end;
        reset_string_buffer ();
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        comment lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ -> comment_start_loc := [];
                      raise (Error (Unterminated_comment, loc))
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        comment lexbuf
      }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char(char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' _
      { if in_comment ()
        then string lexbuf
        else begin
(*  Should be an error, but we are very lax.
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
*)
          let loc = Location.curr lexbuf in
          let warn = Warnings.Other "Illegal backslash escape in string" in
          Location.prerr_warning loc warn;
          store_string_char (Lexing.lexeme_char lexbuf 0);
          store_string_char (Lexing.lexeme_char lexbuf 1);
          string lexbuf
        end
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        let s = Lexing.lexeme lexbuf in
        for i = 0 to String.length s - 1 do
          store_string_char s.[i];
        done;
        string lexbuf
      }
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }
