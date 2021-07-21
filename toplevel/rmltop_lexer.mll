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

(* file: rmltop_directives.ml *)
(* author: Louis Mandel *)
(* created: 2005-10-25  *)

{
 open Lexing

 exception EOF
 exception Syntax_error

 type directive =
   | Rml_phrase of string
   | OCaml_phrase of string
   | Suspend
   | Resume
   | Step of int option
   | Sampling of float
   | Run of string
   | Exec of string
   | Quit

 let expr_buffer = Buffer.create 512
}


let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let sep =  blank | newline

let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

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


rule phrase = parse
  | sep* '#'         { directive lexbuf }
  | sep*             { Rml_phrase (expr lexbuf) }
  | eof              { raise EOF }

and directive = parse
  | "suspend"        { end_of_phrase lexbuf; Suspend }
  | "resume"         { end_of_phrase lexbuf; Resume }
  | "step" sep* ";;" { Step None }
  | "step" sep*      { Step (Some (int_expr lexbuf)) }
  | "sampling" sep*  { Sampling (float_expr lexbuf) }
  | "run"            { Run (expr lexbuf) }
  | "exec"           { Exec (expr lexbuf) }
  | "quit"           { end_of_phrase lexbuf; Quit }
  | sep+             { OCaml_phrase (expr lexbuf) }
  | eof              { raise EOF }
  | _                { error lexbuf; assert false }

and expr = parse
  | ";;"             { let s = Buffer.contents expr_buffer in
                       Buffer.reset expr_buffer;
                       s }
  | eof              { raise EOF }
  | _                { Buffer.add_string expr_buffer (lexeme lexbuf);
	               expr lexbuf }

and float_expr = parse
  | float_literal    { let x = float_of_string (Lexing.lexeme lexbuf) in
                       end_of_phrase lexbuf;
                       x }
  | eof              { raise EOF }
  | ";;"             { raise Syntax_error }
  | _                { error lexbuf; assert false }

and int_expr = parse
  | int_literal      { let x = int_of_string (Lexing.lexeme lexbuf) in
                        end_of_phrase lexbuf;
                        x }
  | eof              { raise EOF }
  | _                { error lexbuf; assert false }

and end_of_phrase = parse
  | sep*  ";;"       { () }
  | eof              { raise EOF }
  | _                { error lexbuf; assert false }

and error = parse
  | ";;"     { raise Syntax_error }
  | _        { error lexbuf }
