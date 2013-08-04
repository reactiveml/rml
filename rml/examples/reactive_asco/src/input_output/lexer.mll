(***************************************************************)
(*                        Reactive Asco                        *)
(*             http://reactiveml.org/reactive_asco             *)
(*                                                             *)
(*                                                             *)
(*  Authors: Guillaume Baudart (guillaume.baudart@ens.fr)      *)
(*           Louis Mandel (louis.mandel@lri.fr)                *)
(*                                                             *)
(***************************************************************)

{
open Parser
open Lexing

exception Lexing_error of string

 let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos_fname = pos.pos_fname;
	pos_lnum = pos.pos_lnum + 1;
	pos_bol = pos.pos_cnum;
	pos_cnum = pos.pos_cnum;}
}


let BLANK = [' ' '\t']

let LINE = [^ '\n']*
let COMMENT = ';' LINE

let NUM = ['0'-'9']
let ALPHA =  ['a'-'z' 'A'-'Z' '_' ]
let WORD = ALPHA (ALPHA | NUM)*
let NUMBER = '-'? ['0'-'9']+

let ANY = _

let ACTION = "ACTION"
let EVENT = "EVENT"
let NOTE = "NOTE"
let CHORD = "CHORD"
let TRILL = "TRILL"


let GROUP = "GROUP"
let UNTIL = "UNTIL"
let TIGHT = "@tight"
let LOOSE = "@loose"
let GLOBAL = "@global"
let LOCAL = "@local"
let CAUSAL = "@causal"
let PARTIAL = "@partial"


let LPAR = "("
let RPAR = ")"
let LACC = "{"
let RACC = "}"
let LBRA = "["
let RBRA = "]"
let QUOTE = "'"



rule make_token = parse
  | '\n'                {newline lexbuf; make_token lexbuf }
  | BLANK+              {make_token lexbuf}     (* Skip blanks and comments*)
  | COMMENT             {make_token lexbuf}

  | eof                 {Token_EOS}     (* Give up on end of score *)

  | UNTIL               {Token_UNTIL}
  | GROUP               {Token_GROUP}
  | LPAR                {Token_LPAR}
  | RPAR                {Token_RPAR}
  | LACC                {Token_LACC}
  | RACC                {Token_RACC}
  | LBRA                {Token_LBRA}
  | RBRA                {Token_RBRA}
  | QUOTE               {Token_QUOTE}

  | LOOSE               {Token_LOOSE}
  | TIGHT               {Token_TIGHT}
  | GLOBAL              {Token_GLOBAL}
  | LOCAL               {Token_LOCAL}
  | CAUSAL              {Token_CAUSAL}
  | PARTIAL             {Token_PARTIAL}


  | ACTION              {Token_ACTION}
  | EVENT               {Token_EVENT}
  | NOTE                {Token_NOTE}
  | CHORD               {Token_CHORD}
  | TRILL               {Token_TRILL}

  | ("." NUM+
  | NUM+ "." NUM*) as num
		{ Token_FLOAT (float_of_string num) }

  | NUM+
    {
      let s = (Lexing.lexeme lexbuf)
      in Token_INT(int_of_string(s))
    }

  | WORD
      {
        let s = (Lexing.lexeme lexbuf)
        in Token_STRING(s)
      }

  | ANY { raise (Lexing_error (lexeme lexbuf)) }


{

let build_prog_from_string s =
  make_score make_token (Lexing.from_string s)
;;

}
