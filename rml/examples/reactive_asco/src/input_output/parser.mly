/***************************************************************/
/*                        Reactive Asco                        */
/*             http://reactiveml.org/reactive_asco             */
/*                                                             */
/*                                                             */
/*  Authors: Guillaume Baudart (guillaume.baudart@ens.fr)      */
/*           Louis Mandel (louis.mandel@lri.fr)                */
/*                                                             */
/***************************************************************/

%{
open Types
let event_counter = ref 0
%}

%token <float> Token_FLOAT
%token <string> Token_STRING
%token <int> Token_INT

%token Token_ACTION

%token Token_EVENT
%token Token_NOTE
%token Token_CHORD
%token Token_TRILL


%token Token_GROUP
%token Token_UNTIL


%token Token_RPAR Token_LPAR
%token Token_RACC Token_LACC
%token Token_RBRA Token_LBRA
%token Token_GLOBAL Token_LOCAL Token_CAUSAL Token_PARTIAL
%token Token_LOOSE Token_TIGHT

%token Token_QUOTE



%token Token_EOS

%start make_score             /* Entry point */
%start make_simu

%type  <Types.score * Types.instr_score> make_score
%type <Types.simulation> make_simu
%%

make_score:
  | score Token_EOS
      { let es, is = $1 in
        let nis = Utils.delay_to_date is in
        (es, nis) }

score:
  | score_event score
      { let (elec_score,instr_score) = $2 in
        let (se, ie) = $1 in
	match se.seq with
	| [] -> (elec_score, ie::instr_score)
	| _ -> (se::elec_score, ie::instr_score) }
  | score_event
      { let (se, ie) = $1 in
        match se.seq with
        | [] -> ([], [ie])
        | _ -> ([se], [ie]) }

score_event:
  | instr_event asco_event_list
      { incr event_counter;
	({ event = !event_counter; seq = $2; }, $1) }

instr_event:
  | Token_NOTE label delay               { $3 }
  | Token_CHORD notes delay              { $3 }
  | Token_TRILL notes delay              { $3 }

notes:
  | Token_LPAR label_list Token_RPAR     {}

label_list:
  | label label_list                     { $1::$2 }
  | /* empty*/                           { [] }

asco_event_list:
  | delay asco_event asco_event_list     { ($1, $2)::$3 }
  | asco_event asco_event_list           { (0.0, $1)::$2 }
  | /* empty*/                           { [] }

asco_event:
  | action                               { $1 }
  | group                                { $1 }
  | asco_until                           { $1 }

action:
  | Token_QUOTE strings Token_QUOTE
      { Action(Message $2) }

strings:
  | string strings
      { $1^" "^$2 }
  | /* empty */
      { "" }

group:
  | Token_GROUP sync err Token_LACC asco_event_list Token_RACC
      { Group({group_synchro= $2;
	       group_error = $3;
	       group_seq = $5;}) }

asco_until:
  | Token_UNTIL label Token_LACC asco_event_list Token_RACC
      { Until({until_seq= $4;
	       until_event= $2;}) }


sync:
  | Token_LOOSE                          { Loose }
  | Token_TIGHT                          { Tight }

err:
  | Token_GLOBAL                         { Global }
  | Token_LOCAL                          { Local }
  | Token_CAUSAL                         { Causal }
  | Token_PARTIAL                        { Partial }

make_simu:
  | simulation Token_EOS                { $1 }

simulation:
  | event simulation                     { $1::$2 }
  | /* empty*/                           { [] }

event:
  | Token_EVENT label delay tempo        { ($2, $3, $4) }

string:
  | Token_STRING                         { $1 }
  | Token_INT                            { string_of_int $1 }
  | Token_FLOAT                          { string_of_float $1 }

delay: Token_FLOAT                       { $1 }
label: Token_INT                         { $1 }
tempo: Token_FLOAT                       { $1 }
