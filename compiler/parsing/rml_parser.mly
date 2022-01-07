/**********************************************************************/
/*                                                                    */
/*                           ReactiveML                               */
/*                    http://reactiveML.org                           */
/*                    http://rml.inria.fr                             */
/*                                                                    */
/*                          Louis Mandel                              */
/*                                                                    */
/*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          */
/*  This file is distributed under the terms of the Q Public License  */
/*  version 1.0.                                                      */
/*                                                                    */
/*  ReactiveML has been done in the following labs:                   */
/*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    */
/*  - Verimag, CNRS Grenoble (2005-2006)                              */
/*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  */
/*                                                                    */
/**********************************************************************/

/* file: parser.mly */

/* Warning: */
/* This file is based on the original version of parser.mly  */
/* from the Objective Caml 3.07 distribution, INRIA          */

/* first modification: 2004-05-05 */
/* modified by: Louis Mandel */

/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The parser definition */

%{
open Location
open Rml_asttypes
open Parse_ident
open Parse_ast

let mkident id (ps, pe) =
  { pident_id = id;
    pident_loc = loc_of_pos (ps, pe) }
    
let mkident_loc id loc =
  { pident_id = id;
    pident_loc = loc; }

let mksimple id pos =
  { psimple_id = id;
    psimple_loc = loc_of_pos pos; }
let _mksimple_loc id loc =
  { psimple_id = id;
    psimple_loc = loc; }

let mkte d loc =
  { pte_desc = d; pte_loc = loc_of_pos loc }
let mkpatt d loc =
  { ppatt_desc = d; ppatt_loc = loc_of_pos loc }
let mkexpr d loc =
  { pexpr_desc = d;
    pexpr_loc = loc_of_pos loc; }
let mkconf d loc =
  { pconf_desc = d;
    pconf_loc = loc_of_pos loc; }
let mkimpl d loc =
  { pimpl_desc = d; pimpl_loc = loc_of_pos loc }
let mkintf d loc =
  { pintf_desc = d; pintf_loc = loc_of_pos loc }

let mkexpr_until body cfg_when_opt_expr_opt_list loc =
  match cfg_when_opt_expr_opt_list with
  | [] -> raise Parsing.Parse_error
  | _ :: _ ->
      mkexpr (Pexpr_until (body, cfg_when_opt_expr_opt_list)) loc

let reloc_patt x loc = { x with ppatt_loc = loc_of_pos loc };;
let reloc_expr x loc = { x with pexpr_loc = loc_of_pos loc };;

let mkoperator name pos =
  { pexpr_desc = Pexpr_ident (mkident (Pident name) pos);
    pexpr_loc = loc_of_pos pos; }


(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitely in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -stypes option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexpr d loc = { pexpr_desc = d;
		 pexpr_loc = gloc_of_pos loc; };;
let ghpatt d loc = { ppatt_desc = d; ppatt_loc = gloc_of_pos loc };;
let _ghte d loc = { pte_desc = d; pte_loc = gloc_of_pos loc };;
let ghimpl d loc = { pimpl_desc = d; pimpl_loc = loc };;

let ghexpr_unit loc = ghexpr (Pexpr_constant(Const_unit)) loc

let mkassert e =
  mkexpr (Pexpr_assert (e))
;;

let mkinfix arg1 name arg2 pos =
  mkexpr(Pexpr_apply(mkoperator name pos, [arg1; arg2])) pos

let mkuminus name arg pos1 =
  match name, arg.pexpr_desc with
  | "-", Pexpr_constant(Const_int n) ->
      mkexpr(Pexpr_constant(Const_int(-n)))
  | _, Pexpr_constant(Const_float f) ->
      mkexpr(Pexpr_constant(Const_float(-. f)))
  | _ ->
      mkexpr(Pexpr_apply(mkoperator ("~" ^ name) pos1, [arg]))

let rec mktailexpr loc = function
    [] ->
      ghexpr(Pexpr_construct( mkident_loc (Pident "[]") none, None)) loc
  | e1 :: el ->
      let exp_el = mktailexpr loc el in
      let l = {loc_start = e1.pexpr_loc.loc_start;
               loc_end = exp_el.pexpr_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {pexpr_desc = Pexpr_tuple [e1; exp_el];
		 pexpr_loc = l;}
      in
      {pexpr_desc = Pexpr_construct(mkident_loc (Pident "::") l,
				    Some arg);
       pexpr_loc = l;}

let rec mktailpatt loc = function
    [] ->
      ghpatt(Ppatt_construct(mkident_loc (Pident "[]") none, None)) loc
  | p1 :: pl ->
      let pat_pl = mktailpatt loc pl in
      let l = {loc_start = p1.ppatt_loc.loc_start;
               loc_end = pat_pl.ppatt_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {ppatt_desc = Ppatt_tuple [p1; pat_pl]; ppatt_loc = l} in
      {ppatt_desc = Ppatt_construct(mkident_loc (Pident "::") l,
				    Some arg);
       ppatt_loc = l}

let array_function str name =
  mkident_loc (Pdot(str, name)) none

let rec deep_mkrangepatt c1 c2 loc =
  if c1 = c2 then ghpatt(Ppatt_constant(Const_char c1)) loc else
  ghpatt(Ppatt_or(ghpatt(Ppatt_constant(Const_char c1)) loc,
                  deep_mkrangepatt (Char.chr(Char.code c1 + 1)) c2 loc)) loc

let rec mkrangepatt c1 c2 loc =
  if c1 > c2 then mkrangepatt c2 c1 loc else
  if c1 = c2 then mkpatt (Ppatt_constant(Const_char c1)) loc else
  reloc_patt (deep_mkrangepatt c1 c2 loc) loc

let syntax_error () =
  raise Rml_syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Rml_syntaxerr.Error(Rml_syntaxerr.Unclosed(loc_of_pos opening_loc, opening_name,
                                           loc_of_pos closing_loc, closing_name)))

%}

/* Tokens */

%token AMPERAMPER          /* "&&" */
%token AMPERSAND           /* "&" */
%token AND                 /* "and" */
%token AS                  /* "as" */
%token ASSERT              /* "assert" */
%token AWAIT               /* "await" */
%token BACKQUOTE           /* "`" */
%token BACKSLASHSLASH      /* " \/ " */
%token BAR                 /* "|" */
%token BARBAR              /* "||" */
%token BARRBRACKET         /* "|]" */
%token BARGRATER           /* "|>" */
%token BEGIN               /* "begin" */
%token <char> CHAR
%token CLASS               /* "class" */
%token COLON               /* ":" */
%token COLONCOLON          /* "::" */
%token COLONEQUAL          /* ":=" */
%token COLONGREATER        /* ":>" */
%token COMMA               /* "," */
%token CONSTRAINT          /* "constraint" */
%token CONTROL             /* "control" */
%token DEFAULT             /* "default" */
%token DO                  /* "do" */
%token DONE                /* "done" */
%token DOPAR               /* "dopar" */
%token DOT                 /* "." */
%token DOTDOT              /* ".." */
%token DOWNTO              /* "downto" */
%token ELSE                /* "else" */
%token EMIT                /* "emit" */
%token END                 /* "end" */
%token EOF
%token EQUAL               /* "=" */
%token EXCEPTION           /* "exception" */
%token EXTERNAL            /* "external" */
%token FALSE               /* "false" */
%token <float> FLOAT
%token FOR                 /* "for" */
%token FUN                 /* "fun" */
%token FUNCTION            /* "function" */
%token FUNCTOR             /* "functor" */
%token GATHER              /* "gather" */
%token GREATER             /* ">" */
%token GREATERRBRACE       /* ">}" */
%token GREATERRBRACKET     /* ">]" */
%token HALT                /* "halt" */
%token IF                  /* "if" */
%token IMMEDIATE           /* "immediate" */
%token IN                  /* "in" */
%token INCLUDE             /* "include" */
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT             /* "inherit" */
%token INITIALIZER         /* "initializer" */
/* %token INOUT */               /* "inout" */
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAST                /* "last" */
%token LAZY                /* "lazy" */
%token LBRACE              /* "{" */
%token LBRACELESS          /* "{<" */
%token LBRACKET            /* "[" */
%token LBRACKETBAR         /* "[|" */
%token LBRACKETLESS        /* "[<" */
%token LESS                /* "<" */
%token LESSMINUS           /* "<-" */
%token LET                 /* "let" */
%token <string> LIDENT
%token LOOP                /* "loop" */
%token LPAREN              /* "(" */
%token MATCH               /* "match" */
%token MEMORY              /* "memory" */
%token METHOD              /* "method" */
%token MINUS               /* "-" */
%token MINUSDOT            /* "-." */
%token MINUSGREATER        /* "->" */
%token MODULE              /* "module" */
%token MUTABLE             /* "mutable" */
%token <nativeint> NATIVEINT
%token NEW                 /* "new" */
%token NOTHING             /* "nothing" */
%token OBJECT              /* "object" */
%token OF                  /* "of" */
%token ONE                 /* "one" */
%token OPEN                /* "open" */
%token <string> OPTLABEL
%token OR                  /* "or" */
/* %token OUT */                 /* "out" */
/* %token PARSER */
%token PAUSE               /* "pause" */
%token PLUS                /* "+" */
%token PRE                 /* "pre" */
%token <string> PREFIXOP
%token PRESENT             /* "present" */
%token PRIVATE             /* "private" */
%token PROC                /* "proc"  */
%token PROCESS             /* "process" */
%token QUESTION            /* "?" */
%token QUESTIONQUESTION    /* "??" */
%token QUOTE               /* "'" */
%token RBRACE              /* "}" */
%token RBRACKET            /* "]" */
%token REC                 /* "rec" */
%token RPAREN              /* "(" */
%token RUN                 /* "run" */
%token SEMI                /* ";" */
%token SEMISEMI            /* ";;" */
%token SHARP               /* "#" */
%token SIG                 /* "sig" */
%token SIGNAL              /* "signal" */
%token SLASHBACKSLASH      /* " /\ " */
%token STAR                /* "*" */
%token <string> STRING
%token STRUCT              /* "struct" */
%token THEN                /* "then" */
%token TILDE               /* "~" */
%token TO                  /* "to" */
%token TRUE                /* "true" */
%token TRY                 /* "try" */
%token TYPE                /* "type" */
%token <string> UIDENT
%token UNDERSCORE          /* "_" */
%token UNTIL               /* "until" */
%token VAL                 /* "val" */
%token VIRTUAL             /* "virtual" */
%token WHEN                /* "when" */
%token WHILE               /* "while" */
%token WITH                /* "with" */

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc above_IN
%nonassoc IN WHEN
%nonassoc below_BARBAR
%nonassoc BARBAR BARGRATER              /* below SEMI e; e || e*/
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET SIGNAL DO DOPAR           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BACKSLASHSLASH             /* expr (e or e or e) */
%right    AMPERSAND AMPERAMPER SLASHBACKSLASH
                                        /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%left RUN
%nonassoc DOT
%nonassoc below_LPAREN
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT HALT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT NOTHING PAUSE LOOP


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parse_ast.implementation> implementation
%start interface                        /* for interface files */
%type <Parse_ast.interface> interface
%start interactive                      /* for interactive mode */
%type <Parse_ast.implementation> interactive

%%

/* Entry points */

implementation:
    structure EOF                        { $1 }
;
interface:
    signature EOF                        { List.rev $1 }
;
interactive:
    /* empty */                          { exit 0 }
  | interactive_defs                     { $1 }
  | seq_expr SEMISEMI                    { [ghimpl (Pimpl_expr $1) (gloc_of_pos $loc)] }
;
interactive_defs:
    structure_item SEMISEMI              { [$1] }
  | structure_item interactive_defs      {  $1 :: $2 }


/* implementation */

structure:
    structure_tail                             { $1 }
  | seq_expr structure_tail                    { ghimpl (Pimpl_expr $1) ($1).pexpr_loc :: $2 }
;
structure_tail:
    /* empty */                                { [] }
  | SEMISEMI                                   { [] }
  | SEMISEMI seq_expr structure_tail           { ghimpl (Pimpl_expr $2) ($2).pexpr_loc :: $3 }
  | SEMISEMI structure_item structure_tail     { $2 :: $3 }
  | structure_item structure_tail              { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppatt_desc = Ppatt_any; _}, exp] -> mkimpl(Pimpl_expr exp) $loc
        | _ -> mkimpl(Pimpl_let($2, List.rev $3)) $loc }
  | SIGNAL signal_comma_list
      { mkimpl(Pimpl_signal(List.rev $2, None)) $loc  }
  | SIGNAL signal_comma_list DEFAULT par_expr GATHER par_expr
      { mkimpl(Pimpl_signal(List.rev $2, Some(Default, $4, $6))) $loc  }
  | SIGNAL signal_comma_list MEMORY par_expr GATHER par_expr
      { mkimpl(Pimpl_signal(List.rev $2, Some(Memory, $4, $6))) $loc  }
  | TYPE type_declarations
      { mkimpl(Pimpl_type(List.rev $2)) $loc  }
  | EXCEPTION UIDENT constructor_arguments
      { mkimpl(Pimpl_exn(mksimple $2 ($startpos($2), $endpos($2)), $3)) $loc  }
  | EXCEPTION UIDENT EQUAL constr_longident
      { mkimpl(Pimpl_exn_rebind(mksimple $2 ($startpos($2), $endpos($2)), $4)) $loc  }
  | OPEN UIDENT
      { mkimpl(Pimpl_open $2) $loc  }
  | EXTERNAL DOT LIDENT LIDENT lucky_declarations lucky_declarations
      EQUAL lucky_files
      { match $3 with
        | "luc" ->
	    mkimpl(Pimpl_lucky(mksimple $4 ($startpos($4), $endpos($4)), List.rev $5, List.rev $6, $8)) $loc 
	| _ -> raise (Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($1), $endpos($1)))))
      }
;

/* interface */

signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident_colon core_type
      { mkintf(Pintf_val($2, $3)) $loc }
  | EXTERNAL val_ident_colon core_type EQUAL primitive_declaration
      { mkintf(Pintf_val($2, $3)) $loc }
  | TYPE type_declarations
      { mkintf(Pintf_type(List.rev $2)) $loc }
  | EXCEPTION UIDENT constructor_arguments
      { mkintf(Pintf_exn(mksimple $2 ($startpos($2), $endpos($2)), $3)) $loc }
  | OPEN UIDENT
      { mkintf(Pintf_open $2) $loc }
;

/* Core expressions */

par_expr:
  | seq_expr  %prec below_BARBAR  { $1}
  | seq_expr BARBAR par_expr      { mkexpr(Pexpr_par($1, $3)) $loc }
  | seq_expr BARGRATER par_expr   { mkexpr(Pexpr_merge($1, $3)) $loc }
;
seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_expr $1 $loc }
  | expr SEMI seq_expr            { mkexpr(Pexpr_seq($1, $3)) $loc }
;
expr:
    simple_expr %prec below_SHARP
      { $1 }
  | simple_expr simple_expr_list
      { mkexpr(Pexpr_apply($1, List.rev $2)) $loc }
  | LET rec_flag let_bindings IN par_expr
      { mkexpr(Pexpr_let($2, List.rev $3, $5)) $loc }
  | FUNCTION opt_bar match_cases
      { mkexpr(Pexpr_function(List.rev $3)) $loc }
  | FUN simple_pattern fun_def
      { let when_opt, expr = $3 in
        mkexpr(Pexpr_function([$2, when_opt, expr])) $loc }
  | MATCH par_expr WITH opt_bar match_cases
      { mkexpr(Pexpr_match($2, List.rev $5)) $loc }
  | TRY par_expr WITH opt_bar match_cases
      { mkexpr(Pexpr_trywith($2, List.rev $5)) $loc }
  | TRY par_expr WITH error
      { syntax_error() }
  | expr_comma_list %prec below_COMMA
      { mkexpr(Pexpr_tuple(List.rev $1)) $loc }
  | constr_longident simple_expr %prec prec_constr_appl /*%prec below_SHARP */
      { mkexpr(Pexpr_construct($1, Some $2)) $loc }
  | IF par_expr THEN expr ELSE expr
      { mkexpr(Pexpr_ifthenelse($2, $4, Some $6)) $loc }
  | IF par_expr THEN expr
      { mkexpr(Pexpr_ifthenelse($2, $4, None)) $loc }
  | WHILE par_expr DO par_expr DONE
      { mkexpr(Pexpr_while($2, $4)) $loc }
  | FOR val_ident EQUAL par_expr direction_flag par_expr DO par_expr DONE
      { mkexpr(Pexpr_for($2, $4, $6, $5, $8)) $loc }
  | FOR val_ident EQUAL par_expr direction_flag par_expr DOPAR par_expr DONE
      { mkexpr(Pexpr_fordopar($2, $4, $6, $5, $8)) $loc }
  | expr COLONCOLON expr
      { mkexpr(Pexpr_construct(mkident (Pident "::") ($startpos($2), $endpos($2)),
                               Some(ghexpr(Pexpr_tuple[$1;$3]) $loc))) $loc }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 $loc }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 $loc }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 $loc }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 $loc }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 $loc }
  | expr PLUS expr
      { mkinfix $1 "+" $3 $loc }
  | expr MINUS expr
      { mkinfix $1 "-" $3 $loc }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 $loc }
  | expr STAR expr
      { mkinfix $1 "*" $3 $loc }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 $loc }
  | expr LESS expr
      { mkinfix $1 "<" $3 $loc }
  | expr GREATER expr
      { mkinfix $1 ">" $3 $loc }
  | expr OR expr
      { mkinfix $1 "or" $3 $loc }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 $loc }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 $loc }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 $loc }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 ($startpos($1), $endpos($1)) $loc }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexpr(Pexpr_record_update($1, $3, $5)) $loc }
  | simple_expr DOT LPAREN par_expr RPAREN LESSMINUS expr
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "Array" "set")) $loc,
                           [$1; $4; $7])) $loc }
  | simple_expr DOT LBRACKET par_expr RBRACKET LESSMINUS expr
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "String" "set")) $loc,
                           [$1; $4; $7])) $loc }
  | ASSERT simple_expr %prec below_SHARP
      { mkassert $2 $loc }
  | PRE pre_expr
      { let k,s = $2 in mkexpr(Pexpr_pre (k,s)) $loc }
  | LAST QUESTION simple_expr
      { mkexpr(Pexpr_last $3) $loc }
  | DEFAULT QUESTION simple_expr
      { mkexpr(Pexpr_default $3) $loc }
  | EMIT simple_expr
      { mkexpr(Pexpr_emit $2 ) $loc }
  | EMIT simple_expr simple_expr
      { mkexpr(Pexpr_emit_val($2, $3)) $loc }
  | SIGNAL signal_comma_list IN par_expr
      { mkexpr(Pexpr_signal(List.rev $2, None, $4)) $loc }
  | SIGNAL signal_comma_list DEFAULT par_expr GATHER par_expr IN par_expr
      { mkexpr(Pexpr_signal(List.rev $2, Some(Default, $4, $6), $8)) $loc }
  | SIGNAL signal_comma_list MEMORY par_expr GATHER par_expr IN par_expr
      { mkexpr(Pexpr_signal(List.rev $2, Some(Memory, $4, $6), $8)) $loc }
  | DO par_expr UNTIL opt_bar until_cases DONE
      { mkexpr_until $2 $5 $loc }
  | DO par_expr WHEN event_config DONE
      { mkexpr(Pexpr_when($4, $2)) $loc }
  | CONTROL par_expr WITH event_config DONE
      { mkexpr(Pexpr_control($4, None, $2)) $loc }
  | CONTROL par_expr WITH event_config WHEN par_expr DONE
      { mkexpr(Pexpr_control($4, Some $6, $2)) $loc }
  | PRESENT event_config THEN expr ELSE expr
      { mkexpr(Pexpr_present($2, $4, $6)) $loc }
  | PRESENT event_config THEN expr
      { mkexpr(Pexpr_present($2, $4, ghexpr(Pexpr_nothing) $loc)) $loc }
  | PRESENT event_config ELSE expr
      { mkexpr(Pexpr_present($2, ghexpr(Pexpr_nothing) $loc, $4)) $loc }
  | AWAIT await_flag event_config %prec above_IN
      { if (snd $2) = One
        then raise(Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($2), $endpos($2)))))
        else mkexpr(Pexpr_await(fst $2, $3)) $loc }
  | AWAIT await_flag event_config IN par_expr
      { match $2 with
        | Immediate, All -> raise(Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($2), $endpos($2)))))
	| im, k -> mkexpr(Pexpr_await_val(im, k, $3, None, $5)) $loc }
  | AWAIT await_flag event_config WHEN par_expr IN par_expr
      { match $2 with
        | Immediate, All -> raise(Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($2), $endpos($2)))))
	| im, k ->
	    mkexpr(Pexpr_await_val(im, k, $3, Some $5, $7)) $loc }
  | PROCESS proc_def
      { $2 }
  | PROC simple_pattern proc_fun_def
      { mkexpr(Pexpr_function([$2, None, $3])) $loc }
  | RUN simple_expr simple_expr_list
      { let e = mkexpr(Pexpr_apply($2, List.rev $3)) $loc in
        mkexpr(Pexpr_run(e)) $loc }
  | RUN simple_expr
      { mkexpr(Pexpr_run($2)) $loc }
;
simple_expr:
    val_longident
      { mkexpr(Pexpr_ident $1) $loc }
  | constant
      { mkexpr(Pexpr_constant $1) $loc }
  | constr_longident %prec prec_constant_constructor
      { mkexpr(Pexpr_construct($1, None)) $loc }
  | LPAREN par_expr RPAREN
      { reloc_expr $2 $loc }
  | LPAREN par_expr error
      { unclosed "(" ($startpos($1), $endpos($1)) ")" ($startpos($3), $endpos($3)) }
  | BEGIN par_expr END
      { reloc_expr $2 $loc }
  | BEGIN END
      { mkexpr (Pexpr_constant Const_unit) $loc }
  | BEGIN par_expr error
      { unclosed "begin" ($startpos($1), $endpos($1)) "end" ($startpos($3), $endpos($3)) }
  | LPAREN par_expr type_constraint RPAREN
      { mkexpr(Pexpr_constraint($2, $3)) $loc }
  | simple_expr DOT label_longident
      { mkexpr(Pexpr_record_access($1, $3)) $loc }
  | simple_expr DOT LPAREN par_expr RPAREN
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "Array" "get")) $loc,
                           [$1; $4])) $loc }
  | simple_expr DOT LPAREN par_expr error
      { unclosed "(" ($startpos($3), $endpos($3)) ")" ($startpos($5), $endpos($5)) }
  | simple_expr DOT LBRACKET par_expr RBRACKET
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "String" "get")) $loc,
                           [$1; $4])) $loc }
  | simple_expr DOT LBRACKET par_expr error
      { unclosed "[" ($startpos($3), $endpos($3)) "]" ($startpos($5), $endpos($5)) }
  | LBRACE record_expr RBRACE
      { mkexpr(Pexpr_record($2)) $loc }
  | LBRACE record_expr error
      { unclosed "{" ($startpos($1), $endpos($1)) "}" ($startpos($3), $endpos($3)) }
  | LBRACE simple_expr WITH record_expr RBRACE
      { mkexpr(Pexpr_record_with ($2, $4)) $loc }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexpr(Pexpr_array(List.rev $2)) $loc }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" ($startpos($1), $endpos($1)) "|]" ($startpos($4), $endpos($4)) }
  | LBRACKETBAR BARRBRACKET
      { mkexpr(Pexpr_array []) $loc }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { reloc_expr (mktailexpr $loc (List.rev $2)) $loc }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" ($startpos($1), $endpos($1)) "]" ($startpos($4), $endpos($4)) }
  | PREFIXOP simple_expr
      { mkexpr(Pexpr_apply(mkoperator $1 ($startpos($1), $endpos($1)), [$2])) $loc }
  | NOTHING
      { mkexpr Pexpr_nothing $loc }
  | PAUSE
      { mkexpr Pexpr_pause $loc }
  | HALT
      { mkexpr Pexpr_halt $loc }
  | LOOP par_expr END
      { mkexpr (Pexpr_loop $2) $loc }
  | SHARP ident
      { match $2 with
        | "suspend" ->
	    mkexpr
	      (Pexpr_apply
		 (mkexpr (Pexpr_ident
			    (mkident (Pdot("Rmltop_controller",
					   "set_suspend")) ($startpos($2), $endpos($2)))) $loc,
		  [mkexpr (Pexpr_constant Const_unit) $loc])) $loc
(* !!!!!!!!!!
	    mkexpr
	      (Pexpr_seq
		 (mkexpr
		    (Pexpr_apply
		       (mkexpr (Pexpr_ident
				  (mkident (Pdot("Rmltop_controller",
						 "set_suspend")) 2)),
			[mkexpr (Pexpr_constant Const_unit)])),
		  mkexpr Pexpr_pause))
!!!!!!!!!! *)
	| _ -> raise (Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($2), $endpos($2))))) }
;
very_simple_expr: /* simple_expr without "LPAREN expr RPAREN" */
    val_longident
      { mkexpr(Pexpr_ident $1) $loc }
  | constant
      { mkexpr(Pexpr_constant $1) $loc }
  | constr_longident %prec prec_constant_constructor
      { mkexpr(Pexpr_construct($1, None)) $loc }
  | BEGIN par_expr END
      { reloc_expr $2 $loc }
  | BEGIN END
      { mkexpr (Pexpr_constant Const_unit) $loc }
  | BEGIN par_expr error
      { unclosed "begin" ($startpos($1), $endpos($1)) "end" ($startpos($3), $endpos($3)) }
  | very_simple_expr DOT label_longident
      { mkexpr(Pexpr_record_access($1, $3)) $loc }
  | very_simple_expr DOT LPAREN par_expr RPAREN
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "Array" "get")) $loc,
                           [$1; $4])) $loc }
  | very_simple_expr DOT LPAREN par_expr error
      { unclosed "(" ($startpos($3), $endpos($3)) ")" ($startpos($5), $endpos($5)) }
  | very_simple_expr DOT LBRACKET par_expr RBRACKET
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "String" "get")) $loc,
                           [$1; $4])) $loc }
  | very_simple_expr DOT LBRACKET par_expr error
      { unclosed "[" ($startpos($3), $endpos($3)) "]" ($startpos($5), $endpos($5)) }
  | LBRACE record_expr RBRACE
      { mkexpr(Pexpr_record($2)) $loc }
  | LBRACE record_expr error
      { unclosed "{" ($startpos($1), $endpos($1)) "}" ($startpos($3), $endpos($3)) }
  | LBRACE simple_expr WITH record_expr RBRACE
      { mkexpr(Pexpr_record_with ($2, $4)) $loc }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexpr(Pexpr_array(List.rev $2)) $loc }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" ($startpos($1), $endpos($1)) "|]" ($startpos($4), $endpos($4)) }
  | LBRACKETBAR BARRBRACKET
      { mkexpr(Pexpr_array []) $loc }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { reloc_expr (mktailexpr $loc (List.rev $2)) $loc }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" ($startpos($1), $endpos($1)) "]" ($startpos($4), $endpos($4)) }
  | PREFIXOP simple_expr
      { mkexpr(Pexpr_apply(mkoperator $1 ($startpos($1), $endpos($1)), [$2])) $loc }
  | NOTHING
      { mkexpr Pexpr_nothing $loc }
  | PAUSE
      { mkexpr Pexpr_pause $loc }
  | HALT
      { mkexpr Pexpr_halt $loc }
  | LOOP par_expr END
      { mkexpr (Pexpr_loop $2) $loc }
  | SHARP ident
      { match $2 with
        | "suspend" ->
	    mkexpr
	      (Pexpr_apply
		 (mkexpr (Pexpr_ident
			    (mkident (Pdot("Rmltop_controller",
					   "set_suspend")) ($startpos($2), $endpos($2)))) $loc,
		  [mkexpr (Pexpr_constant Const_unit) $loc])) $loc
(* !!!!!!!!!!
	    mkexpr
	      (Pexpr_seq
		 (mkexpr
		    (Pexpr_apply
		       (mkexpr (Pexpr_ident
				  (mkident (Pdot("Rmltop_controller",
						 "set_suspend")) 2)),
			[mkexpr (Pexpr_constant Const_unit)])),
		  mkexpr Pexpr_pause))
!!!!!!!!!! *)
	| _ -> raise (Rml_syntaxerr.Error(Rml_syntaxerr.Other (loc_of_pos ($startpos($2), $endpos($2))))) }
;
pre_expr:
    simple_expr
      { Status, $1 }
  | QUESTION simple_expr
      { Value, $2 }
;
event_config:
    very_simple_expr  %prec below_LPAREN
      { mkconf(Pconf_present($1, None)) $loc }
  | very_simple_expr LPAREN pattern RPAREN
      { mkconf(Pconf_present($1, Some $3)) $loc }
  | event_config BACKSLASHSLASH event_config
      { mkconf(Pconf_or($1,$3)) $loc }
  | event_config SLASHBACKSLASH event_config
      { mkconf(Pconf_and($1,$3)) $loc }
  | LPAREN event_config RPAREN
      { $2 }
  | LPAREN event_config error
      { unclosed "(" ($startpos($1), $endpos($1)) ")" ($startpos($3), $endpos($3)) }
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
  | val_longident LESS pattern GREATER
      {	[$3, { pexpr_desc = Pexpr_get (mkexpr(Pexpr_ident $1) $loc);
	       pexpr_loc = loc_of_pos ($startpos($1), $endpos($1)); }] }
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
;
let_binding:
    val_ident fun_binding
      { ({ppatt_desc = Ppatt_var $1; ppatt_loc = loc_of_pos ($startpos($1), $endpos($1))}, $2) }
  | pattern EQUAL par_expr
      { ($1, $3) }
  | PROCESS val_ident proc_binding
      { ({ppatt_desc = Ppatt_var $2; ppatt_loc = loc_of_pos ($startpos($2), $endpos($2))}, $3) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL par_expr
      { ghexpr(Pexpr_constraint($3, $1)) $loc }
;
strict_binding:
    EQUAL par_expr
      { $2 }
  | simple_pattern fun_binding
      { ghexpr(Pexpr_function([$1, None, $2])) $loc }
;
proc_binding:
    strict_proc_binding
      { $1 }
  | type_constraint EQUAL par_expr
      { ghexpr(Pexpr_constraint(ghexpr(Pexpr_process($3)) $loc, $1)) $loc }
;
strict_proc_binding:
    EQUAL par_expr
      { ghexpr(Pexpr_process($2)) $loc }
  | simple_pattern proc_binding
      { ghexpr(Pexpr_function([$1, None, $2])) $loc }
;
match_cases:
    pattern match_action
      { let when_opt, expr = $2 in
        [$1, when_opt, expr] }
  | match_cases BAR pattern match_action
      { let when_opt, expr = $4 in
        ($3, when_opt, expr) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def
      { let when_opt, expr = $2 in
        when_opt, ghexpr(Pexpr_function([$1, None, expr])) $loc }
;
proc_fun_def:
    MINUSGREATER par_expr                       { mkexpr (Pexpr_process $2) $loc }
  | simple_pattern proc_fun_def
      { ghexpr(Pexpr_function([$1, None, $2])) $loc }
;
proc_def:
    simple_expr                                 { mkexpr(Pexpr_process $1) $loc }
/*
    MINUSGREATER par_expr                       { mkexpr(Pexpr_process $2) }
  | simple_pattern proc_def
      { ghexpr(Pexpr_function([$1, $2])) }
*/
;
match_action:
    MINUSGREATER par_expr                       { None, $2 }
  | WHEN par_expr MINUSGREATER par_expr         { Some $2, $4 }
;
until_action:
    MINUSGREATER par_expr                       { None, $2 }
  | WHEN par_expr                               { Some $2, ghexpr_unit $loc }
  | WHEN par_expr MINUSGREATER par_expr         { Some $2, $4 }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
record_expr:
    lbl_expr_list opt_semi                      { List.rev $1 }
;
lbl_expr_list:
    label_longident EQUAL expr
      { [$1,$3] }
  | lbl_expr_list SEMI label_longident EQUAL expr
      { ($3, $5) :: $1 }
;
expr_semi_list:
    expr                                        { [$1] }
  | expr_semi_list SEMI expr                    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { $2 }
  | COLON error                                 { syntax_error() }
;

until_cases:
    event_config                                { [$1, None, None] }
  | until_handlers                              { List.rev $1 }
;
until_handlers:
    event_config until_action
      { let when_opt, expr = $2 in
        [$1, when_opt, Some expr] }
  | until_handlers BAR event_config until_action
      { let when_opt, expr = $4 in
        ($3, when_opt, Some expr) :: $1 }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpatt(Ppatt_alias($1, $3)) $loc }
  | pattern_comma_list  %prec below_COMMA
      { mkpatt(Ppatt_tuple(List.rev $1)) $loc }
  | constr_longident pattern %prec prec_constr_appl
      { mkpatt(Ppatt_construct($1, Some $2)) $loc }
  | pattern COLONCOLON pattern
      { mkpatt(Ppatt_construct(mkident (Pident "::") ($startpos($2), $endpos($2)),
			       Some(ghpatt(Ppatt_tuple[$1;$3]) $loc))) $loc }
  | pattern BAR pattern
      { mkpatt(Ppatt_or($1, $3)) $loc }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpatt(Ppatt_var $1) $loc }
  | UNDERSCORE
      { mkpatt(Ppatt_any) $loc }
  | signed_constant
      { mkpatt(Ppatt_constant $1) $loc }
  | CHAR DOTDOT CHAR
      { mkrangepatt $1 $3 $loc }
  | constr_longident
      { mkpatt(Ppatt_construct($1, None)) $loc }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpatt(Ppatt_record(List.rev $2)) $loc }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" ($startpos($1), $endpos($1)) "}" ($startpos($4), $endpos($4)) }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { reloc_patt (mktailpatt $loc (List.rev $2)) $loc }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" ($startpos($1), $endpos($1)) "]" ($startpos($4), $endpos($4)) }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpatt(Ppatt_array(List.rev $2)) $loc }
  | LBRACKETBAR BARRBRACKET
      { mkpatt(Ppatt_array []) $loc }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" ($startpos($1), $endpos($1)) "|]" ($startpos($4), $endpos($4)) }
  | LPAREN pattern RPAREN
      { reloc_patt $2 $loc }
  | LPAREN pattern error
      { unclosed "(" ($startpos($1), $endpos($1)) ")" ($startpos($3), $endpos($3)) }
  | LPAREN pattern COLON core_type RPAREN
      { mkpatt(Ppatt_constraint($2, $4)) $loc }
  | LPAREN pattern COLON core_type error
      { unclosed "(" ($startpos($1), $endpos($1)) ")" ($startpos($5), $endpos($5)) }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
;

/* Primitive declarations */

primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;

/* Type declarations */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;

type_declaration:
    type_parameters LIDENT type_kind
      { (mksimple $2 ($startpos($2), $endpos($2)), $1, $3) }
;
type_kind:
    /*empty*/
      { Ptype_abstract }
  | EQUAL core_type
      { Ptype_rebind $2 }
  | EQUAL constructor_declarations
      { Ptype_variant(List.rev $2) }
  | EQUAL BAR constructor_declarations
      { Ptype_variant(List.rev $3) }
  | EQUAL LBRACE label_declarations opt_semi RBRACE
      { Ptype_record(List.rev $3) }
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    QUOTE ident                                 { $2 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1, $2) }
;
constructor_arguments:
    /*empty*/                                   { None }
    | OF core_type                              { Some $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag label COLON core_type          { ($2, $1, $4) }
;

/* Core types */

core_type:
    simple_core_type_or_tuple
      { $1 }
  | core_type MINUSGREATER core_type
      { mkte(Ptype_arrow($1, $3)) $loc }
;

simple_core_type:
    simple_core_type2
      { $1}
  | LPAREN core_type_comma_list RPAREN
      { match $2 with [sty] -> sty | _ -> raise Parsing.Parse_error }

simple_core_type2:
    QUOTE ident
      { mkte(Ptype_var $2) $loc }
  | type_longident
      { mkte(Ptype_constr($1, [])) $loc }
  | simple_core_type2 type_longident
      { mkte(Ptype_constr($2, [$1])) $loc }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mkte(Ptype_constr($4, List.rev $2)) $loc }
  | simple_core_type PROCESS
      { mkte(Ptype_process ($1, Def_static.Dontknow)) $loc }
  | simple_core_type PROCESS PLUS
      { mkte(Ptype_process ($1, Def_static.Noninstantaneous)) $loc }
  | simple_core_type PROCESS MINUS
      { mkte(Ptype_process ($1, Def_static.Instantaneous)) $loc }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mkte(Ptype_tuple($1 :: List.rev $3)) $loc }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
label:
    LIDENT                                      { mksimple $1 ($startpos($1), $endpos($1)) }
;

/* Constants */

constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | LPAREN RPAREN                               { Const_unit }
  | FALSE                                       { Const_bool false }
  | TRUE                                        { Const_bool true }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(- $2) }
  | MINUS FLOAT                                 { Const_float(-. $2) }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { mksimple $1 ($startpos($1), $endpos($1)) }
  | LPAREN operator RPAREN                      { mksimple $2 ($startpos($2), $endpos($2)) }
;
val_ident_colon:
    LIDENT COLON                                { mksimple $1 ($startpos($1), $endpos($1)) }
  | LPAREN operator RPAREN COLON                { mksimple $2 ($startpos($2), $endpos($2)) }
  | LABEL                                       { mksimple $1 ($startpos($1), $endpos($1)) }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { mksimple $1 ($startpos($1), $endpos($1)) }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | COLONCOLON                                  { mksimple "::" ($startpos($1), $endpos($1)) }
;

val_longident:
    val_ident
      { mkident (Pident $1.psimple_id) ($startpos($1), $endpos($1)) }
  | UIDENT DOT val_ident
      { mkident (Pdot($1, $3.psimple_id)) $loc }
;
constr_longident:
    UIDENT             %prec below_DOT
      { mkident (Pident $1) ($startpos($1), $endpos($1)) }
  | UIDENT DOT UIDENT
      { mkident (Pdot($1, $3)) $loc }
  | LBRACKET RBRACKET
      { mkident (Pident "[]") $loc }
;
label_longident:
    LIDENT
      { mkident (Pident $1)($startpos($1), $endpos($1)) }
  | UIDENT DOT LIDENT
      { mkident (Pdot($1, $3))  $loc }
;
type_longident:
    LIDENT
      { mkident (Pident $1) ($startpos($1), $endpos($1)) }
  | UIDENT DOT LIDENT
      { mkident (Pdot($1, $3)) $loc }
;

/* Signals */
signal_decl:
    LIDENT                                      { (mksimple $1 ($startpos($1), $endpos($1)), None) }
  | LIDENT COLON core_type
      { (mksimple $1 ($startpos($1), $endpos($1)), Some $3) }
;
signal_comma_list:
    signal_decl                                 { [$1] }
  | signal_comma_list COMMA signal_decl         { $3 :: $1}
;

/* Miscellaneous */

rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;

await_flag:
    /* empty */                                 { Nonimmediate, All }
  | IMMEDIATE                                   { Immediate, All }
  | ONE                                         { Nonimmediate, One }
  | IMMEDIATE ONE                               { Immediate, One }
;

/*immediate_flag:*/
    /* empty */                  /*               { Nonimmediate }*/
/*  | IMMEDIATE                                   { Immediate }*/
/*;*/

/*one_flag:*/
    /* empty */                  /*               { All }*/
/*  | ONE                                         { One }*/
/*;*/

opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;

/* Lucky */

lucky_declarations:
    LBRACE RBRACE                               { [] }
  | LBRACE lucky_declarations2 opt_semi RBRACE  { $2 }
;
lucky_declarations2:
    lucky_declaration                           { [$1] }
  | lucky_declarations2 SEMI lucky_declaration  { $3 :: $1 }
;
lucky_declaration:
    lucky_label COLON core_type                 { ($1, $3) }
;
lucky_label:
    LIDENT                                      { mksimple $1 ($startpos($1), $endpos($1)) }
  | UIDENT                                      { mksimple $1 ($startpos($1), $endpos($1)) }
;
/* string list */
lucky_files:
  | LBRACKET string_semi_list opt_semi RBRACKET
      { List.rev $2 }
  | LBRACKET string_semi_list opt_semi error
      { unclosed "[" ($startpos($1), $endpos($1)) "]" ($startpos($4), $endpos($4)) }
;
string_semi_list:
    constant
      { match $1 with
        | Const_string s -> [s]
	| _ -> syntax_error() }
  | string_semi_list SEMI constant
      { match $3 with
        | Const_string s -> s :: $1
	| _ -> syntax_error() }
;
%%
