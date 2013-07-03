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
open Asttypes
open Parse_ident
open Parse_ast

let mkident id pos =
  { pident_id = id;
    pident_loc = rhs_loc pos; }
let mkident_loc id loc =
  { pident_id = id;
    pident_loc = loc; }

let mksimple id pos =
  { psimple_id = id;
    psimple_loc = rhs_loc pos; }
let mksimple_loc id loc =
  { psimple_id = id;
    psimple_loc = loc; }

let mkte d =
  { pte_desc = d; pte_loc = symbol_rloc() }
let mkce d =
  { pce_desc = d; pce_loc = symbol_rloc() }
let mkcer d =
  { pcer_desc = d; pcer_loc = symbol_rloc() }
let mkee d =
  { pee_desc = d; pee_loc = symbol_rloc() }
let mkeer d =
  { peer_desc = d; peer_loc = symbol_rloc() }
let mkpatt d =
  { ppatt_desc = d; ppatt_loc = symbol_rloc() }
let mkexpr d =
  { pexpr_desc = d;
    pexpr_loc = symbol_rloc(); }
let mkimpl d =
  { pimpl_desc = d; pimpl_loc = symbol_rloc() }
let mkintf d =
  { pintf_desc = d; pintf_loc = symbol_rloc() }

let rec mkexpr_until body sig_patt_expr_opt_list pause_kind =
  match sig_patt_expr_opt_list with
  | [] -> raise Parse_error
  | [(s, patt_expr_opt)] ->
      mkexpr (Pexpr_until (s,
			   body,
			   (patt_expr_opt), pause_kind))
  | (s, patt_expr_opt) :: list ->
      mkexpr (Pexpr_until (s,
			   mkexpr_until body list pause_kind,
			   (patt_expr_opt), pause_kind))

let reloc_patt x = { x with ppatt_loc = symbol_rloc () };;
let reloc_expr x = { x with pexpr_loc = symbol_rloc () };;

let mkoperator name pos =
  { pexpr_desc = Pexpr_ident (mkident (Pident name) pos);
    pexpr_loc = rhs_loc pos; }

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
let ghexpr d = { pexpr_desc = d;
		 pexpr_loc = symbol_gloc (); };;
let ghpatt d = { ppatt_desc = d; ppatt_loc = symbol_gloc () };;
let ghte d = { pte_desc = d; pte_loc = symbol_gloc () };;
let ghimpl d = { pimpl_desc = d; pimpl_loc = symbol_gloc () };;

let mkassert e =
  mkexpr (Pexpr_assert (e))
;;

let mkinfix arg1 name arg2 =
  mkexpr(Pexpr_apply(mkoperator name 2, [arg1; arg2]))

let mkuminus name arg =
  match name, arg.pexpr_desc with
  | "-", Pexpr_constant(Const_int n) ->
      mkexpr(Pexpr_constant(Const_int(-n)))
  | _, Pexpr_constant(Const_float f) ->
      mkexpr(Pexpr_constant(Const_float(-. f)))
  | _ ->
      mkexpr(Pexpr_apply(mkoperator ("~" ^ name) 1, [arg]))

let rec mktailexpr = function
    [] ->
      ghexpr(Pexpr_construct( mkident_loc (Pident "[]") none, None))
  | e1 :: el ->
      let exp_el = mktailexpr el in
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

let rec mktailpatt = function
    [] ->
      ghpatt(Ppatt_construct(mkident_loc (Pident "[]") none, None))
  | p1 :: pl ->
      let pat_pl = mktailpatt pl in
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

let rec deep_mkrangepatt c1 c2 =
  if c1 = c2 then ghpatt(Ppatt_constant(Const_char c1)) else
  ghpatt(Ppatt_or(ghpatt(Ppatt_constant(Const_char c1)),
                  deep_mkrangepatt (Char.chr(Char.code c1 + 1)) c2))

let rec mkrangepatt c1 c2 =
  if c1 > c2 then mkrangepatt c2 c1 else
  if c1 = c2 then mkpatt(Ppatt_constant(Const_char c1)) else
  reloc_patt (deep_mkrangepatt c1 c2)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

%}

/* Tokens */

%token AMPERAMPER          /* "&&" */
%token AMPERSAND           /* "&" */
%token AND                 /* "and" */
%token AS                  /* "as" */
%token ASSERT              /* "assert" */
%token AT                  /* "at" */
%token AWAIT               /* "await" */
%token BACKQUOTE           /* "`" */
%token BACKSLASHSLASH      /* " \/ " */
%token BANGBANG            /* "!!" */
%token BAR                 /* "|" */
%token BARBAR              /* "||" */
%token BARRBRACKET         /* "|]" */
%token BARGRATER           /* "|>" */
%token BASE                /* "base" */
%token BEGIN               /* "begin" */
%token BY                  /* "by" */
%token <char> CHAR
%token CLASS               /* "class" */
%token CLOCK               /* "clock" */
%token COLON               /* ":" */
%token COLONCOLON          /* "::" */
%token COLONEQUAL          /* ":=" */
%token COLONCOLONEQUAL     /* "::=" */
%token COLONGREATER        /* ":>" */
%token COMMA               /* "," */
%token CONSTRAINT          /* "constraint" */
%token CONTROL             /* "control" */
%token DEFAULT             /* "default" */
%token DO                  /* "do" */
%token DOMAIN              /* "domain" */
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
%token EQUALGREATER        /* "=>" */
%token EQUALGREATERLBRACE  /* "=>{" */
%token EXCEPTION           /* "exception" */
%token EXTERNAL            /* "external" */
%token FALSE               /* "false" */
%token <float> FLOAT
%token FOR                 /* "for" */
%token FUN                 /* "fun" */
%token FUNCTION            /* "function" */
%token FUNCTOR             /* "functor" */
%token GATHER              /* "gather" */
%token GLOBAL_CK           /* "global_ck" */
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
%token LESSLESSMINUS       /* "<<-" */
%token LET                 /* "let" */
%token <string> LIDENT
%token LOCAL_CK            /* "local_ck" */
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
%token NEWCLOCK            /* "newclock" */
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
%token QUIET               /* "quiet" */
%token QUOTE               /* "'" */
%token RBRACE              /* "}" */
%token RBRACKET            /* "]" */
%token REC                 /* "rec" */
%token REGION              /* "region" */
%token RESET               /* "reset" */
%token RPAREN              /* "(" */
%token RUN                 /* "run" */
%token SCHEDULE            /* "schedule" */
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
%token TOPCK               /* "topck" */
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

%nonassoc IN
%nonassoc below_BARBAR
%nonassoc BARBAR BARGRATER              /* below SEMI e; e || e*/
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET SIGNAL MEMORY DO DOPAR NEWCLOCK  /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%right    LESSLESSMINUS
%right    COLONCOLONEQUAL
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%nonassoc   MINUSGREATER EQUALGREATER EQUALGREATERLBRACE                /* core_type2 (t -> t -> t) */
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
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT HALT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT NOTHING PAUSE LOOP
          TOPCK GLOBAL_CK LOCAL_CK QUIET


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
/*
    structure_item SEMISEMI              { [$1] }
  | seq_expr SEMISEMI                    { [ghimpl (Pimpl_expr $1)] }
*/
    /* empty */                          { exit 0 }
  | interactive_defs                     { $1 }
  | seq_expr SEMISEMI                    { [ghimpl (Pimpl_expr $1)] }
;
interactive_defs:
    structure_item SEMISEMI              { [$1] }
  | structure_item interactive_defs      {  $1 :: $2 }


/* implementation */

structure:
    structure_tail                             { $1 }
  | seq_expr structure_tail                    { ghimpl (Pimpl_expr $1) :: $2 }
;
structure_tail:
    /* empty */                                { [] }
  | SEMISEMI                                   { [] }
  | SEMISEMI seq_expr structure_tail           { ghimpl (Pimpl_expr $2) :: $3 }
  | SEMISEMI structure_item structure_tail     { $2 :: $3 }
  | structure_item structure_tail              { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppatt_desc = Ppatt_any}, exp] -> mkimpl(Pimpl_expr exp)
        | _ -> mkimpl(Pimpl_let($2, List.rev $3)) }
  | signal_kind signal_comma_list opt_at_expr opt_default_gather
      { mkimpl(Pimpl_signal($1, List.rev $2, $4)) }
  | TYPE type_declarations
      { mkimpl(Pimpl_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mkimpl(Pimpl_exn(mksimple $2 2, $3)) }
  | EXCEPTION UIDENT EQUAL constr_longident
      { mkimpl(Pimpl_exn_rebind(mksimple $2 2, $4)) }
  | OPEN UIDENT
      { mkimpl(Pimpl_open $2) }
  | EXTERNAL DOT LIDENT LIDENT lucky_declarations lucky_declarations
      EQUAL lucky_files
      { match $3 with
        | "luc" ->
	    mkimpl(Pimpl_lucky(mksimple $4 4, List.rev $5, List.rev $6, $8))
	| _ -> raise (Syntaxerr.Error(Syntaxerr.Other (rhs_loc 1)))
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
      { mkintf(Pintf_val($2, $3)) }
  | EXTERNAL val_ident_colon core_type EQUAL primitive_declaration
      { mkintf(Pintf_val($2, $3)) }
  | TYPE type_declarations
      { mkintf(Pintf_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mkintf(Pintf_exn(mksimple $2 2, $3)) }
  | OPEN UIDENT
      { mkintf(Pintf_open $2) }
;

/* Core expressions */

par_expr:
  | seq_expr  %prec below_BARBAR  { $1}
  | seq_expr BARBAR par_expr      { mkexpr(Pexpr_par($1, $3)) }
  | seq_expr BARGRATER par_expr   { mkexpr(Pexpr_merge($1, $3)) }
;
seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_expr $1 }
  | expr SEMI seq_expr            { mkexpr(Pexpr_seq($1, $3)) }
;
expr:
    simple_expr %prec below_SHARP
      { $1 }
  | simple_expr simple_expr_list
      { mkexpr(Pexpr_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN par_expr
      { mkexpr(Pexpr_let($2, List.rev $3, $5)) }
  | FUNCTION opt_bar match_cases
      { mkexpr(Pexpr_function(List.rev $3)) }
  | FUN simple_pattern fun_def
      { mkexpr(Pexpr_function([$2, $3])) }
  | MATCH par_expr WITH opt_bar match_cases
      { mkexpr(Pexpr_match($2, List.rev $5)) }
  | TRY par_expr WITH opt_bar match_cases
      { mkexpr(Pexpr_trywith($2, List.rev $5)) }
  | TRY par_expr WITH error
      { syntax_error() }
  | expr_comma_list %prec below_COMMA
      { mkexpr(Pexpr_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec prec_constr_appl /*%prec below_SHARP */
      { mkexpr(Pexpr_construct($1, Some $2)) }
  | IF par_expr THEN expr ELSE expr
      { mkexpr(Pexpr_ifthenelse($2, $4, Some $6)) }
  | IF par_expr THEN expr
      { mkexpr(Pexpr_ifthenelse($2, $4, None)) }
  | WHILE par_expr DO par_expr DONE
      { mkexpr(Pexpr_while($2, $4)) }
  | FOR val_ident EQUAL par_expr direction_flag par_expr DO par_expr DONE
      { mkexpr(Pexpr_for($2, $4, $6, $5, $8)) }
  | FOR val_ident EQUAL par_expr direction_flag par_expr DOPAR par_expr DONE
      { mkexpr(Pexpr_fordopar($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexpr(Pexpr_construct(mkident (Pident "::") 2,
                               Some(ghexpr(Pexpr_tuple[$1;$3])))) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BACKSLASHSLASH expr
      { mkexpr(Pconf_or($1,$3)) }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr SLASHBACKSLASH expr
      { mkexpr(Pconf_and($1,$3)) }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexpr(Pexpr_record_update($1, $3, $5)) }
  | simple_expr DOT LPAREN par_expr RPAREN LESSMINUS expr
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "Array" "set")),
                           [$1; $4; $7])) }
  | simple_expr DOT LBRACKET par_expr RBRACKET LESSMINUS expr
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "String" "set")),
                           [$1; $4; $7])) }
  | ASSERT simple_expr %prec below_SHARP
      { mkassert $2 }
  | PRE pre_expr
      { let k,s = $2 in mkexpr(Pexpr_pre (k,s)) }
  | LAST simple_expr
      { mkexpr(Pexpr_last $2) }
  | DEFAULT QUESTION simple_expr
      { mkexpr(Pexpr_default $3) }
  | EMIT simple_expr
      { mkexpr(Pexpr_emit $2 ) }
  | EMIT simple_expr simple_expr
      { mkexpr(Pexpr_emit_val($2, $3)) }
  | signal_kind signal_comma_list opt_at_expr opt_default_gather opt_reset IN par_expr
      { mkexpr(Pexpr_signal($1, List.rev $2, $3, $4, $5, $7)) }
  | DO par_expr pause_kind UNTIL opt_bar until_cases DONE
      { mkexpr_until $2 (List.rev $6) $3 }
  | DO par_expr WHEN simple_expr DONE
      { mkexpr(Pexpr_when($4, $2)) }
  | CONTROL par_expr WITH simple_expr DONE
      { mkexpr(Pexpr_control($4, None, $2)) }
  | CONTROL par_expr WITH simple_expr LPAREN pattern RPAREN DONE
      { mkexpr(Pexpr_control($4,
			     Some($6, mkexpr (Pexpr_constant(Const_bool true))),
			     $2)) }
  | CONTROL par_expr WITH simple_expr LPAREN pattern RPAREN WHEN par_expr DONE
      { mkexpr(Pexpr_control($4, Some ($6, $9), $2)) }

  | PRESENT par_expr THEN expr ELSE expr
      { mkexpr(Pexpr_present($2, $4, $6)) }
  | PRESENT par_expr THEN expr
      { mkexpr(Pexpr_present($2, $4, ghexpr(Pexpr_nothing))) }
  | PRESENT par_expr ELSE expr
      { mkexpr(Pexpr_present($2, ghexpr(Pexpr_nothing), $4)) }
  | AWAIT await_flag simple_expr
      { if (snd $2) = One
        then raise(Syntaxerr.Error(Syntaxerr.Other (rhs_loc 2)))
        else mkexpr(Pexpr_await(fst $2, $3)) }
  | AWAIT await_flag simple_expr LPAREN pattern RPAREN IN par_expr
      { match $2 with
        | Immediate, All -> raise(Syntaxerr.Error(Syntaxerr.Other (rhs_loc 2)))
	| im, k -> mkexpr(Pexpr_await_val(im, k, $3, $5, $8)) }
  | AWAIT await_flag simple_expr LPAREN pattern RPAREN WHEN par_expr IN par_expr
      { match $2 with
        | Immediate, All -> raise(Syntaxerr.Error(Syntaxerr.Other (rhs_loc 2)))
	| im, k ->
	    mkexpr(Pexpr_await_val(im, k, $3, $5,
				   { pexpr_desc = Pexpr_when_match($8, $10);
				     pexpr_loc = Location.none; })) }
  | PROCESS proc_def
      { $2 }
  | PROC simple_pattern proc_fun_def
      { mkexpr(Pexpr_function([$2, $3])) }
  | RUN simple_expr simple_expr_list
      { let e = mkexpr(Pexpr_apply($2, List.rev $3)) in
        mkexpr(Pexpr_run(e)) }
  | RUN simple_expr
      { mkexpr(Pexpr_run($2)) }
  | NEWCLOCK LIDENT opt_schedule IN par_expr
      { mkexpr (Pexpr_newclock (mksimple $2 2, $3, None, $5)) }
  | DOMAIN LIDENT opt_by opt_schedule DO par_expr DONE
      { mkexpr (Pexpr_newclock (mksimple $2 2, $4, $3, $6)) }
  | pause_kind PAUSE clock_expr
      { mkexpr (Pexpr_pause ($3, $1)) }
;
simple_expr:
    val_longident
      { mkexpr(Pexpr_ident $1) }
  | constant
      { mkexpr(Pexpr_constant $1) }
  | constr_longident %prec prec_constant_constructor
      { mkexpr(Pexpr_construct($1, None)) }
  | LPAREN par_expr RPAREN
      { reloc_expr $2 }
  | LPAREN par_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN par_expr END
      { reloc_expr $2 }
  | BEGIN END
      { mkexpr (Pexpr_constant Const_unit) }
  | BEGIN par_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN par_expr type_constraint RPAREN
      { mkexpr(Pexpr_constraint($2, $3)) }
  | simple_expr DOT label_longident
      { mkexpr(Pexpr_record_access($1, $3)) }
  | simple_expr DOT LPAREN par_expr RPAREN
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "Array" "get")),
                           [$1; $4])) }
  | simple_expr DOT LPAREN par_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET par_expr RBRACKET
      { mkexpr(Pexpr_apply(ghexpr(Pexpr_ident(array_function "String" "get")),
                           [$1; $4])) }
  | simple_expr DOT LBRACKET par_expr error
      { unclosed "[" 3 "]" 5 }
  | LBRACE record_expr RBRACE
      { mkexpr(Pexpr_record($2)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 5 }
 | LBRACE simple_expr WITH record_expr RBRACE
      { mkexpr(Pexpr_record_with ($2, $4)) }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexpr(Pexpr_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexpr(Pexpr_array []) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { reloc_expr (mktailexpr (List.rev $2)) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexpr(Pexpr_apply(mkoperator $1 1, [$2])) }
  | NOTHING
      { mkexpr Pexpr_nothing }
  | TOPCK
      { mkexpr Pexpr_topck }
  | GLOBAL_CK
      { mkexpr Pexpr_topck }
  | LOCAL_CK
      { mkexpr Pexpr_base }
  | HALT
      { mkexpr Pexpr_halt }
  | LOOP par_expr END
      { mkexpr (Pexpr_loop $2) }
  | SHARP ident
      { match $2 with
        | "suspend" ->
	    mkexpr
	      (Pexpr_apply
		 (mkexpr (Pexpr_ident
			    (mkident (Pdot("Rmltop_controller",
					   "set_suspend")) 2)),
		  [mkexpr (Pexpr_constant Const_unit)]))
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
	| _ -> raise (Syntaxerr.Error(Syntaxerr.Other (rhs_loc 2))) }
;
pre_expr:
    simple_expr
      { Status, $1 }
  | QUESTION simple_expr
      { Value, $2 }
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
clock_expr:
    /* empty */ { mkexpr Pexpr_base }
  | simple_expr { $1 }
;
pause_kind:
| /*empty*/ { Strong }
| QUIET { Weak }
;
opt_schedule:
   /* empty */ { None}
  | SCHEDULE par_expr { Some $2 }
opt_by:
  /* empty */ { None }
  | BY par_expr { Some $2 }
opt_reset:
  /* empty */ { None }
  | RESET simple_expr { Some $2 }
opt_default_gather:
  | /*empty*/ { None }
  | DEFAULT par_expr GATHER par_expr { Some ($2, $4) }
signal_kind:
  | SIGNAL { Signal }
  | MEMORY { Memory }
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
  | val_longident LESS pattern GREATER
      {	[$3, { pexpr_desc = Pexpr_get (mkexpr(Pexpr_ident $1));
	       pexpr_loc = rhs_loc 1; }] }
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
;
let_binding:
    val_ident fun_binding
      { ({ppatt_desc = Ppatt_var $1; ppatt_loc = rhs_loc 1}, $2) }
  | pattern EQUAL par_expr
      { ($1, $3) }
  | PROCESS val_ident proc_binding
      { ({ppatt_desc = Ppatt_var $2; ppatt_loc = rhs_loc 2}, $3) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL par_expr
      { ghexpr(Pexpr_constraint($3, $1)) }
;
strict_binding:
    EQUAL par_expr
      { $2 }
  | simple_pattern fun_binding
      { ghexpr(Pexpr_function([$1, $2])) }
;
proc_binding:
    strict_proc_binding
      { $1 }
  | type_constraint EQUAL par_expr
      { ghexpr(Pexpr_constraint(ghexpr(Pexpr_process($3)), $1)) }
;
strict_proc_binding:
    EQUAL par_expr
      { ghexpr(Pexpr_process($2)) }
  | simple_pattern proc_binding
      { ghexpr(Pexpr_function([$1, $2])) }
;
match_cases:
    pattern match_action                        { [$1, $2] }
  | match_cases BAR pattern match_action        { ($3, $4) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def
      { ghexpr(Pexpr_function([$1, $2])) }
;
proc_fun_def:
    MINUSGREATER par_expr                       { mkexpr (Pexpr_process $2) }
  | simple_pattern proc_fun_def
      { ghexpr(Pexpr_function([$1, $2])) }
;
proc_def:
    simple_expr                                 { mkexpr(Pexpr_process $1) }
/*
    MINUSGREATER par_expr                       { mkexpr(Pexpr_process $2) }
  | simple_pattern proc_def
      { ghexpr(Pexpr_function([$1, $2])) }
*/
;
match_action:
    MINUSGREATER par_expr                       { $2 }
  | WHEN par_expr MINUSGREATER par_expr         { mkexpr(Pexpr_when_match
							   ($2, $4)) }
;
until_action:
    /* empty */                     { mkexpr Pexpr_nothing }
  | MINUSGREATER par_expr                       { $2 }
  | WHEN par_expr
      { mkexpr(Pexpr_when_match($2, { pexpr_desc = Pexpr_nothing;
				      pexpr_loc = Location.none; })) }
  | WHEN par_expr MINUSGREATER par_expr         { mkexpr(Pexpr_when_match
							   ($2, $4)) }
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
    simple_expr                                 { [$1, None] }
  | until_handlers                              { $1 }
;
until_handlers:
    simple_expr LPAREN pattern RPAREN until_action
                                              { [$1, Some($3, $5)] }
  | simple_expr MINUSGREATER par_expr
                                              { [$1,
						 Some(mkpatt(Ppatt_any), $3)] }
/*
  | until_handlers BAR simple_expr LPAREN pattern RPAREN MINUSGREATER par_expr
                                              { ($3, Some($5, $8)) :: $1 }
*/
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpatt(Ppatt_alias($1, $3)) }
  | pattern_comma_list  %prec below_COMMA
      { mkpatt(Ppatt_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpatt(Ppatt_construct($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpatt(Ppatt_construct(mkident (Pident "::") 2,
			       Some(ghpatt(Ppatt_tuple[$1;$3])))) }
  | pattern BAR pattern
      { mkpatt(Ppatt_or($1, $3)) }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpatt(Ppatt_var $1) }
  | UNDERSCORE
      { mkpatt(Ppatt_any) }
  | signed_constant
      { mkpatt(Ppatt_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepatt $1 $3 }
  | constr_longident
      { mkpatt(Ppatt_construct($1, None)) }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpatt(Ppatt_record(List.rev $2)) }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { reloc_patt (mktailpatt (List.rev $2)) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpatt(Ppatt_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpatt(Ppatt_array []) }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { reloc_patt $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON poly_type RPAREN
      { mkpatt(Ppatt_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
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
    type_parameters LIDENT opt_clock_effect_parameters type_kind
      { let type_params = List.map (fun x -> Kclock x) $1 in
        (mksimple $2 2, type_params @ $3, $4) }
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
  | type_var                                    { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter_list:
    type_var                                    { [$1] }
  | type_parameter_list COMMA type_var      { $3 :: $1 }
;
opt_clock_effect_parameters:
  | /* empty */ { [] }
  | LBRACE clock_parameters BAR clock_row_parameters BAR effect_row_parameters RBRACE
      { let clock_params = List.map (fun x -> Kcarrier x) $2 in
        let clock_row_params = List.map (fun x -> Kcarrier_row x) $4 in
        let effect_params = List.map (fun x -> Keffect_row x) $6 in
         clock_params @ clock_row_params @ effect_params }
  | LBRACE clock_parameters BARBAR effect_row_parameters RBRACE
      { let clock_params = List.map (fun x -> Kcarrier x) $2 in
        let effect_params = List.map (fun x -> Keffect_row x) $4 in
         clock_params @ effect_params }
;
clock_parameters:
    /*empty*/                                    { [] }
  | clock_parameter_list                         { List.rev $1 }
;
clock_parameter_list:
    clock_var                                    { [$1] }
  | clock_parameter_list COMMA clock_var    { $3 :: $1 }
;
clock_row_parameters:
    /*empty*/                                    { [] }
  | clock_row_parameter_list                     { List.rev $1 }
;
clock_row_parameter_list:
    clock_row_var                                    { [$1] }
  | clock_row_parameter_list COMMA clock_row_var     { $3 :: $1 }
;
effect_row_parameters:
    /*empty*/                                   { [] }
  | effect_row_parameter_list           { List.rev $1 }
;
effect_row_parameter_list:
    effect_row_var                                    { [$1] }
  | effect_row_parameter_list COMMA effect_row_var    { $3 :: $1 }
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
    mutable_flag label COLON poly_type          { ($2, $1, $4) }
;

/* Core types */
poly_type:
  | core_type { $1 }
  | annot_vars DOT core_type
      { mkte (Ptype_forall ($1, $3)) }
;
core_type:
  | core_type_na { $1 }
  | core_type_na MINUSGREATER core_type
      { (* ct1 -> ct2 is ct1 =>{ <0> } ct2  *)
        let eff_empty = mkeer (Peff_row_one (mkee Peff_empty)) in
        let eer = mkeer (Peff_row (eff_empty, mkeer Peff_row_empty)) in
        mkte(Ptype_arrow($1, $3, eer)) }
  | core_type_na EQUALGREATER core_type
      { mkte(Ptype_arrow($1, $3, mkeer Peff_row_fresh)) }
  | core_type_na EQUALGREATERLBRACE effect_row RBRACE core_type
      { mkte(Ptype_arrow($1, $5, $3)) }
;

core_type_na:
  | simple_type
      { $1 }
  | type_star_list
      { mkte(Ptype_tuple(List.rev $1)) }
;
two_bars: BAR BAR { () } | BARBAR { () }
simple_type:
  | type_var
      { mkte (Ptype_var $1) }
  | LBRACKET clock_row_type RBRACKET
      { mkte (Ptype_depend $2) }
  | type_longident clock_effect_params
      { mkte(Ptype_constr($1, $2)) }
  | type_params type_longident clock_effect_params
      { mkte(Ptype_constr($2, $1 @ $3)) }
  | LPAREN core_type RPAREN
      { $2 }
  | simple_type PROCESS
      { mkte(Ptype_process ($1, Static.Dontknow, mkce Pcar_fresh, mkeer Peff_row_fresh)) }
  | simple_type PROCESS LBRACE clock_type_or_empty two_bars effect_row_or_empty RBRACE
      { mkte(Ptype_process ($1, Static.Dontknow, $4, $6)) }
;

type_params:
  | simple_type
      { [Kclock $1] }
  | LPAREN core_type COMMA core_type_comma_list RPAREN
      { let l = $2::$4 in List.map (fun x -> Kclock x) l }

clock_effect_params:
  | /*empty*/  { [] }
  | LBRACE clock_type_list BARBAR effect_row_list RBRACE { $2 }
  | LBRACE clock_type_list BAR clock_row_list BAR effect_row_list RBRACE { $2 @ $4 @ $6 }
;
type_var:
  | QUOTE ident { $2 }
;
clock_var:
  | QUOTE QUOTE ident { $3 }
;
clock_row_var:
  | QUOTE ident { $2 }
;
effect_var:
  | QUOTE QUOTE ident { $3 }
;
effect_row_var:
  | QUOTE ident { $2 }
;
annot_vars:
  | /* empty*/ { [] }
  | annot_var annot_vars { $1::$2 }
;
annot_var:
  | clock_var { Kcarrier (mkce (Pcar_var $1)) }
  | clock_row_var { Kcarrier_row (mkcer (Pcar_row_var $1)) }
;
clock_type:
  | clock_var { mkce (Pcar_var $1) }
  | TOPCK { mkce (Pcar_topck) }
  | GLOBAL_CK { mkce (Pcar_topck) }
;
clock_row_type:
  | LIDENT { mkcer (Pcar_row_ident $1) }
  | clock_row_var { mkcer (Pcar_row_var $1) }
  | clock_type { mkcer (Pcar_row (mkcer (Pcar_row_one $1), mkcer Pcar_row_empty)) }
  | clock_type SEMI ne_clock_row { mkcer (Pcar_row (mkcer (Pcar_row_one $1), $3)) }
;
ne_clock_row:
  | DOTDOT { mkcer Pcar_row_fresh }
  | clock_type SEMI ne_clock_row { mkcer (Pcar_row (mkcer (Pcar_row_one $1), $3)) }
;
effect:
  | simple_effect { $1 }
  | effect PLUS effect { mkee (Peff_sum ($1, $3)) }
;
simple_effect:
 | effect_var { mkee (Peff_var $1) }
 | LESS clock_row_type GREATER { mkee (Peff_depend $2) }
 | LBRACE effect_row RBRACE { mkee (Peff_one $2) }
 | LPAREN effect RPAREN  { $2 }
;
effect_row:
 | effect_row_var { mkeer (Peff_row_var $1) }
 | effect { mkeer (Peff_row (mkeer (Peff_row_one $1), mkeer Peff_row_empty)) }
 | effect SEMI ne_effect_row { mkeer (Peff_row (mkeer (Peff_row_one $1), $3))  }
;
ne_effect_row:
 | DOTDOT { mkeer Peff_row_fresh }
 | effect SEMI ne_effect_row { mkeer (Peff_row (mkeer (Peff_row_one $1), $3)) }
;

label:
    LIDENT                                      { mksimple $1 1 }
;

clock_type_or_empty:
  | /* empty */ { mkce Pcar_fresh }
  | clock_type  { $1 }
;
effect_row_or_empty:
  | /* empty */ { mkeer Peff_row_fresh }
  | effect_row  { $1 }
;


type_star_list :
  | type_star_list STAR simple_type
      { $3 :: $1 }
  | simple_type STAR simple_type
      { [$3;$1] }
;

core_type_comma_list :
  | core_type COMMA core_type_comma_list
      { $1 :: $3 }
  | core_type
      { [$1] }
;
clock_type_comma_list :
  | clock_type COMMA clock_type_comma_list
      { $1 :: $3 }
  | clock_type
      { [$1] }
;
clock_row_comma_list :
  | clock_row_type COMMA clock_row_comma_list
      { $1 :: $3 }
  | clock_row_type
      { [$1] }
;
effect_row_comma_list :
  | effect_row COMMA effect_row_comma_list
      { $1 :: $3 }
  | effect_row
      { [$1] }
;

clock_type_list:
  | /* empty */ { [] }
  | clock_type_comma_list { List.map (fun x -> Kcarrier x) $1 }
;
clock_row_list:
  | /* empty */ { [] }
  | clock_row_comma_list { List.map (fun x -> Kcarrier_row x) $1 }
;
effect_row_list:
  | /* empty */ { [] }
  | effect_row_comma_list { List.map (fun x -> Keffect_row x) $1 }
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
    LIDENT                                      { mksimple $1 1 }
  | LPAREN operator RPAREN                      { mksimple $2 2 }
;
val_ident_colon:
    LIDENT COLON                                { mksimple $1 1 }
  | LPAREN operator RPAREN COLON                { mksimple $2 2 }
  | LABEL                                       { mksimple $1 1 }
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
    UIDENT                                      { mksimple $1 1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | COLONCOLON                                  { mksimple "::" 1 }
;

val_longident:
    val_ident
      { mkident (Pident $1.psimple_id) 1 }
  | UIDENT DOT val_ident
      { mkident_loc (Pdot($1, $3.psimple_id)) (symbol_rloc()) }
;
constr_longident:
    UIDENT             %prec below_DOT
      { mkident (Pident $1) 1 }
  | UIDENT DOT UIDENT
      { mkident_loc (Pdot($1, $3)) (symbol_rloc()) }
  | LBRACKET RBRACKET
      { mkident_loc (Pident "[]") (symbol_rloc()) }
;
label_longident:
    LIDENT
      { mkident (Pident $1) 1 }
  | UIDENT DOT LIDENT
      { mkident_loc (Pdot($1, $3)) (symbol_rloc()) }
;
type_longident:
    LIDENT
      { mkident (Pident $1) 1 }
  | UIDENT DOT LIDENT
      { mkident_loc (Pdot($1, $3)) (symbol_rloc()) }
;

/* Signals */
signal_decl:
    LIDENT                                      { (mksimple $1 1, None) }
  | LIDENT COLON core_type
      { (mksimple $1 1, Some $3) }
;
signal_comma_list:
    signal_decl                                 { [$1] }
  | signal_comma_list COMMA signal_decl         { $3 :: $1}
;
at_or_clock:
  | AT { () }
  | CLOCK { () }
opt_at_expr:
    /* empty */       { mkexpr Pexpr_base, mkexpr Pexpr_base }
  | at_or_clock simple_expr { $2, $2 }
  | at_or_clock simple_expr REGION simple_expr { $2, $4 }
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
    LIDENT                                      { mksimple $1 1 }
  | UIDENT                                      { mksimple $1 1 }
;
/* string list */
lucky_files:
  | LBRACKET string_semi_list opt_semi RBRACKET
      { List.rev $2 }
  | LBRACKET string_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
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
