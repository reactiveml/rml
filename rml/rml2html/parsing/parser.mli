type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | AWAIT
  | BACKQUOTE
  | BAR
  | BARBAR
  | BARRBRACKET
  | BARGRATER
  | BEGIN
  | CHAR of (char)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | CONTROL
  | DEFAULT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | EMIT
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (float)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GATHER
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IMMEDIATE
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INOUT
  | INT of (int)
  | INT32 of (int32)
  | INT64 of (int64)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LOOP
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint)
  | NEW
  | NOTHING
  | OBJECT
  | OF
  | ONE
  | OPEN
  | OPTLABEL of (string)
  | OR
  | OUT
  | PAUSE
  | PLUS
  | PRE
  | PREFIXOP of (string)
  | PRESENT
  | PRIVATE
  | PROCESS
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | RUN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | SIGNAL
  | STAR
  | STRING of (string)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | UNTIL
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

val implementation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parse_ast.implementation
val interface :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parse_ast.interface
