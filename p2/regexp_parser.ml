type token =
  | PLUS
  | ASTERISK
  | LEFTPAR
  | RIGHTPAR
  | LEFTBRACKET
  | RIGHTBRACKET
  | BACKSLASH
  | QUESTION
  | MINUS
  | ACCENT
  | END
  | DOT
  | CHAR of (char)
  | NOTOR
  | OR

open Parsing;;
let _ = parse_error;;
# 2 "regexp_parser.mly"
open Regexp

let escaped_char = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | 'b' -> '\b'
  |  c  -> c
# 30 "regexp_parser.ml"
let yytransl_const = [|
  257 (* PLUS *);
  258 (* ASTERISK *);
  259 (* LEFTPAR *);
  260 (* RIGHTPAR *);
  261 (* LEFTBRACKET *);
  262 (* RIGHTBRACKET *);
  263 (* BACKSLASH *);
  264 (* QUESTION *);
  265 (* MINUS *);
  266 (* ACCENT *);
  267 (* END *);
  268 (* DOT *);
  270 (* NOTOR *);
  271 (* OR *);
    0|]

let yytransl_block = [|
  269 (* CHAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\006\000\006\000\007\000\007\000\008\000\008\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\003\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\001\000\001\000\
\001\000\003\000\001\000\002\000\001\000\002\000\001\000\001\000\
\002\000\002\000\002\000\001\000\003\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\024\000\023\000\
\032\000\000\000\000\000\028\000\000\000\000\000\015\000\016\000\
\000\000\000\000\000\000\011\000\006\000\007\000\009\000\010\000\
\008\000\012\000\014\000\013\000\001\000\000\000\026\000\025\000\
\027\000\004\000\029\000\000\000\000\000\000\000\020\000\031\000\
\005\000\022\000\030\000\018\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\016\000\017\000\018\000\019\000\037\000"

let yysindex = "\004\000\
\041\255\000\000\052\255\053\255\009\255\000\000\000\000\000\000\
\000\000\249\254\029\255\000\000\003\255\014\255\000\000\000\000\
\253\254\014\255\017\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\052\255\000\000\000\000\
\000\000\000\000\000\000\014\255\027\255\014\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\255\000\000\000\000\000\000\000\000\000\000\
\013\255\033\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\000\000\255\255\253\255\245\255\022\000\011\000"

let yytablesize = 66
let yytable = "\012\000\
\013\000\012\000\036\000\029\000\001\000\038\000\035\000\030\000\
\034\000\012\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\030\000\017\000\017\000\005\000\027\000\040\000\028\000\
\036\000\017\000\015\000\041\000\012\000\031\000\032\000\003\000\
\043\000\004\000\044\000\005\000\033\000\003\000\019\000\039\000\
\007\000\008\000\021\000\003\000\003\000\004\000\042\000\005\000\
\003\000\000\000\000\000\006\000\007\000\008\000\003\000\000\000\
\004\000\000\000\005\000\005\000\000\000\000\000\014\000\007\000\
\008\000\015\000"

let yycheck = "\001\000\
\003\000\003\000\014\000\011\001\001\000\009\001\004\001\015\001\
\011\000\011\000\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\015\001\006\001\007\001\007\001\013\001\006\001\015\001\
\036\000\013\001\013\001\030\000\030\000\001\001\002\001\003\001\
\006\001\005\001\038\000\007\001\008\001\004\001\006\001\018\000\
\012\001\013\001\006\001\003\001\011\001\005\001\036\000\007\001\
\015\001\255\255\255\255\011\001\012\001\013\001\003\001\255\255\
\005\001\255\255\007\001\007\001\255\255\255\255\010\001\012\001\
\013\001\013\001"

let yynames_const = "\
  PLUS\000\
  ASTERISK\000\
  LEFTPAR\000\
  RIGHTPAR\000\
  LEFTBRACKET\000\
  RIGHTBRACKET\000\
  BACKSLASH\000\
  QUESTION\000\
  MINUS\000\
  ACCENT\000\
  END\000\
  DOT\000\
  NOTOR\000\
  OR\000\
  "

let yynames_block = "\
  CHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 34 "regexp_parser.mly"
              ( _1 )
# 145 "regexp_parser.ml"
               : Regexp.regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "regexp_parser.mly"
              ( empty )
# 151 "regexp_parser.ml"
               : Regexp.regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "regexp_parser.mly"
                           ( _1 )
# 158 "regexp_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 39 "regexp_parser.mly"
                           ( concat _1 _2 )
# 166 "regexp_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 40 "regexp_parser.mly"
                           ( alt _1 _3 )
# 174 "regexp_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "regexp_parser.mly"
                           ( '(' )
# 180 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "regexp_parser.mly"
                           ( ')' )
# 186 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "regexp_parser.mly"
                           ( '\\' )
# 192 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "regexp_parser.mly"
                           ( '[' )
# 198 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "regexp_parser.mly"
                           ( ']' )
# 204 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "regexp_parser.mly"
                           ( '*' )
# 210 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "regexp_parser.mly"
                           ( '?' )
# 216 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "regexp_parser.mly"
                           ( '|' )
# 222 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 51 "regexp_parser.mly"
                           ( escaped_char _2 )
# 229 "regexp_parser.ml"
               : 'escaped_symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 54 "regexp_parser.mly"
                   ( _1 )
# 236 "regexp_parser.ml"
               : 'base_symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'escaped_symbol) in
    Obj.repr(
# 55 "regexp_parser.mly"
                   ( _1 )
# 243 "regexp_parser.ml"
               : 'base_symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'base_symbol) in
    Obj.repr(
# 58 "regexp_parser.mly"
                                  ( symbol_of_char _1 )
# 250 "regexp_parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'base_symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'base_symbol) in
    Obj.repr(
# 59 "regexp_parser.mly"
                                  ( symbol_of_range _1 _3 )
# 258 "regexp_parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 62 "regexp_parser.mly"
                   ( single _1 )
# 265 "regexp_parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'symbols) in
    Obj.repr(
# 63 "regexp_parser.mly"
                   ( alt (single _1) _2 )
# 273 "regexp_parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 66 "regexp_parser.mly"
                          ( except _1 )
# 280 "regexp_parser.ml"
               : 'except_symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'except_symbols) in
    Obj.repr(
# 67 "regexp_parser.mly"
                          ( all (except _1) _2 )
# 288 "regexp_parser.ml"
               : 'except_symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 71 "regexp_parser.mly"
    ( single (symbol_of_char _1) )
# 295 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "regexp_parser.mly"
    ( any )
# 301 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "regexp_parser.mly"
    ( repeat _1 )
# 308 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "regexp_parser.mly"
    ( concat _1 (repeat _1) )
# 315 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "regexp_parser.mly"
    ( alt empty_string _1 )
# 322 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'escaped_symbol) in
    Obj.repr(
# 81 "regexp_parser.mly"
    ( single (symbol_of_char _1) )
# 329 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 83 "regexp_parser.mly"
    ( _2 )
# 336 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'except_symbols) in
    Obj.repr(
# 85 "regexp_parser.mly"
    ( _3 )
# 343 "regexp_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'symbols) in
    Obj.repr(
# 87 "regexp_parser.mly"
    ( _2 )
# 350 "regexp_parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Regexp.regexp)
