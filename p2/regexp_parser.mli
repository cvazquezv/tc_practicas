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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Regexp.regexp
