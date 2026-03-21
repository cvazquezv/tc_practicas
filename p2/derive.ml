(* EJERCICIO 2 *)
open Regexp;;

(*val regexp_of_string : string -> Regexp.regexp*)
(*construye valor tipo regexp a partir de un string con una expresión regular
en formato Posix utilizando el parser*)
let regexp_of_string s = Regexp_parser.main Regexp_lexer.token (Lexing.from_string s);;

(*val nullable         : Regexp.regexp -> Regexp.regexp*)
(*para una expresión regular r devuelva v(r)*)
let nullable r = 
match r with
| Empty -> Empty (*ν(∅) = ∅*)
| Epsilon -> Epsilon (*ν(ε) = ε*)
| Single _ | Any | Except _ -> Empty (*ν(a) = ν(.) = ν(^a) = ∅*)
| Concat (r1, r2) -> if nullable r1 = Epsilon && nullable r2 = Epsilon then Epsilon else Empty (*ν(r·s) = ν(r)·ν(s)*)
| Repeat _ -> Epsilon (*ν(r* ) = ε*)
| Alt (r1, r2) -> if nullable r1 = Epsilon || nullable r2 = Epsilon then Epsilon else Empty (*ν(r + s) = ε si ν(r) = ε ó ν(s) = ε | ∅ en otro caso*)
;;

(*val derive           : char -> Regexp.regexp -> Regexp.regexp*)
(*para un carácter c y una expresión regular r devuelva  ∂c(r)*)
let derive c r = 
match r with
| Empty -> Empty (*∂c(∅) = ∅*)
| Epsilon -> Empty (*∂c(ε) = ∅*)
| Single a when a = c -> Epsilon (*∂c(a) = ε si a = c, ∅ en otro caso*)
| Single _ -> Empty (*∂c(a) = ∅ si a ≠ c*)
| Except a when a = c -> Empty (*∂c(^a) = ∅ si a = c, ε en otro caso*)
| Except _ -> Epsilon (*∂c(^a) = ε si a ≠ c*)
| Any -> Epsilon (*∂c(.) = ε*)
| Concat (r1, r2) -> Alt (Concat (derive c r1, r2), Concat (nullable r1, derive c r2)) (*∂c(r·s) = ∂c(r)·s + ν(r)·∂c(s)*)
| Repeat r -> Concat (derive c r, Repeat r) (*∂c(r* ) = ∂c(r)·r* *)
| Alt (r1, r2) -> Alt (derive c r1, derive c r2) (*∂c(r + s) = ∂c(r) + ∂c(s)*)
| All (r1, r2) -> All (derive c r1, derive c r2) (*∂c(r & s) = ∂c(r) & ∂c(s)*)
;;

(*val matches_regexp   : string -> Regexp.regexp -> bool*)
(*calcula si un string encaja con una expresión regular*)
(*Una cadena formada por los caracteres c1c2...cn encaja con una expresi´on regular r si la
derivada de r para toda la cadena acepta ε, esto es, si se cumple que:
ν(∂cn(...∂c2(∂c1(r)))) = ε*)
let matches_regexp str r = 
    match str with
    | "" -> nullable r = Epsilon (*si la cadena es vacía, verificamos si la expresión regular acepta ε*)
    | _ -> let primero = str.[0] in
           let resto = String.sub str 1 (String.length str - 1) in
           matches_regexp resto (derive primero r) (*vamos derivando en cadena desde el primer caracter*)
;;

(*val matches          : string -> string -> bool*)
(*donde matches str1 str2 calcula si str1 encaja con la expresióón
regular en formato Posix de str2*)
let matches str1 str2 = false;;