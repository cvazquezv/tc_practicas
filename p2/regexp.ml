(*EJERCICIO 1*)
(*=============TIPOS=============*)
(*Definir el tipo de SYMBOL*)
type symbol = Char of char | Range of char * char;;

(*Definir el tipo de REGEXP*)
type regexp = 
  | Empty (* conjunto vacío *)
  | Epsilon (* cadena vacía *)
  | Single of symbol (* un único carácter o rango *)
  | Except of symbol (* todo carácter excepto... : ^c *)
  | Any (* cualquier carácter: . *)
  | Concat of regexp * regexp (* concatenación: r·s *)
  | Repeat of regexp (* repetición: r* *)
  | Alt of regexp * regexp (* or: r + s *)
  | All of regexp * regexp (* and: r & s *);;


(*=============FUNCIONES=============*)
(*val symbol_of_char  : char -> symbol*)
let symbol_of_char c = Char c;;

(*val symbol_of_range : char -> char -> symbol*)
let symbol_of_range c1 c2 = Range (c1, c2);;

(*val empty        : regexp*)
let empty = Empty;;

(*val empty_string : regexp*)
let empty_string = Epsilon;;

(*val single       : symbol -> regexp*)
let single c = Single c;;

(*val except       : symbol -> regexp*)
let Except c = Except c;;

(*val any          : regexp*)
let any = Any;;

(*val concat       : regexp -> regexp -> regexp*)
let concat r s = Concat (r, s);;

(*val repeat       : regexp -> regexp*)
let repeat r = Repeat r;;

(*val alt          : regexp -> regexp -> regexp*)
let alt r s = Alt (r, s);;

(*val all          : regexp -> regexp -> regexp*)
let all r s = All (r, s);;