type symbol = Char of char | Range of char * char;;
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

val symbol_of_char  : char -> symbol
val symbol_of_range : char -> char -> symbol

val empty        : regexp
val empty_string : regexp
val single       : symbol -> regexp
val except       : symbol -> regexp
val any          : regexp
val concat       : regexp -> regexp -> regexp
val repeat       : regexp -> regexp
val alt          : regexp -> regexp -> regexp
val all          : regexp -> regexp -> regexp

