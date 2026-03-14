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
