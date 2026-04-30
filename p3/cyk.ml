(*TIPOS DE SÍMBOLOS EN LA GRAMÁTICA*)

type symbol = T of char | NT of char

(*T : Terminal (minúsculas)*)
(*NT : No Terminal (mayúsculas)*)

type rule = { (*ejemplo del pdf: S->AB*)
    left: symbol; (*parte izquierda de la regla: S*)
    right: symbol list (*parte derecha de la regla: AB*)
}

type grammar = {
    start: symbol; (*símbolo inicial de la gramática*)
    rules: rule list (*lista de reglas de la gramática*)
}

(*==================Ejercicio 1=====================*)

let char_to_symbol c =
    if c>='a' && c<='z' then T c (*si el char es minúscula entonces es TERMINAL*)
    else if c>='A' && c<='Z' then NT c (*si el char es mayúscula entonces es NO TERMINAL*)
    else failwith "Formato de gramática incorrecto";;

let parse_rule line = (*supuestamente las reglas están en formato SAB y la regla sería S->AB*)
    if String.length line < 2 then failwith "Formato de regla incorrecto" (*si la línea es menor a 2 caracteres ya no vale*)
    else
        let left = char_to_symbol line.[0] in (*el primer char es el símbolo izquierdo de la regla*)
        match left with
        | T _ -> failwith "El símbolo izquierdo no puede ser terminal" (*si el símbolo izquierdo es terminal ya no vale*)
         
        | NT _ ->
                let right = List.init (String.length line - 1) (fun i -> char_to_symbol line.[i + 1]) in (*los chars a partir del segundo char ya son la parte derecha de la regla*)
                (*List.init n f crea una lista de tamaño n, aplicando la función f a cada elemento*)

                {left; right};; (*devuelvo un record con el símbolo izquierdo y la lista de símbolos derechos*)
                (*con SAB la S ya quedaría en left y right quedaría como [A; B]*)

let parser lines =
    match lines with
    | [] -> failwith "Archivo vacío" (*si el archivo no tiene líneas ya no vale*)
    | first_line :: rule_lines ->
            let axioma = char_to_symbol first_line.[0] in (*el primer símbolo de la primera línea del archivo es el símbolo inicial de la gramática*)
            let rules = List.map parse_rule rule_lines in (*el resto de las líneas son las reglas de la gramática*)
            (*List.map f l crea una lista aplicando la función f a cada elemento de l*)
            {axioma; rules};; (*devuelvo un record con el símbolo inicial y la lista de reglas*)

let read_file file =
    try
        let channel = open_in file in (*open_in abre el fichero para leer y devuelve input channel*)
        let rec aux acc = (*en el acc guardo la lista de líneas*)
            try
            let line = input_line channel in (*input_line lee una línea del fichero hasta \n y devuelve string*)
            aux (line :: acc) 
            with End_of_file -> (*si termino de leer el file*)
            close_in channel; (*close_in cierra el fichero y libera el recurso*)
            List.rev acc (*invierto la lista para que salga en orden*)
        in
        aux []
    with Sys_error err ->
        Printf.printf "Error al abrir el archivo: %s\n" err;
        exit 1;;

let is_cnf g = (*la FNC es de la siguiente forma según la teoría:*)
    (*si las reglas A -> son de la siguiente forma:
    . si w es un solo caracter, tiene que ser sí o sí TERMINAL
    . si |w| > 1, entonces w debe ser una secuencia de DOS NO TERMINALES X1X2
        * si X1 o X2 son terminales, entonces X1 -> T ó X2 -> T 
        * si es NT se va binarizando*)
    let is_valid_rule rule =
        match rule.right with
        | [T _] -> true (*si la parte derecha es un solo terminal, es válida*)
        | [NT _; NT _] -> true (*si la parte derecha son dos no terminales, es válida*)
        | _ -> false (*cualquier otra forma no es válida*)
    in
    List.for_all is_valid_rule g.rules;; (*List.for_all f l devuelve true si f devuelve true para todos los elementos de l*)

let cykg file = 
    let lines = read_file file in (*leo el archivo*)
    let g = parser lines in (*parseo las líneas para crear la gramática*)
    if is_cnf g then
        Printf.printf "yes\n"
    else
        Printf.printf "no\n"

(*==================Ejercicio 2=====================*)


(*==================Ejercicio 3=====================*)


(*==================Ejercicio 4=====================*)

let () = 
	if Array.length Sys.argv = 3 then
		let param = Sys.argv.(1) in
        let file = Sys.argv.(2) in
		match param with
        | "-g" -> cykg file
        | "-p" -> Printf.printf "aun no implementado \n";
        | _ -> Printf.printf "Usage: cyk -g <file> | -p <file> \n";;
	else
		Printf.printf "Usage: cyk -g <file> | -p <file> \n";;

