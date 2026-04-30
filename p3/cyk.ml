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

(*==================Ejercicio 2=====================*)


(*==================Ejercicio 3=====================*)


(*==================Ejercicio 4=====================*)

let () = 
	if Array.length Sys.argv = 3 then
		let param = Sys.argv.(1) in
        let file = Sys.argv.(2) in
		match param with
        | "-g" ->
                let g = read_file file in
                if is_cnf g then
                    Printf.printf "yes\n"
                else
                    Printf.printf "no\n"
        | "-p" -> Printf.printf "aun no implementado \n";
        | _ -> Printf.printf "Usage: cyk -g <file> | -p <file> \n";;
	else
		Printf.printf "Usage: cyk -g <file> | -p <file> \n";;

