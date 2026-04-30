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

let read_file

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

