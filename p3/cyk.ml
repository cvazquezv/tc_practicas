(*TIPOS DE SÍMBOLOS EN LA GRAMÁTICA*)

type symbol = T of char | NT of char

(*T : Terminal (minúsculas)*)
(*NT : No Terminal (mayúsculas)*)

type rule = { (*ejemplo del pdf: S->AB*)
    left: symbol; (*parte izquierda de la regla: S*)
    right: symbol list (*parte derecha de la regla: AB*)
}

type grammar = {
    axioma: symbol; (*símbolo inicial de la gramática*)
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
let parse_string s = 
    if String.length s = 0 || not (String.for_all (fun c -> c>='a' && c<='z' ) s) then failwith "Cadena no válida" (*si la cadena es vacía ya no vale*)
    else List.init (String.length s) (fun i -> char_to_symbol s.[i]);; (*convierto cada char de la cadena a símbolo y lo guardo en una lista*)


let nt_t g terminal = (*devuelve la lista de no terminales que generan el terminal dado*) (*para la primera fila de la torre*)
    let rec aux rules acc =
        match rules with
        | [] -> acc (*si ya no quedan más reglas*)
        | rule :: rest ->
            match rule.right with
            | [T t] when t = terminal -> aux rest (rule.left :: acc) (*si la parte derecha de la regla es el terminal que buscamos, agregamos el símbolo izquierdo a la lista de no terminales*)
            | _ -> aux rest acc (*si no, seguimos buscando*)
    in
    aux g.rules [];; (*iniciamos la búsqueda con la lista de reglas y una lista vacía para acumular los no terminales encontrados*)

let nt_nt g nt1 nt2 = (*devuelve la lista de no terminales que generan la secuencia de dos no terminales dada*) (*para las filas siguientes de la torre*)
    let rec aux rules acc =
        match rules with
        | [] -> acc (*si ya no quedan más reglas*)
        | rule :: rest ->
            match rule.right with
            | [n1; n2] when n1 = nt1 && n2 = nt2 -> aux rest (rule.left :: acc) (*si la parte derecha de la regla es la secuencia de no terminales que buscamos, agregamos el símbolo izquierdo a la lista de no terminales*)
            | _ -> aux rest acc (*si no, seguimos buscando*)
    in
    aux g.rules [];; (*iniciamos la búsqueda con la lista de reglas y una lista vacía para acumular los no terminales encontrados*)

let combine_cells g cell1 cell2 = (*combina las celdas para obtener los no terminales que generan la secuencia de símbolos*)
    let rec aux1 l1 acc = (*recorre los NT de la primera celda*)
        match l1 with
        | [] -> acc (*si ya no quedan más no terminales en la primera celda*)
        | nt1 :: rest1 ->
            let rec aux2 l2 acc2 = (*combina nt1 con todos los NT de la segunda celda*)
                match l2 with
                | [] -> aux1 rest1 acc2 
                (*cuando se termina la segunda celda, seguimos con el siguiente símbolo de la primera*)
                | nt2 :: rest2 ->
                    let nts = nt_nt g nt1 nt2 in (*obtenemos los no terminales que generan la secuencia de nt1 y nt2*)
                    aux2 rest2 (nts @ acc2) (*agregamos los no terminales encontrados a la lista acumulada*)
            in
            aux2 cell2 acc (*iniciamos la búsqueda con la segunda celda*)
    in
    aux1 cell1 [];; (*iniciamos la búsqueda con la primera celda*)

let cyk g cadena =
    let string = Array.of_list cadena in (*convertimos la lista de símbolos a un array para facilitar el acceso por índices*)
    let n = Array.length string in (*obtenemos la longitud de la cadena*)
    let table = Array.make_matrix n n [] in (*creamos una tabla de tamaño n x n para almacenar los no terminales que generan cada subcadena*)
    (*table.(i).(j) contiene la lista de no terminales que generan la subcadena desde i hasta j*)
    
    (*llenamos la primera fila de la tabla con los no terminales que generan cada símbolo terminal de la cadena*)
    for i = 0 to n-1 do
        match string.(i) with
        | T t -> table.(i).(0) <- nt_t g t (*si el símbolo es terminal, obtenemos los no terminales que lo generan y los guardamos en la primera fila de la tabla*)
        | NT _ -> failwith "La cadena no puede contener símbolos no terminales" (*si la cadena contiene símbolos no terminales ya no vale*)
    done;

    (*llenamos el resto de la tabla combinando las celdas anteriores*)
    for j = 1 to n-1 do 
        for i = 0 to n-j-1 do
            for k = 0 to j-1 do
                let left_cell = table.(i).(k) in 
                let right_cell = table.(i+k+1).(j-k-1) in
                let combined = combine_cells g left_cell right_cell in (*combinamos las celdas para obtener los no terminales que generan la subcadena desde i hasta i+j*)
                table.(i).(j) <- combined @ table.(i).(j) (*agregamos los no terminales encontrados a la celda actual*)
            done
        done
    done;

    List.mem g.axioma table.(0).(n-1) (*verificamos si el símbolo inicial de la gramática está en la celda que genera toda la cadena, es decir, en table.(0).(n-1)*)

    (*al final, verificamos si el símbolo inicial de la gramática está en la celda que genera toda la cadena*)
let loop g = (*se leen las cadenas del stdin*)
    try
        while true do
            let line = input_line stdin in  (*leemos una línea del input*)
            (try
                let cadena = parse_string line in     (*convertimos el string en lista de símbolos*)
                if cyk g cadena then        (*aplicamos CYK*)
                    Printf.printf "yes\n"
                else
                    Printf.printf "no\n"
            with Failure err ->
                Printf.printf "Error: %s\n" err)      (*si hay error en la cadena, lo mostramos*)
        done
    with End_of_file -> ()  (*cuando no hay más líneas, terminamos silenciosamente*)


let cykp file = 
    let lines = read_file file in (*leo el archivo*)
    match lines with
    | [] -> failwith "Archivo vacío" (*si el archivo no tiene líneas ya no vale*)
    | _ -> 
        let g = parser lines in (*parseo las líneas para crear la gramática*)
        if not (is_cnf g) then failwith "La gramática no está en FNC" (*si la gramática no está en FNC ya no vale*)
        else
            loop g;;

(*==================Ejercicio 3=====================*)


(*==================Ejercicio 4=====================*)

let () = 
	if Array.length Sys.argv = 3 then (
		let param = Sys.argv.(1) in
        let file = Sys.argv.(2) in
		match param with
        | "-g" -> cykg file
        | "-p" -> cykp file;
        | _ -> Printf.printf "Usage: cyk -g <file> | -p <file> \n"
    )
    else Printf.printf "Usage: cyk -g <file> | -p <file> \n";;

