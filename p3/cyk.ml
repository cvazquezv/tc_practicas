(*TIPOS DE SÍMBOLOS EN LA GRAMÁTICA*)

type symbol = T of char | NT of char

(*T : Terminal (minúsculas)*)
(*NT : No Terminal (mayúsculas)*)

type rule = { (*ejemplo del pdf: S->AB*) (*left = NT 'S'; right = [NT 'A'; NT 'B']*)
    left: symbol; (*parte izquierda de la regla: S*)
    right: symbol list (*parte derecha de la regla: AB*)
}

type grammar = {
    axioma: symbol; (*símbolo inicial de la gramática*) (*va a ser el primer símbolo de la primera regla*)
    rules: rule list (*lista de reglas de la gramática*)
}

(*open_in, close_in e input_line vinene de la librería stdlib de ocaml*)
(*==================Ejercicio 1=====================*)

let is_terminal c = 
    c >= 'a' && c <= 'z';; (*si el char es minúscula, es terminal*)

let is_non_terminal c =
    c >= 'A' && c <= 'Z';; (*si el char es mayúscula, es no terminal*)

let char_to_symbol c = (*convierte char c a symbol*)
    if is_terminal c then T c (*si el char es minúscula entonces es TERMINAL*)
    else if is_non_terminal c then NT c (*si el char es mayúscula entonces es NO TERMINAL*)
    else failwith "Caracter inválido";;


(*convertir línea de texto en regla*)
let parse_rule line = (*supuestamente las reglas están en formato SAB y la regla sería S->AB*) (*left: S; right: [A; B]*)
    try
    if String.length line < 2 then failwith "Formato de regla incorrecto" (*si la línea es menor a 2 caracteres ya no vale*)
    else
        let left = char_to_symbol line.[0] in (*el primer char es el símbolo izquierdo de la regla*)
        match left with
        | T _ -> failwith "El símbolo izquierdo no puede ser terminal" (*si el símbolo izquierdo es terminal ya no vale*)
         
        | NT _ ->
            (*crea una lista de lenght line - 1 porque ya no se cuenta la parte de la izquierda*)
                let right = List.init (String.length line - 1) (fun i -> char_to_symbol line.[i + 1]) in (*los chars a partir del segundo char ya son la parte derecha de la regla*)
                (*List.init n f crea una lista de tamaño n, aplicando la función f a cada elemento*)

                {left; right} (*devuelvo un record con el símbolo izquierdo y la lista de símbolos derechos*)
                (*con SAB la S ya quedaría en left y right quedaría como [A; B]*)
    with Failure _ -> failwith "Formato de gramática incorrecto";; (*si char_to_symbol lanza una excepción*)


    (*convertir todas las líneas del fichero en una gramática*)
let parser lines =
    match lines with
    | [] -> failwith "Archivo vacío" (*si el archivo no tiene líneas ya no vale*)
    | first_line :: _ ->
            let axioma = char_to_symbol first_line.[0] in (*el primer símbolo de la primera línea del archivo es el símbolo inicial de la gramática*)
            let rules = List.map parse_rule lines in (*el resto de las líneas son las reglas de la gramática*)
            (*List.map f l crea una lista aplicando la función f a cada elemento de l*)
            {axioma; rules};; (*devuelvo un record con el símbolo inicial y la lista de reglas*)


(*lee un fichero y devuelve una lista de líneas*)
let read_file file =
    try
        let channel = open_in file in (*open_in abre el fichero para leer y devuelve input channel (como un a "conexión")*)
        let rec aux acc = (*en el acc guardo la lista de líneas*)
            try
                let line = input_line channel in (*input_line lee una línea del fichero asociado a channel hasta \n y devuelve string*)
                aux (line :: acc) (*añado la línea al acumulador*)
            with End_of_file -> (*si termino de leer el file*)
                close_in channel; (*close_in cierra el fichero y libera el recurso (como el fclose(f) de c)*)
            List.rev acc (*invierto la lista para que salga en orden*)
        in
        aux []
    with Sys_error err ->
        print_endline ("Error al abrir el archivo: " ^ err);
        exit 1;;


(*comprueba si la gramática está en forma normal de chomsky (comprueba si todas las reglas son válidas)*)
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

    (*ejecuta la opción de comprobar si una gramática está en FNC*)
let cykg file = 
    let lines = read_file file in (*leo el archivo*)
    let g = parser lines in (*parseo las líneas para crear la gramática*) (*parser separaba el axioma y parseaba todas las líneas*)
    if is_cnf g then
        Printf.printf "yes\n"
    else
        Printf.printf "no\n"

(*==================Ejercicio 2=====================*)

(*convierte una cadena en lista de símbolos: "bbab" -> [T 'b'; T 'b'; T 'a'; T 'b']*)
let parse_string s = 
    if String.length s = 0 || not (String.for_all (fun c -> is_terminal c ) s) then failwith "Cadena no válida" (*si la cadena es vacía ya no vale*) (*si tiene algún símbolo que no es terminal tampoco vale*)
    else try List.init (String.length s) (fun i -> char_to_symbol s.[i]) with Failure _ -> failwith "Formato de cadena incorrecto";; (*convierto cada char de la cadena a símbolo y lo guardo en una lista*)

(*devuelve la lista de no terminales que generan el terminal dado*) (*para la primera fila de la torre*) (*si tengo Bb, nt_t g 'b' -> [NT 'B']*)
let nt_t g terminal = 
    let rec aux rules acc =
        match rules with
        | [] -> acc (*si ya no quedan más reglas*)
        | rule :: rest ->
            match rule.right with
            | [T t] when t = terminal -> aux rest (rule.left :: acc) (*si la parte derecha de la regla es el terminal que buscamos, agregamos el símbolo izquierdo a la lista de no terminales*)
            | _ -> aux rest acc (*si no, seguimos buscando*)
    in
    aux g.rules [];; (*iniciamos la búsqueda con la lista de reglas y una lista vacía para acumular los no terminales encontrados*)

(*devuelve la lista de no terminales que generan la secuencia de dos no terminales dada*) (*para las filas siguientes de la torre*)
let nt_nt g nt1 nt2 = (*si tengo SAB, entonces nt_nt g (NT 'A') (NT 'B') -> [NT 'S']*)
    let rec aux rules acc =
        match rules with
        | [] -> acc (*si ya no quedan más reglas*)
        | rule :: rest ->
            match rule.right with
            | [n1; n2] when n1 = nt1 && n2 = nt2 -> aux rest (rule.left :: acc) (*si la parte derecha de la regla es la secuencia de no terminales que buscamos, agregamos el símbolo izquierdo a la lista de no terminales*)
            | _ -> aux rest acc (*si no, seguimos buscando*)
    in
    aux g.rules [];; (*iniciamos la búsqueda con la lista de reglas y una lista vacía para acumular los no terminales encontrados*)

    (*combina dos celdas de la tabla cyk para obtener los no terminales que generan la secuencia de símbolos*)
let combine_cells g cell1 cell2 = 
    let rec aux1 l1 acc = (*recorre los NT de la primera celda*)
        match l1 with
        | [] -> acc (*si ya no quedan más no terminales en la primera celda: ya acabé todo*)
        | nt1 :: rest1 -> (*cojo el NT de la izquierda de la primera celda*)
            let rec aux2 l2 acc2 = (*combina nt1 con todos los NT de la segunda celda*)
                match l2 with
                | [] -> aux1 rest1 acc2 
                (*cuando se termina la segunda celda, seguimos con el siguiente símbolo de la primera*)
                | nt2 :: rest2 -> (*cojo el NT de la izquierda de la segunda celda*)
                    let nts = nt_nt g nt1 nt2 in (*obtenemos los no terminales que generan la secuencia de nt1 y nt2*)
                    aux2 rest2 (nts @ acc2) (*agregamos los no terminales encontrados a la lista acumulada*)
            in
            aux2 cell2 acc (*iniciamos la búsqueda con la segunda celda*)
    in
    aux1 cell1 [];; (*iniciamos la búsqueda con la primera celda*)

(*aplica el algoritmo CYK a una gramática y una cadena*)
let cyk g cadena =
    let palabra = Array.of_list cadena in (*convertimos la lista de símbolos a un array para facilitar el acceso por índices*)
    let n = Array.length palabra in (*obtenemos la longitud de la cadena*)
    let table = Array.make_matrix (n+1) (n+1) [] in (*creamos una tabla de tamaño (n+1) x (n+1) para almacenar las listas de no terminales que generan cada subcadena*) 
    (*se crea con dimensiones (n+1) para simplificar el acceso a los índices: así uso directamente j como longitud de la subcadena (la fila 0 no la uso porque no trabajo con subcadenas de longitud 0)*)
    (*Array.make_matrix dimx dimy e devuelve una tabla o matriz de dimx filas x dimy columnas con los elementos inicializados a e*)
    (*table.(i).(j) representa la celda Nij del pdf donde j es la longitud de la subcadena e i la posición inicial de la subcadena*)
    (*para cada subcadena de longitud j que empieza en i, pruebo todas las formas de dividirla en dos partes:
    izquierda: longitud k, empieza en i
    derecha: longitud j-k, empieza en i+k*)
    
    (*llenamos la primera fila de la tabla con los no terminales que generan cada símbolo terminal de la cadena*)
    (*Paso 1 del pdf: Ni1 = {A|A →wi1 ∈P}*)
    for i = 0 to n-1 do
        match palabra.(i) with
        | T t -> table.(i).(1) <- nt_t g t (*para cada terminal de la cadena, obtenemos los no terminales que lo generan y los guardamos en la primera fila de la tabla*)
        | NT _ -> failwith "La cadena no puede contener símbolos no terminales" (*si la cadena contiene símbolos no terminales ya no vale*)
    done;

    (*llenamos el resto de la tabla combinando las celdas anteriores*)
    (*paso 2 del pdf: 
    Para j =2,3,...,n
    − Para i =1,2,...,n−j+1
    − Para k =1,2,...,j−1, 
    En mi caso i va de 0 a j-1*)
    for j = 2 to n do
    (* j = longitud de la subcadena que estamos analizando (2, 3, ..., n)     
     equivale a subir un nivel en la tabla: j=2 es la fila 2, j=n es la    
     fila superior. Cada nivel analiza subcadenas un símbolo más largas*)

        for i = 0 to n-j do 
        (* i = posición de inicio de la subcadena dentro de la palabra 
         va de 0 hasta n-j porque por ejemplo una cadena de longitud 4
         solo tiene 3 posiciones en las que puede empezar (0, 1, 2), es
         decir: longitud de palabra - longitud de subcadena
         Ej: palabra "bbab" (n=4), j=2 -> i va de 0 a 2: "bb","ba","ab"*)

            for k = 1 to j-1 do
            (* k = longitud de la parte izquierda al dividir la subcadena   
            Dividimos la subcadena en dos trozos:              
            izquierda: empieza en i, longitud k   -> tabla.(k).(i)    
            derecha: empieza en i+k, longitud j-k -> tabla.(j-k).(i+k)
            Ej: subcadena "bba" (i=0, j=3):                               
            k=1: "b"|"ba"  -> tabla.(1).(0) x tabla.(2).(1)            
            k=2: "bb"|"a"  -> tabla.(2).(0) x tabla.(1).(2)*)
                
                let left_cell = table.(i).(k) in (*Nik*)
                let right_cell = table.(i+k).(j-k) in (*N(i+k)(j-k)*)
                (* añadir a Nij todos los símbolos no terminales A para los cuales A → BC ∈ P (trnsiciones), con B ∈ Nik y C ∈ N(i+k)(j−k)*)
                let combined = combine_cells g left_cell right_cell in (*combinamos las celdas para obtener los no terminales que generan esas combinaciones*)
                table.(i).(j) <- combined @ table.(i).(j) (*agregamos los no terminales encontrados a la celda actual*)
            done
        done
    done;
    (*La cadena w pertenece a L(G)si y sólo si S ∈ N1n   En el array, como empieza en 0, sería table.(0).(n)*)
    List.mem g.axioma table.(0).(n) (*verificamos si el símbolo inicial de la gramática está en la celda superior. Si aparece, la cadena pertenece al lenguaje*)

(*lee las cadenas por teclado o por stdin en bucle*)
let rec loop g = 
    try
        let line = input_line stdin in  (*leemos una línea del input*) (*input_line stdin lee una línea desde la entrada estándar (teclado, pipe, redirección)*)
        let cadena = parse_string line in     (*convertimos el string en lista de símbolos*)
        if cyk g cadena then        (*aplicamos CYK*)
            print_endline "yes"
        else
            print_endline "no";

        loop g (*seguimos leyendo más líneas*)
    
    with 
    | Failure err ->
        print_endline ("Error: " ^ err); (*si hay error en el input, lo mostramos*)
        loop g (*seguimos leyendo más líneas*)
    | End_of_file -> ()  (*cuando no hay más líneas, terminamos silenciosamente*) (*ctrl+D o fin de fichero*)

    (*ejecuta la opción de comprobar si una cadena pertenece a la gramática*)
let cykp file = 
    let lines = read_file file in (*leo el archivo*)
    let g = parser lines in (*parseo las líneas para crear la gramática*)
    if not (is_cnf g) then failwith "La gramática no está en FNC" (*si la gramática no está en FNC ya no vale*)
    else
        loop g;; (*si está bien, empieza a leer cadenas*)


(*==================MAIN=====================*)

let () = 
	if Array.length Sys.argv = 3 then ( (*tiene que haber dos argumentos además del nombre del programa*)
		let param = Sys.argv.(1) in
        let file = Sys.argv.(2) in
		match param with
        | "-g" -> cykg file
        | "-p" -> cykp file
        | _ -> Printf.printf "Usage: cyk -g <file> | -p <file> \n"
    )
    else Printf.printf "Usage: cyk -g <file> | -p <file> \n";;

