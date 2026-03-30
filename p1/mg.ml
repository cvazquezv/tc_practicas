(*DEFINICIONES*)
(*- defino x y z como las propias cadenas de ~*)
(*- defino a b c como el número de ~ en cada cadena (x y z respectivamente)*)

let print_mg mg = Printf.printf "%s\n" mg;;

let generate_th a b c= 
	let guiones n = String.make n '~' in
	let x = guiones a in (*String.make n 'char': crea una cadena de n caracteres 'char'*)
	let  y = guiones b in (*String.make 2 '-' -> "--"*)
	let z = guiones c in
	x ^ "m" ^ y ^ "g" ^ z;;
	

let mgn n =
	if n < 0 then raise (Failure "negativo")
	else
		let a = ref 0 in  
		let c = ref 1 in (*este va a ser el nivel*)
		let impr = ref 0 in 
		while !impr < n do 
			print_mg (generate_th !a (!c - !a) !c); (*a + b = c -> b = c - a*)
			impr := !impr + 1;
			if !a < (!c - 1) then incr a (*a solo va de 0 a c-1, porque luego se cambia el nivel y se vuelve a empezar*)
			else  begin incr c; a := 0; end
		done;;

let count_guiones s =
	(*doy por supuesto que hay una m y una g sí o sí porque para llegar a esta comprobación, valid_chars s tiene que ser true*)
	let m_index = String.index s 'm' in
	let g_index = String.index s 'g' in
	let a = m_index in 
	let b = g_index - m_index - 1 in
	let c = String.length s - g_index - 1 in
	(b >= 1) && ((a + b) = c);;
	(*imagina el teorema ~~m~g~~~*)
	(*para a -> la posición de m es 2 = a*)
	(*para b -> la posición de g es 4 y ahora 4-2(la posición de m)=2 -1(porque empieza en 0) = 1 = b*)
	(*para c -> la longitud del teorema es 8 - 4(la posición de g) = 4 -1(porque empieza en 0) = 3 = c*)

let valid_chars s =
	
	let count_m = ref 0 in
	let count_g = ref 0 in
	let ok = ref true in
	for i = 0 to String.length s - 1 do (*recorro todo el string buscando las ms y las gs*)
		if s.[i] = 'g' then incr count_g
		else if s.[i] = 'm' then incr count_m
			(*si hay un símbolo que no pertenece a {m, g, ~} entonces ya no es teorema*)
		else if s.[i] <> '~' then ok := false
	done;
	if !ok && !count_m = 1 && !count_g = 1 then (*comprobar que hay solo 1 m y 1 g*)
		(*commprobar que la m está antes que la g*)
		let m_index = String.index s 'm' in (*String.index s c me da la posición del primer carácter c en s*)
		let g_index = String.index s 'g' in
		 m_index < g_index;
	else false;;

let mgc s =
	match s with
	| "" -> "vacio"
	| _ -> if valid_chars s && count_guiones s then "yes" else "no";;



(*main: comprobar si faltan argumentos y si son válidos (si es string o número)*)
let () = 
	if Array.length Sys.argv = 2 then
		let param = Sys.argv.(1) in
		try
			let n = int_of_string param in mgn n
		with Failure _ ->
			Printf.printf "%s\n" (mgc param)
	else
		Printf.printf "Usage: %s <string s | number n> \n" Sys.argv.(0);;


		
(*COMPROBAR OUTPUTS*) (*ocamlc -o mg mg.ml*)
(*ARCHIVOS: mg15.txt, mg20.txt, mg50.txt*)
(*COMANDO EN CONSOLA: ./mg -a 15 > outmg15.txt  ./mg -a 20 > outmg20.txt   ./mg -a 50 > outmg50.txt*)
(*diff -w mg15.txt outmg15.txt  |  diff -w mg20.txt outmg20.txt  |  diff -w mg50.txt outmg50.txt*)
(*./mg ~~m~g~~~      yes*)
(*./mg ~~m~g~~      no*)
(*./mg ~~m~m~~~g~~~~~~     no*)
(*./mg ~~~~~m~~g~~      no*)
(*./mg ~~~~m~~g~~~~~~     yes*)
(*./mg ~~~g~~~m~~~~~~     no*)