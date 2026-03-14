(*=========== función q =============*)

type ant = H | V (*define el movimiento anterior: si viene de uno Vertical o de uno Horizontal*)

(*mirar si gcd es 1: irreducible*)
let is_irreducible a b =
	let mcd c d =
		if d = 0 then c
  		else mcd d (c mod d)
	in mcd a b = 1

(*imprimir fracciones*)
let print_fraction num den =
	Printf.printf "%d/%d " num den;;

(*función principal q*)
let q option n =
	if n < 0 then raise (Failure "negativo")
	else 
		match option with
			| "-a" | "-u" -> 
				let num = ref 1 in
				let den = ref 1 in
				let ant = ref V in
				let impr = ref 0 in 

				while !impr < n do 
					if option = "-a" || (option = "-u" && is_irreducible !num !den)then begin
						print_fraction !num !den;
						incr impr
					end;
					
					if !ant = V then 
						if !num = 1 then begin (*viene de una diagonal que subía y del estado inicial*)
							incr den;
							ant := H
						end
						else begin
							decr num;
							incr den
						end
					else
						if !den = 1 then begin (*viene de una diagonal que bajaba*)
							incr num;
							ant := V 
						end
						else begin
							incr num;
							decr den
						end
				done;


			| _ -> Printf.printf "%s no es una opción de q: <-a> or <-u>\n" option


(*main: comprobar si faltan argumentos y si son válidos*)
let () = 
	if Array.length Sys.argv = 3 then
		let option = Sys.argv.(1) in
		let n = int_of_string Sys.argv.(2) in 
		q option n;
		print_newline()
	else
		Printf.printf "Usage: %s <-a|-u> <n> \n" Sys.argv.(0);;