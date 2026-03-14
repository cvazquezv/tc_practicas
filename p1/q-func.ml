(*=========== función q =============*)

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
		let generate_fractions num den ant impr =
			if impr >= n then ()
			else 
				if option = "-a" || (option = "-u" && is_irreducible num den) then

		in match option with
			| "-a" | "-u" -> generate_fractions 1 1 V 0
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