(*=========== función q =============*)

type dir = B | S (*define la dirección de la diagonal:
S = Subida (hacia arriba a la derecha) (lo trato como vertical también al tocar borde izquierdo)
B = Bajada (hacia abajo a la izquierda) (lo trato como horizontal también al tocar borde de arriba)*)

(*mirar si gcd es 1: irreducible*)
let is_irreducible a b =
	let rec mcd c d =
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
				let rec aux (num, den, dir) impr =
					if impr < n then
						let printable = (option = "-a") || (option = "-u" && is_irreducible num den) in
							if printable then print_fraction num den;
						let next = match (num, den, dir) with
							| (_, 1, B) -> (num+1, 1, S) (*viene de una diagonal que bajaba --> vas hacia abajo*)
							| (1, _, S) -> (1, den+1, B) (*viene de una diagonal que subía y del estado inicial --> vas a la derecha*)
							| (_, _, B) -> (num + 1, den-1, B) (*sigue hacia abajo en la diagonal*)
							| (_, _, S) -> (num-1, den+1, S) (*sigue hacia arriba en la diagonal*)
							in
							let impr2 = if printable then impr + 1 else impr in
							aux next impr2
					in aux (1, 1, S) 0		(*estado inicial*)


			| _ -> Printf.printf "%s no es una opción de q: <-a> o <-u>\n" option


(*main: comprobar si faltan argumentos*)
let () = 
	if Array.length Sys.argv = 3 then (*Sys.argv me da el número de argumentos que metí en el comando*)
		let option = Sys.argv.(1) in (*Sys.argv.(n) me da el n parámetro*)
		let n = int_of_string Sys.argv.(2) in 
		q option n;
		print_newline()
	else
		Printf.printf "Usage: %s <-a|-u> <n> \n" Sys.argv.(0);;

(*=========== TIEMPOS DE EJECUCIÓN =============*)
(*n = 10 000 000*)
(*time ./q -a 10_000_000
real    0m23.139s
user    0m14.988s
sys     0m2.339s
*)
(*time ./q -u 10_000_000
real    0m28.631s
user    0m22.443s
sys     0m2.724s
*)

(*CONCLUSIÓN*)
(*El tiempo de -a es menor, ya que -u hace trabajo extra: para cada fracción candidata, calcula el mcd y lo compara con 1.
Con -u no solo miras n posiciones, también miras todas aquellas que descartas porque no son irreducibles, por lo tanto, tiene más iteraciones del bucle.
Es decir, con q -u N, estaría recorriendo más de N fracciones en total.*)


(*COMPROBAR OUTPUTS*) (*ocamlc -o q q.ml*)
(*ARCHIVOS: qa15.txt, qa20.txt, qa50.txt, qu15.txt, qu20.txt, qu50.txt*)
(*COMANDO EN CONSOLA: ./q -a 15 > outqa15.txt  ./q -a 20 > outqa20.txt   ./q -a 50 > outqa50.txt*)
(*diff -w qa15.txt outqa15.txt  |  diff -w qa20.txt outqa20.txt  |  diff -w qa50.txt outqa50.txt*)
(*COMANDO EN CONSOLA: ./q -u 15 > outqu15.txt  ./q -u 20 > outqu20.txt   ./q -u 50 > outqu50.txt*)
(*diff -w qu15.txt outqu15.txt  |  diff -w qu20.txt outqu20.txt  |  diff -w qu50.txt outqu50.txt*)