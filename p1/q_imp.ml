(*=========== función q =============*)

type ant = H | V (*define el movimiento anterior: si viene de uno Vertical o de uno Horizontal*)

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
				(*defino el estado inicial (1/1) y que venga de vertical para que siga hacia la derecha*)
				let num = ref 1 in
				let den = ref 1 in
				let ant = ref V in
				let impr = ref 0 in 

				while !impr < n do 
					if option = "-a" || (option = "-u" && is_irreducible !num !den) then begin
						print_fraction !num !den;
						incr impr
					end;
					(*cambiar a la siguiente fracción*)
					if !ant = V then 
						if !num = 1 then begin (*viene de una diagonal que subía y del estado inicial --> vas a la derecha*)
							incr den;
							ant := H
						end
						else begin (*sigue hacia arriba en la diagonal*)
							decr num;
							incr den
						end
					else
						if !den = 1 then begin (*viene de una diagonal que bajaba --> vas hacia abajo*)
							incr num;
							ant := V 
						end
						else begin (*sigue hacia abajo en la diagonal*)
							incr num;
							decr den
						end
				done;


			| _ -> Printf.printf "%s no es una opción de q: <-a> or <-u>\n" option


(*main: comprobar si faltan argumentos y si son válidos*)
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
real    0m6.081s
user    0m4.254s
sys     0m0.581s
*)
(*time ./q -u 10_000_000
real    0m7.793s
user    0m5.865s
sys     0m0.540s
*)

(*CONCLUSIÓN*)
(*El tiempo de -a es menor, ya que -u hace trabajo extra: para cada fracción candidata, calcula el mcd y lo compara con 1.
Con -u no solo miras n posiciones, también miras todas aquellas que descartas porque no son irreducibles, por lo tanto, tiene más iteraciones del bucle*)

(*COMPROBAR OUTPUTS*) (*ocamlc -o q_imp q_imp.ml*)
(*ARCHIVOS: qa15.txt, qa20.txt, qa50.txt, qu15.txt, qu20.txt, qu50.txt*)
(*COMANDO EN CONSOLA: ./q_imp -a 15 > outqa15.txt  ./q_imp -a 20 > outqa20.txt   ./q_imp -a 50 > outqa50.txt*)
(*diff -w qa15.txt outqa15.txt  |  diff -w qa20.txt outqa20.txt  |  diff -w qa50.txt outqa50.txt*)
(*COMANDO EN CONSOLA: ./q_imp -u 15 > outqu15.txt  ./q_imp -u 20 > outqu20.txt   ./q_imp -u 50 > outqu50.txt*)
(*diff -w qu15.txt outqu15.txt  |  diff -w qu20.txt outqu20.txt  |  diff -w qu50.txt outqu50.txt*)