let sustituir s patron sustituto = (*junto las reglas 3 y 4 porque al final es hacer lo mismo pero repetido*)
  let rec aux s i hijos= 
    if i + String.length patron > String.length s then (*si la posición + len(patron) ya no tengo espacio para mirar más patrones en el string*)
      List.rev hijos (*para que los pueda meter en la cola en el orden en el que salieron de verdad*)
    else if String.sub s i (String.length patron) = patron then (*String.sub s i n -> coge n caracteres empezando por la posición i del string s*)
      let prefijo = String.sub s 0 i in (*cojo lo que está antes del patrón*)
      let sufijo = String.sub s (i + String.length patron) (String.length s - i - String.length patron) in (*cojo lo que está después del patrón*) 
      (*i + String.length patron es la posición del primer carácter que no pertenece al patrón*)
      (*String.length s - i - String.length patron es la longitud del string - el prefijo - el patrón*)
      aux s (i + 1) ((prefijo ^ sustituto ^ sufijo) :: hijos) (*si el patrón se encontró, lo sustituyo, añado el nuevo string a los hijos y sigo buscando en la siguiente posición*)
    else
      aux s (i + 1) hijos (*si no encuentro el patrón, simplemente paso a la siguiente posición*)
  in
  aux s 0 [];;

let regla1 s = (*si termina en I añádele U*)
  if s.[String.length s - 1] = 'I' then
    [s ^ "U"]
  else
    [];;

let regla2 s = (*Mx -> Mxx*)
  if s <> "" && s.[0] = 'M' then
    let x = String.sub s 1 (String.length s - 1) in
    [s ^ x]
  else
    [];;

let regla3 s = (*sustituir III por U*)
  sustituir s "III" "U";;

let regla4 s = (*eliminar UU*)
  sustituir s "UU" "";;

let sucesores s =
  (regla1 s) @ (regla2 s) @ (regla3 s) @ (regla4 s);;


let miu n =
  let q = Queue.create () in
  let axioma = "MI" in
  Queue.push axioma q;
  let impr = ref 0 in
  while !impr < n do
    let actual = Queue.pop q in
    Printf.printf "%s" actual;
    incr impr;
    List.iter (fun s -> Queue.push s q) (sucesores actual); (*List.iter f l -> aplica la función f a cada elemento de la lista l*) (*mete cada hijo en la cola*)
    Printf.printf "\n";
  done;;
  

let () = 
	if Array.length Sys.argv = 2 then
		let n = int_of_string Sys.argv.(1) in miu n
	else
		Printf.printf "Usage: %s <number n> \n" Sys.argv.(0);;


(*COMPROBAR OUTPUTS*) (*ocamlc -o miu miu.ml*)
(*ARCHIVOS: miu20.txt, miu30.txt, miu50.txt*)
(*COMANDO EN CONSOLA: ./miu 20 > outmiu20.txt  ./miu 30 > outmiu30.txt   ./miu 50 > outmiu50.txt*)
(*diff -w miu20.txt outmiu20.txt  |  diff -w miu30.txt outmiu30.txt  |  diff -w miu50.txt outmiu50.txt*)