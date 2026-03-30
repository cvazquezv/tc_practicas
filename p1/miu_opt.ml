let sustituir s patron sustituto = (*junto las reglas 3 y 4 porque al final es hacer lo mismo pero repetido*)
  let rec aux i hijos= 
    if i + String.length patron > String.length s then (*si la posición + len(patron) ya no tengo espacio para mirar más patrones en el string*)
      List.rev hijos (*para que los pueda meter en la cola en el orden en el que salieron de verdad*)
    else if String.sub s i (String.length patron) = patron then (*String.sub s i n -> coge n caracteres empezando por la posición i del string s*)
      let prefijo = String.sub s 0 i in (*cojo lo que está antes del patrón*)
      let sufijo = String.sub s (i + String.length patron) (String.length s - i - String.length patron) in (*cojo lo que está después del patrón*) 
      (*i + String.length patron es la posición del primer carácter que no pertenece al patrón*)
      (*String.length s - i - String.length patron es la longitud del string - el prefijo - el patrón*)
      aux (i + 1) ((prefijo ^ sustituto ^ sufijo) :: hijos) (*si el patrón se encontró, lo sustituyo, añado el nuevo string a los hijos y sigo buscando en la siguiente posición*)
    else
      aux (i + 1) hijos (*si no encuentro el patrón, simplemente paso a la siguiente posición*)
  in
  aux 0 [];;

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

let encolar s q vistos =
  if not (List.mem s !vistos) then begin
    Queue.push s q;
    vistos := s :: !vistos
  end;;

let miu option n =
  let q = Queue.create () in
  let vistos = ref [] in
  let axioma = "MI" in begin
    Queue.push axioma q;
    if option = "-u" then
      vistos := axioma :: !vistos;
  end;
  let impr = ref 0 in
  while !impr < n do
    let actual = Queue.pop q in
    Printf.printf "%s" actual;
    incr impr;
    if option = "-a" then
      List.iter (fun s -> Queue.push s q) (sucesores actual) (*List.iter f l -> aplica la función f a cada elemento de la lista l*) (*mete cada hijo en la cola*)
    else
      List.iter (fun s -> encolar s q vistos) (sucesores actual); 
    Printf.printf "\n";
  done;;
  
(*voy a usar las opciones -a (all) y -u (unique)*)
let () = 
	if Array.length Sys.argv = 3 then
		let option = Sys.argv.(1) in (*-a ó -u*)
		let n = int_of_string Sys.argv.(2) in 
    if n<0 then
      raise (Failure "negativo")
    else if option = "-a" || option = "-u" then
      miu option n
    else
      Printf.printf "%s no es una opción de miu: <-a> o <-u>\n" option
	else
		Printf.printf "Usage: %s <-a|-u> <n> \n" Sys.argv.(0);;


(*COMPROBAR OUTPUTS*) (*ocamlc -o miu miu.ml*)
(*ARCHIVOS: miua20.txt, miua30.txt, miua50.txt, miuu20.txt, miuu30.txt, miuu50.txt*)
(*COMANDO EN CONSOLA: ./miu -a 20 > outmiua20.txt  ./miu -a 30 > outmiua30.txt   ./miu -a 50 > outmiua50.txt*)
(*diff -w miua20.txt outmiua20.txt  |  diff -w miua30.txt outmiua30.txt  |  diff -w miua50.txt outmiua50.txt*)
(*COMANDO EN CONSOLA: ./miu -u 20 > outmiuu20.txt  ./miu -u 30 > outmiuu30.txt   ./miu -u 50 > outmiuu50.txt*)
(*diff -w miuu20.txt outmiuu20.txt  |  diff -w miuu30.txt outmiuu30.txt  |  diff -w miuu50.txt outmiuu50.txt*)




