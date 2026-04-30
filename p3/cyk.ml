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


(*==================Ejercicio 2=====================*)


(*==================Ejercicio 3=====================*)


(*==================Ejercicio 4=====================*)