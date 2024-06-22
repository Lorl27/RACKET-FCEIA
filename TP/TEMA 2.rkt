;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |TEMA 2|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#| Trabajo Práctico Final - Tema 2

Integrantes:
- Cardamone, Bruno
- Yorio, Sofía Cristina
|#

;;;;;;;; EJERCICIO 1 -------------------------------------------------------------
; Definir una constante MAZO-ORDENADO que sea una lista con los números del 0 a N.

(define MAZO-ORDENADO (list 1 2 3 4 5 6 7))

;;;;;;;; EJERCICIO 2 -------------------------------------------------------------
; Diseñar una función barajar que construya una mazo desordenado, es decir que
; devuelva una lista con los elementos de MAZO-ORDENADO pero con los elementos en un orden aleatorio.
; Para ésta función no de los casos de prueba, ya que el resultado contendrá números aleatorios.

; Representamos un mazo de cartas.
; barajar:  List(Number) --> List(Number)
; Dada una lista con números naturales, cambiamos el orden de los números de la lista entre sí.
(define (barajar l)
  (local [(define x (random (add1 (length MAZO-ORDENADO))))] ; define una constante x que es un número aleatorio entre 0 y la cantidad de elementos de Mazo Ordenado
  (cond  [(empty? l) l] 
         [(not (member? x l)) (barajar l)]
         [else (cons x (barajar (remove x l)))]
         )
    )
  )

;;;;;;;; EJERCICIO 3 -------------------------------------------------------------
; Diseñar una función jugar, que dado un número natural n juega n veces al juego "Nada en su lugar".
; La función debe devolver una lista de n elementos de mazos desordenados.
; Al igual que en el ejercicio anterior, no de los casos de prueba.

; Representamos a n como la cantidad de jugadas.
; jugar: Natural --> List(Number)
; Dado un número natural n, ejecuta n veces la función barajar y crea una lista con los resultados.
(define (jugar n)
   (if (zero? n) empty (cons (barajar MAZO-ORDENADO) (jugar (sub1 n))))
  )

;;;;;;;; Ejercicio 4 -------------------------------------------------------------
; Diseñar un predicado perdedora? que dada una lista de naturales, correspondiente a una
; tirada de cartas, determine si es una jugada perdedora.
; Representamos a l y l2 cómo dos listas de números.
; comparar: List(Number) List(Number) --> Boolean
; Dada dos listas de la misma cantidad de elementos tienen algún número igual en la misma posición.
(check-expect (comparar (list 2 7 3 1 4 6 5) (list 3 7 1 6 2 4 5)) #false)
(check-expect (comparar (list 5 2 3 1 4 6 7) (list 2 1 7 5 6 3 4)) #true)
(check-expect (comparar (list 6 3 2 1 5 7 4) (list 1 6 5 3 7 4 2)) #true)
(define (comparar l l2)
  (cond [(empty? l) #t]
        [(= (first l) (first l2))  #f]
        [else (comparar (rest l) (rest l2))]
        )
  )

; Representamos una tirada de cartas.
; perdedora?: List(Natural) --> Boolean
; Dada una lista de naturales l, compara si tiene algún número igual en la misma posición que el mazo ordenado.
(check-expect (perdedora? (list 5 2 6 3 4 7 1)) #f)
(check-expect (perdedora? MAZO-ORDENADO) #f)
(check-expect (perdedora? (list 5 6 7 1 2 3 4)) #t)
(define (perdedora? l)
  (comparar l MAZO-ORDENADO)
  )


;;;;;;;; Ejercicio 5  ---------------------------------------------------------
; Diseñar una función montecarlo que aplique el método montecarlo al juego, es decir,
; dado un natural n, realice n jugadas del juego y calcule la probabilidad de ganar al mismo.
; No dé los casos de prueba.

; Representamos la cantidad de jugadas.
; montecarlo: Natural --> Number
; Dado un natural n, aplica el método montecarlo para saber la probabilidad de n jugadas.
(define (montecarlo n)
  (* (/ (length (filter perdedora? (jugar n))) n) 100)
  )

#| Responda a la pregunta: ¿Le conviene o no aceptar la apuesta de Lautaro?

Le combiene aceptar la apuesta, debido a que con alrededor de un 30% de probabilidad de ganar, hay mayor chance de que Francisco pierda.

|#