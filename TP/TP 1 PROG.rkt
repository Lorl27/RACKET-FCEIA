;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |TP 1 PROG|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

#|
Trabajo Práctico Final - Tema 1
Integrantes:
- Ivan Campos Wainer
- Fulvio Todone
|#

#| ---- EJERCICIO 1 ---- |#
;; equivalente: Boolean Boolean -> Boolean
;; La función "equivalente" toma dos valores booleanos,
;; "a" y "b", y comprueba si son lógicamente equivalentes. En otras palabras, verifica
;; si ambos valores tienen el mismo valor de verdad.

;; implica: Boolean Boolean -> Boolean
;; La función "implica" toma dos valores booleanos, "a" y "b", y determina
;; si la implicación lógica "a implica b" es verdadera.
;; La función utiliza la función "equivalente" para evaluar dos condiciones.
;; Primero, verifica si "a" es lógicamente equivalente a #t (verdadero) utilizando la
;; función "equivalente". Luego, verifica si "b" es lógicamente
;; equivalente a #f (falso) utilizando la misma función.
;; Si ambas condiciones se cumplen, es decir, si "a" es verdadero y
;; "b" es falso, la función devuelve #f (falso),
;; indicando que la implicación lógica no se cumple. En caso contrario,
;; devuelve #t (verdadero), indicando que la implicación lógica es válida.

(define (equivalente a b)
  (if (and (or a b) (not (and a b)))
           #f #t))

(define (implica a b)
  (if (and (equivalente a #t) (equivalente b #f)) #f #t))

(check-expect (equivalente #true #false) #false)
(check-expect (equivalente #true #true) #true)
(check-expect (equivalente #false #false) #true)

(check-expect (implica #true #false) #false)
(check-expect (implica #true #true) #true)
(check-expect (implica #false #true) #true)

#| EJERCICIO 2 |#
;; valuaciones: Number -> List(List(Boolean))
;; La función valuaciones toma un número entero no negativo y devuelve
;; una lista de 2^n listas con todas las posibles combinaciones de n valores booleanos

;; Paso a paso describimos su funcionamiento de la siguiente forma
;; La función valuaciones verifica si n es igual a 0. Si es así,
;; devuelve una lista que contiene una lista vacía. Esto es para el caso base de la recursión,
;; cuando no se desea generar ninguna combinación.

;; Si n no es igual a 0, el código se mueve a la siguiente parte, donde se
;; llama a la función valuaciones de forma recursiva con n-1 para generar
;; todas las combinaciones posibles con un elemento menos.

;; El resultado de la llamada recursiva se almacena en la variable rest.

;; Luego, se utiliza la función append para combinar las combinaciones generadas por
;; las llamadas recursivas anteriores. Esto se hace mediante dos map.

;; El primer map toma cada combinación en rest y agrega #f (falso) al principio de
;; cada una utilizando la función cons. Esto genera todas las combinaciones posibles con n elementos,
;; donde el primer elemento es siempre falso.
;; El segundo map hace lo mismo, pero agrega #t (verdadero) al principio de cada combinación. Esto genera
;; todas las combinaciones posibles con n elementos, donde el primer elemento es siempre verdadero.
;; La función devuelve la lista resultante que contiene todas las combinaciones generadas.
;; En resumen, esta función valuaciones genera todas las combinaciones posibles de valores
;; booleanos de longitud n, donde n es un número entero no negativo. Por ejemplo,
;; (valuaciones 2) devolverá [(#f #f) (#f #t) (#t #f) (#t #t)], que representa todas las combinaciones
;; posibles de dos elementos booleanos (la sintaxis del resultado de ejemplo no es la misma de Racket,
;; es solo ilustrativa).

(define (valuaciones n)
  (if (= n 0)
      '(())
      (let ([rest (valuaciones (- n 1))])
        (append
         (map (lambda (val) (cons #f val)) rest)
         (map (lambda (val) (cons #t val)) rest)))
  )
)

(check-expect (valuaciones 0) (list (list)))
(check-expect (valuaciones 1) (list (list #f) (list #t)))
(check-expect (valuaciones 3) (list
 (list #false #false #false)
 (list #false #false #true)
 (list #false #true #false)
 (list #false #true #true)
 (list #true #false #false)
 (list #true #false #true)
 (list #true #true #false)
 (list #true #true #true))
)

#| EJERCICIO 3 |#

;; A : List(Boolean) -> Boolean
;; La función A toma una lista "l" con tres elementos y evalúa una expresión lógica.
;; Utiliza las funciones "and, "or", "implica" y "equivalente" como auxiliares.
;; Verifica si las implicaciones lógicas entre los primeros dos elementos de la lista y
;; el tercer elemento son equivalentes a la implicación lógica entre la disyunción de los
;; primeros dos elementos y el tercer elemento. Retorna el resultado de esta verificación.
;; Es posible ver que esta proposición descrita, al ser una tautología, devolverá siempre #true

(define
  (A l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
  (equivalente (and (implica p1 p3)
                    (implica p2 p3))
               (implica (or p1 p2)
                        p3))))

(check-expect (A (list #t #t #t)) #t)
(check-expect (A (list #f #t #f)) #t)
(check-expect (A (list #f #f #t)) #t)

; B : List(Boolean) -> Boolean
;; La función "B" toma una lista "l" con tres elementos y evalúa una
;; expresión lógica. Utiliza las funciones "and", "implica" y "equivalente" como auxiliares.
;; Verifica si la implicación lógica entre la conjunción de los primeros dos elementos
;; de la lista y el tercer elemento es equivalente a la conjunción de las implicaciones lógicas
;; entre el primer elemento y el tercer elemento, y entre el segundo elemento y el tercer elemento.
;; Retorna el resultado de esta verificación.
(define (B l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
    (equivalente (implica (and p1 p2) p3) (and (implica p1 p3) (implica p2 p3)))
    )
  )
(check-expect (B (list #t #t #t)) #t)
(check-expect (B (list #f #f #f)) #t)
(check-expect (B (list #f #t #f)) #f)

;; C : List(Boolean) -> Boolean
;; La función "C" toma una lista "l" con dos elementos y evalúa una expresión lógica.
;; Utiliza las funciones "equivalente", "or", "not" y "and" como auxiliares.
;; Verifica si la expresión lógica (NOT p1 OR NOT p2) es lógicamente equivalente a la
;; expresión lógica (p1 AND p2). Retorna el resultado de esta verificación.
;; Podemos ver que como esta proposición es una contradicción, los resultados que devuelve
;; siempre son #false
(define (C l)
  (let ([p1 (first l)]
        [p2 (second l)])
    (equivalente (or (not p1) (not p2)) (and p1 p2))
    )
)

(check-expect (C (list #t #t)) #f)
(check-expect (C (list #f #t)) #f)
(check-expect (C (list #f #f)) #f)

#| EJERCICIO 4 |#
;; evaluar: Function Number -> List(Boolean)
;; La función "evaluar" toma una función "P" (que es una proposición lógica)
;; y un número entero "n" como argumentos.
;; Utiliza la función auxiliar "valuaciones" para generar todas las posibles combinaciones de
;; valores booleanos para "n" variables. Luego, aplica la función "P" a cada una de estas
;; combinaciones y devuelve una lista con los resultados.
;; En otras palabras, obtiene la última columna de la tabla de verdad de P.
(define (evaluar P n)
  (map P (valuaciones n))
)

(check-expect (evaluar (lambda (val) (and (car val) (cadr val))) 2) '(#f #f #f #t))
(check-expect (evaluar (lambda (val) (or (car val) (cadr val))) 2) '(#f #t #t #t))
(check-expect (evaluar (lambda (val) (not (car val))) 1) '(#t #f))

#| EJERCICIO 5 |#

;; tautologia?: Function Number -> Boolean
;; La función "tautologia?" toma una función "P" (que es una proposición lógica)
;; y un número entero "n" como argumentos.
;; Utiliza la función auxiliar "evaluar" para obtener una lista de resultados al aplicar la
;; función "P" a todas las combinaciones de valores booleanos para "n" variables. Luego, utiliza la función "foldr"
;; con una función lambda, que representa un and, para hacer un and de todos los elementos de la lista con un #t como valor base.
;; Si todos los elementos de la lista son verdaderos, la función tautologia? devuelve #t, en caso contrario devuelve #f.

(define (tautologia? P n)
  (if (foldr (lambda (x y) (and x y)) #t (evaluar P n)) #t #f))

(check-expect (tautologia? A 3) #t)
(check-expect (tautologia? B 3) #f)
(check-expect (tautologia? C 2) #f)

;; contradiccion?: Function Number -> Boolean
;; La función "contradiccion?" toma una función "P" y un número entero "n"
;; como argumentos. Utiliza la función auxiliar "evaluar" para obtener una lista de resultados
;; al aplicar la función "P" a todas las combinaciones de valores booleanos para "n" variables. Luego,
;; utiliza la función "foldr" con una función lambda, que representa un or, para hacer un or con todos los elementos de la lista
;; con un #f como valor base. Si el or general devuelve #t, eso implica que la proposición tiene algún #t, por lo tanto no es contradicción. 
;; Si al menos uno de los elementos de la lista es verdadero, la función "contradiccion?" devuelve #f, en caso contrario devuelve #t.

(define (contradiccion? P n)
  (if (foldr (lambda (x y) (or x y)) #f (evaluar P n)) #f #t))

(check-expect (contradiccion? A 3) #f)
(check-expect (contradiccion? B 3) #f)
(check-expect (contradiccion? C 2) #t)

;; satisfacible?: Function Number -> Boolean
;; La función "satisfacible? toma una función "P" y un número entero "n" como argumentos.
;; Utiliza la funcion "contradiccion?" para determinar si la función "P" es una contradicción para todas las
;; combinaciones de valores booleanos posibles. Luego, aplica la negación a ese resultado para obtener si la
;; función "P" es satisfacible, es decir, si hay al menos una combinación de valores booleanos que satisface la función "P".

(define (satisfacible? P n) (not (contradiccion? P n)))

(check-expect (satisfacible? A 3) #t)
(check-expect (satisfacible? B 3) #t)
(check-expect (satisfacible? C 2) #f)