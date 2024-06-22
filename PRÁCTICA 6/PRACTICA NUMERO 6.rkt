;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |PRACTICA NUMERO 6|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;                 PRÁCTICA NÚMERO 6:

(make-list 2 "hello") ;(cons "hello" (cons "hello" '()))
(make-list 3 #true) ;(cons #true (cons #true (cons #true '())))
(make-list 0 17) ;'()    

; Un Natural es:
; – 0
; – (add1 Natural)
; interpretación: Natural representa los números naturales

; copiar: Natural String -> Lista(String)
; El propósito de la función copiar es crear una lista de num copias de una cadena str

(define (copiar num str) (cond [(zero? num) '() ]
                               [(positive? num) (cons str (copiar (sub1 num) str))]
                               )
  ) 

(check-expect (copiar 2 "hola") (list "hola" "hola"))
(check-expect (copiar 0 "hola") '())
(check-expect (copiar 4 "abc") (list "abc" "abc" "abc" "abc"))

#|

-- 0: (Constructor) Constante usada para representar el primer número natural
-- add1: (Constructor) Calcula el sucesor de un número natural
-- sub1: (Selector) Devuelve el predecesor de un número natural positivo
-- zero?: (Predicado) Reconoce al natural 0
-- positive?: (Predicado) Reconoce naturales construidos con add1

|#

;-------------------------------------------------------------------------------

#|
Ejercicio 1. Diseñe la función sumanat que toma dos números naturales y sin usar +
 devuelve un natural que es la suma de ambos.
 Use el evaluador paso a paso para evaluar (sumanat 3 2) 
|#

;(define (sumita num1 num2) (add1 (sumita (sub1 num1) num2) ) )

(define (sumNat num1 num2) (cond [(and (zero? num1) (zero? num2)) 0]
                                 [(zero? num1) num2 ]
                                 [(zero? num2) num1 ]
                                 [(positive? num1) (add1 (sumNat (sub1 num1) num2)) ]
                                 )
  )

(sumNat 3 2)

;-------------------------------------------------------------------------------

#|
Ejercicio 2. Diseñe la función multiplicar. Esta función debe tomar como
entrada dos números naturales y debe multiplicarlos sin usar * ni + . Use el
evaluador paso a paso para evaluar (multiplicar 3 2). Puede utilizar el
ejercicio anterior
|#

(define (multiplicar num1 num2) (cond[(or (zero? num1) (zero? num2)) 0]
                                     [(positive? num1) (sumNat num2 (multiplicar (sub1 num1) num2) )
                                          ]) )

(multiplicar 3 2)
(multiplicar 3 4)

;-------------------------------------------------------------------------------

#|
Ejercicio 3. Diseñe la función powernat que toma dos números naturales y
devuelve el resultado de elevar el primero a la potencia del segundo,
usando la función multiplicar definida en el ejercicio anterior. Use el
evaluador paso a paso para evaluar (powernat 4 2).
|# 

;(define (powernat num1 num2) (expt num1 (multiplicar num2 num2) ) )
(define (powernat num1 num2) (cond[(or (zero? num1) (zero? num2)) 1]
                                  [(positive? num1) (multiplicar num2 (powernat (sub1 num1) num2) )]
                                  )
  )

(powernat 4 2)

;---------------------------------------------
#|
Ejercicio 4. Diseñe una función sigma: Natural (Natural -> Number) ->
Number , que dados un número natural n y una función f, devuelve la
sumatoria de f para los valores de 0 hasta n . Es decir, calcular:
|#

(check-expect (sigma 4 sqr) 30) 
(check-expect (sigma 10 identity) 55)

(define (sigma num fun) (cond[(zero? num) 0 ]
                             [(positive? num) (sumNat (fun num) (sigma (sub1 num) fun)) ]
                                 )
  )

;---------------------------------------------
#|
Ejercicio 5: varias sumatorias.
|#

(define (Cubo num) (powernat 1 (powernat 3 num)))

(Cubo 2) 

(define (SumatoriaCUBO num) (sigma num Cubo))

(SumatoriaCUBO 1) (SumatoriaCUBO 2)

(define (G num) (/ 1 (SumatoriaCUBO num))) ;el recíproco

(G 1) (G 2)

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

(define (Potencia2 num) (powernat 2 num))

(Potencia2 2)

(define (SumatoriaPotencia2 num) (sigma num Potencia2))

(SumatoriaPotencia2 2)

(define (R num) (/ 1 (SumatoriaPotencia2 num))) ;el recíproco

(R 2)

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

(define (unoMas num) (sumNat 1 num))

(unoMas 1)

(define (SumatoriaUn num) (sigma num unoMas))

(SumatoriaUn 2)

(define (S num) (/ num (SumatoriaUn num))) ;el recíproco

(S 2)

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

(define (T num) (/ 1 (SumatoriaUn num))) ;el recíproco

(T 2)

;---------------------------------------------------------------------
#| Ejercicio 6. Diseñe la función intervalo, que dado un número natural n ,
devuelve la lista (list 1 2 ... n) . Para 0 devuelve '().

Ayuda: Quizás le convenga devolver la lista en orden descendente. Luego
la función reverse lo resuelve fácilmente.|#

(define (inv num) (cond[(zero? num)'()] 
                       [(positive? num) (cons num (inv (sub1 num) )) ]
                             )
  )

(inv 3)

(define (intervalo num) (reverse (inv num)) )

(intervalo 3)

;--------------------------------------------------------------------------
#|
Ejercicio 7. Diseñe la función factnat que toma un número natural y
devuelve su factorial.
 El factorial de un número natural n se calcula haciendo 1 x 2 x ... x n. 
|#

(define (factnat num)  (cond[(zero? num) 1]
                            [(positive? num) (multiplicar num (factnat (sub1 num))) ]
                            )
  )

(factnat 4) ;24
(factnat 3) ;6
(factnat 2) ;2
(factnat 0) ;1

;----------------------------------------------------------------------------
#|
Ejercicio 8. Diseñe la función fibnat que toma un número natural y
devuelve el valor correspondiente a la secuencia de Fibonacci para ese valor:
fibnat (0) = 1
fibnat (1) = 1
fibnat (n+2) = fibnat (n) + fibnat (n+1)
|#

(define (fibnat num) (cond[(zero? num) 1]
                          [(zero? (sub1 num))1]
                          [(positive? num )  (sumNat (fibnat (sub1(sub1 num))) (fibnat (sub1 num)) ) ]
                          )
  )

(fibnat 5) ; 3 + 5 = 8
(fibnat 6) ; 13
(fibnat 8) ; 34

;----------------------------------------------------------------------------
#|
Ejercicio 9. Diseñe una función list-fibonacci que dado un número n
devuelve una lista con los primeros n+1 valores de la serie de fibonacci, ordenados de mayor a menor.
 Es decir, (list-fibonacci n) = (list (fib n) (fib (- n 1)) ... (fib 0))
|#

(check-expect (list-fibonacci 4) (list 5 3 2 1 1))
(check-expect (list-fibonacci 0) (list 1))

(define (list-fibonacci num) (cond[(zero? num) (list 1)]
                                  [(positive? num) (cons (fibnat num) (list-fibonacci (sub1 num))) ]
                                  )
  )

(list-fibonacci 8)

;-----------------------------------------------------------------------------------
#|
Ejercicio 10. Considere la función g definida como sigue:
g (0) = 1
g (1) = 2
g (2) = 3
g (n) = g (n-1) * g (n-2) * g (n-3) para todo n mayor o igual a 3

Diseñe una función list-g que dado un número n devuelve la lista con los valores que resulta de evaluar a g en n , n-1, n-2 ,..., 0.
 Es decir (list-g n) = (list (g n) (g (- 1 n)) ... (g 1) (g 0)) .
|#

(define (g num) (cond[(zero? num) 1]
                     [(= num 1) 2]
                     [(= num 2) 3]
                     [else (multiplicar (multiplicar (g (sub1(sub1 num))) (g (sub1 num))) (g(sub1(sub1(sub1 num)))) )]
                     )
  )

(g 0) (g 1) (g 2) (g 3)

(define (list-g num) (cond[(zero? num) (list 1)]
                          [(positive? num) (cons (g num) (list-g (sub1 num)))  ]
                          )
  )

(check-expect (list-g 4) (list 36 6 3 2 1))
(check-expect (list-g 0) (list 1))

;------------------------------------------------
#|
Ejercicio 11. Diseñe una función componer, que dados:
• una función f: Number -> Number,
• un natural n , 
• un número x ,
devuelva el resultado de aplicar n veces la función f a x .
Presente al menos dos ejemplos más en su diseño.
|#

(define (componer fun nat num) (cond[(zero? nat) num] 
                                 ;   [(positive? nat) (multiplicar num (componer fun  (sub1 nat) num) )]
                                    [(positive? nat) (fun (componer fun  (sub1 nat) num) )]
  ))
  

(check-expect (componer sqr 2 5) 625)
(check-expect (componer add1 5 13) 18)
 (componer sub1 5 13)
 (componer sqrt 5 13)

;------------------------------------------------
#|
Ejercicio 12. Diseñe una función multiplos que tome dos naturales n y m , y
devuelva una lista con los primeros n múltiplos positivos de m , en orden
inverso: m * n, m * (n-1), ... , m * 2 , m .
|#

(define (multiplos n m) (cond[(zero? n) '()]
                             [(positive? n)  (cons (multiplicar n m) (multiplos (sub1 n) m) )  ]
                             )
  ) 

(check-expect (multiplos 4 7) (list 28 21 14 7))
(check-expect (multiplos 0 11) '())

;----------------------------------------------------------------------------
#|
Ejercicio 13. Estamos interesados en definir una función g que dado un
número natural n y una función f: Natural -> Boolean, devuelve #true si
y sólo si alguno de los valores (f 0) ,(f 1) ,... (f n) es #true.
Por ejemplo:

En lógica, suele usarse el símbolo V para representar el operador or.
Podemos expresar g de la siguiente manera:
Diseñe la función g .
|#

(define (funG num fun)  (fun (sub1 num)) )

(check-expect (funG 3 negative?) #false)
(check-expect (funG 7 even?) #true)

;-------------------------------------------------------------
#|
Ejercicio 14. Diseñe una función circulos que tome un número natural m y
devuelva una imagen cuadrada de lado 2*m2 con m círculos azules
centrados y radios: m2 , (m-1)2 , ... , 22 , 1 respectivamente.
|#

(define (circulos num)
  
  (local [
     (define LADO (multiplicar 2 (sqr num)) )
     (define ESCENA (empty-scene LADO LADO) )
     (define CENTRO (/ LADO 2))

     
     (define (cir x)
       (cond [(zero? x) ESCENA]
             [(positive? x) (place-image (circle (sqr x) "outline" "violet") CENTRO CENTRO (cir (sub1 x))  )]
       ))                
         ] (cir num) )
  
)

(circulos 10)
;+++++++++++++++++++++++++++++++++++++++++++++++++++++
(define (graficar-circulos x )
  (circle x "outline" "red"))


(define fondo (empty-scene 400 400))


(define (circulo m)
  (cond[(zero? m)fondo]
       [(positive? m)(place-image (graficar-circulos (sqr m))200 200
                                  (circulo (sub1 m)))]))

;------------------------------------------------------
#|
Ejercicio 15. Diseñe una función cuadrados que tome un número natural m,
un ángulo ang y devuelva una imagen cuadrada de lado 200 con m
cuadrados azules centrados. Los cuadrados azules tendrán lados de
tamaño: m2 , (m-1)2 , ... , 22, 1 respectivamente. El ángulo ang indica la
rotación del cuadrado de mayor dimensión. El ángulo que corresponde al
cuadrado de lado (m-1)2 debe ser de 20 grados mayor que el que le
corresponde al cuadrado de lado m2, para cualquier valor de m mayor o
igual a 1 

|#

(define LADO1 200)
(define ESCENA1 (empty-scene LADO1 LADO1) )
(define CENTRO1 (/ LADO1 2) )

(define (RECT x) (square x "outline" "green" ) )

(define (ROTAR-RECT x img)  (rotate x (RECT img) ) )

(define (cuadrados num ang) (cond[(zero? num) ESCENA1]
                                 [(positive? num)  (place-image (ROTAR-RECT  ang (sqr num) ) CENTRO1 CENTRO1 (cuadrados (sub1 num) (+  ang 20)) )  ]
                                 )
  )

(cuadrados 10 70)

;------------------------------------------------------------------------
#|
Ejercicio 16. Una persona solicita un préstamo a una entidad bancaria y se
compromete a devolver el importe total del préstamo más intereses en n cuotas mensuales crecientes, con una tasa de interés del i% anual.
 La cuota j-ésima, con j que va de 1 hasta n , se calcula sumando dos valores:
• la parte correspondiente a la devolución del préstamo: total/n ;
• la parte correspondiente a los intereses de la cuota: (total/n) * (i/ (100*12)) * j.

Diseñe una función cuotas que dado un importe total de un préstamo, un
valor n correspondiente al número de cuotas, una tasa i de interés,
devuelva una lista con las cuotas a pagar ordenadas de forma creciente.
|#



(define j 1)

(define (j-cuota prestamo n intereses j) (+ (/ prestamo n) (* (/ prestamo n) (/ intereses (* 100 12) ) j) ) )

(define (cuotas prestamo n interes) (local [
         
         
         (define (aux prestamo n intereses j) (cond[(= j (+ n 1)) '()]
                                                   [(positive? n)  (cons (j-cuota prestamo n intereses j) (aux prestamo n interes (add1 j)) ) ]
                                              ))
                                           ]
                                      
  (aux prestamo n interes j)
                                      )
  )

(check-expect (cuotas 10000 0 18) '())
(check-expect (cuotas 10000 1 12) (list 10100))
(check-expect (cuotas 30000 3 12) (list 10100 10200 10300))
(check-expect (cuotas 100000 4 18) (list 25375 25750 26125 26500))