;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 5 DEFINITIVA|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;                                                              práctica 5:

; ej 1:
  
(list "Sofia" "Ludki" "Delfi" "Anto" "Aitor")

;---------------------------------------------------------------------------------------

;ej 2:

(list #f #t)

;-----------------------------------------------------------

;ej 4:

; contiene-Marcos? : Contactos -> Booleano
; dada una lista de Contactos, determina si "Marcos" es un elemento de la misma

  (check-expect (contiene-Marcos? '()) #false)
  (check-expect (contiene-Marcos? (cons "Sara" (cons "Pedro"  (cons "Esteban" '())))) #false)
  (check-expect (contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '())))) #true)
  (check-expect (contiene-Marcos? (cons "Juan" '())) #false)
  (check-expect (contiene-Marcos? (cons "Marcos" '())) #true)


  (define (contiene-Marcos? lista) (cond [(empty? lista) #false]
                                         [(cons? lista) (if (string=? (first lista) "Marcos") #true (contiene-Marcos? (rest lista)) )  ]
                                )
   )

;----------------------------------------------------------------------------------

;ej 5:

#|

Diseñe la función contiene? que determine si un string aparece en una lista de string.

-------------------------------------------------------------------------------------
DrRacket provee una función member? que toma un valor x y una lista l y chequea si el valor x está en la lista l. Por ejemplo:

  > (member? "Sara" (cons "a" (cons "b" (cons "Sara" '()))))
  #true

-----No use member? para definir la función contiene? ----------------------------------

|#

;contiene?: String List(String) -> Boolean
;Devuelve #t si sl String dado está presente en la lista de Strings

(check-expect(contiene? "Sara" (cons "a" (cons "b" (cons "Sara" '()))) ) #t )

(define (contiene? str lista) (cond [ (empty? lista) #f ]
                                    [ (cons? lista) (if (string=? str (first lista) ) #t (contiene? str (rest lista) ) ) ]
                              )
)

;--------------------------------------------------------------------

;ej 6:

(contiene-Marcos? (cons "Marcos" (cons "C" '()))) ;t  
(contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '())))) ;t

;--------------------------------------------------------------------

;ej 7:

#|

Una Lista-de-montos es:
  – '()
  – (cons NumeroPositivo Lista-de-montos)
  Lista-de-montos representa una lista con montos de dinero
Cree algunos ejemplos que pertenezcan a este tipo de datos para asegurarse de entender bien la definición. Una vez hecho esto, diseñe la función suma que tome como entrada una lista con montos de dinero y devuelva como resultado la suma de los montos presentes en dicha lista. Use el evaluador paso a paso de DrRacket para ver cómo se evalúa (suma l) para una lista no muy larga de montos l.

|#

;LISTA-MONTOS: List(Number)
(define LISTA-MONTOS (list 100 60 1) )

;SUMA: List(Number) -> Number
; Devuelve la suma de todos los elementos de la lista
(define (SUMA lista) (cond[(empty? lista) 0]
                          [(cons? lista) ( + (first lista) (SUMA (rest lista) )) ]
                      )
 )

(check-expect(SUMA LISTA-MONTOS)161)

;--------------------------------------------------------------------

#|

Ejercicio 8. Ahora consideremos la siguiente definición:
Una Lista-de-numeros es:
– '()
– (cons Numero Lista-de-numeros)
Algunos elementos de este tipo de datos son apropiados para la función suma del
ejercicio anterior y otros no.


Diseñe la función pos? que tome una Lista-de-numeros y determine si todos los
elementos de la lista son positivos.

En otras palabras, si (pos? l) devuelve #true ,
entonces l es un elemento del tipo de dato Lista-de-montos.

|#


;Lista-de-numeros: List(Number)
(define Lista-de-numeros (list 13 -89 123 19))

;pos?: List(Number)-> Boleean
;Devuelve #t si TODOS los elementos de la lista son positivos
(define (pos? l) (if (empty? l) #t (if (not(> (first l) 0))
                                                #f
                                                (pos? (rest l) )
                                                )
                                 )
  )

(check-expect(pos? Lista-de-numeros) #f)

;Use el evaluador paso a paso de DrRacket para entender cómo se evalúan las siguientes expresiones:

(pos? (cons 5 '()))
(pos? (cons -1 '()))

#|

Por último, diseñe la función checked-suma. Esta función recibe como entrada una
Lista-de-numeros y devuelve la suma de sus elementos si la lista pertenece a Lista-de-
montos; sino deberá devolver un string indicando un error.

|#

;checked-suma: List(Number) ->Number (si resulta true) / String (si resulta false)
;-- Si la lista contiene números positivos, retorna la suma de los mismos (tipo Number)
;-- Si la lista tiene al menos algún número negativo, retonará un mensaje de error (tipo String)
(define(checked-suma l) (if (boolean=? (pos? l) #true) (SUMA l) "Error,la lista posee números negativos."))

(check-expect (checked-suma(list 5 6 7 5)) 23)
(check-expect (checked-suma(list 5 6 -7 5)) "Error,la lista posee números negativos.")
;-------------------------------------------------------------------------

;EJERCICIO 9:


#|

Diseñe:
• todos-verdaderos , una función que recibe como entrada una lista de valores
booleanos y devuelve #true unicamente si todos los elementos de la lista son
#true .
• uno-verdadero, una función que recibe como entrada una lista de valores
booleanos y devuelve #true si al menos uno de los elementos de la lista es #true
|#


;constantes de listas:
(define L1 (list #t #t #t #t))
(define L2 (list #t #t #f #t))
;-----------------------------------

;todos-verdaderos: List(Boolean)-> Boolean
;Devuelve #t si TODOS son verdaderos (#true)
(define (todos-verdaderos l) (if (empty? l) #t (and
                                                (first l)
                                                (todos-verdaderos (rest l) )
                                                )
                                 )
  )

 

(check-expect(todos-verdaderos L1) #t)
(check-expect (todos-verdaderos L2) #f)

;-  ---   -----   -------    -------      -------   -------

;uno-verdadero: List(Boolean) -> Boolean
;Devuelve #t si ALGUNO de sus elementos es verdadero (#true)
(define (uno-verdadero l) (if (empty? l)  #f (or
                                              (first l)
                                              (uno-verdadero (rest l) ) )
                              ) 
  )

(check-expect(uno-verdadero L1) #t)
(check-expect (uno-verdadero L2) #t)

;-------------------------------------------------------------

#|

Ejercicio 10. Diseñe la función cant-elementos que dada una lista, devuelve la
cantidad de elementos que contiene.

|#

;cant-elementos: List(Any) -> Number
; Si la lista NO es vacía, devuelve la cantidad de elementos que tiene
(define (cant-elementos l) ( cond[(empty? l) 0]
                                 [(cons? l) ( + 1 (cant-elementos (rest l)) ) ] ;se le suma 1 y sigue la recursión
                                 )
  )


(check-expect (cant-elementos '()) 0)
(check-expect (cant-elementos (list 1 2 3 4 5 6 7 8 9 10)) 10)
(check-expect (cant-elementos  (list 1 2 3 4)) 4)

;-------------------------------------------------------------

#|
EJERCICIO 11:
 Diseñe la función promedio, que devuelve el promedio de una lista de
números. Sugerencia: No reinvente la rueda, ¡use los ejercicios anteriores!

|#

;promedio: List(Number)-> Number
; Calcula el promedio de una lista de números
;--- ( suma de los elementos de la lista DIVIDIDO la cantidad de elementos de la lista)

(define (promedio l) (cond [(empty? l) 0]
                           [(cons? l) ( / ( SUMA l) (cant-elementos l) ) ])
  )

(check-expect (promedio (list 1 1 1 1) ) 1 )
(check-expect (promedio (list 1 2 3 4 5 6) ) 3.5 )

;-------------------------------------------------------------------------

; Ejercicio 12. Diseñe la función pares, que dada una lista de
; números l, devuelve una lista con los números pares de l.

;pares: List(Number) -> List(Number)
;Dada una lista de números, devuelve sólo los números PARES de la misma
(define (pares l) (cond [(empty? l) '()]
                        [(cons? l) (cond    [(even? (first l)) (cons (first l) (pares (rest l))) ]
                                            [else  (pares (rest l))]
                              )
                       ]
                   )
)
                       

;pruebas:
(define prueba (list 3 5 7 0 2 8) )
(check-expect (pares prueba) (list 0 2 8) )

;------------------------------------------------------

#|
Ejercicio 13. Diseñe una función cortas que tome una lista de strings y devuelva una
lista con aquellas palabras de longitud menor a 5.
|#

;cortas: List(String)-> List(String)
;Toma una lista de palabras y devuelve una cuya longitud por cada palabra es menor a 5
(define (cortas l) (cond [(empty? l) '()]
                         [(cons? l) (cond [ (< (string-length(first l)) 5) (cons (first l) (cortas (rest l) ) ) ]
                                          [ else (cortas (rest l))] )  ]))

;pruebas:
(check-expect(cortas (list "Lista" "de" "palabras" "sin" "sentido") ) (list "de" "sin") )
(check-expect(cortas (list "hola" "no" "1234567")) (list "hola" "no") )
;-------------------------------------------------------------------------------

#|
Ejercicio 14. Diseñe la función mayores, que
dada una lista de números l y un número n,
devuelve una lista con aquellos elementos de l que son mayores a n.
|#

;mayores l num
; l > num / num < l

;mayores: List(Number) Number -> List(Number)
;Dada una lista de números y un número x, devuelve la lista con aquellos que son mayores a x
(define (mayores l num) (cond [(empty? l) '()]
                                [(cons? l) (cond [(> (first l) num) (cons (first l) (mayores (rest l) num) )]
                                                 [else (mayores (rest l) num) ]
                                           )
                                ]
                           )
  )

;pruebas:
(define prueba1 (list 6 8 3 0 9) )
(check-expect (mayores prueba1 7) (list 8 9) )
(check-expect (mayores prueba1 2) (list 6 8 3 9) )

;-------------------------------------------------------------------------------

#|
Ejercicio 15. Diseñe una función cerca que tome una lista de puntos del plano
(representados mediante estructuras posn),
 y devuelva la lista de aquellos puntos que están a distancia menor a MAX de origen,
 donde MAX es una constante de su programa
|#

; posn.
; posn < MAX
; MAX == var

(define MAX 5) ;var

;cerca: List(posn x y) -> List(posn x y)
;De una lista de coordenadas, devuelve SOLO los que son menor a MAX de origen, en este caso, 5.
( define (cerca l) (cond [(empty? l) '()]
                         [(cons? l) (cond [(and
                                            (< (posn-x (first l)) MAX)
                                            (< (posn-y (first l)) MAX))  (cons (first l) (cerca(rest l)) ) ]
                                          [else (cerca (rest l)) ]
                                      )
                         ] 
                   )
 )


(check-expect (cerca (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)) )
              (list (make-posn 1 2) (make-posn 0 1))
      )

;-------------------------------------------------------------------------------

#|
Ejercicio 16. Diseñe una función llamada positivos que tome una lista de números y
se quede sólo con aquellos que son mayores a 0.
|#

;positivos: List(Number) -> List(Number)
;Dada una lista, devuelve los números que son mayores a 0.
(define (positivos l) (cond [(empty? l) '()]
                            [(cons? l) (cond [(> (first l) 0) (cons (first l) (positivos (rest l)) )]
                                             [else (positivos (rest l)) ]
                                        )
                            ])
 )


(check-expect (positivos (list -5 37 -23 0 12)) (list 37 12) )
(check-expect (positivos (list 6 0 -3 -2222222 188888888 1.6 0.6 )) (list 6 188888888 1.6 0.6) )

;-------------------------------------------------------------------------------

#|
Ejercicio 17. Diseñe la función eliminar, que
dada una lista de números l y un numero n,
 devuelve la lista que resulta de eliminar en l todas las ocurrencias de n.
|#

; l - num

;eliminar: List(Number) Number -> List(Number)
;Dada una lista de números y un número x, retorna la lista sin ese número x
(define (eliminar l num) (cond [(empty? l) '()]
                               [(cons? l) (cond [(not( = (first l) num)) (cons (first l) (eliminar (rest l) num) ) ]
                                                [else (eliminar (rest l) num) ]
                                            )]
                          )
  )

;pruebas:
(check-expect(eliminar (list 1 2 3 2 7 6) 2) (list 1 3 7 6) )
(check-expect(eliminar (list 689 7 5 23 1 8 0 8 8 8) 8) (list 689 7 5 23 1 0) )

;-------------------------------------------------------------------------------

#|
Ejercicio 18. Diseñe la función raices, que dada una lista de números, devuelve una
lista con las raíces cuadradas de sus elementos
|#

; sqrt num

;raices: List(Number) -> List(Number)
; Dada una lista de números , nos devuelve otra con sus raíces cuadradas
(define (raices l) (cond [(empty? l) '() ]
                         [(cons? l) (cons (sqrt(first l )) (raices (rest l)) ) ]
                   )
 )
 

(check-expect(raices (list 9 16 4)) (list 3 4 2) )

;-------------------------------------------------------------------------------

#|
Ejercicio 19. Diseñe la función distancias, que dada una lista de puntos del plano,
devuelva una lista con la distancia al origen de cada uno.
|#

;fórmula distancia al origen:
(define ORIGEN (make-posn 0 0) )

(define (formulita pos1 pos2) (sqrt ( + (sqr ( - (posn-x pos2) (posn-x pos1)) ) (sqr ( - (posn-y pos2) (posn-y pos1)) ) )) )

(define (dist-origen coord)  (formulita coord ORIGEN) )
; -   -    -    -    -    -   --   --  --   --  --  --   --- --

;distancias: List(posn x y) -> List(Number)
;Dada una lista de coordenada, nos devuelve otra con la distancia al origen de c/u.
(define (distancias l) (cond [(empty? l) '()]
                             [(cons? l) (cons (dist-origen (first l)) (distancias(rest l)) ) ]
                         )
  )


; test:
(check-expect(distancias (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5))) (list 5 4 13) )

;-------------------------------------------------------------------------------

#|
Ejercicio 20. Diseñe la función anchos, que dada una lista de imágenes, devuelva una
lista con el ancho de cada una.
|#

;anchos: List(Image) -> List(Number)
; Dada una lista de imagenes, nos devuelve otra su ancho.
(define (anchos l) (cond [(empty? l ) '()]
                         [(cons? l) (cons (image-width (first l)) (anchos(rest l)) ) ]
                         )
  )

;test:
(check-expect(anchos (list (circle 30 "solid" "red") (rectangle 10 30 "outline" "blue"))) (list 60 10) )

;-------------------------------------------------------------------------------

#|
Ejercicio 21. Diseñe la función signos, que dada una lista de números,
 devuelve una
lista con el resultado de aplicarle a cada elemento la función sgn2 definida en la
Práctica 1, segunda parte.
|#

;--------- sgn2 fun:
(define (sgn2 x) (cond [(< x 0) -1] 
                       [(= x 0) 0]
                       [(> x 0) 1] )  )
;----------------------

;signos: List(Number) -> List(Number)
;Dada una lista de números, aplica la función "sgn2" sobre ella
(define (signos l) (cond [(empty? l) '()]
                         [(cons? l) (cons (sgn2 (first l)) (signos (rest l)) ) ]
                     )
  )

;test:
(check-expect(signos (list 45 32 -23 0 12)) (list 1 1 -1 0 1) )

;-------------------------------------------------------------------------------

#|
Ejercicio 22. Diseñe la función cuadrados, que dada una lista de números, devuelva la
lista que resulta de elevar al cuadrado cada uno de los elementos de la lista original.
|#

;cuadrados: List(Number) -> List(Number)
;Dada una lista de números, nos devuelve otra pero elevados al cuadrado
(define (cuadrados l) (cond [(empty? l) '()]
                            [(cons? l) (cons  (sqr (first l)) (cuadrados (rest l)))]
                            )
  )

;test:
(check-expect(cuadrados (list 1 2 3)) (list 1 4 9) )

;-------------------------------------------------------------------------------

#|
Ejercicio 23. Diseñe la función longitudes, que dada una lista de cadenas, devuelve
la lista de sus longitudes (es decir, la cantidad de caracteres que contienen).
|#

;longitudes: List(String) -> List(Number)
;Dada una lista de Strings, nos devuelve una con la cantidad de cáracteres de los mismos.
(define (longitudes l) (cond [(empty? l)'() ]
                             [(cons? l) (cons (string-length (first l)) (longitudes (rest l))) ]
                             )
  )

;test:
(check-expect(longitudes(list "hola" "cómo" "estás?")) (list 4 4 6) )

;-------------------------------------------------------------------------------

#|
Ejercicio 24. Diseñe la función convertirFC, que dada una lista de temperaturas en
grados Fahrenheit, devuelve esta lista de temperaturas convertidas en grados Celsius.
|#


; fahrenheit to Celsius:
;Temperature in degrees Celsius (°C) = (Temperature in degrees Fahrenheit (°F) - 32) * 5/9.
; C = (F - 32) / 1.8
; in R: (/ (- f 32) 1.8)

;FarToCel: Number->Number
;Dado un valor en fahrenheit, nos lo devuelve en Celcius
(define (FarToCel far) (/ (- far 32) 1.8) )

;convertirFC: List(Number) -> List(Number)
;Dada una lista de grados Fahrenheit, nos devuelve la misma en grados Celcius
(define (convertirFC l) (cond [(empty? l)'() ]
                              [(cons? l) (cons (FarToCel(first l)) (convertirFC (rest l)) )]
                              )
  )

;testeo:
(define proof (list 32 123 12 0 )) ;0 50.5556 -11.1111 -17.7778

(convertirFC proof) 

;-------------------------------------------------------------------------------

#|
Ejercicio 25. Diseñe la función prod, que multiplica los elementos de una lista de
números entre sí. Para la lista vacía, devuelve 1.
|#

;prod: List(Number) -> Number
;--- SI NO está vacía: Múltiplica todos los elementos de la lista
;--- SI está vacía: Devuelve 1.
(define (prod l) (cond [(empty? l) 1]
                       [(cons? l) (* (first l) (prod (rest l)))] ;multiplica el primero * el producto del resto de la lista (es decir L = 1l * restl)
                       )
  )

;test:
(check-expect(prod (list 1 2 3 4 5)) 120)
(check-expect(prod (list 0 9 7 3 6 0)) 0)

;-------------------------------------------------------------------------------

#|
Ejercicio 26. Diseñe la función pegar, que dada una lista de strings, devuelve el string
que se obtiene de concatenar todos los elementos de la lista.
|#

;pegar: List(String) -> String
;Devuelve un String, resultado de unir todos los strings de la lista dada.
(define (pegar l) (cond[(empty? l) "" ]
                       [(cons? l) (string-append (first l) (pegar(rest l)))  ]
                       )
  )

(check-expect (pegar (list "Las " "lis" "tas " "son " "complicadas" ".")) "Las listas son complicadas." )

;-------------------------------------------------------------------------------

#|

Ejercicio 27. Diseñe la función maximo que devuelve el máximo de una lista de
números naturales. Para la lista vacía, devuelve 0 .

|#


;usar pos' l como condicion y de ahi si first > 5 entonces... y ahi recursion (ez)

;maximo: List(Number) -> Number
;Dada una lista de números, devuelve el número más grande (mayor)
 (define (maximo l) (cond[(empty? l) 0]
                         [(cons? l) (if (>= (first l) (maximo (rest l)))  (first l) (maximo (rest l)) ) ]
                                         )
   )

 (check-expect (maximo (list 23 543 325 0 75)) 543)
 (check-expect (maximo (list 1 -2 34 607)) 607)

;-------------------------------------------------------------------------------

#|

Ejercicio 28. Diseñe la función sumdist, que dada una lista de puntos del plano,
devuelva la suma de sus distancias al origen.

|#

;sumdist: List(posn x y) -> Number
;Dada una lista de cordenadas, devuelve la SUMA de sus DISTANCIAS respecto al origen.
(define (sumdist l) (cond[(empty? l) 0]
                         [(cons? l) (SUMA(distancias l)) ]
                         )
  ) 

(check-expect (sumdist (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5))) 22 )

;-------------------------------------------------------------------------------

#|

Ejercicio 29. Diseñe la función sumcuad, que dada una lista de números, devuelve la
suma de sus cuadrados. Para la lista vacía, devuelve 0 .

|#

;sumcuad: List(Number) -> Number
;---Si la lista es vacía: Retorna 0
;---Si la lista tiene AL MENOS un elementos, retorna la SUMA de sus CUADRADOS (números elevados a la 2)
(define (sumcuad l) (cond[(empty? l) 0]
                         [(cons? l) (SUMA(cuadrados l)) ]
                     )
  )

(check-expect (sumcuad (list 1 2 3)) 14)




