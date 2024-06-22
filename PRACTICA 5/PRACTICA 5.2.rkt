;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |PRACTICA 5.2|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;      P5.2

; filter : (X -> Boolean) List(X) -> List(X) ;la filtrea

(filter even? (list 1 2 3 4 5)) ;(list 2 4)
(filter string? (list 3 "Lista" #true "heterogénea")) ;(list "Lista" "heterogénea")

#|

Ejercicio 1. Resuelva los ejercicios 12, 13, 15, 16 y 17 de la Práctica 5,
primera parte utilizando filter .

|#

;-------------EJ 12:                       
(define (pares2 l) (filter even? l) )


;pruebas:
(define prueba (list 3 5 7 0 2 8) )
(check-expect (pares2 prueba) (list 0 2 8) )

;--------------EJ 13:

(define (cortas? x) (< (string-length x) 5) )
(define (cortas2 l) (filter cortas? l))
  
;pruebas:
(check-expect(cortas2 (list "Lista" "de" "palabras" "sin" "sentido") ) (list "de" "sin") )


;--------------- EJ 14:
(define (mayores2 l n) (filter (lambda (x) ( > x n) )l))
  
;pruebas:
(define prueba1 (list 6 8 3 0 9) )
(check-expect (mayores2 prueba1 7) (list 8 9) )

;-----------------------EJ 15:

(define MAX 5) ;var

(define (cercania? l) (and (< (posn-x l) MAX) (< (posn-y l) MAX) ))
(define (cerca2 l) (filter  cercania? l ))

(check-expect (cerca2 (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)) )
              (list (make-posn 1 2) (make-posn 0 1)))
  

;------------------EJ 16:
(define (pos? l) (> l 0) )

(define (positivos2 l) (filter pos? l))
(check-expect (positivos2 (list -5 37 -23 0 12)) (list 37 12) )

;---------------------------------- EJ 17:

(define (eliminar2 l num) (filter (lambda(x) (not(= x num)) )l )) 

;pruebas:
(check-expect(eliminar2 (list 1 2 3 2 7 6) 2) (list 1 3 7 6) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 6. Diseñe una función eliminar-0 que tome una lista de
números y devuelva la lista luego de eliminar todas las ocurrencias del número 0

|#

(define (eliminar-0 l ) (filter (lambda(x) (not(= x 0)) ) l) )
;pruebas:
(check-expect(eliminar-0 (list 1 0 3 0 7 6)) (list 1 3 7 6) )

;------------------------------------------------------------------
; map : (X -> Y) List(X) -> List(Y) ;transforma la lista , opera SOBRE ella

(map sqr (list 1 2 3 4 5)) ;(list 1 4 9 16 25)
(map string-length (list "Lista" "de" "palabras")) ;(list 5 2 8)

#|

Ejercicio 8. Diseñe la función raices , que dada una lista de números,
devuelve una lista con las raíces cuadradas de sus elementos.

|#

(define (raices l) (map sqrt l) )
(check-expect (raices (list 4 9 16)) (list 2 3 4))

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 9. Diseñe una función distancias que tome una lista de
puntos del plano y devuelva una lista con la distancia al origen de cada
uno.

|#

;fórmula distancia al origen:
(define ORIGEN (make-posn 0 0) )

(define (formulita pos1 pos2) (sqrt ( + (sqr ( - (posn-x pos2) (posn-x pos1)) ) (sqr ( - (posn-y pos2) (posn-y pos1)) ) )) )

(define (dist-origen coord)  (formulita coord ORIGEN) )
; -   -    -    -    -    -   --   --  --   --  --  --   --- --

(define (distancias2 l)  (map dist-origen l  ) )


; test:
(check-expect(distancias2 (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5))) (list 5 4 13) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 10. Diseñe una función anchos que tome una lista de
imágenes y devuelva una lista con el ancho de cada una.

|#

(define (anchos2 l) (map image-width l))

;test:
(check-expect(anchos2 (list (circle 30 "solid" "red") (rectangle 10 30 "outline" "blue"))) (list 60 10) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 11. Diseñe la función signos, que dada una lista de números,
devuelve una lista con el resultado de aplicarle a cada elemento la
función sgn2 definida en la práctica 1.

|#

;--------- sgn2 fun:
(define (sgn2 x) (cond [(< x 0) -1] 
                       [(= x 0) 0]
                       [(> x 0) 1] )  )
;----------------------

(define (signos2 l) (map sgn2 l)  )

;test:
(check-expect(signos2 (list 45 32 -23 0 12)) (list 1 1 -1 0 1) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 12. Diseñe una función cuadrados que tome una lista de
números y devuelva otra lista donde los elementos que aparezcan sean
el cuadrado de los elementos de la lista original.

|#

(define (cuadrados2 l) (map sqr l) )

;test:
(check-expect(cuadrados2 (list 1 2 3)) (list 1 4 9) )


;-----------------------------------------------------------------------------------------------

#|

Ejercicio 13. Diseñe una función longitudes que tome una lista de
cadenas y devuelva una lista de números que corresponda con la
longitud de cada cadena de la lista original

|#

(define (longitudes2 l) (map string-length l) ) 

;test:
(check-expect(longitudes2(list "hola" "cómo" "estás?")) (list 4 4 6) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 14. Diseñe la función convertirFC, que convierte una lista de
temperaturas medidas en Fahrenheit a una lista de temperaturas
medidas en Celsius.

|#

; fahrenheit to Celsius:
;Temperature in degrees Celsius (°C) = (Temperature in degrees Fahrenheit (°F) - 32) * 5/9.
; C = (F - 32) / 1.8
; in R: (/ (- f 32) 1.8)

;FarToCel: Number->Number
;Dado un valor en fahrenheit, nos lo devuelve en Celcius
(define (FarToCel far) (/ (- far 32) 1.8) )

(define (convertirFC2 l) (map FarToCel l))

;testeo:
(define proof (list 32 123 12 0 )) ;0 50.5556 -11.1111 -17.7778

(convertirFC2 proof)

;-----------------------------------------------------------------------------------------------

#|

 La función foldr recibe tres argumentos:
• La función f con la que se quiere operar los elementos de la lista;
• Un valor c , que es el resultado esperado para la lista vacía;
• La lista l a transformar

|#

(foldr * 1 (list 1 2 3 4 5));120
(foldr string-append "" (list "Pro" "gra" "ma" "ción."));"Programación"

#|

Ejercicio 15. Diseñe una función prod que multiplica los elementos de
una lista de números. Para la lista vacía, devuelve 1

|#

(define (prod2 l) (foldr * 1 l) ) ;foldr: cosa a hacer , si si la lista es vacia , lista

;test:
(check-expect(prod2 (list 1 2 3 4 5)) 120)
(check-expect (prod2 '() ) 1)

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 16. Diseñe una función pegar que dada una lista de strings,
devuelve el string que se obtiene de concatenar todos los elementos de
la lista.

|#

(define (pegar2 l) (foldr string-append "" l) )

(check-expect (pegar2 (list "Las " "lis" "tas " "son " "complicadas" ".")) "Las listas son complicadas." )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 17. Diseñe una función max que devuelve en máximo de una
lista de naturales. Para la lista vacía, devuelve 0.

|#

(define (maximo2 l) (foldr  max 0 l) )

 (check-expect (maximo2 (list 23 543 325 0 75)) 543)

;-----------------------------------------------------------------------------------------------

#|

; Intente utlizar map, fold y filter para
construir sus soluciones.

Ejercicio 18. Diseñe una función sumcuad que dada una lista de
números, devuelve la suma de sus cuadrados. Para la lista vacía,
devuelve 0 .
Dada una lista l, podemos dividir este problema en dos tareas:
• Calcular los cuadrados de todos los elementos de l, y
• sumar estos valores.

|#

(define (suma l) (foldr + 0 l))

(define (sumacuad l) (suma (cuadrados2 l) ) )

(check-expect (sumacuad (list 1 2 4)) 21)

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 19. Diseñe la función sumdist, que dada una lista l de
estructuras posn , devuelve la suma de las distancias al origen de cada
elemento de l.

|#

(define (sumdist2 l) (suma (distancias2 l) ) )

(check-expect (sumdist2 (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5))) 22 )


;-----------------------------------------------------------------------------------------------

#|

Ejercicio 20. Diseñe una función multPos , que dada una lista de
números l , multiplique entre sí los números positivos de l

|#

(define (multPos l)  (prod2 (positivos2 l)) )

(check-expect (multPos (list 2 3 1)) 6)

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 21. Diseñe una función sumAbs , que dada una lista de
números, devuelve la suma de sus valores absolutos

|#

(define (valABS x) (if (>= x 0) x  (* x -1)) )

(define (sumABS l) (suma(map valABS l)) )

(check-expect (sumABS (list -1 2 -5 10)) 18)
;-----------------------------------------------------------------------------------------------

#|

Ejercicio 22. Diseñe la función raices, que dada una lista de números
l, devuelve una lista con las raíces cuadradas de los números no
negativos de l.

|#

(define (raiz l) (raices (positivos2 l)) )

(check-expect (raiz (list -3 -4 4 -4 16 -16)) (list 2 4) )

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 23. Diseñe la función saa, que dada una lista de imágenes,
devuelva la suma de las áreas de aquellas imágenes "Anchas".

|#

(define (ancha? l) (> (image-width l) (image-height l )) ) ;se fija si es ancha

(define (area l) (if (ancha? l) (* (image-height l) (image-width l) ) 0 ) ) ;pregunta si es ancha  o no

(define (Only-Anchas l) (map area l)  ) ;itera sobre cada area de lista

(define (saa l) (suma (Only-Anchas l) ) )

(check-expect (saa (list
                    (circle 20 "solid" "red")
                    (rectangle 40 20 "solid" "blue")
                    (rectangle 10 20 "solid" "yellow")
                    (rectangle 30 20 "solid" "green")   )) 1400)

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 24. Diseñe la función algun-pos, que toma una lista de listas
de números y devuelve #true si y sólo si para alguna lista la suma de
sus elementos es positiva

|#

;(define (pos? l) (> l 0) )
;(define (suma l) (foldr + 0 l))

(define (sumLista l) (map suma l) )

(define (listaPos l) (map pos? (sumLista l) ) )

(define (true? l) (boolean=? l #t) )

(define (algun-pos l)   (cons? (filter true? (listaPos l) ) ) ) ;cons? ya que quiero boolean y no lista

(check-expect (algun-pos (list (list 1 3 -4 -2) (list 1 2 3 -5) (list 4 -9 -7 8 -3))) #t )
(check-expect (algun-pos (list empty (list 1 2 3))) #t)
(check-expect (algun-pos (list (list -1 2 -3 4 -5) empty (list -3 -4))) #f)

;-----------------------------------------------------------------------------------------------

#|

Ejercicio 25. Diseñe la función long-lists, que toma una lista de listas
y devuelve #true si y sólo si las longitudes de todas las sublistas son
mayores a 4.

|#

(define (cantidadesLista x l) (+ 1 l) ) ;x necesario porque si no da error wtf 

(define (cant l) (foldr cantidadesLista 0 l) ) ;cuenta cada elemento de la LISTAS de listas

(define (mayor4? l) (> (cant l)4)  )

(define (listasMayores l) (map mayor4? l  ) ) ;MAPEA cada lista para q me de valores solitarios

(define (long-lists l) (equal? (cant  (filter true? (listasMayores l)) )
                               (cant l) )
                         ) ;asi empty no da f

(check-expect (long-lists (list (list 1 2 3 4 5) (list 1 2 3 4 5 6) (list 87 73 78 83 33))) #t)
(check-expect (long-lists (list '() '() (list 1 2 3))) #f)
(check-expect (long-lists (list (list 1 2 3 4 5) empty)) #f)

;------------------------------------------------------------------------------------------

#|
Ejercicio 26. Diseñe una función todos-true que toma una lista de
valores de cualquier tipo, y devuelve #true si y sólo si todos los valores
booleanos de la lista son verdaderos. Caso contrario, devuelve #false
|#



(define (and2 a b) (and a b) )

(define (todos-v lista) (foldr and2 #t lista))

(define (todos-true l) (todos-v (filter boolean? l) )) ;se queda solo con los booleans

(check-expect (todos-true (list 5 #true "abc" #true "def")) #t)

;------------------------------------------------------------------------------------------

#|

Ejercicio 27. Dada la definición de la estructura alumno:
(define-struct alumno [nombre nota faltas])
; alumno (String, Number, Natural). Interpretación
; - nombre representa el nombre del alumno.
; - nota representa la calificación obtenida por el alumno (entre 0 y 10).
; - faltas: número de clases a las el alumno no asistió.

Diseñe las siguientes funciones:
• destacados , que dada una lista de alumnos, devuelve una lista con el nombre
 de aquellos alumnos que sacaron una nota mayor o igual a 9.

Ejemplo:
(destacados (list (make-alumno "Ada Lovelace" 10 20)
(make-alumno "Carlos Software" 3.5 12))) = (list "Ada Lovelace")

• condicion , que dado un alumno, determine su condición de acuerdo a las siguientes reglas:
◦ si la nota es mayor o igual a 8, su condición es "promovido".
◦ Si la nota es menor a 6, su condición es "libre" .
◦ En cualquier otro caso, la condición es "regular".

• exito , que dada una lista de alumnos, devuelve #true si ninguno está libre. Caso contrario, devuelve #false.

Ejemplo:
(exito (list (make-alumno "Juan Computación" 5 13)
(make-alumno "Carlos Software" 3.5 12)
(make-alumno "Ada Lovelace" 10 20))) = #false

• faltas-regulares , que dada una lista de alumnos, devuelve la suma de las ausencias de los alumnos regulares.

Ejemplo:
(faltas-regulares (list (make-alumno "Juan Computación" 7 2)
(make-cliente "Carlos Software" 3.5 4)
(make-alumno "Ada Lovelace" 10 1))) = 2

• promovidos-ausentes , que dada una lista de alumnos, devuelve una lista con
el nombre de aquellos alumnos promovidos que no asistieron a tres o más clases.

Ejemplo:
(promovidos-ausentes (list (make-alumno "Juan Computación" 9 3)
(make-cliente "Carlos Software" 3.5 2)
(make-alumno "Ada Lovelace" 10 1))) = (list "Juan Computación")


|#

(define-struct alumno [nombre nota faltas])
; alumno (String, Number, Natural).
;Interpretación:
; - nombre representa el nombre del alumno.
; - nota representa la calificación obtenida por el alumno (entre 0 y 10).
; - faltas: número de clases a las el alumno no asistió.

(define (mayor9? l) (>= (alumno-nota l) 9) ) ;Verifica si la nota es mayor/igual a 9

(define (destacadosLISTA LISTAusuarios) (filter mayor9?  LISTAusuarios) ) ;hace una LISTA con los ALUMNOS cuya nota es >= a 9

(define (destacados l) (map alumno-nombre (destacadosLISTA l) ))  ;nos devuelve la LISTA de NOMBRES de alumnos con nota >= 9

(check-expect (destacados (list (make-alumno "Ada Lovelace" 10 20)
                                 (make-alumno "Carlos Software" 3.5 12)))
              (list "Ada Lovelace") )

;---------------
#|(>=  (map alumno-nota alumnis) 8)

(define (USR-LIST listaUS) (filter promovido? listaUS) )
(define (USR-LIST1 listaUS) (filter libre? listaUS) )

(define (condicion alumnis) (cond[(promovido? (map alumno-nota (USR-LIST alumnis)) )  "promovido"]
                                 [(map alumno-nota (USR-LIST1 alumnis))   "libre"]
                                 [else "regular"]
                           )
  )

;-------------
(define (promovidos-ausentes lista) (cond [(empty? lista) '()]
                                          [(cons? lista) (if (OnlyFaltas3 (first lista))
                                                               (cons (alumno-nombre (first lista)) (promovidos-ausentes (rest lista)))
                                                                (promovidos-ausentes (rest lista)))
                                                         ]
                                          )
  )



|#

(define (promovido? l) (>= (alumno-nota l) 8) )
(define (libre? l) (< (alumno-nota l) 6) )

;condicion: List(Args) -> String / Devuelve la cond del alumno
(define (condicion alumnis) (cond[(promovido? alumnis)  "promovido"]
                                 [(libre? alumnis)   "libre"]
                                 [else "regular"]
                           )
  )

(check-expect (condicion (make-alumno "yo" 2 10)) "libre")
;---------------

(define (ESTAlibre? lista) (string=? (condicion lista) "libre") )

(define (Only-Libres lista) (filter ESTAlibre? lista) ) ;CREA una LISTA con los alumnos que son LIBRES

(define (exito lista) (empty? (Only-Libres lista )  ) ) ;como la lista solo es de LIBRES, devolverá #t si ESTA VACIA == no hay LIBRES

(check-expect
             (exito (list (make-alumno "Juan Computación" 5 13)
                          (make-alumno "Carlos Software" 3.5 12)
                          (make-alumno "Ada Lovelace" 10 20)))
              #false )
;-----------------------------

(define (ESTAregular? lista) (string=? (condicion lista) "regular") )

(define (Only-Regulares lista) (filter ESTAregular? lista) ) ;lista de only REGULARES

(define (faltas-alum l r)  (+(alumno-faltas l) r)) ;fldr necesita 2 args por eso la r

(define (faltas-regulares l) (foldr faltas-alum 0 (Only-Regulares l) ) ) ;suma las faltas SOLO en los REGULARES

(check-expect
             (faltas-regulares (list (make-alumno "Juan Computación" 7 2)
                                     (make-alumno "Carlos Software" 3.5 4)
                                     (make-alumno "Ada Lovelace" 10 1))) 2)
;---------------------------------------

(define (faltas3? lista) (>= (alumno-faltas lista) 3) )

(define (ESTApromovido? lista) (string=? (condicion lista) "promovido") )

(define (OnlyFaltas3 lista) (and (faltas3? lista) (ESTApromovido? lista) ) ) ;LISTA de SOLO los q faltaron 3 veces o más y fueron PROMOVIDOS

(define (promovidos-ausentes22 lista) (map alumno-nombre (filter OnlyFaltas3 lista) ) ) 


(check-expect
             (promovidos-ausentes22 (list (make-alumno "Juan Computación" 9 3)
                                        (make-alumno "Carlos Software" 3.5 2)
                                        (make-alumno "Ada Lovelace" 10 1)))
             (list "Juan Computación")
             )