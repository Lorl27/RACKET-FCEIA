;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |PRACTICA 1 PARTE 2 ESTA SI ES 2 LA  OTRA ERA P1 LOL|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; PRACTICA 1 PARTE 2 Aaaa

;F signo, que recibe un número y nos devuelve -1, 0 o 1 de acuerdo a si su argumento es negativo, cero o positivo respectivamente.

(define (sgn1 x) (if (< x 0) -1 (if (= x 0) 0 1)))

(define (sgn2 x) (cond [(< x 0) -1] 
                       [(= x 0) 0]
                       [(> x 0) 1] )  )

#| 
Ejercicio 1. Calcule el resultado de las expresiones (sgn2 (- 2 3)) y (sgn2 6).
|#
(sgn2 (- 2 3))
(sgn2 6)
;---------------------------------------------------

#|
Ejercicio 2. Rehaga, utilizando expresiones cond, los ejercicios 2, 4, 6 y 7 de la
Práctica 1, 1ra parte - sección 1.3.
|#

#|
EJERCICIO NÚMERO 2:
(define img (circle 50  "solid" "red" ))
(define img2 (circle 10  "solid" "blue"))

(if  ( >= (image-height img) (image-width img2)) (if ( = (image-height img) (image-width img2)) "es cuadrada" "es angosta")  "no lo es")
|#
 
(define (ejer2 img1 img2) (cond [ ( < (image-height img1) (image-width img2) ) "La imagen es ancha"]
                                [ ( = (image-height img1) (image-width img2) ) "La imagen es cuadrada"]
                                [ ( > (image-height img1) (image-width img2) ) "La imagen es angosta"]
                                [else "error"] ) )

(ejer2 (circle 50 "solid" "magenta") (circle 90 "outline" "pink"))

#|
EJERCICIO NÚMERO 4:
(define a 20)
(define b 30)
(define c 40)

;(if ( = ( + a c b) 180) (if ( and ( = a c) ( = a b) ( = b c)) "escaleno"  (if  (and (>= a c)  (= c b) )  "isoceles" "escaleno") )  "no es triangulo!!!!!" )
|#
 
(define (ejer4 a b c) (if ( = ( + a b c) 180) (cond [ ( and ( = a c) ( = a b) ( = b c)) "Es equilátero." ]
                                                     [ (or (and (> a c)  (= c b) ) (and (< a b)  (= c b) ) (and (> a c) ( = a b)) (and (< a c) ( = a b))) "Es isoscéles"]
                                                     [ else "Es escaleno."] )
                        "No es un triángulo..." )  )

(ejer4 10 10 10)
(ejer4 70 30 80)
(ejer4 60 60 60)
(ejer4 50 50 80)

#|
EJERCICIO NÚMERO 6:
 (...)
|#


#|
EJERCICIO NÚMERO 7:


(define (pitagorica? num1 num2 num3) (if ( = ( + (sqr num1) (sqr num2) ) (sqr num3) )
    (string-append "Se formo una terna pitag{orica con los números: " (number->string num1) ", " (number->string num2) " y " (number->string num3) )
   (string-append " NO se formo una terna con los números: " (number->string num1) ", " (number->string num2) " y " (number->string num3) )) )

(pitagorica? 12 4 6)
|#

(define (pitagorica? num1 num2 num3) (cond [ ( = (+ (sqr num1) (sqr num2)) (sqr num3) ) "Se formo una terna"]
                                           [else "No hay terna!"]))
(pitagorica? 12 4 6)

;--------------------------------------------------

#|
Ejercicio 3. Calcule el resultado de las expresiones (pitagorica? 3 5 6) y
(pitagorica? 3 5 4). Atención: El resultado de la última expresión debe ser
#true.
|#

(pitagorica? 3 5 6)
(pitagorica? 3 5 4) ;??? porque #t si esto es #f 9+25!=16
;------------------------------------------------

#|
Ejercicio 4. Hemos decidido hilar más fino en la clasificación de imágenes.
Ahora diremos que una imagen es "Muy ancha" si su ancho es más que el doble
que su alto. Del mismo modo, diremos que "Muy angosta" si su alto es más que
el doble que su ancho. Defina una función, utilizando una expresión cond, que
clasifique imágenes en alguna de las categorías "Muy ancha", "Ancha",
"Cuadrada", "Angosta", "Muy angosta".
|#

(define (ejer4.2 img) (cond [ ( < ( * (image-height img) 2) (image-width img) ) "La imagen es MUY ancha"]
                            [ ( > (image-height img) ( * (image-width img) 2) ) "La imagen es MUY angosta"]
                            [ ( < (image-height img) (image-width img) ) "La imagen es ancha"]
                            [ ( = (image-height img) (image-width img) ) "La imagen es cuadrada"]
                            [ ( > (image-height img) (image-width img) ) "La imagen es angosta"]
                            [else "error"] ) )

(ejer4.2 (circle 50 "solid" "magenta") )
(ejer4.2 (rectangle 25 5 "solid" "magenta") )
(ejer4.2 (rectangle 5 25 "solid" "magenta") )
(ejer4.2 (rectangle 2 3 "solid" "purple"))
(ejer4.2 (rectangle 3 2 "solid" "purple"))
;--------------------------------------------------------

#|
Ejercicio 5. Como parte de una aplicación para observar el clima, se pide
clasificar una temperatura de la siguiente forma:
"Muy frío (MF)" (menos de 0 grados);
"Frío (F)" (entre 0 y 15 grados);
"Agradable (A)" (entre 15 y 25 grados);
"Caluroso (C)" (más de 25 grados);

Considere la siguiente función:
(define (clasificar t) (cond [(< t 0) "Muy frío (MF)"]
[(and (> t 0) (< t 15)) "Frío (F)"]
[(and (> t 15) (< t 25)) "Agradable (A)"]
[(> t 25) "Caluroso (C)"]))

Evalúe cada expresión de la forma indicada:
(clasificar -3), en el área de interacción
(clasificar 12), con lápiz y papel
(clasificar 28), con el evaluador paso a paso de DrRacket .

¿Qué sucede con la expresión (clasificar 15)? Tómese un minuto para
entender cuál es el problema ¿Para qué valores sucede lo mismo? Redefina la
función para todas las temperaturas queden clasificadas (decida usted en qué
categoría quedan los valores no contemplados previamente).
|#

(define (clasificar temp) (cond [(<= temp 0) "Muy frío (MF)"]
                                [(and (> temp 0) (<= temp 15)) "Frío (F)"]
                                [(and (> temp 15) (<= temp 25)) "Agradable (A)"]
                                [(> temp 25) "Caluroso (C)"]))
(clasificar 28)

;--------------------------------------------------------

#|
Ejercicio 6. Muchas veces se identifica a los valores booleanos con los números
0 (para #false) y 1 (para #true). Con esta identificación en mente, extienda la
función signo para que pueda procesar booleanos.
|#

;(define (sgn2 x) (cond [(< x 0) -1] 
;                       [(= x 0) 0]
;                       [(> x 0) 1] )  )

(define (sgn3 x) (cond [(number? x) (sgn2 x)]
                       [(string? x) (sgn2 (string->number x)) ] ) )


(define (signo4 x) (cond [ (number? x) (sgn2 x) ] 
                         [ (string? x) (sgn2 (string->number x)) ]
                         [ (boolean? x) (sgn2 (if ( boolean=? x #true) 1 0)) ]      )  )
(signo4 #true)

;------------------------------------------------------------------------------------

#|
Ejercicio 7. Las imágenes "Angostas" son negativas, mientras que las "Anchas"
son positivas. Obviamente, las "Cuadradas" equivalen al 0 de los números.
Extienda la función signo para soportar imágenes.
|#

(define (signo5 x) (cond [ (number? x) (sgn2 x) ] 
                         [ (string? x) (sgn2 (string->number x)) ]
                         [ (boolean? x) (sgn2 (if ( boolean=? x #true) 1 0)) ]
                         [ (image? x) (cond [ (or (string=? (ejer4.2 x) "La imagen es MUY ancha") (string=? (ejer4.2 x) "La imagen es ancha")) 1]
                                            [ (or (string=? (ejer4.2 x) "La imagen es MUY angosta") (string=? (ejer4.2 x) "La imagen es angosta")) -1]
                                            [ (string=? (ejer4.2 x) "La imagen es cuadrada") 0] ) ]
                         )  )

(signo5 (rectangle 3 2 "solid" "purple")) ;angosta == 1 :)

;---------------------------------------------------------------------------

#|
Ejercicio 8. Modifique la función definida en el ejecicio anterior para que, en
caso de no recibir un número, booleano, imagen o string nos muestre el
siguiente mensaje "Clase no soportada por la función.".
Ayuda: Quizás sea interesante mirar la documentación de las expresiones cond,
en particular lo referente a las cláusulas else.
|#

(define (signo6 x) (cond [ (number? x) (sgn2 x) ] 
                         [ (string? x) (sgn2 (string->number x)) ]
                         [ (boolean? x) (sgn2 (if ( boolean=? x #true) 1 0)) ]
                         [ (image? x) (cond [ (or (string=? (ejer4.2 x) "La imagen es MUY ancha") (string=? (ejer4.2 x) "La imagen es ancha")) 1]
                                            [ (or (string=? (ejer4.2 x) "La imagen es MUY angosta") (string=? (ejer4.2 x) "La imagen es angosta")) -1]
                                            [ (string=? (ejer4.2 x) "La imagen es cuadrada") 0] ) ]
                         [else "Clase no soportada por la función..."]
                         )  )

;-------------------------------------------------------------------------------

#|
Ejercicio 9. Como última extensión, modifique la función para que imprima "La
cadena no se puede convertir a un número", en caso que se procese un
string para el que la función number->string no devuelva un número.
|#

(define (signo7 x) (cond [ (number? x) (sgn2 x) ] 
                         [ (string? x) (if (string-numeric? x)(sgn2 (string->number x)) "El str no se puede convertir a Number...") ]
                         [ (boolean? x) (sgn2 (if ( boolean=? x #true) 1 0)) ]
                         [ (image? x) (cond [ (or (string=? (ejer4.2 x) "La imagen es MUY ancha") (string=? (ejer4.2 x) "La imagen es ancha")) 1]
                                            [ (or (string=? (ejer4.2 x) "La imagen es MUY angosta") (string=? (ejer4.2 x) "La imagen es angosta")) -1]
                                            [ (string=? (ejer4.2 x) "La imagen es cuadrada") 0] ) ]
                         [else "Clase no soportada por la función..."]
                         )  )

(signo7 "123a")