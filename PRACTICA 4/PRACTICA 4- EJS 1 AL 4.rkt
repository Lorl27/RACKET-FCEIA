;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4- EJS 1 AL 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define p (make-posn 3 4))
(define q (make-posn -2 0.5)) ;hace coord x y

(posn-y q) ;return y

;posn? -> t/f

;----------------------------------

;ej 1  Calcule el valor de las siguientes 3 expresiones:


 (posn-x p)
 (- (posn-y p) (posn-y q))
(posn-y (make-posn (posn-x p) (posn-x q)))

;---------------------------------------------

;ej 2

#|
Defina una función dist-origen que calcule la distancia al
origen de un punto, representado por una estructura posn . Calcule el valor de
las siguientes expresiones:
• (dist-origen (make-posn (/ 6 2) 4))
• (+ (dist-origen (make-posn 12 5)) 4)
|#

;/////////////////////////////////DISEÑO DE DATOS//////////////////////////////////////
;(define-struct posn [x y])
; posn es (Number , Number)
; interpretación: un elemento en posn representa una posición en coordenadas cartesianas

;-------------CONSTRUCTOR:
; make-posn : Number Number -> posn
;------------SELECTORES:
; posn-x : posn -> Number
; posn-y : posn -> Number
;-------------PREDICADO:
;posn?: Any -> Boolean
;////////////////////////////////////FIN DISEÑO DE DATOS//////////////////////////////

"ej 2:"

(define ORIGEN (make-posn 0 0) )

(define (formulita pos1 pos2) (sqrt ( + (sqr ( - (posn-x pos2) (posn-x pos1)) ) (sqr ( - (posn-y pos2) (posn-y pos1)) ) )) )

(define (dist-origen coord)  (formulita coord ORIGEN) )

(dist-origen (make-posn (/ 6 2) 4))
(+ (dist-origen (make-posn 12 5) ) 4)

"ej 3:"

;------------------------------------------------------------------------------

;ej 3: Diseñe una función simétrico, que dada una posición, nos devuelva su simétrica respecto del origen.

(define (simetrico pos) (make-posn  ( - 0 (posn-x pos)  ) (posn-y pos) ) )

(simetrico (make-posn 3 4))

"ej 4:"

;-------------------------------------------------
#| ej 4: Diseñe una función distancia, que dado dos puntos en el plano
calcule la distancia entre ellos. Extienda la definción para que, en caso que
alguno de los argumentos no sea de tipo posn , muestre el mensaje "Tipos
incorrectos para la función." |#

(define (distancia punto1 punto2) (if (and (posn? punto1) (posn? punto2)) (formulita punto2 punto1) "Tipos incorrector para la función"))

(distancia p q)