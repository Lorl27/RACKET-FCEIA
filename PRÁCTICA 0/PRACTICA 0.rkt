;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |PRACTICA 0|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;usar un paquete: (require [name])
;EJ: (requiere 2htdp/image) --> para imgs

 #|                 PRÁCTICA 0
;                    EJ 1:
( - ( * 12 5) ( * 7 6))
( + ( - ( * 3 5 ) ( * 7 ( / 4 14) ( / 3 1 )
 ( + 1/5 (cos 0.8) ( * (sin 0.5) 3.5))

;                    EJ 2:

(/ 1 (sin (sqrt 3)))
(* (sqrt 2) (sin pi))
(+ 3 (sqrt (- 4)))
(* (sqrt 5) (sqrt (/ 3 (cos pi))))
(/ (sqrt 5) (sin (* 3 0)))
(/ (+ 3) (* 2 4))
(* 1 2 3 4 5 6 7 8)
(/ 120 2 3 2 2 5)

;                  EJ 3:
 ( = (sin 0.6 ) (min  6 7 0 ))

;                  EJ 4
 (string-append "Hola" "mundo")
 (string-append "Pro" "gra" "ma.")
 (number->string 1357)
 (string-append "La respuesta es " (+ 21 21))
 (string-append "La respuesta es " (number->string (+ 21 21)))
 (* (string-length "Hola") (string-length "Chau"))


;                      EJ 5:
(substring "Programar" 2 5)
(substring "Programar" 2 1)
(substring "Programar" 4 4)
(substring "Programar" 2 16)
(substring "Programar" -2 5)

;                       EJ 6:
 (not #t)
(or #t #f)
 (and #t #f)
 (and #t (or #f (not #f)) (not #t))
 (not (= 2 (* 1 3)))
 (or (= 2 (* 1 3)) (< 4 (+ 3 2)))

;                   EJ 7

( > (cos 0) 0 )
( = (string-length "Hola, mundo") 14)
( and  ( >= pi 3 ) ( <= pi 4))
( = (sqr 5) (sqrt 625))
( string-contains? (string-ith "Ada Lovelace" 3)"a")

;             EJ 8:
 (circle 10 "solid" "red")
 (rectangle 10 20 "solid" "blue")
 (rectangle 20 12 "outline" "magenta")
 (overlay (rectangle 20 20 "solid" "blue") (circle 7 "solid" "green"))
 (empty-scene 100 100)
 (place-image (circle 10 "solid" "blue") 40 80 (empty-scene 100 100))
 (+ (image-width (circle 10 "solid" "red")) (image-height (rectangle 10 20 "solid" "blue")))

;                 EJ 9:
(define P "Neptuno")
(define 1TRES 3)
(define CINCO (+ 1 (* 2 2)))
(define VERDADERO #true)
(define PUNTOROJO (circle 3 "solid" "red"))

;                EJ 10:
(define (el-siguiente x) (+ x 1))
(define (doble x) (* x 2))
(define (h x y) (< x (* 2 y)))
(define (comparar x y) (< x (doble y)))
(define (cuad-azul x) (square x "solid" "blue"))


(cuad-azul (doble 10))
(and (h 2 3) (comparar 3 4))
(= (el-siguiente 1) (doble 1)) |#

; EJERCICIOS DE PRÁCTICA !!!!

 ;Defina una función que recibe dos números x e y, y devuelve la distancia al origen del punto (x,y).

(define (formulita x y) (sqrt (+ ( sqr x) ( sqr y))))
(define (dist-origin x y)  (formulita x y ))

(dist-origin 2 6) 
;-------------------------------------------------------------------------------

 ;Defina una función que recibe cuatro números x1, y1, x2 e y2, y devuelve la distancia entre los puntos (x1, y1) y (x2, y2).
(define (formula x1 y1 x2 y2) (sqrt (+ ( sqr ( - x2 x1) ) ( sqr ( - y2 y1)))))
(define (dist-puntoAyB x1 y1 x2 y2)  (formula x1 y1 x2 y2 ))

(dist-puntoAyB 2 6 7 8)

;-------------------------------------------------------------------------------

;Defina la función vol-cubo que recibe la longitud de la arista de un cubo y calcula su volumen.

(define (vol-cubo arista) ( * ( sqr arista) arista))

(vol-cubo 2)

;-------------------------------------------------------------------------------

; Defina la función area-cubo que recibe la longitud de la arista de un cubo y calcula su área.

(define (area-cubo arista) (* ( sqr arista) 6))

(area-cubo 2)

;-------------------------------------------------------------------------------
 ;Defina una función metro-pie, que dada una longitud expresada en metros, devuelva su equivalente en pies.

(define (metro-pie mts) ( * mts 3.281))

(metro-pie 1.60)

;-------------------------------------------------------------------------------
 ;Defina una función cel-far, que dada una temperatura expresada en Celsius, devuelve su equivalente en Fahrenheit.

(define (cel-far Celcius) ( + ( * Celcius 1.8 )32))

(cel-far 40)

;-------------------------------------------------------------------------------

 #|Defina una función posible? que, dados tres números positivos a, b, c,
devuelve #true si es posible construir un triángulo de lados a, b, c.
Caso contrario, devuelve #false.
Por ejemplo,(posible? 1 2 5) debe evaluar a #false, pues no es posible construir un triángulo de lados 1, 2 y 5.
|#
(define (triangulo a b c) ( and ( > (+ a b) c) ( > (+ a c) b) ( > (+ b c) a) ))

(define (posible? a b c) (triangulo a b c))

(posible? 6 2 5)

;-------------------------------------------------------------------------------


 #|¿No sabe lo que es una terna pitagórica? Wikipedia puede brindarle una ayuda.
Defina una función pitagórica? que dados tres números, devuelve #true si estos forman un terna pitagórica.
Caso contrario, devuelve #false.|#

( define ( terna num1 num2 num3)( = (+ (sqr num1) (sqr num2))(sqr num3)))

(define (pitagorica? num1 num2 num3)( terna num1 num2 num3))

(pitagorica? 2 4 7)

;-------------------------------------------------------------------------------


 ;Defina una función suma-long, que dados dos strings devuelve la suma de sus longitudes.

( define (suma-long str1 str2) (+ (string-length str1) (string-length str2)) )

(suma-long "V" "Reciproco")

;-------------------------------------------------------------------------------

; Defina una función comienzaA? que dado un string devuelve #true si el string comienza con "A".
(require racket/string)

(define (comienzaA? str) (string-prefix? str "A"))

(comienzaA? "Algo")
;-------------------------------------------------------------------------------

 ;Defina la función poner- que recibe un string y un número i e inserta "-" en la posición i-ésima del string.


;(define (poner- str x) (string-append ( string-append ( (substring str 0 x) "-" )) (substring str x (string-length str))))
(define (lado1 str x) ( string-append  (substring str 0 x) "-" ) )

(define (lado2 str x) ( string-append (substring str x (string-length str)) "" ) )

(define (poner- str x) ( string-append  (lado1 str x)  (lado2 str x) ) )


(poner- "hola" 2)
