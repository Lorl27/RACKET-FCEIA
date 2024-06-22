;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |PRÁCTICA 1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; PRÁCTICA 1: TIPOS BÁSICOS
#|
;INDICAR SI V O F:
-2
;- 2
;(-2)
 (- 2)
 1/3
 ;1 / 3
 ;(1 / 3)
 (/ 1 3)
 -1/3
 ;-(/ 1 3)
 (-(/ 1 3))

;                  POWERPOINT DE INICIO, EJ PRÁCTICO DE IF:

( if 〈condicion〉 ; es una proposicion
〈exp1 〉 ; si la condicion es V
〈exp2 〉) ; si la condicion es F


 Definir una funci ́on que reciba la medida de dos angulos y en el caso en que sean complementarios devuelva el
mensaje ”Complementarios”;
 en caso contrario, devuelva ”No complementarios”.|#

(require racket/string)

(define angulo1 50)
(define angulo2 60)

(if  (= angulo1 angulo2)    "Son complementarios"  "No complementarios")




;(place-image (circle 10 "solid" "blue") 24 24 (rectangle 48 48 "solid" "grey"))