;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cesar-subir) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

; representaremos alfabetos como Strings.
(define ALFABETO "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ 0123456789")

; Por ejemplo, si nuestros sÃ­mbolos son las cinco primeras letras, los dÃ­gitos y
; el espacio,
; lo representaremos como "ABCDE0123456789 "

; representaremos sÃ­mbolos como strings de longitud 1. En el alfabeto anterior,
; el sÃ­mbolo E lo representamos con el string "E"

; el cÃ3digo del cÃ©sar lo representaremos mediante parejas de sÃ­mbolos.
; Por ejemplo, si queremos decir que el sÃ­mbolo "A" se codifica con el
; sÃ­mbolo "C", tendremos (make-Tupla "A" "C") para representar esta situaciÃ3n.

;;;;;;;; Primero comenzamos definiendo algunas funciones
; sobre strings y listas que nos son de utilidad.

; partir : String -> List(String)

(define (partir x)
  (cond[(string=? x "")'()]
       [else  (cons (substring x 0 1)(partir (substring x 1)))]))
(check-expect (partir "ABC") (list "A" "B" "C"))
(check-expect (partir "12345") (list "1" "2" "3" "4" "5"))
(check-expect (partir "") empty)

; tomar : List(Natural) Natural -> List (Natural)
; dada una lista y un nÃomero natural n, devuelve una lista
; con los primeros n elementos de l. Si l no tiene tantos elementos,
; devuelve l.
(check-expect (tomar (list 1 2 3 4 5) 4) (list 1 2 3 4))
(check-expect (tomar (list 1 2 3 4 5) 10) (list 1 2 3 4 5))
(check-expect (tomar (list 1 2 3 4 5) 0) empty)
(check-expect (tomar empty 5) empty)



(define (tomar x y)
  (cond[(or (zero? y)(empty? x)) empty]
       [else (cons (first x)
                   (tomar (rest x) (sub1 y)))]))


; tirar : List (Natural) Natural -> List (Natural)
; dada una lista y un nÃomero natural n, devuelve una lista
;  sin los primeros n elementos de l. Si l no tiene tantos elementos,
; devuelve empty.
(check-expect (tirar (list 1 2 3 4 5) 2) (list 3 4 5))
(check-expect (tirar (list 1 2 3 4 5) 10) empty)
(check-expect (tirar (list 1 2 3 4 5) 0) (list 1 2 3 4 5))
(check-expect (tirar empty 3) empty)


(define (tirar x y)
  (cond[(empty? x) empty]
       [(zero? y) x]
       [(> (length x) y) (remove y (tirar (rest x) (sub1 y)))]
       [else  empty]))




  
  