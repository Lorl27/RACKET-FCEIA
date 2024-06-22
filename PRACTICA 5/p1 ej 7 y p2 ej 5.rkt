;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |p1 ej 7 y p2 ej 5|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;---------------------------------------------------------------------------------------------------
  ;ej 7 P5.1

(define (primerosPares lista)  (foldr cons (filter odd? lista) (filter even? lista) ) )
  
;luego de los pares filtrado, agrega los impares filtrados gracias a foldr
  ;ya que primero se hace filter even (cons cons cons) y luego de odd.

;-----------------------------------------

;P5.2 EJ 5

(define (mfAm l m) (local [(define (menorN y) (> m y))]
                     (filter menorN l)) )
  ;similar a labdma ;)

(define (eso lista) (filter (lambda(x) (> x lista)) lista) )