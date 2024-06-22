;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |img p6|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (graficar-circulos x m)
  (rotate m (circle x "outline" "red")))


(define fondo (empty-scene 400 400))


(define (circulo m)
  (cond[(zero? m)fondo]
       [(positive? m)(place-image (graficar-circulos (sqr m)3)200 200
                                  (circulo (sub1 m)))]))

;-------------------------------------------------
(define (graficar-cuadrado x m)
  (rotate m(square x "outline" "blue")))

(define (cuadrado m ang)
  (cond[(zero? m)fondo]
       [(positive? m)(place-image (graficar-cuadrado (sqr m)ang)200 200
                     (cuadrado (sub1 m)(+ ang 20)))]))




                 










