;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pablopalbo) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define ancho 400)
(define alto 600)
(define escena (empty-scene alto ancho))

(define (circulo x)
  (place-image (star (+ 10 (/ x delta)) "solid" (cambio x)) x 200 escena))

(define delta 20)

(define inicial 1)

(define (movimiento x)
  (cond [(< x 580) (+ x 3)]
        [(> x 580) (- x 3)]
        [else x]))

(define (cambio x)
  (cond [(>= delta x) (+ delta x)]
        [(>= (* delta 2) x) (+ delta x delta)]
        [(>= (* delta 4) x) (+ delta x delta)]
        [(>= (* delta 6) x) (+ delta x delta)]
        [(>= (* delta 8) x) (+ delta x delta)]
        [(>= (* delta 10) x) (+ delta x delta)]
        [(>= (* delta 12) x) (+ delta x delta)]
        [(>= (* delta 14) x) (+ delta x delta)]
        [(>= (* delta 2) x) "blue"]
        [else x]))

(define (actualizar x)
  (circulo (movimiento x)))

(big-bang inicial
  [to-draw circulo]
  [on-tick actualizar 2])

