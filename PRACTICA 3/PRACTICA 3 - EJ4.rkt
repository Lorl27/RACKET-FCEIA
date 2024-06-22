;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 3 - EJ4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;                                      ej 4

(define ALTO 500)
(define ANCHO 200)
(define DELTA 8)

(define (draw-circle x) (place-image (circle 10 "solid" "gray") 100 x (empty-scene ANCHO ALTO)))

(define (tiruti estado-c teklita) (if (or (> estado-c 500)
                                          (< estado-c 0))

                                      (/ ALTO 2)
                                      (cond [(key=? teklita "up") ( - estado-c DELTA )]
                                            [(key=? teklita "down") (+ estado-c DELTA)]
                                            [(key=? teklita " ") (/ ALTO 2)]
                               )))

(define (mouse-handler n x y event) (cond [(string=? event "button-down") y]
                                          [else n]))

(big-bang ( / ALTO 2) [to-draw draw-circle] [on-key tiruti] [on-mouse mouse-handler])