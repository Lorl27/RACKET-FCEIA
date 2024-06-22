;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 3 - EJ 8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;                                      ej 8:

;constantes del fondo
(define COLOR "Light Purple")
(define ANCHO 700)
(define ALTO 300)
;constantes de la estrella
(define COLOR-S "Cyan")
(define TAMAÑO-S 10)

(define ESTRELLA (star TAMAÑO-S "solid" COLOR-S))

(define FONDO (empty-scene ANCHO ALTO COLOR))

; estado->image
; devuelve el mismo estado para que se dibuje en la pantalla 
(define (pantalla x) x)

;estado number number string-> estado
(define (dibujar-estrellita estado x y event)  (cond[ (and (dibujar-star? x y) (string=? event "button-down")) (place-image ESTRELLA x y estado)]
                                                   [else estado]) )

(define (dibujar-star? x y) (and (>= (- x (image-width ESTRELLA))0) (<= (+ x (image-width ESTRELLA)) ANCHO) (>= (- y (image-height ESTRELLA))0) (<= (+ y (image-height ESTRELLA))ALTO) ))

(define (teclado estado tecla) (if (key=? tecla "\b") FONDO estado))

(big-bang FONDO
          [to-draw pantalla]
          [on-mouse dibujar-estrellita]
          [on-key teclado])

;PARA PEKI X SI LO VE -> M FALTO LO Q SE AGRANDE LA ESTRELLA GAY y eso lmaoooo