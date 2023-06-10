;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4-EJ5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 5. Usando una estructura posn como estado, modifique el ejercicio
4 de la Práctica 3 para que el objeto pueda moverse tanto vertical como
horizontalmente, respondiendo a las teclas de dirección habituales. El
comportamiento respecto al mouse debe ser el siguiente: Cuando se presiona
un botón, el objeto debe aparecer en la coordenada donde se produjo el
evento.


|#

;constantes------
(define ALTO 500)
(define ANCHO 200)
(define DELTA 8) ;variable sobre la cual el OBJ se va a mover
(define INICIAL (make-posn (/ ANCHO 2) (/ ALTO 2))) ; El Estado es una estructura posn, donde se ubicará el OBJ

;draw-circle: Estado -> Image
(define (draw-circle pos) (place-image (circle 10 "solid" "purple") (posn-x pos) (posn-y pos)
                                     (empty-scene ANCHO ALTO)))

;tiruti: Estado String -> Estado
(define (tiruti estado-c teklita) (if (or (> (posn-y estado-c) 500)
                                          (< (posn-y estado-c) 0)
                                          (< (posn-x estado-c ) 0)
                                          (> (posn-x estado-c) 200)   ;LIMITES DEL OBJ
                                        )

                                          INICIAL ;si es true, (se sobrepasa el limite), se ubica en el el medio de la escena.
                                                                     
                                      (cond [(key=? teklita "up")    (make-posn (posn-x estado-c) (- (posn-y estado-c) DELTA))] 
                                            [(key=? teklita "down")  (make-posn (posn-x estado-c) (+ (posn-y estado-c) DELTA) )]
                                            [(key=? teklita "left")  (make-posn ( -  (posn-x estado-c) DELTA ) (posn-y estado-c) )]
                                            [(key=? teklita "right") (make-posn ( + (posn-x estado-c) DELTA ) (posn-y estado-c) )]
                                            [(key=? teklita " ")     INICIAL]
                                            )  ;si es falso, dependiendo de que tecla apriete hace x cosa
                               )
  )

;mouse-handler: Estado Number Number String-> Estado
(define (mouse-handler n x y event)
            (cond [(string=? event "button-down") (make-posn x y) ]
                  [else n] )
  )

(big-bang INICIAL
  [to-draw draw-circle]
  [on-key tiruti]
  [on-mouse mouse-handler])
