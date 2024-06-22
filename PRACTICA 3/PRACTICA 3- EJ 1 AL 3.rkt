;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 3- EJ 1 AL 3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;                                  práctica 3:

;                                     ej 1

(define (interpretar color) (place-image (circle 10 "solid" color) 100 100 (empty-scene 200 200)))

(define (manejarTeclado estado-c k ) (cond [(key=? k "b") "blue"]
                                      [(key=? k "r") "red"]
                                      [(key=? k "v") "green"]
                                      [(key=? k "n") "black"]
                                      [(key=? k " ") estado-c]
                                      [else estado-c]))

(big-bang "blue" [to-draw interpretar] [on-key manejarTeclado])

; teoria->>(big-bang <estado inicial> ;;
; [to-draw : pantalla]
;[ on-key: teclado]
; [on-mouse: mouse ]
;[ on-tick:reloj]
;[ stop-wehn: ... ])

;---------------------------------------------------------

;                                  ej 2/3

(define (interpretar2 n) (place-image (cond [(and(< n  50)(> n 0)) (circle n "solid" "yellow")]
                                            [(and(< n  100)(> n 51)) (circle n "solid" "red")]
                                            [(> n 100) (circle n "solid" "green")]
                                            [else (circle n "solid" "aqua marine")]
                                            ) 150 150 (empty-scene 300 300)))

(define (decrementar n) (if (= n 0) 100 (- n 1)))

(define (incrementar n) (if ( > n 200) 0 ( + n 1)))

(define (teclas k d) (if (= (string-length d) 2)  (* 10 d) k))

(define (algo n) (or (< n 10)(> n 110)))
;(big-bang 100 [to-draw interpretar2] [on-tick decrementar] )


(big-bang 50 [to-draw interpretar2] [on-tick incrementar] [on-key teclas]  [stop-when algo])