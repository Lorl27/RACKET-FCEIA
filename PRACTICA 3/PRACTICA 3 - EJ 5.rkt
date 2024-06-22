;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 3 - EJ 5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;                                      ej 5
(define altura 400)
(define anchura 500)
(define COLOR-F "Dark Slate Gray")

(define (zero color) (place-image (circle 50 "solid" color) 250 200 (rectangle anchura altura "solid" COLOR-F) ) )


(define (cambio x) (cond [(string=? x "yellow") "red"]  
                         [(string=? x "red"  ) "green"]
                         [( string=? x "green")   "blue"]
                         [(string=? x "blue")  "yellow"]
                         [else x ])) 
                                  

(big-bang "yellow" [to-draw zero] [on-tick cambio 1])