;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |PRACTICA 1 - PT2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;PRACTICa 1 - 1ra parte
; parte de banderas porque la otra esta en la pc UNU


#|
El juego FizzBuzz es un juego para ni ̃nos donde
sentados en una ronda van diciendo los n ́umeros del
1 a n de manera alternada. Cuando (alguien debe
decir un n ́umero que sea m ́ultiplo de 3, en su lugar
debe decir la palabra ”Fizz”, en el caso de que sea
m ́ultiplo de 5, debe decir ”Buzz” y debe decir
”FizzBuzz” si el n ́umero es m ́utiplo de ambos.
Definir una funci ́on que dado un n ́umero n devuelva
la palabra que deba decir el ni ̃no que est ́a en la
posici ́on n de la ronda.|#

;(define (fun number) (string-append "el numero es" (number->string number)))

(define (juego num) 
(cond [ ( = ( modulo num 3)  0 ) "Fizz"]
      [ ( = ( modulo num 5)  0 ) "Buzz"]
      [ ( = ( modulo num 15)  0 ) "FizzBuzz"]
      [else num]) )

(juego 3)

;--------------------------------------------------------------------------------------

(require 2htdp/image)
                   
(define (fig1 color) (rectangle 20 50 "solid" color))
(define (fig2 color1) (rectangle 40 50 "solid" color1))
(define (fig3 color2) (rectangle 60 50 "solid" color2))
 

(define (vertical color1 color2 color3) (overlay/align "left" "middle" (fig1 color1) (fig2 color2) (fig3 color3) ) )

(vertical "red" "white" "red")
(vertical "green" "white" "red")
(vertical "blue" "red" "pink")

;---------------------------------------------- 

(define (tamaño x y color)  (rectangle x y "solid" color))

(define (horizontal colo1 colo2 colo3) (overlay/align "center" "bottom" (tamaño 80 20 colo3) (tamaño 80 40 colo2) (tamaño 80 60 colo1) ) )


(horizontal "black" "red" "yellow")
(horizontal "red" "white" "blue")
(horizontal "blue" "white" "blue")

;----------------------------------------------

(define (verticall colo1 colo2 colo3) (overlay/align "left" "middle" (tamaño 20 50 colo3) (tamaño 40 50 colo2) (tamaño 60 50 colo1) ) )


(define (banderita sentido color1 color2 color3) (cond [ (string=? sentido "vertical") (verticall color3 color2 color1 ) ]
                                                      [ (string=? sentido "horizontal") (horizontal color1 color2 color3 ) ]
                                                      [else "error"] ) )
(banderita "vertical" "blue" "white" "red")
(banderita "horizontal" "red" "orange" "brown")

;----------------------------------------------

;( rotate 150 (triangle 10 "solid" "green"))

(overlay/align "left" "middle" ( rotate 150 (triangle 50 "solid" "green") ) (banderita "horizontal" "red" "white" "black" ) )

;----------------------------------------------

(overlay/align "center" "middle"  (circle 8 "solid" "yellow")  (banderita "horizontal" "Light Sky Blue" "white" "Light Sky Blue" ) )

;----------------------------------------------

(overlay/align "center" "middle"  (star 8 "solid" "yellow")  (banderita "vertical" "green" "red" "yellow" ) )

;---------------------------------------------

(overlay/align "center" "middle"  (circle 11 "solid" "blue") (rhombus 42 130 "solid" "yellow")  (banderita "horizontal" "green" "green" "green" ) )

;---------------------
(define (t x y color)  (rectangle x y "solid" color))

(define (h colo1 colo2 colo3 x ) (overlay/align "center" "bottom" (t x 20 colo3) (t x 40 colo2) (t x 60 colo1) ) )

(define (v colo1 colo2 colo3  y) (overlay/align "left" "middle" (t 20 y colo3) (t 40 y colo2) (t 60 y colo1) ) )


(define (b sentido color1 color2 color3 x y) (cond [ (string=? sentido "vertical") (v color3 color2 color1 y) ]
                                                      [ (string=? sentido "horizontal") (h color1 color2 color3 x ) ]
                                                      [else "error"] ) )

(b "vertical" "cyan" "purple" "magenta" 200 300)
(b "horizontal" "cyan" "purple" "magenta" 60 300)