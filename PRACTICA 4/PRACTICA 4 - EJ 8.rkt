;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4 - EJ 8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 8. Considere la siguiente definición de una estructura para nuestro
editor de texto:

(define-struct Texto [s color tam])
; Texto es (String, Color, Number)
; Intepretación: El primer elemento es la cadena a mostarse, mientras que el segundo y
el tercero determinan el color y tamaño de la fuente en píxeles respectivamente.

Modifique el ejercicio del editor de textos de acuerdo a lo siguiente:

• La cadena se muestra con el color y tamaño que determina el estado actual. Decida usted qué color y tamaño serán el inicial.

• Como en la práctica anterior, cada vez que se presiona una letra o número este debe agregarse a la cadena que se está mostrando.
 El comportamiento para la tecla "backspace" debe mantenerse.

• Elija 6 colores de su preferencia, y modifique su programa para que
al presionar las teclas "f1", "f2", ..., "f6", el color del texto cambie a estos colores.

• Al presionar la tecla "up" , la fuente debe incrementarse, mientras que
al presionar la tecla "down", la fuente debe achicarse (siempre que sea posible).

|#

(define-struct Texto [str tam color])
; Texto es (String, Color, Number)
; Intepretación: El primer elemento es la cadena a mostarse, mientras que
;el segundo y el tercero determinan el tamaño y el color de la fuente en píxeles respectivamente.

; ---- CONSTANTES DEL PROGRAMA:
(define ALTO 60)
(define ANCHO 800)
(define ESCENA-V (empty-scene ANCHO ALTO) )

(define TAMAN 20) ;Tamaño inical
(define COLOUR "blue") ;color inicial
;--------------------------------------------

(define INICIAL (make-Texto "" TAMAN COLOUR ) )

;pantalla: Estado->Image
(define(pantalla txt) (place-image/align (text (Texto-str txt) (Texto-tam txt) (Texto-color txt) )
                                          0 0 "left" "top"        ;sitúa el txt en la pantalla (esq. sup. izq.)
                                          ESCENA-V
                        )
  )

;condi: Estado String-> Estado 
(define (condi estado tecla)  (if ( or
                                   ( = (string-length tecla) 1)
                                   (key=? tecla "\b")
                                   (key=? tecla "f1")
                                   (key=? tecla "f2")
                                   (key=? tecla "f3")
                                   (key=? tecla "f4")
                                   (key=? tecla "f4")
                                   (key=? tecla "f5")
                                   (key=? tecla "f6")
                                   (key=? tecla "up")
                                   (key=? tecla "down")  ;expeciones de las letras prohibidas...
                                  )

                                  (cond [(key=? tecla "\b") (if (= (string-length (Texto-str estado) ) 0)
                                                                
                                                              estado ;para que no se borre más del límite
                                                              
                                                             (make-Texto (string-append(substring (Texto-str estado) 0 ( - (string-length (Texto-str estado) ) 1) )"")
                                                                         (Texto-tam estado) (Texto-color estado) ) ;borra
                                                              )  ]
                                    [(key=? tecla "f1") (make-Texto (Texto-str estado) (Texto-tam estado)  "deep pink" ) ]
                                    [(key=? tecla "f2") (make-Texto (Texto-str estado) (Texto-tam estado)  "Peru" ) ]
                                    [(key=? tecla "f3") (make-Texto (Texto-str estado) (Texto-tam estado)  "Chartreuse" ) ]
                                    [(key=? tecla "f4") (make-Texto (Texto-str estado) (Texto-tam estado)  "Royal Blue" ) ]
                                    [(key=? tecla "f5") (make-Texto (Texto-str estado) (Texto-tam estado)  "Dark Magenta" ) ]
                                    [(key=? tecla "f6") (make-Texto (Texto-str estado) (Texto-tam estado)  "black" ) ]
                                    [(key=? tecla "up") (make-Texto (Texto-str estado) (+ (Texto-tam estado) TAMAN) (Texto-color estado) )]
                                    [(key=? tecla "down") (make-Texto (Texto-str estado) ( if (= (- (Texto-tam estado) TAMAN) 0) ;no  dismunuye si es <0, asi no da error.
                                                                                              (Texto-tam estado)
                                                                                              (- (Texto-tam estado) TAMAN)) (Texto-color estado) )]
                                    [(Texto? estado) (make-Texto (string-append (Texto-str estado) tecla) (Texto-tam estado) (Texto-color estado) ) ] ;une tecla1 con tecla2, teclaN etc
                                    [else estado]
                                   )
                                  
                                  estado) ;si NO es un valor aceptado , ej:left. retorna el mismo estado(no toma left)

 )            
  
                          

(big-bang INICIAL [to-draw pantalla] [on-key condi] )