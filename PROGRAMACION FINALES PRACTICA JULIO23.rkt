;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |PROGRAMACION FINALES PRACTICA JULIO23|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; PRÁCTICA DE LOS EXAMENES FINALES - PROGRAMACIÓN

#|
Ejercicio 22. Considere la funcion f definida como sigue:

    f(0) = 0

    f(1) = 2

    f(2) = 5

    f(n) = f(n-1) + 2 * f(n-3), para todo n mayor o igual a 3.

Diseñe una función Fs que dado un número n devuelve la lista con todos los valores entre (f 0) y (f n).

Presente al menos dos ejemplos más en su diseño.
|#

(define (f num) (cond[(zero? num) 0]
                     [(= 1 num ) 2]
                     [(= 2 num ) 5]
                     [else ( + (f (sub1 num)) (* 2 (f (sub1(sub1(sub1 num)))))) ]
                     )
  )

(f 3)

;Fa: Natural -> List(Natural)
(define (Fa nat) (cond[(zero? nat) (list nat)]
                      [else (cons (f nat) (Fa (sub1 nat))) ]
                      )
  )

(define (Fs nat) (reverse (Fa nat)))

(check-expect (Fs 5) (list 0 2 5 5 9 19))
(check-expect (Fs 0) (list 0))
(check-expect (Fs 1) (list 0 2 ))
(check-expect (Fs 2) (list 0 2 5) )

;-------------------------------------------------------------------

#|
Ejercicio 23. Diseñe una función dibujar-elipses que tome un número natural n y,
 devuelva una imagen de 300 x 300 con n elipses de contorno azul centradas en el centro de la imagen.
 Los tamaños de las elipses serán: (10*n) * (5*n) , (10*(n-1)) * (5*(n-1)), ..., 20 * 10, 10 * 5.
El ángulo de rotación de cada elipse coincide con el valor del doble de n.

|#

(define (hacer-ellipse ancho alto) (ellipse (* 10 ancho) (* 5 alto) "outline" "red"))
(define (ROT-ELLIP ang ancho alto) (rotate ang (hacer-ellipse ancho alto)) )

(define FONDO (empty-scene 300 300))

(define CENTRO (/ 300 2))

(define (dibujar-ellipses nat) (cond[(zero? nat) FONDO]
                                    [else (place-image (ROT-ELLIP (* 2 nat) nat nat) CENTRO CENTRO (dibujar-ellipses (sub1 nat)) ) ]
                                    )
  )

;(dibujar-ellipses 30)

;-------------------------------------------------------------------

#|
Ejercicio 24. La conjetura de Goldbach postula que todo entero par, positivo y mayor a 2
puede expresarse como la suma de dos números primos.
 Por ejemplo: 22 = 3 + 19.
 Esta conjetura es uno de los más famosos resultados de teoría de números que
no ha podido ser demostrado correcto.
 
 Sin embargo, ha sido confirmado empíricamente hasta números muy grandes.
 
 Escriba una función que dado un número par n >= 4 encuentre dos números primos cuya suma resulte n.
 Por ejemplo:

    (goldbach 22) = (make-posn 3 19)

Ayuda: Puede utilizar la función prime? definida a continuación que decide si un número dado es primo.

|#

    (define (fun n c)
      (cond [(< n (sqr c)) #t]
            [(zero? (modulo n c)) #f]
            [else (fun n (add1 c))]))
     
    (define (primo? n) (fun n 2))

(define (intervalo n)
  (local [ (define (intervalo-aux x) (cond[(> x n) '() ]
                                          [else (cons x (intervalo-aux (add1 x))) ]))] (intervalo-aux 1)))

(define (lista-primos n) (filter primo? (rest(rest (intervalo n))))) 

(define (resta l n) (cond[(empty? l) empty]
                        [else (cons ( - n (first l)) (resta (rest l) n))]))

(define (goldbach n) (cond
              [(and (even? n) (>= n 4)) (local [ (define B (first (filter primo? (resta (lista-primos n) n))))] (make-posn (- n B) B))]
              [(not (>= n 4)) "Menor a 4"]
              [else (string-append "No se encontraron dos primos cuya suma sea " (number->string n))]))
    


(goldbach 22)


;---------------------------------------------------


(define (sumar-sumandos n)
  (if (= n 0)
      0
      (+ (/ 1.0 (* n n)) (sumar-sumandos (- n 1)))))

(define (aprox-pi n)
  (sqrt (+ 6 (sumar-sumandos n))))

(aprox-pi 6)
