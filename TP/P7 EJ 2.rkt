;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |P7 EJ 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

EJERCICIO 2: El método de Montecarlo
El Método de Montecarlo es un método numérico estadístico usado para aproximar expresiones
matemáticas. En general, este método se usa cuando obtener una solución exacta es imposible o muy
costoso computacionalmente

|#

;; Estas primera parte nos define una funciÃ3n que genera nÃomeros aleatorios
;; en un intervalo real determinado.
;; Recordemos que la funciÃ3n random nos devuelve un natural, pero para
;; nuestro problema necesitamos nÃomeros aleatorios fraccionarios.
(define RAND-MAX 4294967087) ; nÃomero mÃ¡ximo para valores aleatorios

; aleatorio : Number Number -> Number
; dados dos nÃomeros a y b, devuleve un nÃomero aleatorio en el intervalo [a,b].
(define (aleatorio a b) (+ a (* (- b a) (/  (+ 1 (random RAND-MAX)) (* 1.0 RAND-MAX)))))

;; fin de la parte para generar nÃomeros aleatorios. No es necesario entender cÃ3mo funciona,
;; sÃ3lo basta con entender el propÃ3sito de aleatorio (Â¿se acuerdan de la receta?)

;; Definimos algunas constantes:
(define RADIO 1.0) ; radio del cÃ­rculo
(define CENTRO (make-posn 0 0)) ; coordenadas del centro del cÃ­rculo

(define MAX 300000) ; cantidad de puntos a generar para nuestra estimaciÃ3n

; generar-puntos : Natural -> List(posn)
; Dado un natural n, devuelve una lista con n puntos aleatorios,
; con ambas componentes en el intervalo [-RADIO, RADIO]. Es decir, dentro del cuadrado.
(define (generar-puntos n)
  (cond [(zero? n) empty]
        [else (cons (make-posn (aleatorio (- RADIO) RADIO) (aleatorio (- RADIO) RADIO) )
                    (generar-puntos (- n 1)))]
        ))

; distancia : posn posn -> Number
; dados dos puntos, devuelve su distancia
(check-expect (distancia (make-posn 15 0) (make-posn 3 0)) 12)
(check-expect (distancia (make-posn 2 1) (make-posn 6 4)) 5)
(check-expect (distancia (make-posn 0 16) (make-posn 0 0)) 16)
(define (distancia p q)
  (sqrt (+ (sqr (abs (- (posn-x p) (posn-x q)))) (sqr (abs (- (posn-y p) (posn-y q)))))))

; adentro? : posn -> Boolean
; dada una posiciÃ3n, determina si cae en el cÃ­rculo centrado en CENTRO y cuyo radio es RADIO
(check-expect (adentro? (make-posn 0 0)) #t)
(check-expect (adentro? (make-posn RADIO (* 2 RADIO))) #f)
(check-expect (adentro? (make-posn RADIO RADIO)) #f)
(define (adentro? p) (<= (distancia p CENTRO) RADIO)) 


; generamos una lista con muchos puntos aleatorios
(define LISTA (generar-puntos MAX))

; nos quedamos con aquellos que estÃ¡n dentro del cÃ­rculo 
(define ADENTRO (filter adentro? LISTA))

; aproximamos el Ã¡rea del cÃ­rculo a partir de la proporciÃ3n de puntos que
; caen dentro del cÃ­rculo:
(define AREA (* 4.0 (/ (length ADENTRO) (length LISTA))))


;; Como en nuestro caso ya sabemos cuÃ¡l es el Ã¡rea de la superficie (pi),
;; podemos usar nuestro resultado como una estimaciÃ3n de este nÃomero.
;; Veamos quÃ© tan buena estimaciÃ3n es, calculando el error relativo porcentual
;; respecto del valor que nos proporciona racket:
; calculamos el porcentaje de error:
(define ERROR (* 100 (/ (abs (- pi AREA)) pi)))


(string-append "Nuesta aproximaciÃ3n de pi es: " (number->string  (exact->inexact AREA)))

(string-append "Con un porcentaje de error de: " (number->string ERROR) "% (si lo comparamos con el valor que nos proporciona DrRacket)")
