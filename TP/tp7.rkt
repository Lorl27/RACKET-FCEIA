;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tp7) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; PRÁCTICA 7

#|

Ejercicio 1. Según la profesora de probabilidad y estadística, al tirar muchas veces un dado -si este no
está cargado- la frecuencia con la que ocurre cada número tiende a ser la misma. Es decir, si tiramos un
dado de seis caras doce millones de veces y anotamos los resultados, es de esperar que cada cara del
dado aparezca aproximadamente dos millones de veces.

|#

(define CARAS 6) ; Como no todos los dados tienen 6 caras, comenzamos definiendo una constante para este valor

#|
Diseñe una función simular-dado, que reciba un número natural n y devuelva una lista con n
números aleatorios entre 1 y CARAS.
|#

;simular-dado Natural -> List(Natural)
; dado un número natural n, devuelve una lista con 
; n números aleatorios entre 1 y CARAS.

(define (simular-dado n) (cond [(zero? n) '()]
                               [else (cons (+ 1 (random CARAS)) (simular-dado (sub1 n))) ]
                               ) ;el +1 es necesario porque random incluye el 0. queremos entre 1 y CARAS(6)
  )

(simular-dado 1) (simular-dado 2)
;-----------------------------------------------------------------
(define (inv num) (cond[(zero? num)'()] 
                       [(positive? num) (cons num (inv (sub1 num) )) ]
                             )
  )


(define (intervalo num) (reverse (inv num)) )
;-------------------------------------------------------------
;Considere ahora las siguientes definiciones:
(define MAX 60000)
(define EXPERIMENTO (simular-dado MAX))
(define VALORES (intervalo CARAS))
;La lista EXPERIMENTO tendrá MAX números aleatorios entre 1 y CARAS.
;La lista VALORES usa un ejercicio previo, y es simplemente (list 1 2 ... CARAS).

#|
Diseñe la función frecuencia, que dado un número natural n y una lista de naturales, devuelve la
cantidad de veces que n aparece en la lista. Es decir, su frecuencia absoluta
|#

(define (frecuencia nat listn) (cond[(empty? listn) 0]
                                    [(zero? nat) 0]
                                    [(= nat (first listn)) (+ 1 (frecuencia nat (rest listn)))]
                                    [else ( frecuencia nat (rest listn) ) ]
                                    )
  )

(frecuencia 1 (list 1 1 2 3))
(frecuencia 2 (list 1 3 4 2 46 2 57 2))

;-------------------------
#|
Utilizando esta función, podemos calcular la frecuencia relativa de un valor en una lista. Esto es, la
proporción de veces que aparece:
|#
(define (frecuencia-relativa n l) (/ (frecuencia n l) (length l)))

(frecuencia-relativa 1 (list 1 1 2 3))

;La función frec-rel-exp nos devuelve la frecuencia relativa de un valor en nuestro EXPERIMENTO:
(define (frec-rel-exp n) (frecuencia-relativa n EXPERIMENTO))

(frec-rel-exp 1)

;Usando map, podemos calcular las frecuencias relativas de cada valor en el experimento:
(define FRECUENCIAS-RELATIVAS (map frec-rel-exp VALORES))

;¿Qué valor de frecuencia aproximado debería repetirse en la lista FRECUENCIAS-RELATIVAS?
; Antes de proceder a evaluar en DrRacket, piense. Luego, evalúe.

;1/6 = 0.1666...
FRECUENCIAS-RELATIVAS

;______________________________________________________________________________

; EJERCICIO 3 - NÚMEROS PRIMOS

#|

En el siglo III antes de Cristo, el filósofo Eratóstenes describió un método para encontrar todos los
números primos hasta un número predeterminado n.

La idea es la siguiente: se parte de una lista donde se encuentran todos los naturales entre 2 y n. Se
repite el siguiente procedimiento con los elementos de la lista:
1. Se marca el primer elemento -llamémosle k- como número primo.
2. Se borran todos los múltiplos de k de la lista (salvo k).

Una vez finalizado este procedimiento, tenemos la garantía que los números que quedaron marcados
son todos los números primos entre 2 y n.
|#

; intervalo : N N -> List(N)
; dados dos números naturales, devuelve una lista con todos los números entre a y b

(check-expect (intervalos 2 4) (list 2 3 4))
(check-expect (intervalos 4 4) (list 4))
(check-expect (intervalos 5 4) empty)

(define (intervalos n1 n2) (cond[ (or (zero? n1) (zero? n2) ) '() ]
                                [(= n1 n2) (list n1)]
                                [(> n2 n1) (cons n1 (intervalos (add1 n1) n2)) ]
                                [else empty]
                               )
  ) ;como solo se hara si n2 > n1 , en el momento que n2 < n1, devolverá empty ergo la lista q quiero.

;-------------------------------------------------------

; elimimar-multiplos: N List(N) -> List(N)
; dados un natural positivo n y una lista l, elimina todos los múltiplos de n en l.
(check-expect (eliminar-multiplos 3 (list 4 6 11 15)) (list 4 11))
(check-expect (eliminar-multiplos 1 (intervalos 1 1000)) empty)

(define (eliminar-multiplos num listn) (cond[(empty? listn) '()]
                                            [(not(= (modulo (first listn) num)0)) (cons (first listn) (eliminar-multiplos num (rest listn)))]
                                            [else (eliminar-multiplos num (rest listn))]))


; eratostenes : List(N) -> List(N)
; dada una lista que inicialmente es de la forma [2, .., n] para algún n, realiza el procedimiento de eratostenes
(check-expect (eratostenes (intervalos 2 10)) (list 2 3 5 7))
(check-expect (eratostenes empty) empty)
(check-expect (eratostenes (intervalos 2 17)) (list 2 3 5 7 11 13 17))

(define (eratostenes listn) (cond[(empty? listn) empty]
                                  [else (cons (first listn) (eratostenes (eliminar-multiplos (first listn) (rest listn))) ) ]
                                  )
  )

; criba-eratostenes : N -> List(N)
; dado un natural n>=2, devuelve la lista de todos los nUmeros primos hasta n
(check-expect (criba-eratostenes 10) (list 2 3 5 7))
(check-expect (criba-eratostenes 2) (list 2))
(check-expect (criba-eratostenes 20) (list 2 3 5 7 11 13 17 19))

(define (criba-eratostenes nat) (eratostenes(intervalos 2 nat)))

#|
Para terminar este ejercicio, se pide crear un archivo "primos.txt" que guarde la lista de primos hasta
10000. En el archivo, los valores deben aparecer uno por línea. Recuerde que el contenido de un
archivo es siempre un string, y que las funciones write-file y number->string pueden ser de utilidad,
entre otras.
|#

(define num (map number->string (criba-eratostenes 10000) ) )

(define (lista-num l) (cond[(empty? l) ""]
                       [else (string-append (first l) "\n" (lista-num (rest l)) )]))


(write-file "primos.txt"  (lista-num num) ) ;el txt ya debe estar creado con anterioridad. d´accord?

;--------------------------------------------------------------------------

;EJERCICIO 3.1:

(define MAXIM 10000)

;un número (<= m (sqr MAXIM)) será PRIMO solo si : su divisor NO pertecene a LISTA-DE-PRIMOS

(define lista-str   (read-csv-file "primos.txt") )

(define (lista-numeros l) (cond[(empty? l) empty]
                               [else(append (map string->number (first l)) (lista-numeros (rest l)))]
                               )
  )

(define LISTA-DE-PRIMOS (lista-numeros lista-str) )

;Diseñe un predicado es-primo? que dado un nátural m menor o igual que (sqr MAXIM) determine si este
;es primo o compuesto. [compuesto== no primo)

(define (es-primo? num) (cond[(or (zero? num) (= num 1) ) #f] ;ya que 0 y 1 JAMÁS serán PRIMOS
                             [ (member num LISTA-DE-PRIMOS) #t ]
                             [else #f])
  )

(check-expect (es-primo? 1) #f)   (check-expect (es-primo? 2) #t)
(check-expect (es-primo? 397) #t) (check-expect (es-primo? 400) #f)

;abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzzrujjhigydtjiuttetrukouftenshfgcgukg

#|
Generalice el diseño anterior mediante una función todos-primos?, que dada una lista de números
menores o iguales a (sqr MAXIM) , determine si todos ellos son primos.
 Para los casos de test, tal vez le
interese saber que 86077909, 96928157, 49987169 y 54257153 son primos (debería ser más
sencillo encontrar ejemplos de números que no lo son
|#

(define (todos-primos? lista) (cond[(empty? lista)#t]
                                   [(es-primo? (first lista)) (todos-primos? (rest lista)) ]
                                   [else #f]
                                   )
  )

(check-expect (todos-primos? (list 397 400)) #f)
(check-expect (todos-primos? (list 2 397 )) #t)
;(check-expect (todos-primos? (list 86077909 96928157 49987169 54257153 )) #t)
; da f pq la  lista tiene coo lim a 10mil y no (sqr 10000)
; se soluciona con hacer sqr en cebes arriba y hacer otra lista-primos ;)

;----------------------------------------------------------------------------
;EJ 4: CIFRADO CÉSAR -- YA HECHO
;EJ 5: JUGANDO AL SOLITARIO -- TEMA 2 (PEDIR)
;EJ 6: PROPOSIONALES -- YA PEDIDO , TEMA 1.