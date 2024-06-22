;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eltp) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
#|               TRABAJO PRÁCTICO REALIZADO POR: PABLO PODESTA Y ANTONELLA GRASSI.
               --------------------------------------------------------------------       
                          PROGRAMACIÓN TEMA 3 - EL CÓDIGO DEL CÉSAR:

El Cifrado del César es un mecanismo de criptografía simétrica muy simple,
 en la cual el emisor y el receptor utilizan una clave previamente conocida para
 codificar y decodificar mensajes.

Una letra en el texto original es reemplazada por otra letra
 que se encuentra un número fijo de posiciones más adelante en el alfabeto

|#

;__________________________________________________________

;-Representaremos alfabetos como Strings.
; Por ejemplo, si nuestros símbolos son las cinco primeras letras, los dí­gitos y
; el espacio,
; lo representaremos como "ABCDE0123456789 "

;-El código del césar lo representaremos mediante parejas de símbolos.
; Por ejemplo, si queremos decir que el símbolo "A" se codifica con el
; símbolo "C", tendremos (make-Tupla "A" "C") para representar esta situación.

;-Representaremos símbolos como strings de longitud 1.
; En el alfabeto anterior,
; el sí­mbolo E lo representamos con el string "E"

;-----------------------------------------------------------------
;;;;;;;; Primero comenzamos definiendo algunas funciones
;;;;;;;; sobre strings y listas que nos serán de utilidad.
;-----------------------------------------------------------------

; partir : String -> List(String)
; Dado un string, devuele una lista de strings con cada sÍmbolo(letra) separado

(check-expect (partir "ABC") (list "A" "B" "C"))
(check-expect (partir "12345") (list "1" "2" "3" "4" "5"))
(check-expect (partir "") empty)
(check-expect (partir "DFA") (list "D" "F" "A"))
(check-expect (partir "DEF") (list "D" "E" "F"))

(define (partir str) (cond[(string=? str "")'()] ;primero se pregunta si el string es vacío, si lo es devuelve '()
                          [else  (cons (substring str 0 1) (partir (substring str 1)) ) ]
       );y si no es vacio, crea una lista con el primer cáracter del string y luego,
  ) ; se realiza una recursión con los demás caracteres. Logrando que cuando llegue al caso base
; [ '()  ] devuelva el resultado esperado , es decir, una lista con cada letra separada.

;-----------------------------------------------------------------

; tomar : List(Natural) Natural -> List (Natural)
; dada una lista y un número natural n, devuelve una lista
; con los primeros n elementos de l.
;-Si l no tiene tantos elementos, devuelve l.

(check-expect (tomar (list 1 2 3 4 5) 4) (list 1 2 3 4))
(check-expect (tomar (list 1 2 3 4 5) 10) (list 1 2 3 4 5))
(check-expect (tomar (list 1 2 3 4 5) 0) empty)
(check-expect (tomar empty 5) empty)
(check-expect (tomar (list 6 7 8 9 10) 21) (list 6 7 8 9 10))

(define (tomar l n) (cond[ (or (zero? n)(empty? l) ) empty]
                         ; primero pregunta si es un número natural vacio o una lista vacia
                         ; si alguna de las 2 cosas se cumple, devuelve empty, es decir, '().                              
                         [else (cons (first l) (tomar (rest l) (sub1 n))) ]
       ); luego: si no es una lista o un número vacio, crea una lista de numeros hasta el  
  )     ; número n que se quiera quitar. (Agarra el primer número de la lista y,
      ; realiza una recursividad con los números restantes de la lista y el anterior número n,
    ; Para así, obtener el resultado deseado ya que en un punto llegará al caso base (empty) y nos
; devolverá la lista completa hasta el número n que queríamos eliminar de la misma)

;-----------------------------------------------------------------

; tirar : List (Natural) Natural -> List (Natural)
; dada una lista y un número natural n, devuelve una lista
;  sin los primeros n elementos de l.
;--Si l no tiene tantos elementos,  devuelve empty.

(check-expect (tirar (list 1 2 3 4 5) 2) (list 3 4 5))
(check-expect (tirar (list 1 2 3 4 5) 10) empty)
(check-expect (tirar (list 1 2 3 4 5) 0) (list 1 2 3 4 5))
(check-expect (tirar empty 3) empty)
(check-expect (tirar (list 10 21 30 90) 2) (list 30 90))
(check-expect (tirar (list 40 90 100 70) 0) (list 40 90 100 70))
(check-expect (tirar (list 40 90 100 70) 3) (list 70))



(define (tirar l n)
  (cond[(empty? l) empty] ;primero busca si la lista es vacía, si lo es devuelve empty [ '() ]
       [(zero? n) l] ;y si es un número vacío, devuelve la lista completa
       [(> (length l) n) (remove n (tirar (rest l) (sub1 n)))] ; si la longitud de la lista es mayor
       ; a la del número dado, entonces remueve/saca todos los elementos de la lista que tengan a ese número
       ;(Lo que hace es utilizar la función REMOVE de Racket con el número n dado y, luego se hace una recursión
       ;con el resto de la lista y cada vez se va restando el número dado(sub1 n) hasta llegar al caso base
       ;donde devolverá la lista completa a la que se quería llegar).
       [else empty])); si no cumple con ninguna de estas 3 condiciones entonces, devolverá empty 

;-----------------------------------------------------------------

; OBSERVACION: para cualquier n <= length l, (append (tomar n l) (tirar n l)) = l
;Probamos que, efectivamente, se cumple:
(check-expect (append (tomar (list 1 2 3 4 5) 3) (tirar (list 1 2 3 4 5) 3)) (list 1 2 3 4 5) )

;-----------------------------------------------------------------

(define-struct Tupla [f s])
; -- Tupla es [Any Any]
; -- Representa un par de elementos de cualquier tipo.
; f : cualquier elemento, de cualquier tipo
; s : cualquier elemento, de cualquier tipo

; emparejar : List (X) List(Y) -> List Tuplas
; dadas dos listas [a0,..., an] y [b0, ...., bn] de la misma longitud, devuelve una lista
; de tuplas con parejas tomadas de ambas listas: [(make-posn a0 b0), ...., (make-posn an bn)]

(check-expect (emparejar (list "a" 2) (list "b" 4)) (list (make-Tupla "a" "b") (make-Tupla 2 4)))
(check-expect (emparejar (list "h" "l") (list "o" "a")) (list (make-Tupla "h" "o") (make-Tupla "l" "a")))
(check-expect (emparejar (list "t" "1") (list "g" "f")) (list (make-Tupla "t" "g") (make-Tupla "1" "f")))
(check-expect (emparejar (list "d" "7") (list "p" "4")) (list (make-Tupla "d" "p") (make-Tupla "7" "4")))
(check-expect (emparejar (list "" "") (list "" ""))(list(make-Tupla "" "") (make-Tupla "" "")))

(define (emparejar x y) (cond[(empty? x) empty] ;primero busca si la lista es vacia, si lo es devuelve empty
                             [else (cons (make-Tupla (first x) (first y)) (emparejar (rest x) (rest y))) ]
                             );si no es vacia entonces crea una lista de tipo Tupla que lo que hace
  ); es emparejar el primero de la lista con el primero de la otra lista y el segundo de la lista con el segundo de
;la otra lista (a través de realizar una lista de una Tupla con los primeros elementos de cada lista y,
; la recursión sobre los elementos restantes de ambas listas)

;-----------------------------------------------------------------
;;;;;;;;;;;;; Ahora comienzan las funciones específicas para el método del César
;-----------------------------------------------------------------

;tuplaToPosn: Tupla -> Posn
; Ésta función lo que hace es transformar de make-tupla a make-posn,
; Toma el primer elemento de la Tupla y lo convierte en el primer elemento del Posn,
; Lo mismo sucede con el segundo elemento de la Tupla.

(check-expect (tuplaToPosn (make-Tupla "SI" "NO")) (make-posn "SI" "NO") )
(check-expect (tuplaToPosn (make-Tupla 1 6)) (make-posn 1 6) )

(define (tuplaToPosn tupla) (make-posn (Tupla-f tupla)  (Tupla-s tupla ) ) )

; cifrado : N String -> List(posn)
; dada una clave de desplazamiento y un alfabeto s, devuelve una lista
; con parejas de strings, donde el primer elemento es el caracter a cifrar, y el segundo
; su codigo del Cesar de acuerdo a la clave.
;--- Se asume que 0 < n < (string-length s) ----;

(check-expect (cifrado 1 "ABC") (list (make-posn "A" "B") (make-posn "B" "C") (make-posn "C" "A")))
(check-expect (cifrado 2 "ABC") (list (make-posn "A" "C") (make-posn "B" "A") (make-posn "C" "B"))) 
(check-expect (cifrado 3 "ABC") (list (make-posn "A" "A") (make-posn "B" "B") (make-posn "C" "C")))
(check-expect (cifrado 1 "DEF") (list (make-posn "D" "E") (make-posn "E" "F") (make-posn "F" "D"))) 
(check-expect (cifrado 1 "") empty )

(define (cifrado num str)
     (cond[(empty? (partir str)) empty] ; primero usando la funcion partir, parte el string y si éste es vacio, devolverá la lista vacía.
          [else (map tuplaToPosn (emparejar (partir str) 
                                 (append(tirar(partir str)num)(tomar(partir str)num))) )]
       )
) 

;Lo que hace ésta función es utilizar la función EMPAREJAR con:
; 1: el string partido (utilizando la función PARTIR)
; 2: la unión de la lista que resulta de :
;   a) utilizar la función TIRAR en el string partido (con la función PARTIR) con el número dado, es decir,
;     obtenemos una lista a partir de ese número dado.
; y b), utilizar la función TOMAR en el string partido (con la función PARTIR) con el número dado, es decir,
;    obtenemos una lista hasta ese número dado.
;-- Por lo que de 2, resulta una lista en donde el primer elemento, queda en lo último de la lista
;---Entonces, de 1 y 2 obtenemos una lista de Tuplas, en la cuál el primer elemento pertenece a 1 y el segundo elemento pertenece a 2.
; Luego, se realiza con MAP la transformación de Tupla a Posn (utiizando la función tuplaToPosn)

;-----------------------------------------------------------------

; encriptar-simbolo : String List(posn) -> String
; dado un string s de longitud 1(uno) que es un símbolo del alfabeto
; y, una lista de parejas que representa un código del césar,
; devuelve el código que le corresponde a s
(check-expect (encriptar-simbolo "A" (cifrado 2 "ABC")) "C")
(check-expect (encriptar-simbolo "A" (cifrado 1 "ABC")) "B")
(check-expect (encriptar-simbolo "A" (cifrado 3 "ABC")) "A")
(check-expect (encriptar-simbolo "H" (cifrado 1 "HIJ")) "I")
(check-expect (encriptar-simbolo "" (cifrado 4 "ABC")) "")

(define (encriptar-simbolo letra lista-p)
  (cond[(empty? lista-p) letra ] ; primero verifica si la lista esta vacia, si lo está devuelve la misma letra dada.
       [(string=? letra (posn-x (first lista-p) ))  (posn-y(first lista-p)) ];luego pregunta si el posn-x
       ; del primero de la lista es igual a la letra dada, si lo es devuelve el posn-y del primer elemento de la lista.
       [else (encriptar-simbolo letra (rest lista-p))  ];y si no cumple con ninguna de las anteriores
       ));entonces se realiza la recursión co el resto de la lista hasta que se llegue al caso en donde,
        ; se cumpla alguna de las condiciones previas.

;-----------------------------------------------------------------

; encriptar-mensaje : String String Natural -> String
; dado un string, un alfabeto y una clave, devuelve el string encriptado
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 3) "DEF")
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 4) "EFA")
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 5) "FAB")
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 6) "ABC")
(check-expect (encriptar-mensaje "" "ABCDEF" 6) "")


(define (encriptar-mensaje str alfabeto nat)
  (cond[(=(string-length str)0) ""]; si el tamaño de la lista es igual a 0 entonces da el string vacío ("")
       [else     (string-append (encriptar-simbolo (first(partir str)) (cifrado nat alfabeto))
                                (encriptar-mensaje (substring str 1)  alfabeto nat))  ] ))
;si no es vacio, entonces realiza un string-append (une los strings) resultantes de:
; 1) utilizar la función ENCRIPTAR-SIMBOLO con la primera letra resultante de utilizar la función PARTIR con el string y,
;   del make-posn generado del número nat y alfabeto dado.
; 2) utilizar la recursión con las letras restantes y el alfabeto y número nat dado, es decir
; lo que hace, 1) y 2) es ir pasando letra por letra y cifrandola una por una para luego unirlos.

;----------------------------------------------------------------------------

; desencriptar-simbolo : String List(Tupla) -> String
; dado un string s de longitud 1 que es un símbolo del
; alfabeto y una lista de parejas que representa un código del césar,
; devuelve el caracter desencriptado que le corresponde a s
(check-expect (desencriptar-simbolo "A" (cifrado 2 "ABC")) "B")
(check-expect (desencriptar-simbolo "A" (cifrado 1 "ABC")) "C")
(check-expect (desencriptar-simbolo "A" (cifrado 3 "ABC")) "A")
(check-expect (desencriptar-simbolo "" (cifrado 4 "ABC")) "")

(define (desencriptar-simbolo str tup)
  (cond[(empty? tup) str] ;primero se fija si la tupla es vacia, si lo es devuelve el mismo string dado.
       [(string=? str (posn-y (first tup) ))  (posn-x(first tup)) ] ;luego, pregunta si el posn-y
       ; del primero de la lista es igual a la letra dada, si lo es devuelve el posn-x del primer elemento de la lista.
       [else (desencriptar-simbolo str (rest tup))  ])) ;y si no cumple con ninguna de las anteriores
       ;entonces se realiza la recursión con el resto de la lista hasta que se llegue al caso en donde,
       ; se cumpla alguna de las condiciones previas.

;---------------------------------------------------------------------------

; desencriptar-mensaje : String String Natural -> String
; dado un string, un alfabeto y una clave, devuelve el string encriptado
(check-expect (desencriptar-mensaje "DEF" "ABCDEF" 3) "ABC")
(check-expect (desencriptar-mensaje "EFA" "ABCDEF" 4) "ABC")
(check-expect (desencriptar-mensaje "BCD" "ABCDEF" 5) "CDE")
(check-expect (desencriptar-mensaje "BCD" "BCDEFGHIJKLM" 2) "LMB")
(check-expect (desencriptar-mensaje "" "ABCDEF" 4) "")

(define (desencriptar-mensaje str alfabeto nat)
  (cond[(=(string-length str)0) ""] ; primero pregunta si el largo de la palabra es igual a 0, si lo es devuelve "" (string vacío)
       [else(string-append (desencriptar-simbolo (first(partir str)) (cifrado nat alfabeto))
                                    (desencriptar-mensaje (substring str 1)  alfabeto nat))  ] ))

;si no es vacio, entonces realiza un string-append (une los strings) resultantes de:
; 1) utilizar la función DESENCRIPTAR-SIMBOLO con la primera letra resultante de utilizar la función PARTIR con el string y,
;   del make-posn generado del número nat y alfabeto dado.
; 2) utilizar la recursión con las letras restantes y el alfabeto y número nat dado, es decir
; lo que hace, 1) y 2) es ir pasando letra por letra y descifrandola una por una para luego unirlos.

;____________________________________________________________________________;
;;;;;;;; PARA TERMINAR: Testeamos lo que nos dice el enunciado:
;-- Si definimos:
    (define ALFABETO "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ 0123456789")
    (define CLAVE 3)
    (define CODIGO-DEL-CESAR (cifrado CLAVE ALFABETO))

;--y evaluamos las siguientes expresiones

(check-expect (encriptar-mensaje "HOLA" ALFABETO CLAVE) "KRÑD")
(check-expect  (encriptar-mensaje "ATACAR A LAS 18" ALFABETO CLAVE) "DWDFDU2D2ÑDV24B" )
(check-expect (encriptar-mensaje "LA OPERACION ES REVERSIBLE" ALFABETO CLAVE) "ÑD2RSHUDFLRP2HV2UHYHUVLEÑH" )
(check-expect  (desencriptar-mensaje (encriptar-mensaje "LA OPERACION ES REVERSIBLE" ALFABETO CLAVE) ALFABETO CLAVE) "LA OPERACION ES REVERSIBLE" )

;-- TODAS pasan el check-expect mencionado.