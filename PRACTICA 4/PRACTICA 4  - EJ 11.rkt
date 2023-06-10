;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4  - EJ 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 11. Representaremos una casa con una estructura con los siguientes campos:

• 1er campo: propietario/a.
• 2do campo: dirección.
• 3er campo: superficie en metros cuadrados.
• 4to campo: zona.

Teniendo en cuenta esto se pide:

• Diseñe una estructura Casa que contenga los campos descriptos más arriba.

• Diseñe una función venta que tome como entrada un valor de tipo Casa y
devuelva un mensaje sobre los datos de la venta de dicha propiedad.

--Dicho mensaje deberá dar los datos sobre
el- propietario que vende,
-en qué dirección se encuentra y
-el monto de dinero que recibe por dicha venta, luego de realizados los descuentos correspondientes por el
 sellado de la escritura.

Para realizar los cálculos se deben tener en cuenta las siguientes cuestiones:

◦ Existen 4 zonas:
▪ Zona A: el metro cuadrado en esta zona tiene un valor de 20000 pesos

▪ Zona B: el metro cuadrado en esta zona tiene un valor de 15000 pesos

▪ Zona C: el metro cuadrado en esta zona tiene un valor de 10000 pesos

▪ Zona D: el metro cuadrado en esta zona tiene un valor de 5000 pesos

◦ El/la propietario/a debe pagar un sellado que se descontará del precio de la venta.
 Si el precio de la venta supera el millón de pesos,  el sellado tiene un costo del 5 % del valor y
si no lo supera se debe abonar el 3% del valor de la propiedad.
-----------------------------------------------------------------------
Ejemplo:
• Supogamos que el señor José Romero vende una propiedad que se encuentra en la calle Rueda 3456.
 Dicha propiedad posee una superficie de 120 metros cuadrados y, por la dirección en que se encuentra,pertenece a la zona C.
 
 El monto de dinero que José recibe por dicha venta es de 1140000 pesos.
 
 Si aplicáramos la función venta en este caso desearíamos que esta función nos devolviera el siguiente mensaje:
->"El señor José Romero recibirá 1140000 pesos por la venta de su propiedad ubicada en la calle Rueda 3456."

--En caso que la función venta reciba como entrada un dato que no sea de tipo Casa,
 deberá mostrar el mensaje "Tipo de dato incorrecto" .

--En caso que la función venta reciba como entrada un dato de tipo Casa con una zona distinta a A, B, C o D
 deberá mostrar el mensaje "No se puede calcular el precio de venta por no disponer de los valores
 del metro cuadrado para la zona solicitada" 

|#

(define-struct Casa [ propietario direccion mts-cuadrados zona ] )
;Una Casa es: (String, String, Number, String)
;Interpretación: El primer elemento representa el nombre completo del vendedor; El segundo, donde se ubica la vivienda
; el tercero, la cantidad de metros cuadrados que tiene y el último, su zona.

;----CONSTANTES DEL PROGRAMA:--------
(define ZONA-A 20000)
(define ZONA-B 15000)
(define ZONA-C 10000)
(define ZONA-D 5000)
;------------------------------------

;PESOS: Estructura Casa -> Number
;Dependiendo de la ZONA, el PRECIO varía
(define (PESOS data) (cond[(string=? (Casa-zona data) "A") (* ZONA-A (Casa-mts-cuadrados data) ) ]
                          [(string=? (Casa-zona data) "B") (* ZONA-B (Casa-mts-cuadrados data) ) ]
                          [(string=? (Casa-zona data) "C") (* ZONA-C (Casa-mts-cuadrados data) ) ]
                          [(string=? (Casa-zona data) "D") (* ZONA-D (Casa-mts-cuadrados data) ) ]
                          )
  )

;SACAR POCENTAJE:  Multiplicar el número por el porcentaje. DIVIDIR EL RESULTADO X 100.
;PORC: Number Number -> Number
(define (PORC porcentaje numero) (/ (* porcentaje numero) 100) ) ;Nos da el %

;SELLADO: Estructua Casa -> Number
;Nos da el valor de la casa, con los intereses
(define (SELLADO data) (if (< (PESOS data) 1000000) (PORC 3 (PESOS data)) (PORC 5 (PESOS data)) ) )

;venta: Estructura Casa -> String
;Según el dato ingresado,, nos dará su valor concatenado a toda la info de la CASA.
(define (venta data) (cond[(not (Casa? data) ) "Tipo de dato Incorrecto..."]
                          [(not (or (string=? (Casa-zona data) "A") (string=? (Casa-zona data) "B") (string=? (Casa-zona data) "C") (string=? (Casa-zona data) "D") )) "No se puede calcular el precio de venta por no disponer de los valores del metro cuadrado para la zona solicitada" ]
                          [else (string-append "La persona llamada "(Casa-propietario data)" recibirá " (number->string ( - (PESOS data) (SELLADO data) ) )" pesos por la venta de su propiedad ubicada en " (Casa-direccion data) )]
                          )
  )

(define EJEMPLO (make-Casa "José Romero" "calle Rueda 3456" 120 "C") )

(venta EJEMPLO)