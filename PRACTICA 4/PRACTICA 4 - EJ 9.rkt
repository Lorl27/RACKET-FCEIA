;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4 - EJ 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 9. En este ejercicio representaremos un auto con una estructura con
los siguientes campos:
• 1er campo: Modelo.
• 2do campo: Año.
• 3to campo: Tipo de combustible (diesel o nafta).
• 4to campo: Rendimiento óptimo, expresado en kilómetros por litro.

Teniendo en cuenta esto se pide que:
• Diseñe una estructura Auto que contenga los campos descriptos más arriba.

• Diseñe una función costo-viaje que tome como entrada un valor de tipo Auto y un número de kilómetros a recorrer
 y calcule el costo del viaje. Para esto debe tener en cuenta:

◦ Cantidad de combustible: el número de litros necesarios para recorrer J kilómetros con un auto nuevo
está determinado por su rendimiento óptimo. Sin embargo, con el correr de los años, el rendimiento disminuye.
 Se estima que si el auto tiene:
▪ Entre 1 y 5 años, el rendimiento disminuye 2%.
▪ Entre 6 y 10 años, el rendimiento disminuye 6%.
▪ Entre 10 y 15 años, el rendimiento disminuye 10%.
▪ Más de 15 años, el rendimiento disminuye 15%.


Es decir, si un auto 0km rinde 13km/litro,
 después de un año rendirá 12.74 km/litro,
y después de 12 años 11,7  km/litro.

◦ Peajes: por cada 100 kilómetros recorridos, se debe pagar un peaje de $50.

◦ Precio combustible: el precio actual del litro de nafta es $19 y el litro de diesel $17.

Ejemplo: Un gol naftero 2013 de rendimiento óptimo 13km/litro, debido
a sus 4 años de antigüedad tendrá un rendimiento de 12,74 km/litros.
 Por lo tanto, recorrer 450 kms tendrá un costo de: (450 / 12,74) * $19 + $200

|#

(define-struct Auto [ modelo año T-Combus rendimientoOP ] )
; Auto es (String, Number, String, Number)
;Interpretación: el primer elemento determina el modelo del auto, el segundo el año,  el tercero qué tipo de
;combustible tiene el Auto y el última determina cuál es su rendimiento Óptimo en km/l.

;(define AUTO (make-Auto 2010 1 "nafta" 13) )

;calc-por Number-> Number
;Calcula el valor decimal de un porcentaje sobre 100 (le restal el % a 100)
(define (calc-por porcentaje) (/ ( - 100 porcentaje ) 100 ))

;--------------CONSTANTES DEL PROGRAMA:
(define t1 (calc-por 2) )
(define t2 (calc-por 6) )
(define t3 (calc-por 10) )
(define t4 (calc-por 15) )
;-------------------------------------

;combus-precio: Estructura Auto -> Number
;Dependiendo del tipo de combustile de AUTO, retorna su precio x litro (cada lt vale...)
(define (combus-precio auto) (cond[(string=? (Auto-T-Combus auto) "nafta") 19 ]
                                  [(string=? (Auto-T-Combus auto) "diesel") 17 ] )  )

;cant-combus: Estructura Auto -> Number
;Nos devuelve el rendiiento del auto segun sus años
(define (cant-combus auto) (cond[ (and (>= (Auto-año auto) 1)  (< (Auto-año auto) 5))  (* (Auto-rendimientoOP auto) t1) ]
                                [ (and (>= (Auto-año auto) 5)  (< (Auto-año auto) 10)) (* (Auto-rendimientoOP auto) t2) ]
                                [ (and (>= (Auto-año auto) 10) (< (Auto-año auto) 15)) (* (Auto-rendimientoOP auto) t3) ]
                                [ (>= (Auto-año auto) 15)                              (* (Auto-rendimientoOP auto) t4) ]
                                [else (Auto-rendimientoOP auto) ]
                                )
  )

(check-expect (cant-combus (make-Auto 2010 1 "nafta" 13) ) 12.74)
(check-expect (cant-combus (make-Auto 2010 12 "nafta" 13) ) 11.7)

;peaje: Number-> Number
;cada 100km, el pasaje sera de $50
(define (peaje km) (* (floor (/ km 100) ) 50 ) ) ;necesario el FLOOR. ya q sin el seria 4.5*50 y NO 4*50 (25 d diferencia)

;costo-viaje: Estructura Auto Number -> Number
;Calcula el precio del viaje según los km dados y las caracteristicas del auto(combus y antiguedad)
(define (costo-viaje auto km) (+ (peaje km)
                                 (/ (* (combus-precio auto) km) (cant-combus auto) ) ;combus $$$ / cant usada seegun rendimiento.
                               )
  )

(costo-viaje (make-Auto 2010 4 "nafta" 13) 450)