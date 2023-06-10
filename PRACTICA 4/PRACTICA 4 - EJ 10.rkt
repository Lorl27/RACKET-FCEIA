;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4 - EJ 10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 10. En este ejercicio representaremos un/a Estudiante con una estructura con los siguientes campos:

• 1er campo: Nombre del/a estudiante.
• 2do campo: Promedio de sus calificaciones (un valor entre 0 y 10).
• 3er campo: Porcentaje de asistencia a clases (un valor entre 0 y 100).

Teniendo en cuenta esto se pide:

• Diseñe una estructura Estudiante que contenga los campos descriptos más arriba.

• Diseñe una función condicion que tome como entrada un valor de tipo Estudiante
 y devuelva un string indicando su condición.
----------Condiciones posibles: "Libre" , "Regular" y "Promovido/a" .

Para calcular la condición del/a estudiante deben tenerse en cuenta las siguientes reglas:

◦ Si el/la estudiante tiene un porcentaje de inasistencia mayor al 40%
queda automáticamente libre, sin importar el promedio de sus
calificaciones.

◦ Si el/la estudiante tiene una asistencia mayor o igual al 60%:
▪ y tiene una nota inferior a 6, también se considera libre.
▪ y tiene una nota mayor o igual a 6 y menor estricta que 8, se considera regular.
▪ y una nota mayor o igual a 8, se considera promovido/a.

En caso en que la función condicion reciba como entrada un dato que no corresponda a una estructura Estudiante
 deberá responder con un mensaje de error (como por ejemplo: "Tipo de dato inválido" ).

Ayuda: Recuerde que cada estructura que define viene acompañada de un
predicado que determina si un objeto es o no una estructura de ese tipo

|#

(define-struct Estudiante [nombre promedio asistencia] )
;Un Estudiante es (String, Number, Number)
;Interpretación: El primer elemento es el nombre, el segundo el promedio entre 0-10 y el último,
; el porcentaje de asistencia a clases entre 0-100.

;condicion: Estructura Estudiante-> String
;dado un estudiante, nos devuelvesu estado acádemico.
(define (condicion estudiante) (cond [ (not (Estudiante? estudiante) ) "Tipo de dato no válido..." ]
                                     [ (or (< (Estudiante-asistencia estudiante) 60) (and (< (Estudiante-promedio estudiante) 6) (> (Estudiante-asistencia estudiante) 60) ) ) "Libre" ]
                                     [ (and (>= (Estudiante-promedio estudiante) 6) (< (Estudiante-promedio estudiante) 8) (> (Estudiante-asistencia estudiante) 60) ) "Regular" ]
                                     [ (and (>= (Estudiante-promedio estudiante) 8) (> (Estudiante-asistencia estudiante) 60) ) "Promovidx" ]
                                     )
  )

(define Santi (make-Estudiante "SantiCas" 2 45))
(define Valen (make-Estudiante "ValenDomin" 6 100))
(define Peluka (make-Estudiante "Gian" 9 90))

(condicion Santi) (condicion Valen) (condicion Peluka) (condicion "p")