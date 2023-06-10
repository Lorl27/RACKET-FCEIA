;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |PRACTICA 4 - EJ 12|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|

Ejercicio 12. En este ejercicio representaremos una persona mediante una estructura con 5 campos:

• 1er campo: el nombre y apellido.
• 2do campo: el valor numérico de su peso.
• 3er campo: un string que representa la unidad en la cual está dado el peso (valores posibles: "g" o "kg").
• 4to campo: el valor numérico de la estatura.
• 5to campo: un string que representa la unidad en la cual está dada la estatura (valores posibles: "m" o "cm").

Teniendo en cuenta esto se pide:
• Diseñe una estructura Persona que le permita representar a cualquier persona con su peso y estatura.

• Diseñe una función IMC que tome como entrada un valor de tipo Persona y calcule su índice de masa corporal.
 
-- En caso que no reciba como entrada una estructura de tipo Persona deberá mostrar
el siguiente mensaje de error: "Tipo de dato inválido" .
------------------------------------------------------------------------
Ayuda: Por si no lo sabe, el índice de masa corporal (IMC) de una persona se calcula según la siguiente fórmula:
IMC= (PESO kg) / (ESTATURA elevada al cuadrado MTS)
---------------------------------------------------------------
Ayuda: Le puede servir conocer las siguientes equivalencias:
1 kg = 1000 g
1 m = 100 cm
|#

(define-struct Personita [ nombre-apellido peso-N peso-STR altura-N altura-STR ] )
;Una Personita es (String, Number, String, Number, String)
;Interpretación: El primer valor es representado por su nombre y apellido; El segundo, por su peso en números
;El tercero, por la unidad del peso; el cuarto, por la altura númerica y el últim por la altura en unidades.

(define (IMC-formula1 persona) (/ (Personita-peso-N persona) (sqr (Personita-altura-N persona)) ) )

(define (IMC-formula2 persona) (/ (* (Personita-peso-N persona)1000) (sqr (* (Personita-altura-N persona) 100) ) ) )

(define (IMC persona)  (cond [(not(Personita? persona)) "Tipo de dato inválido..."]
                             [else (if ( and
                                         (string=? (Personita-peso-STR persona) "kg" )
                                         (string=? (Personita-altura-STR persona) "cm") )
                                       (IMC-formula1 persona)
                                       (IMC-formula2 persona) )] 
                             )
  )

(define LUDKI (make-Personita "Ludki algo" 70 "kg" 180 "cm") )
(define LUDKI2 (make-Personita "Ludki algo" 70 "g" 180 "m") )

(IMC LUDKI) (IMC LUDKI2)