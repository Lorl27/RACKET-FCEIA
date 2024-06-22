;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |PRACTICA EN PAPEL 5.2|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;                                       PRÁCTICA 5.2 EN PAPEL:


;Sistema de contraseñas
#|
Considerando los ejercicios del Sistema de contraseñas enunciados en Ejercicios para
resolver en papel - 5.1 Estructuras y Listas, resuelva utilizando patrones  Ejercicio. 1,
 Ejercicio. 2 y  Ejercicio. 3 de esta práctica. |#

;---------------------------EXTRAS

(define-struct Usr [login pass permiso] )
;Usr es (String, String, Number)
;Un elemento Usr representa el registro de una persona con acceso al sistema operativo donde
;login: es el nombre identificador de la persona,
;pass: es la contraseña de acceso,
;permiso: es el identificador de los permisos en el sistema que tiene la persona (ADMIN/USRPERMISO)

; Si tiene permiso de administración, el valor es 0; en otro caso es 1.
;----------------------------------------------------------------------
; 0 es el identificador de permisos de administración del sistema
(define ADMIN 0)
;1 es el identificador de permisos de uso básico del sistema
(define USRPERMISO 1)
;---------------------------------------------------------------
;Constantes que pueden ser usadas para casos de prueba.
(define ANA (make-Usr "ana" "12345678" ADMIN))
(define LUIS (make-Usr "luis" "12345678" USRPERMISO))
(define MARTA (make-Usr "marta" "R34dlsoA" ADMIN))
(define L1 (list ANA LUIS))
(define L2 (list ANA LUIS MARTA))
 ;--------------------------

#|
 Ejercicio 1. De dos nuevas definiciones de la función cantAdmin (5.1. - Ejercicio. 4)
de acuerdo a lo siguiente.
a) cantAdmin recorre más de una vez la lista recibida.
*b) cantAdmin recorre una única vez la lista recibida.


|#


(define (esAdmin? persona) (equal? (Usr-permiso persona) ADMIN) )

;A

(define (cantAdmin listaUsu) (length (filter esAdmin? listaUsu) ) )

;B

(define (sumarAmdmis x s) (flder + 1 (esAdmin? x) ) )