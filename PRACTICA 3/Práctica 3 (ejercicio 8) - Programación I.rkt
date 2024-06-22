;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3-8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Constantes del fondo
(define ANCHO_FONDO 800)
(define ALTO_FONDO 500)
(define COLOR_FONDO "lightblue")
(define FONDO (empty-scene ANCHO_FONDO ALTO_FONDO COLOR_FONDO))

; Constantes de las estrellas
(define COLOR_ESTRELLA "yellow")
(define MINTAM_ESTRELLA 15)
(define MAXTAM_ESTRELLA 30)

; El estado es una imagen que representa el cielo dibujado por el programa

; dibujar-pantalla : Estado -> Image
; Devuelve el estado actual para que sea dibujado en pantalla
(define (dibujar-pantalla e) e)

; agregar-cielo : Estado Image Number Number -> Estado
; Dado el estado actual del cielo y una imagen dada, agrega esta última sólo si entra dentro de la imagen del estado.
(define (agregar-cielo e img x y) 
  (if (and (>= (- x (/ (image-width img) 2)) 0)
           (<= (+ x (/ (image-width img) 2)) ANCHO_FONDO)
           (>= (- y (/ (image-height img) 2)) 0)
           (<= (+ y (/ (image-height img) 2)) ALTO_FONDO)
      )
      (place-image img x y e)
      e
  )
)

; calcular-tam-estrella : Number -> Number
; Dada la coordenada x en la que se quiere colocar la estrella, determina el tamaño de la misma
; en el rango [MINTAM_ESTRELLA, MAXTAM_ESTRELLA] en base a una transformación lineal al ancho del FONDO.
(define (calcular-tam-estrella x) (+ MINTAM_ESTRELLA (* (- MAXTAM_ESTRELLA MINTAM_ESTRELLA) (/ x ANCHO_FONDO))))

; agregar-estrella : Estado Number Number -> Estado
; Dado el estado del cielo estrellado y dos coordenadas enteras, devuelve un nuevo estado
; con una nueva estrella en la posición indicada, si es que ésta cabe en la pantalla.
; El tamaño de una estrella varía entre MINTAM_ESTRELLA y MAXTAM_ESTRELLA, según la posición
; en la coordenada x en la cual se la quiere colocar. A mayor coordenada x, mayor su tamaño.
(define (agregar-estrella e x y)
  (agregar-cielo e (star (calcular-tam-estrella x) "solid" COLOR_ESTRELLA) x y)
)

; manejador-mouse : Estado Number Number String -> Estado
; Maneja los eventos del mouse mediante on-mouse:
; - Al hacer click en la pantalla, dibuja una estrella en su posición
(define (manejador-mouse e xm ym ev) (cond [(string=? ev "button-down") (agregar-estrella e xm ym)]
                                           [else e]
                                     )
)

; manejador-teclado : Estado String -> Estado
; Maneja los eventos del teclado mediante on-key:
; - Al presionar backspace, elimina todas las estrellas del cielo, reiniciando al estado inicial.
; - Cualquier otro evento distinto es ignorado, y devuelve el mismo estado.
(define (manejador-teclado e k) (cond [(key=? k "\b") FONDO]
                                      [else e]
                                )
)

; Función big-bang principal
(big-bang FONDO
          [on-draw dibujar-pantalla]
          [on-mouse manejador-mouse]
          [on-key manejador-teclado]
)