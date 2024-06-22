;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |PRÁCTICA 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;                                        	PRÁCTICA 2

#|
Ejercicio 1. Diseñe una función distancia-origen, que recibe dos
números x e y, devolviendo como resultado la distancia al origen del
punto (x,y).
|#

; Representamos la distancia al origen a través de una fórmula, que nos la devuelve en número

;formulita: Number Number -> Number
; Sirve para calcular la distancia de 2 números dados respecto al origen

;dist-origin: Number Number -> Number
; Cumple la misma función que formulita, pero está hecha para que el código sea más ordenado.
; Le pasamos 2 números y nos devuelve su distancia con respecto al origen.

;entrada: 2 6  salida: 6.32455532034
;entrada: 1 10 salida: 10.0498756211

;(check-expect (dist-origin 2 6)  6.32455532034)
;(check-expect (dist-origin 1 10)  10.0498756211)

(define (formulita x y) (sqrt (+ ( sqr x) ( sqr y))))
(define (dist-origin x y)  (formulita x y ))

(dist-origin 2 6)  ;validación de los ejemplos
(dist-origin 1 10) ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 2. Diseñe una función distancia-puntos, que recibe cuatro
números x1, y1, x2 e y2 y devuelve la distancia entre los puntos (x1,
y1) y (x2, y2).
|#

; Representamos la distancia entre ambos puntos a través de una fórmula, que nos la devuelve en número

;formula: Number Number Number Number -> Number
; Sirve para calcular la distancia de 2 coordenadas con otrras 2, nos retorna la distancia en número

;dist-puntoAyB: Number Number Number Number -> Number
; Cumple la misma función que formula, pero está hecha para que el código sea más ordenado.
; Le pasamos 4 números y nos devuelve la distancia entre num1,num2 y num3,num4

;entrada: 2 6 7 8  salida: 5.38516480713
;entrada: 1 10 3 45 salida: 35.0570962859

;(check-expect (dist-puntoAyB 2 6 7 8) 5.38516480713)
;(check-expect (dist-puntoAyB 1 10 3 45) 35.0570962859)

(define (formula x1 y1 x2 y2) (sqrt (+ ( sqr ( - x2 x1) ) ( sqr ( - y2 y1)))))
(define (dist-puntoAyB x1 y1 x2 y2)  (formula x1 y1 x2 y2 ))

(dist-puntoAyB 2 6 7 8)  ;validación de los ejemplos
(dist-puntoAyB 1 10 3 45)  ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 3. Diseñe la función vol-cubo que recibe la longitud de la arista de un cubo y calcula su volumen.
|#

; La arista es el lado común entre 2 caras, el cubo tiene 6 caras.
; Para calcular el volumén del cubo se debe hacer: arista * arista * arista

;vol-cubo: Number -> Number
; Sirve para calcular el volumén de un cubo, utlizando a su arista. Lo que hace es elevarla al cubo.

;entrada: 2   salida: 8
;entrada: 10  salida: 1000

(check-expect (vol-cubo 2) 8)
(check-expect (vol-cubo 10) 1000)

(define (vol-cubo arista) ( * ( sqr arista) arista))

(vol-cubo 2) ;validación de los ejemplos
(vol-cubo 10) ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 4. Diseñe la función area-cubo que recibe la longitud de la arista de un cubo y calcula su área.
|#

; La arista es el lado común entre 2 caras, el cubo tiene 6 caras.
; Para calcular el área de una arista: arista * arista
; Para calcular el área del cubo se debe hacer: (arista * arista) * 6

;area-cubo: Number -> Number
; Sirve para calcular el área de un cubo, utlizando a su arista. Lo que hace es elevarla al cuadrado y
; multiplicarla por las caras del cuadrado (6).

;entrada: 2   salida: 24
;entrada: 5  salida: 150

(check-expect (area-cubo 2) 24)
(check-expect (area-cubo 5) 150)


(define (area-cubo arista) (* ( sqr arista) 6))

(area-cubo 2) ;validación de los ejemplos
(area-cubo 5) ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 5. Diseñe la función string-insert, que consume un string y un número i
e inserta "-" en la posición i-ésima del string.
|#

; Debemos agarrar a una cadena de texto y separarla en un numero x dado por el usuario,
; donde se va a insertar un "-" en medio.

;lado1: String Number-> String
;Toma una cadena de texto y un número x, lo que hace es separar el string entre el inicio y ese número x,
; para agregarle "-" al final.

;lado2: String Number-> String
;Toma una cadena de texto y un número x, lo que hace es separar el string entre el número x y la máxima longitud
; del string dado

;poner-: String Number-> String
;Toma una cadena de texto y un número x, lo que hace es unir la función lado1 y lado2, es decir,
; agarra el string, lo separa en el número x dado y allí le inserta "-".

;entrada: "hola" 2          salida: "ho-la"
;entrada: "hermafrodita" 5  salida: "herma-frodita"
;entrada: "hermafrodita" 9  salida: "hermafrod-ita"

(check-expect (poner- "hola" 2) "ho-la")
(check-expect (poner- "hermafrodita" 5) "herma-frodita")
(check-expect (poner- "hermafrodita" 9) "hermafrod-ita")

(define (lado1 str x) ( string-append  (substring str 0 x) "-" ) )

(define (lado2 str x) ( string-append (substring str x (string-length str)) "" ) )

(define (poner- str x) ( string-append  (lado1 str x)  (lado2 str x) ) )

(poner- "hola" 2)         ;validación de los ejemplos
(poner- "hermafrodita" 5) ;validación de los ejemplos
(poner- "hermafrodita" 9) ;validación de los ejemplos

;---------------------------------------

 #|
Ejercicio 6. Diseñe la función string-last, que extrae el último caracter de una cadena no vacía.
|#

; Se debe retornar la última palabra/número/etc de un string dado

;string-last: String->String
;Toma un string y devuelve el último caracter, lo que hace es subdividirlo entre
;el penúltimo y el último.


;entrada: "Valenn Delfi Pekiiii" salida: "i"
;entrada: "1233454655677770"     salida: "0"

(check-expect (string-last "Valenn Delfi Pekiiii") "i")
(check-expect (string-last "1233454655677770") "0")

(define (string-last str) (substring str ( - (string-length str) 1) (string-length str ) ))

(string-last "Valenn Delfi Pekiiii") ;validación de los ejemplos
(string-last "1233454655677770")     ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 7. Diseñe la función string-remove-last, que recibe una cadena y devuelve la misma cadena sin el último caracter.
|#

; Se debe retornar la última palabra/número/etc de un string dado

;string-remove-last: String->String
;Toma un string y lo devuelve sin el último caracter, lo que hace es subdividirlo entre
;el antepen{ultimo y el penúltimo; Arroja ese nuevo string como resultado.


;entrada: "Valenn Delfi Peki" salida: "k"
;entrada: "1233454655670"     salida: "7"

(check-expect (string-remove-last "Valenn Delfi Peki") "k")
(check-expect (string-remove-last "1233454655670") "7")

(define (string-remove-last str) (substring str ( - (string-length str) 2)  ( - (string-length str) 1)))

(string-remove-last "Valenn Delfi Peki") ;validación de los ejemplos
(string-remove-last "1233454655670")     ;validación de los ejemplos

;---------------------------------------

#|
Ejercicio 8. Para cada una de las funciones diseñadas en la sección
anterior, agregue las líneas de código necesarias para automatizar los
casos de prueba.
|#

; Representamos temperaturas mediante números
; far->cel : Number -> Number
; recibe una temperatura en Fahrenheit, devuelve su equivalente en Celsius
; entrada: 32, salida: 0
; entrada: 212, salida: 100
; entrada: 122, salida: 50
(define (far->cel f)
(* 5/9 (- f 32)))

(check-expect (far->cel 32) 0)
(check-expect (far->cel 212) 100)
(check-expect (far->cel 122) 50)
;(check-expect (far->cel 122) 60) ;f

;---------------------------------------------------------------------------------

#|
Ejercicio 9. Un Instituto de Portugués decide lanzar las siguientes promociones
buscando aumentar la cantidad de alumnos:

• Si se anotan 2 amigos, cada uno obtiene un 10% de descuento sobre el valor de la cuota;
mientras que si se anotan 3 o más el descuento alcanza el 20%

• Si al momento de pagar se decide abonar 2 meses juntos se recibe un descuento del 15%;
en caso de cancelar 3 o más meses a la vez la reducción es del 25%.

Las promociones son combinables, pero nunca pueden superar el 35% de descuento.

El valor original de la cuota mensual es de $650.


La administración del Instituto nos solicitó diseñar la función monto-persona,
la cual recibe la cantidad de personas que se están anotando y la cantidad de meses que abonan
(para que se aplique la promoción deben pagar la misma cantidad de meses),
 y devuelve el monto que el Instituto debe cobrarle a cada uno.

Para desarrollar monto-persona es conveniente definir ciertas constantes, ya que
los precios pueden variar con el tiempo.


Ejemplos:

• Supogamos que Pedro y Juan deciden anotarse al curso de Portugués pagando 2 meses juntos,
 obtendrán un descuento del 25%, debiendo pagar $975 cada uno.

• Si Pedro y Juan también invitan a Paula y cancelan 3 meses juntos,
recibirán una reducción del 35%, debiendo abonar $1267.50 cada uno.

• Si José se anota solo, pero paga 5 cuotas juntas, entonces deberá abonar $2437.5
|#

;                      <<<<<<<<<<<<<<<<<<<< D O I N G - T H I N G S >>>>>>>>>>>>>>>>>>>>>>>>>
;calcularPorcentaje: Number Number-> Number
;Calcula el porcentaje dado de un precio x
;(define (calcularPorcentaje porciento precio) ( / (* porciento precio) 100) )

;Cuota mensual= $650

;2 amigos= 10% de $650
;3 o más = 20% de $650

;pagando 2 meses juntos= 15% de $650
;pagando 3 o más= 25% de $650

;Combinables pero el límite es %35 de $650.

;hacer función que calcule el monto según la cant. de personas y meses abonados.

(check-expect (monto-persona 2 1) 1105)
(check-expect (monto-persona 2 2) 975)
(check-expect (monto-persona 3 3) 1267.50)
(check-expect (monto-persona 5 1) 2437.5)
(check-expect (monto-persona 1 1) 650)

;Precio de la cuota mensual del Instituto.
(define CUOTA-MENSUAL 650)

;calcularPorcentaje: Number Number-> Number
;Calcula el porcentaje dado, en forma decimal.
(define (calcularPorcentaje porciento) ( / porciento 100) )

;Descuento por pagar 3 ó más meses.
(define PORCENTAJE-3-O-MAS-MESES (calcularPorcentaje 25 ) )

;Descuento por pagar 2 meses
(define PORCENTAJE-2-MESES (calcularPorcentaje 15 ))

;Descuento por 2 personas
(define PORCENTAJE-2-PERSONAS (calcularPorcentaje 10))

;Descuento cuando 3 ó más personas pagan, calcula el total.
(define PORCENTAJE-3-O-MAS-PERSONAS (calcularPorcentaje 20))

;El descuento máximo que puede haber.
(define LIMITE (calcularPorcentaje 35 ))

;calcularMeses Number-Number
;Calcula cuánto sale el monto según los meses dados
(define (calcularMeses meses) (* CUOTA-MENSUAL meses))

;calcularPrecio Number Number-> Number
;Recibe los meses dados y le aplica el porcentaje correspondiente
(define (calcularPrecio porcentaje meses) (* porcentaje (calcularMeses meses)) )

;PRECIO: Number Number-> Number
;Según el porcentaje y meses dados, calcula el monto final.
(define (PRECIO porcentaje meses) ( - (calcularMeses meses) (calcularPrecio porcentaje meses) ) )
 
;monto-persona: Number Number->Number
;Dados un número de personas y de meses abonados, calcula el monto a cobrar.
(define (monto-persona meses-abonados cant-personas) (cond [ (and ( = cant-personas 2)  ( = meses-abonados 2))   ( PRECIO ( + PORCENTAJE-2-MESES PORCENTAJE-2-PERSONAS) meses-abonados) ]
                                                           [ (and ( = cant-personas 2)  ( >= meses-abonados 3))  ( PRECIO  ( + PORCENTAJE-3-O-MAS-MESES PORCENTAJE-2-PERSONAS)  meses-abonados)  ] 
                                                           [ (and ( >= cant-personas 3) ( = meses-abonados 2))   ( PRECIO  ( + PORCENTAJE-2-MESES PORCENTAJE-3-O-MAS-PERSONAS) meses-abonados )   ]
                                                           [ (and ( >= cant-personas 3) ( >= meses-abonados  3)) ( PRECIO  LIMITE  meses-abonados)  ]
                                                           [ (or ( = cant-personas 0) (= meses-abonados 0))       "Ingrese un dato para proseguir..." ]
                                                           [ ( = cant-personas 2)                                ( PRECIO PORCENTAJE-2-PERSONAS meses-abonados ) ]
                                                           [ ( >= cant-personas 3)                               ( PRECIO PORCENTAJE-3-O-MAS-PERSONAS meses-abonados ) ]
                                                           [ ( = meses-abonados 2)                               (PRECIO PORCENTAJE-2-MESES meses-abonados) ]
                                                           [ (>= meses-abonados 3)                               ( PRECIO PORCENTAJE-3-O-MAS-MESES meses-abonados)] 
                                                           [ (or ( = cant-personas 1) (= meses-abonados 1))       CUOTA-MENSUAL ]
                                                           [ else                                                 "error"] ) )
 
(monto-persona 2 2)
(monto-persona 2 3)
(monto-persona 3 2)
(monto-persona 2 3)
(monto-persona 1 1)
(monto-persona 2 1)
(monto-persona 1 2)
(monto-persona 1 3)
(monto-persona 3 1)
(monto-persona 3 0)
(monto-persona 2 0)
(monto-persona 1 0)
(monto-persona 0 0)
(monto-persona 0 1)
(monto-persona 0 2)
(monto-persona 0 3)

;---------------------------------------------------------------------------------

#|

Ejercicio 10. Tomando como base los resultados obtenidos en un
laboratorio de análisis clínicos, un médico determina si una persona
tiene anemia o no, lo cual depende de su nivel de hemoglobina en la
sangre y de su edad.
Si el nivel de hemoglobina que tiene una persona es menor que el
valor mínimo que le corresponde de acuerdo a su edad, el resultado
del análisis es "anemia positivo" y en caso contrario es "anemia
negativo".
El médico se basa en los siguientes valores mínimos para cada grupo
de edades:

• edad <= 1 mes: nivel mínimo de hemoglobina normal 13 g/dl

• 1 mes < edad <= 6 meses: nivel mínimo de hemoglobina normal 10 g/dl

• 6 meses < edad <= 12 meses: nivel mínimo de hemoglobina normal 11 g/dl

• 1 año < edad <= 5 años: nivel mínimo de hemoglobina normal 11.5 g/dl

• 5 años < edad <= 10 años: nivel mínimo de hemoglobina normal 12.6 g/dl

• 10 años < edad: nivel mínimo de hemoglobina normal 13 g/dl

Diseñe una función anemia que recibiendo la edad de una persona
expresada en meses y la hemoglobina en sangre expresada en g/dl
devuelva #true si la persona está anémica, #false en caso contrario.

|#

;Diseñar función que reciba la edad en meses de una persona y la hemoglobina expresada en g/dl
; Devolver #true si presenta anemia ó #false si no tiene.

;Para tener anemia, el nivel de hemoglobina debe ser menor al valor mínimo para tu edad.

(check-expect (anemia? 127 10)#true)
(check-expect (anemia? 127 16)#false)

;anemia?: Number Number-> Boolean
;Según la edad expresada en meses y gl/dl en números, calcula si la persona sufre de anemía o no. Retorna un boolean
(define (anemia? edad-meses gl/dl) (cond[ (<= edad-meses 1)                             (if ( <= gl/dl 13) #true #false)]
                                        [ (and (> 6 edad-meses) (<= 1 edad-meses ) )    (if (<= gl/dl 10) #true #false) ]
                                        [ (and (> 12 edad-meses) (<= 6 edad-meses ) )   (if (<= gl/dl 11) #true #false) ]
                                        [ (and (> 60 edad-meses) (<= 12 edad-meses ) )  (if (<= gl/dl 11.5) #true #false) ]
                                        [ (and (> 120 edad-meses) (<= 60 edad-meses ) ) (if (<= gl/dl 12.6) #true #false) ]
                                        [( > edad-meses 120)                            (if (<= gl/dl 13) #true #false) ]
                                        [else                                           "valores no válidos"]) )

(anemia? 1 12)
(anemia? 1 125)
(anemia? 62 12)
(anemia? 108040 1)

;----------------------------------------------------------------------------------
#|

Ejercicio 11. Decimos que una terna de números a,b,c es
autopromediable si uno de sus valores concide con el promedio de los
otros dos.
Por ejemplo, la terma (7,5,9) es autopromediable puesto que 7 es el
promedio entre 5 y 9.
Diseñe una función que dados tres números, devuelva el producto de
ellos en caso que formen una terna autopromediable, y la suma de los
mismos en caso contrario.
Defina todas las constantes y funciones auxiliares que crea
convenientes para obtener un buen diseño.

|#

;terna autopromediable: uno de sus valores coincide con el promedio de los otros 2.
;Si es autopromediable-> devolver el producto
;Si NO es-> devolver la suma.

(check-expect(terna-autopromediable 7 5 9) 21)
(check-expect(terna-autopromediable 3 4 5) 60)

;promedio?: Number Number Number-> Boolean
;Dados 3 números cualquiera, retorna #true si la suma de 2 de sus números es igual al tercero.
(define (promedio? num1 num2 num3) ( or (equal? (/ (+ num1 num2) 2 ) num3)  (equal? (/ (+ num1 num3) 2 ) num2) (equal? (/ (+ num3 num2) 2 ) num1) )  )

;pitagorica?: Number Number Number-> Boolean
;Dados 3 números cualquiera, retorna #true si conforman a una terna pitagórica
(define (pitagorica? num1 num2 num3) (cond [ ( = (+ (sqr num1) (sqr num2)) (sqr num3) ) #true]
                                           [else #false]))

;terna-autopromediable:Number Number Number-> Boolean
;Dados 3 números cualquiera, retorna el producto de los 3 si es una terna-autopromediable. En caso de no serlo, retorna la suma. Usamos las funciones promedio? y pitagorica? definidas anteriormente para simplificar el código.
( define( terna-autopromediable num1 num2 num3) (if (and ( equal? (pitagorica? num1 num2 num3) #true) ( equal? (promedio? num1 num2 num3) #true) ) (* num1 num2 num3) (+ num1 num2 num3) ) )

 
(terna-autopromediable 7 5 9)
(terna-autopromediable 3 4 5)

;--------------------------------------------------------------

#|

Ejercicio 12. El consumo promedio de una Chevrolet Zafira modelo
2010 es de 8km/l en ciudad y 11km/l en ruta. Es decir, ese modelo de
auto utiliza un litro de nafta para recorrer 8km en ciudad, pero en ruta
la misma cantidad de combustible alcanza para recorrer 11km.
Estos valores fueron calculados utilizando nafta grado 2 (conocida
como "súper").
 Al cargar combustible con nafta grado 3 (conocida
como "premium"), el rendimiento mejora un 10%. Por lo tanto, con
cada litro de combustible grado 3 se puede recorrer un 10% más de
distancia de la especificada en el párrafo anterior.

Diseñe una función autonomía, que dados los siguientes argumentos:

• La cantidad de litros restantes en el tanque de combustible, y
• La clase de combustible que se está utilizando,

devuelva un string indicando la autonomía del auto, tanto en ciudad
como en ruta.


Por ejemplo,

si quedan 20 litros de nafta grado 2 en el tanque de
combustible, se espera que el string que devuelva la función autonomía sea
"Autonomía en ciudad: 160km. Autonomía en ruta: 220km."

En cambio, si quedan 20 litros de nafta grado 3, se espera que el string sea:
 "Autonomía en ciudad: 176km. Autonomía en ruta: 242km."

Utilice constantes y todas las funciones auxiliares que crea conveniente
para lograr un buen diseño

|#

; consumo promedio: 8km/l CIUDAD - 11km/l RUTA (NAFTA GRADO 2 - PREMIUM)
; consumo promedio: 10% + (NAFTA GRADO 3 - SUPER)

(check-expect(autonomia 20 "premium")"Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect(autonomia 20 "super")"Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")

(define consumoPREMIUMcity 8)
(define consumoPREMIUMruta 11)

(define consumoSUPERcity (* consumoPREMIUMcity 1.10))                                               
(define consumoSUPERruta (* consumoPREMIUMruta 1.10))

;autonomia: Number Number-> String
;Recibe la cantidad de combustibles restantes y según el tipo usado, devuelve cuánto km en ciudad y en ruta le quedan.
(define (autonomia litros-restantes combustible-usado) (cond[(string=? combustible-usado "premium") (string-append "Autonomía en ciudad: "(number->string ( * litros-restantes  consumoPREMIUMcity))"km. Autonomía en ruta: "(number->string ( * litros-restantes  consumoPREMIUMruta))"km." )  ] 
                                                            [(string=? combustible-usado "super") (string-append "Autonomía en ciudad: "(number->string ( * litros-restantes  consumoSUPERcity))"km. Autonomía en ruta: "(number->string ( * litros-restantes  consumoSUPERruta))"km." ) ]
                                                            [else "error"]   ))
(autonomia 50 "super")
(autonomia 0 "grado 2")