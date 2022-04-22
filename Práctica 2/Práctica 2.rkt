;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 2

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;Ejercicio 1)-

;Representamos las coordenadas de un punto cualquiera utilizando dos números, los cuales notaremos como
;x e y.
;La distancia entre el punto y el origen de coordenadas la representamos con un número.

;distancia_origen: Number Number -> Number
;distancia_origen toma las coordenadas de un punto y devuelve la distancia al origen.

(check-expect (distancia_origen 4 3) 5)
(check-expect (distancia_origen -4 -3) 5)
(check-expect (distancia_origen 0 0) 0)
(check-expect (distancia_origen 10 0) 10)

(define (distancia_origen x y)
        (sqrt(+ (sqr x)(sqr y)))
  )
;__________________________________________
;Ejercicio 2)-

;Representamos las coordenadas de cada punto con números. En el caso del punto 1 serán x1 e y1, y para
;el punto 2 serán x2 e y2.
;La distancia entrel los puntos la representamos con un número.

;distancia_puntos: Number Number Number Number -> Number
;distancia_punto toma las coordenadas de dos puntos y devuelve la distancia entre ambos


(check-expect (distancia_puntos 0 0 4 3) 5)
(check-expect (distancia_puntos 1 1 5 4) 5)
(check-expect (distancia_puntos -1 -1 3 2) 5)
(check-expect (distancia_puntos 0 -10 0 0) 10)

(define (distancia_puntos x1 y1 x2 y2)
        (sqrt (+ (sqr (- x1 x2))(sqr (- y1 y2))))
  )

;__________________________________________
;Ejercicio 3)-

;Representamos la arista del cubo y a su volumen con números positivos,
;ya que no se permiten distancias negativas.

;vol-cubo: Number -> Number
;vol-cubo recibe la longitud de la arista de un cubo y nos devuelve su volumen.

(check-expect (vol-cubo 0) 0)
(check-expect (vol-cubo 1) 1)
(check-expect (vol-cubo 2) 8)
(check-expect (vol-cubo -1) "No se admiten valor negativos")

(define (vol-cubo arista)
        (if (< arista 0)
            "No se admiten valor negativos"
            (expt arista 3)
            )
  )

;__________________________________________
;Ejercicio 4)-

;Representamos la arista del cubo y a su área con números positivos,
;ya que no se permiten distancias negativas.

;area-cubo: Number -> Number
;area-cubo recibe la longitud de la arista de un cubo y nos su área.

(check-expect (area-cubo 0) 0)
(check-expect (area-cubo 1) 6)
(check-expect (area-cubo 2) 24)
(check-expect (area-cubo -1) "No se admiten valor negativos")

(define (area-cubo arista)
        (if (< arista 0)
            "No se admiten valor negativos"
            (* 6 (sqr arista))
            )
  )

;__________________________________________
;Ejercicio 5)-

;Representamos a la frase con un String y a la posición donde se inserta el "-"
;con un número.

;string-insert: String Number -> String
;Toma una frase y la posición donde se quiere insertar "-", y nos devuelve
;una nueva cadena con el caracter ya insertado.

(check-expect (string-insert "Hola" 0) "-Hola")
(check-expect (string-insert "Hola" 1) "H-ola")
(check-expect (string-insert "Hola" 3) "Hol-a")
(check-expect (string-insert "Hola" 4) "Hola-")


(define (string-insert s i)
  (string-append (substring s 0 i)"-" (substring s i (string-length s)))
  )


;__________________________________________
;Ejercicio 6)-

;Representamos a la frase con un String.

;string-last: String -> String
;Toma una cadena no vacía y nos devuelve el último caracter de dicha cadena

(check-expect (string-last "") "No se admiten cadenas vacías")
(check-expect (string-last "Hola") "a")

(define (string-last s)
  (cond [(string=? "" s) "No se admiten cadenas vacías"]
        [else (string-ith s (- (string-length s) 1))]
        )
  )

;__________________________________________
;Ejercicio 7)-

;Representamos a la frase con un String.

;string-remove-last: String -> String
;Toma una cadena no vacía y nos devuelve la cadena sin el último caracter

(check-expect (string-remove-last "") "No se admiten cadenas vacías")
(check-expect (string-remove-last "Hola") "Hol")

(define (string-remove-last s)
  (cond [(string=? "" s) "No se admiten cadenas vacías"]
        [else (substring s 0 (- (string-length s) 1))]
        )
  )

;__________________________________________
;Ejercicio 9)- CORREGIR

;Reprensetarmoes la cantidad de personas que se quieren anotar, y la cantidad de meses
;que pagan, con números.
;La salida nos devolverá un String en donde se indique el precio a pagar por persona

;monto-persona: Number Number -> String
;Toma una cantidad de personas y cuantos meses se anotan al instituto, y nos devuelve en forma de mensaje
;cuanto debe pagar cada uno.

(check-expect (monto-persona 2 2) "Cada estudiante deberá abonar $975")
(check-expect (monto-persona 1 1) "El estudiante deberá abonar $650")
(check-expect (monto-persona 2 1) "Cada estudiante deberá abonar $585")
;VERIFICAR (check-expect (monto-persona 3 3) "Cada estudiante deberá abonar $1267.50") 
;VERIFICAR (check-expect (monto-persona 1 5) "El estudiante deberá abonar $2437.5")

(define CUOTA 650.0) ;Precio cuota en pesos

(define 2_PERSONAS 0.1) ;Descuento 2 personas
(define 3_PERSONAS 0.2) ;Descuento 3 personas o más

(define 2_MESES 0.15) ;Descuento 2 meses
(define 3_MESES 0.25) ;Descuento 3 meses o más

(define MAX_DESCUENTO 0.35)

(define (monto-persona cant-personas cant-meses)
  (if (< (descuento-promociones cant-personas cant-meses) MAX_DESCUENTO)
      (mensaje-monto cant-personas (* cant-meses CUOTA (- 1 (descuento-promociones cant-personas cant-meses))))
      (mensaje-monto cant-personas (* cant-meses CUOTA (- 1 MAX_DESCUENTO)))
      )
  )

;descuento: Number Number -> Number
;Toma una cantidad de personas y los meses que pagan, y nos devuelve cual es su descuento
;de promociones correspondiente.

(check-expect (descuento-promociones 1 1) 0)
(check-expect (descuento-promociones 2 1) 0.1)
(check-expect (descuento-promociones 2 2) 0.25)
(check-expect (descuento-promociones 4 5) 0.45)

(define (descuento-promociones cant-personas cant-meses)
  (+ (cond [(= cant-personas 1) 0]
           [(= cant-personas 2) 2_PERSONAS]
           [else 3_PERSONAS])
     (cond [(= cant-meses 1) 0]
           [(= cant-meses 2) 2_MESES]
           [else 3_MESES])
     )
  )

;El monto a pagar y la cantidad de personas lo representaremos con un número.
;La salida la reprensetaremos con un String, combinando el monto a pagar con una frase

;mensaje-monto: Number Number -> String
;Toma una cantidad de personas y cuanto debe pagar cada una, y nos devuelve el mensaje adecuado,
;sobre cuanto tiene queb pagar cada uno.

(check-expect (mensaje-monto 1 900) "El estudiante deberá abonar $900")
(check-expect (mensaje-monto 2 900) "Cada estudiante deberá abonar $900")

(define (mensaje-monto cant-personas monto)
  (if (= cant-personas 1)
      (string-append "El estudiante deberá abonar $"
                     (number->string monto))
      (string-append "Cada estudiante deberá abonar $"
                     (number->string monto))
      )
  )


;__________________________________________
;Ejercicio 10)-

;Representaremos la edad de la persona en meses con un Number.
;El resultado de su condición se espresará con un Boolean

;anemia: Number Number -> Boolean
;Recibe la edad en meses y el nivel de hemogoblina de una persona, y nos expresa
;mediante un Booleano, si esta persona es anémica o no.

(check-expect (anemia 1 12) #t) 
(check-expect (anemia 1 13) #f)
(check-expect (anemia 150 15) #f)
(check-expect (anemia 45 11) #t)

(define MIN_1MONTH 13)
(define MIN_6MONTHS 10)
(define MIN_1YEAR 11)
(define MIN_5YEARS 11.5)
(define MIN_10YEARS 12.6)
(define MIN_MORE_10YEARS 13)

;Sabemos que la equivalencia entre año y meses es 1 año = 12 meses.

(define (anemia edad hemoglobina)
  (cond [(<= edad 1) (if (< hemoglobina MIN_1MONTH) #t #f)]
        [(<= edad 6) (if (< hemoglobina MIN_6MONTHS) #t #f)]
        [(<= edad 12) (if (< hemoglobina MIN_1YEAR) #t #f)]
        [(<= edad 60) (if (< hemoglobina MIN_5YEARS) #t #f)]
        [(<= edad 120) (if (< hemoglobina MIN_10YEARS) #t #f)]
        [else (if (< hemoglobina MIN_MORE_10YEARS) #t #f)]
        )
  )

;__________________________________________
;Ejercicio 11)-

;Vamos a representar a cada uno de los números a,b,c con valores tipo Number.
;La salida, la representaremos con valores tipo Number.

;autopromediable: Number Number Number -> Number
;Dados tres números, devuelva el producto de ellos en caso que formen una terna
;autopromediable, y la suma de los mismos en caso contrario. Se dice que una terna es autopromediable
;si uno de sus valores coincide con el promedio de los otros dos.

(check-expect (autopromediable 7 5 9) 315)
(check-expect (autopromediable 5 7 9) 315)
(check-expect (autopromediable 7 5 8) 20)
(check-expect (autopromediable 8 5 9) 22)

(define (autopromediable a b c)
  (if (or (ver-prom a b c) (ver-prom c b a) (ver-prom b a c))
      (* a b c)
      (+ a b c)
      )
  )


;ver-prom: Number Number Number -> Boolean
;Dados tres números, devuelve True si el primero de ellos es promedio de los otros dos,
;o False en caso contrario

(check-expect (ver-prom 10 10 10) #t)
(check-expect (ver-prom 9 10 10) #f)
(check-expect (ver-prom 7 5 9) #t)

(define (ver-prom a b c)
  (= a (/ (+ b c) 2))
  )


;__________________________________________
;Ejercicio 12)-

;Representaremos la cantidad de litros restantes con un Number.
;Representaremos la clase de combustible con un String, el cual será "Grado 2" o "Grado 3".
;La autonomía restante en ciudad y ruta, la representaremos con un String.

;autonomia: Number String -> String
;Dada la cantidad de litros restantes, y el tipo de combustible que estamos utilizando,
;devuelve la autonomía en ciudad y ruta.

(check-expect (autonomia 20 "Grado 2") "Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect (autonomia 20 "Grado 3") "Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")
(check-expect (autonomia 20 "Grado 1") "Ingrese un tipo correcto de combustible.")


(define CITY_G2 8)  ;en km/l
(define ROAD_G2 11) ;en km/l

(define IMPROVE_G3 0.1) ;Mejora Grado 3

(define (autonomia litros combustible)
  (cond [(string=? combustible "Grado 2") (mensaje_autonomia (* litros CITY_G2)(* litros ROAD_G2))]
        [(string=? combustible "Grado 3") (mensaje_autonomia (* litros CITY_G2 (+ 1 IMPROVE_G3))
                                                 (* litros ROAD_G2 (+ 1 IMPROVE_G3)))]
        [else "Ingrese un tipo correcto de combustible."])
  )



;mensaje: Number Number -> String
;Dadas las autonomías en ciudad y ruta, nos devuelve un string unificándolas.

(check-expect (mensaje_autonomia 160 220) "Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect (mensaje_autonomia 176 242) "Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")

(define (mensaje_autonomia city road)
  (string-append "Autonomía en ciudad: " (number->string city)
                 "km. Autonomía en ruta: " (number->string road) "km.")
  )


