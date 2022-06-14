;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Práctica 5.2. - Patrones sobre listas|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 5 - Segunda parte - Patrones sobre listas

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;CLASIFICANDO ELEMENTOS DE UNA LISTA
;__________________________________________
;Ejercicio 1)

;pares: List(Number) -> List(Number)
;Dada una lista de numeros,nos devuelve una nueva lista con los
;numeros pares.

(check-expect (pares empty) empty)
(check-expect (pares (list 1 3 5)) empty)
(check-expect (pares (list 1 23 4 5 6)) (list 4 6))
(check-expect (pares (list 4 6 8)) (list 4 6 8))

(define (pares l) (filter even? l))

;__________________________________________
;Ejercicio 2)

;menor5: String -> Boolean
;Dado un String,nos determina si este tiene una longitud menor a 5.

(check-expect (menor5 "hola!") #false)
(check-expect (menor5 "hola") #true)
(check-expect (menor5 "Buenos días") #false)

(define (menor5 s) (< (string-length s) 5))

;cortas: List(String) -> List(String)
;Dada una lista de Strings, nos devuelve una lista con las palabras
;que tengan una longitud menor a 5.

(check-expect (cortas (list "hola" "mundo")) (list "hola"))
(check-expect (cortas (list "hola" "!")) (list "hola" "!"))
(check-expect (cortas (list "Martin" "Sferco")) empty)

(define (cortas l) (filter menor5 l))

;__________________________________________
;Ejercicio 3)

(define MAX 5)

;distanciaLim: posn -> Boolean
;Dado un posn, nos determina si se encuentra a menos de MAX
;unidades del origen.

(check-expect (distanciaLim (make-posn 0 1)) #true)
(check-expect (distanciaLim (make-posn 3 4)) #false)

(define (distanciaLim p)
  (< (sqrt (+ (sqr (posn-x p))
              (sqr (posn-y p))))
     MAX)
  )

;cerca: List(posn) -> List(Posn)
;Dada una lista de puntos, nos devuelve una lista de aquellos
;que se encuentran a menos de MAX unidades del origen.

(check-expect (cerca (list (make-posn 0 1) (make-posn 3 4))) (list (make-posn 0 1)))
(check-expect (cerca empty) empty)
(check-expect (cerca (list (make-posn 3 4) (make-posn 5 6)))empty)

(define (cerca l) (filter distanciaLim l))

;__________________________________________
;Ejercicio 4)

;positivos: List(Number) -> List(Number)
;Dada una lista de números, nos devuelve una lista con aquellos
;que sean mayores a 0.

(check-expect (positivos (list 0 1 2 4)) (list 1 2 4))
(check-expect (positivos (list -1 -2 0)) empty)
(check-expect (positivos empty) empty)

(define (positivos l) (filter positive? l))

;__________________________________________
;Ejercicio 5)

;eliminar: List(Number) Number -> List(Number)
;Dada una lista de numeros, nos devuelve una lista eliminando las
;recurrencias del numero indicado en el segundo argumento.

(check-expect (eliminar (list 1 2 3 2 7 6) 2) (list 1 3 7 6))
(check-expect (eliminar (list 1 1 1 1) 1) empty)

(define (eliminar l n)
  (filter (lambda (x) (not (= x n))) l))

;__________________________________________
;APLICANDO TRANSFORMACIONES A CADA ELEMENTO
;DE UNA LISTA
;__________________________________________
;Ejercicio 6)

;raices: List(Number) -> List(Number)
;Dada una lista de numeros, nos determina las raices de aquellos
;numeros que sean mayores o iguales a 0.

(check-expect (raices (list 1 4 9)) (list 1 2 3))
(check-expect (raices (list -2 -3)) empty)
(check-expect (raices (list -1 0 1)) (list 0 1))

(define (raices l)
  (map sqrt (filter (lambda (x) (>= x 0)) l))
  )

;__________________________________________
;Ejercicio 7)

;distancia: posn -> Number
;Dado un punto del plano, nos devuelve su distancia al origen.

(check-expect (distancia (make-posn 3 4)) 5)
(check-expect (distancia (make-posn 1 0)) 1)
(check-expect (distancia (make-posn 0 0)) 0)

(define (distancia p)
  (sqrt (+ (sqr (posn-x p))
           (sqr (posn-y p))))
  )

;distancias: List(posn) -> List(Number)
;Dada una lista de puntos, nos devuelve una lista con la distancia
;al origen de cada uno.

(check-expect (distancias (list (make-posn 3 4) (make-posn 1 0))) (list 5 1))
(check-expect (distancias empty) empty)

(define (distancias l) (map distancia l))

;__________________________________________
;Ejercicio 8)

;anchos: List(Image) -> List(Number)
;Dada una lista de imágenes, nos devuelve una lista con el ancho de
;cada una de ellas.

(check-expect (anchos (list (square 10 "solid" "blue") (square 30 "solid" "blue"))) (list 10 30))
(check-expect (anchos empty) empty)

(define (anchos l) (map image-width l))

;__________________________________________
;Ejercicio 9)

;sgn2: Number -> Number
;Dado un numeros nos devuelve 1, 0 o -1 si es positivo, cero o negativo
;respectivamente.

(check-expect (sgn2 3) 1)
(check-expect (sgn2 -4) -1)
(check-expect (sgn2 0) 0)

(define (sgn2 n)
  (cond [(> n 0) 1]
        [(= n 0) 0]
        [else -1])
  )

;signos: List(Number)-> List(Number)
;Dada una lista de numeros, nos devuelve una lista que resulta de aplicar
;la función sgn2 a cada uno de los elementos.

(define (signos l) (map sgn2 l))

;__________________________________________
;Ejercicio 10)

;cuadrados: List(Number) -> List(Number)
;Dada una lista de numeros, nos devuelve una lista con el cuadrado de
;cada uno de los elementos de la lista.

(check-expect (cuadrados (list 1 2 3)) (list 1 4 9))
(check-expect (cuadrados empty) empty)
(check-expect (cuadrados (list -1 1)) (list 1 1))

(define (cuadrados l) (map sqr l))

;__________________________________________
;Ejercicio 11)

;longitudes: List(String) -> List(Number)
;Dada una lista de Strings, nos devuelve una lista con las longitudes
;de cada uno de los String.

(check-expect (longitudes (list "hola" "como" "estas?"))(list 4 4 6))
(check-expect (longitudes empty) empty)

(define (longitudes l) (map string-length l))

;__________________________________________
;Ejercicio 12)

;Como datos, utilizaremos una lista del tipo listTemperaturas.
;listTemperaturas es:
;'()
;(cons Number listTemperaturas)

;convertirT: Number -> Number
;Dada una temperatura en Fahrenheit, nos devuelve su equivalente en Celsius

(check-within (convertirT 32) 0 0.1)
(check-within (convertirT 100) 37.7 0.1)

(define (convertirT t) (* (- t 32) 5/9))

;convertirFC: listTemperaturas -> listTemperaturas
;Dada una lista de temperaturas en Fahrenheit, nos devuelve una lista
;con las conversiones de la misma en grados Celcius.

(check-within (convertirFC (list 32 100)) (list 0 37.7) 0.10)
(check-expect (convertirFC empty) empty)

(define (convertirFC l) (map convertirT l))

;__________________________________________
;OPERANDO CON LOS ELEMENTOS DE UNA LISTA
;__________________________________________
;Ejercicio 13)

;prod: List(Number) -> Number
;Dada una lista de numeros, nos devuelve el producto entre sus elementos.

(check-expect (prod empty) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)
(check-expect (prod (list -1 2)) -2)

(define (prod l) (foldr * 1 l))

;__________________________________________
;Ejercicio 14)

;pegar: List(String) -> String
;Dada una lista de strings, nos devuelve un único string que resulta de
;concatenar todos los elementos de la lista.

(check-expect (pegar empty) "")
(check-expect (pegar (list "Hola " "mundo!"))"Hola mundo!")

(define (pegar l) (foldr string-append "" l))

;__________________________________________
;Ejercicio 15)

;maximo: List(Number) -> List(Number)
;Dada una lista de naturales, nos devuelve el mayor de ellos.

(check-expect (maximo (list 1 2 3 4)) 4)
(check-expect (maximo empty) 0)

(define (maximo l) (foldr max 0 l))

;__________________________________________
;Ejercicio 16)

;sumdist: List(posn) -> Number
;Dada una lista de puntos del plano, nos devuelve la suma de sus distancias
;al origen.

(check-expect (sumdist (list (make-posn 3 4) (make-posn 0 1))) 6)
(check-expect (sumdist empty) 0)
(check-expect (sumdist (list (make-posn 0 2))) 2)

(define (sumdist l)
  (foldr + 0 (map distancia l)))

;__________________________________________
;Ejercicio 17)

;sumcuad: List(Number) -> Number
;Dada una lista de numeros, nos devuelve la suma de sus cuadrados.

(check-expect (sumcuad empty) 0)
(check-expect (sumcuad (list 1 2 3)) 14)
(check-expect (sumcuad (list -1 -2)) 5)

(define (sumcuad l)
  (foldr + 0 (map sqr l)))





