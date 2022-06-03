;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Práctica 5.1. - Listas|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 5 - Primera parte - Listas

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;Ejercicio 1)

(define lista5contactos (cons "Pacusse Lautaro"
                              (cons "Zinny Rosendo"
                                    (cons "Sarmiento Juan Cruz"
                                          (cons "Carrillo Fausto"
                                                (cons "Gallo Paula" '()))))))
;__________________________________________
;Ejercicio 2)

;(cons "1" (cons "2" '())) es un ejemplo de una lista de Contactos porque
;los valores de la lista siempre son o un String o una lista vacía.

;Por otro lado,  (cons 2 '()) no es una lista de contactos ya que el primer elemento
;de dicha lista es un número, lo cual no corresponde a la definición de Contactos

;__________________________________________
;Ejercicio 3)

;Definiremos una lista de valores booleanos.
;Booleanos es:
;'().
;(cons Boolean Booleanos) 

;__________________________________________
;Ejercicio 4)

;Definiremos una lista de Contactos de la siguiente manera:
;Contactos es:
;'().
;(cons nombre-de-contacto Contactos) donde nombre-de-contacto es un String.

;contiene-Marcos?: Contactos -> Boolean
;Dada una lista de Contactos, determina si "Marcos" es un elemento de la lista

(check-expect (contiene-Marcos? '()) #false)
(check-expect (contiene-Marcos? (cons "Sara" (cons "Pedro"  (cons "Esteban" '())))) #false)
(check-expect (contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '())))) #true)
(check-expect (contiene-Marcos? (cons "Juan" '())) #false)
(check-expect (contiene-Marcos? (cons "Marcos" '())) #true)

(define (contiene-Marcos? l)
         (cond [(empty? l) #f]
               [(cons? l) (or (string=? "Marcos" (first l))
                              (contiene-Marcos? (rest l)))]
               ))

;__________________________________________
;Ejercicio 5)

;Definiremos una lista de String:
;Strings es:
;'()
;(cons String Strings)

;contiene?: Strings String -> Boolean
;Dada una lista de Strings y un String objetivo, determina si el objetivo se encuentra
;dentro de dicha lista.

(check-expect (contiene? '() "a") #false)
(check-expect (contiene? (cons "A" (cons "Marcos" (cons "C" '()))) "C") #true)
(check-expect (contiene? (cons "Juan" '()) "Marcos") #false)
(check-expect (contiene? (cons "Marcos" '()) "Marcos") #true)

(define (contiene? l objetivo)
  (cond [(empty? l) #f]
        [(cons? l) (or (string=? objetivo (first l))
                       (contiene? (rest l) objetivo))])
  )

;__________________________________________
;Ejercicio 7)

;Definiremos una lista de montos de dinero de la siguiente manera:
;Lista-de-montos es:
;'()
;(cons NumeroPositivo Lista-de-montos) siendo NumeroPositivo un Number

;suma: Lista-de-montos -> Number
;Dada una Lista-de-montos, nos devuelve la suma de los montos presentes en la lista.
;Consideramos que una lista vacía, tiene un monto de 0.

(check-expect (suma (cons 100 (cons 200 (cons 300 '())))) 600)
(check-expect (suma '()) 0)

(define (suma l)
  (cond [(empty? l) 0]
        [(cons? l) (+ (first l) (suma (rest l)))]))


;__________________________________________
;Ejercicio 8)

;Una Lista-de-numeros es:
;'()
;(cons Number Lista-de-numeros)

;pos?: Lista-de-numeros -> Boolean
;Dada una Lista-de-numeros, determina si todos sus elementos son positivos.
;Anteriormente consideramos que una lista vacía podia ser una Lista-de-montos.

(check-expect (pos? (cons 100 (cons 200 (cons 300 '())))) #t)
(check-expect (pos? '()) #t)
(check-expect (pos? (cons 100 (cons 200 (cons -300 '())))) #f)

(define (pos? l)
  (cond  [(empty? l) #t]
         [(cons? l) (and (>= (first l) 0) (pos? (rest l)))]))

;checked-suma: Lista-de-numeros -> Number
;Dada una Lista-de-numeros, nos devuelve la suma de sus elementos si esta es
;una Lista-de-montos. En caso contrario nos devuelve un error.

(check-expect (checked-suma (cons 100 (cons 200 (cons 300 '())))) 600)
(check-expect (checked-suma '()) 0)
(check-error (checked-suma (cons 100 (cons 200 (cons -300 '()))))
             "checked-suma: expects Lista-de-montos")

(define (checked-suma l)
  (if (pos? l)
      (suma l)
      (error "checked-suma: expects Lista-de-montos"))
  )

;__________________________________________
;Ejercicio 9) - Preguntar sobre lista vacía

;Definiremos una lista de valores booleanos.
;Booleanos es:
;'().
;(cons Boolean Booleanos)

;todos-verdaderos: Booleanos -> Boolean
;Dada una lista de Booleanos, determina si todos los elementos de la lista son #t.

(check-expect (todos-verdaderos (cons #t (cons #t (cons #t '())))) #t)
(check-expect (todos-verdaderos (cons #t (cons #t (cons #f '())))) #f)
(check-expect (todos-verdaderos (cons #t (cons #f (cons #f '())))) #f)
(check-expect (todos-verdaderos '()) #t)

(define (todos-verdaderos l)
  (cond [(empty? l) #t]
        [(cons? l) (and (first l) (todos-verdaderos (rest l)))]))


;uno-verdadero: Booleanos -> Boolean
;Dada una lista de Booleanos, determina si todos los elementos de la lista son #t.

(check-expect (uno-verdadero (cons #t (cons #t (cons #t '())))) #t)
(check-expect (uno-verdadero (cons #t (cons #t (cons #f '())))) #t)
(check-expect (uno-verdadero (cons #t (cons #f (cons #f '())))) #t)
(check-expect (uno-verdadero (cons #f (cons #f (cons #f '())))) #f)
(check-expect (uno-verdadero '()) #f)

(define (uno-verdadero l)
  (cond [(empty? l) #f]
        [(cons? l) (or (first l) (uno-verdadero (rest l)))]))

;__________________________________________
;Ejercicio 10)

;cant-elementos: Lista -> Number
;Dada una Lista cualquiera, nos devuelve la cantidad de elementos que tiene.

(check-expect (cant-elementos (cons 1 (cons 2 (cons 3 '())))) 3)
(check-expect (cant-elementos (cons 1 (cons 2 (cons 3 (cons 4 '()))))) 4)
(check-expect (cant-elementos '()) 0)

(define (cant-elementos l)
  (cond [(empty? l) 0]
        [(cons? l) (+ 1 (cant-elementos (rest l)))]))


;__________________________________________
;Ejercicio 11)

;promedio: Lista-de-numeros -> Number
;Dada una Lista-de-numeros, nos devuelve el promedio de ellos.

(check-expect (promedio (cons 1 (cons 2 (cons 3 '())))) 2)
(check-expect (promedio (cons 2 (cons 2 '()))) 2)
(check-expect (promedio '()) 0)

(define (promedio l)
  (cond [(empty? l) 0]
        [(cons? l) (/ (suma l) (cant-elementos l))])
  )

;__________________________________________
;Ejercicio 12)

;pares: List(Number) -> List(Number)
;Dada una Lista-de-numeros, nos devuelve una nueva List(Number) con los elementos
;que sean pares.

(check-expect (pares (list 4 6 3 7 5 0)) (list 4 6 0))
(check-expect (pares empty) empty)
(check-expect (pares (list 1 3 5)) empty)

(define (pares l)
  (cond [(empty? l) empty]
        [(cons? l) (if (= 0 (modulo (first l) 2))
                       (cons (first l) (pares (rest l)))
                       (pares (rest l)))]
        )
  )

;__________________________________________
;Ejercicio 13)

;cortas: List(String) -> List(String)
;Dada una List(String), nos devuelve una nueva List(String) con todas
;las palabras que tengan una longitud menor a 6.

(check-expect (cortas (list "Lista" "de" "palabras" "sin" "sentido")) (list "de" "sin"))
(check-expect (cortas empty) empty)
(check-expect (cortas (list "HOLLA" "HOLAA")) empty)

(define (cortas l)
  (cond [(empty? l) empty]
        [(cons? l) (if (< (string-length (first l)) 5)
                   (cons (first l) (cortas (rest l)))
                   (cortas (rest l)))])
  )

;__________________________________________
;Ejercicio 14)

;mayores: List(Number) Number -> List(Number)
;Dada una Lista-de-numeros y un N, nos devuelve una nueva Lista-de-numeros formada
;por los elementos que sean mayores a N.

(check-expect (mayores (list 1 2 3 4) 5) empty)
(check-expect (mayores (list 1 2 3 4) 2) (list 3 4))
(check-expect (mayores empty 3) empty)
(check-expect (mayores (list 1 2 3 4) 0) (list 1 2 3 4))

(define (mayores l n)
  (cond [(empty? l) empty]
        [(cons? l) (if (> (first l) n)
                       (cons (first l) (mayores (rest l) n))
                       (mayores (rest l) n))])
  )

;__________________________________________
;Ejercicio 15)

;Los datos que le pasaremos serán del tipo List(posn)

;distancia: posn -> Number
;Dado un punto del espacio nos devuelve su distancia al origen.

(check-expect (distancia (make-posn 4 3)) 5)
(check-expect (distancia (make-posn 4 0)) 4)
(check-expect (distancia (make-posn 0 0)) 0)

(define (distancia p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p))))
  )

;cerca: List(posn) Number -> List(posn)
;Dada una lista con puntos del espacio y una distancia máxima, nos devuelve una lista
;con los puntos cuya distancia al origen sea menor a la distancia máxima.

(check-expect (cerca empty 5) empty)
(check-expect (cerca (list (make-posn 3 4) (make-posn 10 10)) 5) empty)
(check-expect (cerca (list (make-posn 1 1) (make-posn 10 10)) 5) (list (make-posn 1 1)))
(check-expect (cerca (list (make-posn 1 1) (make-posn 10 10)) 0) empty)

(define (cerca l max)
  (cond [(empty? l) empty]
        [(cons? l) (if (< (distancia (first l)) max)
                       (cons (first l) (cerca (rest l) max))
                       (cerca (rest l) max))]
        )
  )

;__________________________________________
;Ejercicio 16)

;positivos: List(Number) -> List(Number)
;Dada una lista de números, nos devuelve una nueva lista con aquellos elementos
;que sean mayores a 0.

(check-expect (positivos (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (positivos (list 1 2 3 -54)) (list 1 2 3))
(check-expect (positivos (list -2 -3 -4)) empty)
(check-expect (positivos empty) empty)

(define (positivos l)
  (cond [(empty? l) empty]
        [(cons? l) (if (> (first l) 0)
                       (cons (first l) (positivos (rest l)))
                       (positivos (rest l)))]))


;__________________________________________
;Ejercicio 17)

;eliminar: List(Number) Number -> List(Number)
;Dada una lista de números y un números, nos devuelve una nueva lista en la cual
;no aparece ninguna el vez el número que le pasamos.

(check-expect (eliminar (list 1 2 3 4) 5) (list 1 2 3 4))
(check-expect (eliminar (list 1 2 3 4) 4) (list 1 2 3))
(check-expect (eliminar (list 1 4 3 4) 4) (list 1 3))
(check-expect (eliminar (list 5 5 5 5) 5) empty)
(check-expect (eliminar empty 5) empty)

(define (eliminar l n)
  (cond [(empty? l) empty]
        [(cons? l) (if (not (= n (first l)))
                       (cons (first l) (eliminar (rest l) n))
                       (eliminar (rest l) n))]))

;__________________________________________
;Ejercicio 18)

;raices: List(Number) -> List(Number)
;Dada una lista de numeros, nos devuelve una nueva lista con todas las raices
;cuadradas de los elementos de la primer lista.
;Aquellos los numeros que sean menores a 0, no le calcularemos su raiz.

(check-expect (raices (list 1 4 9 16)) (list 1 2 3 4))
(check-expect (raices empty) empty)
(check-expect (raices (list -1 -2 0 4 9 -5)) (list 0 2 3))

(define (raices l)
  (cond [(empty? l) empty]
        [(cons? l) (if (>= (first l) 0)
                       (cons (sqrt (first l)) (raices (rest l)))
                       (raices (rest l)))]
        )
  )

;__________________________________________
;Ejercicio 19)

;A la lista de puntos en el plano, la representamos con una lista
;cuyos elementos son datos de tipo posn.

;distancias: List(posn) -> List(Number)
;Dada una lista de puntos en el espacio, nos determina la distancia
;al origen de cada uno de los puntos.

(check-expect (distancias (list (make-posn 3 4) (make-posn 10 0))) (list 5 10))
(check-expect (distancias empty) empty)

(define (distancias lp)
  (cond [(empty? lp) empty]
        [(cons? lp) (cons (distancia (first lp)) (distancias (rest lp)))])
  )

;__________________________________________
;Ejercicio 20)

;anchos: List(Image) -> List(Number)
;Dada una lista de imagenes, nos devuelve una nueva lista con los anchos
;de las mismas.

(check-expect (anchos empty) empty)
(check-expect (anchos (list (circle 30 "solid" "red") (rectangle 10 30 "outline" "blue"))) (list 60 10))

(define (anchos li)
  (cond [(empty? li) empty]
        [(cons? li) (cons (image-width (first li)) (anchos (rest li)))])
  )

;__________________________________________
;Ejercicio 21)

;signo: Number -> Number
;Toma un numero como argumento y devuelve 0 si es 0, 1 si es positivo
;y -1 si es negativo

(check-expect (signo 0) 0)
(check-expect (signo 4) 1)
(check-expect (signo -4) -1)

(define (signo x)
        (cond [(< x 0) -1]
              [(= x 0) 0]
              [(> x 0) 1]
              )
  )

;signos: List(Number) -> List(Number)
;Dada una lista de numeros nos devuelve una lista con los signos de
;cada uno de los elementos, de acuerdo a la funcion signo.

(check-expect (signos (list 0 1 2 0 -4 -4 2)) (list 0 1 1 0 -1 -1 1))
(check-expect (signos empty) empty)
(check-expect (signos (list -2 -1 0 0 1 2)) (list -1 -1 0 0 1 1))

(define (signos l)
  (cond [(empty? l) empty]
        [(cons? l) (cons (signo (first l)) (signos (rest l)))])
  )

;__________________________________________
;Ejercicio 22)

;cuadrados: List(Number) -> List(Number)
;Dada una lista de numeros, nos devuelve una lista con los cuadrados
;de los elementos de la primera lista.

(check-expect (cuadrados (list 1 2 3 4)) (list 1 4 9 16))
(check-expect (cuadrados empty) empty)
(check-expect (cuadrados (list -2 -1 0 1 2)) (list 4 1 0 1 4))

(define (cuadrados l)
  (cond [(empty? l) empty]
        [(cons? l) (cons (sqr (first l)) (cuadrados (rest l)))])
  )

;__________________________________________
;Ejercicio 23)

;longitudes: List(String) -> List(Number)
;Dada una lista de strings, nos devuelve una lista con las longitudes
;de los mismos.

(check-expect (longitudes (list "hola" "" "Martin")) (list 4 0 6))
(check-expect (longitudes empty) empty)

(define (longitudes l)
  (cond [(empty? l) empty]
        [(cons? l) (cons (string-length (first l)) (longitudes (rest l)))])
  )

;__________________________________________
;Ejercicio 24)

;Como datos, utilizaremos una lista del tipo listTemperaturas.
;listTemperaturas es:
;'()
;(cons Number listTemperaturas)

;convertirFC: listTemperaturas -> listTemperaturas
;Dada una lista de temperaturas en Fahrenheit, nos devuelve una lista
;con las conversiones de la misma en grados Celcius.

(check-within (convertirFC (list 32 100)) (list 0 37.7) 0.10)
(check-expect (convertirFC empty) empty)

(define (convertirFC l)
  (cond [(empty? l) empty]
        [(cons? l) (cons (* (- (first l) 32) 5/9) (convertirFC (rest l)))])
  )

;__________________________________________
;Ejercicio 25)

;prod: List(Number) -> Number
;Dada una lista de numeros, nos devuelve el producto entre ellos.

(check-expect (prod empty) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)
(check-expect (prod (list 1 -2 3)) -6)

(define (prod l)
  (cond [(empty? l) 1]
        [(cons? l) (* (first l) (prod (rest l)))]
        )
  )

;__________________________________________
;Ejercicio 26)

;pegar: List(String) -> String
;Dada una lista de Strings, nos devuelve un unico string que resulta
;de concatenar todos los Strings de la lista dada.

(check-expect (pegar (list "Las " "listas " "son " "faciles.")) "Las listas son faciles.")
(check-expect (pegar empty) "")
(check-expect (pegar (list "Hello World!")) "Hello World!")

(define (pegar l)
  (cond [(empty? l) ""]
        [(cons? l) (string-append (first l) (pegar (rest l)))])
  )

;__________________________________________
;Ejercicio 27)

;maximo: List(Number) -> Number
;Dada una lista de numeros naturales (positivos), nos devuelve el
;maximo de ellos.

(check-expect (maximo (list 1 2 3)) 3)
(check-expect (maximo empty) 0)
(check-expect (maximo (list 3 2 1)) 3)

(define (maximo l)
  (cond [(empty? l) 0]
        [(cons? l) (max (first l) (maximo (rest l)))])
  )

;__________________________________________
;Ejercicio 28)

;sumdist: List(posn) -> Number
;Dada una lista de puntos, nos devuelve la suma de sus distancia
;al origen.

(check-expect (sumdist (list (make-posn 3 4) (make-posn 5 0))) 10)
(check-expect (sumdist empty) 0)
(check-expect (sumdist (list (make-posn 0 0))) 0)

(define (sumdist l)
  (cond [(empty? l) 0]
        [(cons? l) (+ (distancia (first l)) (sumdist (rest l)))])
  )

;__________________________________________
;Ejercicio 29)

;sumcuad: List(Number) -> Number
;Dada una lista de numeros, nos devuelve la suma de sus elementos al
;cuadrado.

(check-expect (sumcuad (list 3 4)) 25)
(check-expect (sumcuad empty) 0)
(check-expect (sumcuad (list 1 2 3)) 14)

(define (sumcuad l)
  (cond [(empty? l) 0]
        [(cons? l) (+ (sqr (first l)) (sumcuad (rest l)))])
  )