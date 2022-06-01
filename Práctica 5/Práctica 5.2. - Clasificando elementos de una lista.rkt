;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Práctica 5.2. - Clasificando elementos de una lista|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 5 - Primera parte - Clasificando elementos de una lista

;Martín Sferco
;Comisión 1 - Grupo 7
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

