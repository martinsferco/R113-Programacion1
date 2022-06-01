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
               [(cons? l) (if (string=? (first l) "Marcos")
                              #t
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

(define (contiene? l o)
  (cond [(empty? l) #f]
        [(cons? l) (if (string=? (first l) o)
                       #t
                       (contiene? (rest l) o))])
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