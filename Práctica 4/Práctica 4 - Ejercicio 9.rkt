;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 9

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Definimos una nueva estructura Auto
(define-struct Auto [modelo year combustible rendimiento])

;La estructura Auto es del tipo (String Number String Number)
;El primer campo nos dice el modelo del auto.
;El segundo campo nos dice cuando se fabrico el auto.
;El tercer campo nos dice que tipo de combustible utiliza.
;El cuarto campo nos dice su rendimiento optimo, en km/l.


(define PRECIO-PEAJES 50) ;Expresado en pesos.
(define DISTANCIA-PEAJES 100) ;Expresado en km.

(define PRECIO-NAFTA 19) ;Expresado en pesos.
(define PRECIO-DIESEL 17) ;Expresado en pesos.

(define AÑO-ACTUAL 2022)

;antiguedad: Auto -> Number
;Dado un Auto, nos devuelve su antiguedad, dependiendo del año
;actual en el que nos encontremos.

(check-expect (antiguedad (make-Auto "gol" 2022 "diesel" 13)) 0)
(check-expect (antiguedad (make-Auto "gol" 2000 "diesel" 13)) 22)
(check-expect (antiguedad (make-Auto "gol" 2015 "diesel" 13)) 7)

(define (antiguedad auto)
  (- AÑO-ACTUAL (Auto-year auto))
  )

;rendimiento: Auto -> Number
;Recibe un dato de tipo Auto y nos devuelve su rendimiento de acuerdo
;a su antiguedad.
;El rendimiento sera expresado con un numero entre 0 y 1, siendo 1
;el mayor rendimiento del auto:

;Entre 1 y 5 años, el rendimiento disminuye 2%.
;Entre 6 y 10 años, el rendimiento disminuye 6%.
;Entre 10 y 15 años, el rendimiento disminuye 10%.
;Más de 15 años, el rendimiento disminuye 15%

(check-expect (rendimiento-dism (make-Auto "gol" 2022 "diesel" 13)) 1)
(check-expect (rendimiento-dism (make-Auto "gol" 2020 "diesel" 13)) 0.98)
(check-expect (rendimiento-dism (make-Auto "gol" 2014 "diesel" 13)) 0.94)
(check-expect (rendimiento-dism (make-Auto "gol" 2011 "diesel" 13)) 0.90)
(check-expect (rendimiento-dism (make-Auto "gol" 2000 "diesel" 13)) 0.85)

(define (rendimiento-dism auto)
  (cond [(= (antiguedad auto) 0) 1]
        [(<= (antiguedad auto) 5) 0.98]
        [(<= (antiguedad auto) 10) 0.94]
        [(<= (antiguedad auto) 15) 0.90]
        [else 0.85])
  )

;rendimiento-real: Auto -> Number
;Dado un Auto, nos devuelve el rendimiento real, dependiendo de su antiguedad
;y su rendimiento optimo.

(check-expect (rendimiento-real (make-Auto "gol" 2022 "diesel" 10)) 10)
(check-expect (rendimiento-real (make-Auto "gol" 2020 "diesel" 10)) 9.8)
(check-expect (rendimiento-real (make-Auto "gol" 2014 "diesel" 10)) 9.4)
(check-expect (rendimiento-real (make-Auto "gol" 2011 "diesel" 10)) 9)
(check-expect (rendimiento-real (make-Auto "gol" 2000 "diesel" 10)) 8.5)

(define (rendimiento-real auto)
  (* (Auto-rendimiento auto) (rendimiento-dism auto))
  )

;cantidad-peajes: Number -> Number
;Dada la cantidad de km que se tiene que recorrer, nos dice
;cuanto tendremos que pagar de peajes

(check-expect (cantidad-peajes 50) 0)
(check-expect (cantidad-peajes 150) 1)
(check-expect (cantidad-peajes 100) 1)
(check-expect (cantidad-peajes 200) 2)

(define (cantidad-peajes distancia)
  (floor (/ distancia DISTANCIA-PEAJES))
  )

;precio-peajes: Number -> Number
;Dada la distancia recorrida, nos devuelve el precio a pagar debido a peajes.

(check-expect (precio-peajes 50) 0)
(check-expect (precio-peajes 150) PRECIO-PEAJES)
(check-expect (precio-peajes 100) PRECIO-PEAJES)
(check-expect (precio-peajes 200) (* 2 PRECIO-PEAJES))

(define (precio-peajes distancia)
  (* PRECIO-PEAJES (cantidad-peajes distancia))
  )

;precio-combustible: Auto -> Number
;Dado un auto, nos devuelve cual es el precio del combustible que utiliza.

(check-expect (precio-combustible (make-Auto "gol" 2002 "diesel" 14)) PRECIO-DIESEL)
(check-expect (precio-combustible (make-Auto "gol" 2002 "nafta" 14)) PRECIO-NAFTA)

(define (precio-combustible auto)
  (if (string=? (Auto-combustible auto) "diesel") 17 19))

;costo-viaje: Auto Number -> Number
;Dado un Auto y la distancia que se quiere recorrer, nos devuelve
;el costo total del viaje.

(check-within (costo-viaje (make-Auto "gol" 2018 "nafta" 13) 450) 871.11 0.1)

(define (costo-viaje auto distancia)
  (+ (precio-peajes distancia) (* (precio-combustible auto) (/ distancia (rendimiento-real auto))))
  )



