;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 5

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado es un String y representa  el color del círculo del
;centro de la escena. Los posibles valores de estado son:
;"yellow"
;"red"
;"green"
;"blue"

(define ALTO-ESCENA 500)
(define ANCHO-ESCENA 500)

(define RADIO 200)

;graficador-estado: Estado -> Image
;Recibe el Estado actual, y nos devuelve una imagen con un círculo
;de color correspondiente al estado actual.

(define (graficador-estado color)
  (overlay (circle RADIO "solid" color)
           (empty-scene ANCHO-ESCENA ALTO-ESCENA)
           )
  )

;modificador-color: Estado -> Estado
;Recibe el estado actual, y nos devuelve el estado siguiente de acuerdo
;a la siguiente secuencia: Yellow -> Red -> Green -> Blue -> Yellow -> ...

(check-expect (modificador-color "yellow") "red")
(check-expect (modificador-color "red") "green")
(check-expect (modificador-color "green") "blue")
(check-expect (modificador-color "blue") "yellow")

(define (modificador-color color)
  (cond [(string=? color "yellow") "red"]
        [(string=? color "red") "green"]
        [(string=? color "green") "blue"]
        [else "yellow"])
  )




(define ESTADO-INICIAL "yellow")

(big-bang ESTADO-INICIAL
  [to-draw graficador-estado]
  [on-tick modificador-color 1]
  )


