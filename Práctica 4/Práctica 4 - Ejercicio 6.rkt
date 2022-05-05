;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 6

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado del sistema sera de tipo posn, y representa las coordenadas
;de la mosca dentro de la escena.

(define ALTO-ESCENA 1000)
(define ANCHO-ESCENA 1000)

(define ESCENA (empty-scene ANCHO-ESCENA ALTO-ESCENA))

(define TAM-MOSCA 20)
(define MOSCA (circle TAM-MOSCA "solid" "black"))

(define DELTA 10)

;mostrar-mosca: Estado -> Image
;Dado el estado actual, nos devuelve una imagen en donde se coloca la mosca
;sobre la escena en las coordenadas adecuadas.

(define (mostrar-mosca estado)
  (place-image MOSCA (posn-x estado)(posn-y estado) ESCENA)
  )


;


;coor-random: Number -> Estado
;Recibe una coordenadas, y suma o resta aleatoriamente DELTA unidades.

(define (coor-random coor)
  (if (= (random 2) 0) (- coor DELTA) (+ coor DELTA)) 
  )

;mover-mosca: Estado -> Estado
;Dado el estado actual, nos devuelve un nuevo estado sumando o restando
;delta unidades a ambas coordenadas.

(define (mover-mosca estado)
  (make-posn (coor-random (posn-x estado))(coor-random (posn-y estado)))
  )


(define ESTADO-INICIAL (make-posn (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2)))

(big-bang ESTADO-INICIAL
  [to-draw mostrar-mosca]
  [on-tick mover-mosca 0.0001]
  )