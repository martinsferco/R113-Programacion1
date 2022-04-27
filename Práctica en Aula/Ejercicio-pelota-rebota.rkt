;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio-pelota-rebota) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;______________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA EN AULA - Ejercicio de pelota que rebota

;Martín Sferco
;Comisión 1 - Grupo 7
;______________________________________________

;CONSIGNA: Diseñar un programa que muestra una pelota que rebote
;a lo alto de una escena y que vaya modificando su forma de círculo
;a elipse
;______________________________________________

;Represenetaremos el Estado con un dato de tipo Number y representará
;la coordenada vertical de la figura a lo largo del tiempo


(define ANCHO-ESCENA 500)
(define ALTO-ESCENA 500)

(define ESCENA (empty-scene ANCHO-ESCENA ALTO-ESCENA))

(define RADIO 40) ;Radio elipse

(define POSY-MIN RADIO)
(define POSY-MAX (- ALTO-ESCENA RADIO))

(define DELTA 5)




(define ESTADO-INICIAL ...)

(big-bang ESTADO-INICIAL
  (to-draw interpretador-estado)
  )
