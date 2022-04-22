;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 1

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Estado es un String que representa el color del círculo a dibujar en pantalla.
;Puede tomar los siguientes valores:
; "blue"
; "red"
; "green"
; "black"


(define ALTURA-ESCENA 500)
(define ANCHO-ESCENA 500)

(define TAMAÑO-CIRCULO 200)



;muestra-circulo: Estado -> Image
;Dado un estado, nos devuelve la imagen correspondiente a mostrar por el programa.

(define (muestra-circulo estado)
       (overlay (circle TAMAÑO-CIRCULO "solid" estado)
                (empty-scene ALTURA-ESCENA ANCHO-ESCENA)
                )
  )


;color-tecla: Estado String -> Estado
;Recibe el estado actual y la tecla presionada, y nos devuelve el nuevo estado correspondiente a
;la tecla que se presionó.

(check-expect (color-tecla "blue" "a") "blue")
(check-expect (color-tecla "green" "a") "blue")
(check-expect (color-tecla "red" "a") "blue")
(check-expect (color-tecla "blue" "r") "red")
(check-expect (color-tecla "green" "r") "red")
(check-expect (color-tecla "red" "r") "red")
(check-expect (color-tecla "blue" "v") "green")
(check-expect (color-tecla "green" "v") "green")
(check-expect (color-tecla "red" "v") "green")
(check-expect (color-tecla "blue" "n") "black")
(check-expect (color-tecla "green" "n") "black")
(check-expect (color-tecla "red" "n") "black")
(check-expect (color-tecla "blue" " ") ESTADO-INICIAL)
(check-expect (color-tecla "green" " ") ESTADO-INICIAL)
(check-expect (color-tecla "red" " ") ESTADO-INICIAL)


(define (color-tecla estado tecla)
        (cond [(string=? tecla "a") "blue"]
              [(string=? tecla "r") "red"]
              [(string=? tecla "v") "green"]
              [(string=? tecla "n") "black"]
              [(string=? tecla " ") ESTADO-INICIAL]
              [else estado])
  )

(define ESTADO-INICIAL "blue")

(big-bang ESTADO-INICIAL
         [to-draw muestra-circulo]
         [on-key color-tecla]
  )