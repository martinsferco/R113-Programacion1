;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 9

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Los Estados los representaremos con Strings, y representarán las posibles combinaciones
;que tenemos de figuras con colores. Representaremos a los estados de la siguiente manera:
;"ca":Círculo azul
;"cv":Círculo verde
;"ta":Triángulo azul
;"tv":Triángulo verde

(define ANCHO-ESCENA 500)
(define ALTO-ESCENA 500)
(define COLOR-ESCENA "white")

(define TAMAÑO-CIRCULO 100)
(define TAMAÑO-TRIANGULO (* TAMAÑO-CIRCULO 2)) ;Ya que el círculo depende del radio


;generador-figuras: Estado -> Image
;Dado un estado, nos devuelve la figura correspondiente a dicho estado.

(define (generador-figuras estado)
  (cond [(string=? "ca" estado) (circle TAMAÑO-CIRCULO "solid" "lightblue")]
        [(string=? "cv" estado) (circle TAMAÑO-CIRCULO "solid" "lightgreen")]
        [(string=? "ta" estado) (triangle TAMAÑO-TRIANGULO "solid" "lightblue")]
        [(string=? "tv" estado) (triangle TAMAÑO-TRIANGULO "solid" "lightgreen")]
        )
  )


;muestra-figuras: Estado -> Image
;Recibe uno de los 4 estados, y nos devuelve la figura correspondiente para mostrar
;sobre la escena deseada.

(define (muestra-figuras estado)
  (place-image (generador-figuras estado)
               (random ANCHO-ESCENA) (random ALTO-ESCENA)
               (rectangle ANCHO-ESCENA ALTO-ESCENA "solid" COLOR-ESCENA))
  )


;modificador-figura: Estado String -> Estado
;Recibe el estado actual y la tecla presionada. Si la tecla es "t" el estado cambia a un triángulo
;del mismo color. Si la tecla es "c", cambia a un círculo del mismo color. En caso contrario, la
;figura mantiene su forma

(check-expect (modificador-figura "tv" "t") "tv")
(check-expect (modificador-figura "cv" "t") "tv")
(check-expect (modificador-figura "ta" "t") "ta")
(check-expect (modificador-figura "ca" "t") "ta")
(check-expect (modificador-figura "tv" "c") "cv")
(check-expect (modificador-figura "cv" "c") "cv")
(check-expect (modificador-figura "ta" "c") "ca")
(check-expect (modificador-figura "ca" "c") "ca")

(define (modificador-figura estado tecla)
  (cond [(key=? tecla "t") (if (string=? "v" (string-ith estado 1)) "tv" "ta")]
        [(key=? tecla "c") (if (string=? "v" (string-ith estado 1)) "cv" "ca")]
        [else estado]
        )
  )


;alternador-color: Estado -> Estado
;Por cada segundo, va alternado entre estados de color verde y azul, independientemente
;del tipo de figura.

(check-expect (alternador-color "tv") "ta")
(check-expect (alternador-color "cv") "ca")
(check-expect (alternador-color "ta") "tv")
(check-expect (alternador-color "ca") "cv")

(define (alternador-color estado)
  (cond [(string=? (string-ith estado 1) "v")
         (string-append (string-ith estado 0) "a")]
        [(string=? (string-ith estado 1) "a")
         (string-append (string-ith estado 0) "v")]
        )
  )


(define ESTADO-INICIAL "tv")

(big-bang ESTADO-INICIAL
  [to-draw muestra-figuras]
  [on-key modificador-figura]
  [on-tick alternador-color 1]
  )