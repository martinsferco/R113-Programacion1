;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 2 y 3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 2 y 3

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado es un Number, que representa el tamaño del radio (en píxeles) del círculo representado
;en pantalla.

(define ALTURA-ESCENA 300)
(define ANCHO-ESCENA 300)

;clasificador-tamaño: Estado -> String
;Dada el estado actual del tamaño del radio, nos devuelve un String que representa un color
;correspondiente a su tamaño:
; Entre 0 y 50 - AMARILLO
; Entre 51 y 100 - ROJO
; Mayor a 100 - VERDE

(check-expect (clasificador-tamaño 25) "yellow")
(check-expect (clasificador-tamaño 56) "red")
(check-expect (clasificador-tamaño 150) "green")

(define (clasificador-tamaño tamaño)
  (cond [(<= tamaño 50) "yellow"]
        [(<= tamaño 100) "red"]
        [else "green"])
  )

;mostrar-figura: Estado -> Image
;Toma el estado actual del sistema y nos devuelve la imagen correspondiente,
;la cual será mostrada por la computadora.

(define (mostrar-figura tamaño)
  (overlay (circle tamaño "solid" (clasificador-tamaño tamaño))
           (empty-scene ALTURA-ESCENA ANCHO-ESCENA))
  )


;disminuir-tamaño: Estado -> Estado
;Dado el estado actual del tamaño del radio, nos devuelve el nuevo estado,
;el cual consta en disminuir en una unidad el estado anterior. Si el estado actual
;es 0, se vuelve al Estado Inicial.

(check-expect (disminuir-tamaño 50) 49)
(check-expect (disminuir-tamaño 0) RADIO-INICIAL)

(define (disminuir-tamaño tamaño)
  (if (= tamaño 0) RADIO-INICIAL (- tamaño 1)
  )
  )


;incrementar-tamaño: Estado -> Estado
;Dado el estado actual del tamaño del radio, nos devuelve el nuevo estado,
;el cual consta en aumentar en una unidad el estado anterior. Si el círculo resultante
;no cabe en la escena, volvemos a 0.

(check-expect (incrementar-tamaño 50) 51)
(check-expect (incrementar-tamaño (/ ANCHO-ESCENA 2)) 0)

(define (incrementar-tamaño tamaño)
  (if (= tamaño (/ ANCHO-ESCENA 2)) 0 (+ tamaño 1))
  )


;reajuste-diametro: Estado String -> Estado
;Dado el estado actual, si se presiona un dígito, devuelve el nuevo estado con el valor de
;10*d. Para cualquier otra tecla no ocurre nada.

(check-expect (reajuste-diametro 100 "2") 20)
(check-expect (reajuste-diametro 100 "a") 100)

(define (reajuste-diametro tamaño tecla)
  (if (string-numeric? tecla) 
      (* 10 (string->number tecla))
      tamaño
      )
  )

;finalización: Estado -> Boolean
;Dado el estado actual, devuelve #true si el valor es menor a 10 o mayor a 110. En caso contrario
;devuelve #false.

(check-expect (finalización 50) #f)
(check-expect (finalización 5) #t)
(check-expect (finalización 120) #t)

(define (finalización tamaño)
  (if (or (< tamaño 10) (> tamaño 110)) #t #f)
  )


(define RADIO-INICIAL 50)

(big-bang RADIO-INICIAL
          [to-draw mostrar-figura]
          [on-tick incrementar-tamaño]
          [on-key reajuste-diametro]
          ;[stop-when finalización]
  )