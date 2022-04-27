;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 4

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado es un Number, que representa la coordenada vertical sobre la que se dibujará
;un círculo.

(define ALTURA-ESCENA 1000)
(define ANCHO-ESCENA 300)

(define RADIO 50)
(define COLOR "lightgreen")

(define DELTA 60)

;grafica-circulo: Estado -> Image
;Toma el Estado actual del sistema, y dibuja un círculo centrado horizontalmente,
;y su posición vertical está definida por el Estado.

(define (grafica-circulo altura)
  (place-image (circle RADIO "solid" COLOR)
               (/ ANCHO-ESCENA 2) altura
               (empty-scene ANCHO-ESCENA ALTURA-ESCENA)
               )
  )

;modificador-posición: Estado String -> Estado
;Toma el estado actual y le suma delta unidades si se presiona la flecha de arriba, y en caso contrario,
;si se presiona la flecha de abajo, le resta delta unidades. Nunca permite que la figura se salga
;de la escena.


(define (modificador-posición altura tecla)      
      (cond [(string=? tecla "up") (if (>= (- altura RADIO) DELTA)
                                       (- altura DELTA) altura)]
            
            [(string=? tecla "down") (if (>= (- ALTURA-ESCENA altura RADIO) DELTA)
                                     (+ altura DELTA) altura)]
            [(string=? tecla " ") ESTADO-INICIAL]
            [else altura]
        )
  )


;controlador-mouse: Estado Number Number String -> Estado
;Dado el estado actual, nos devuelve un nuevo estado en donde la altura corresponde a la coordenada vertical
;del Mouse al hacer click en un lugar de la escena. Nunca permite que la figura se salga de la escena.


(define (controlador-mouse altura x y event)
  (cond [(string=? event "button-down")
         (if (and (>= y RADIO)(<= y (- ALTURA-ESCENA RADIO)))
             y
             (if (<= y (/ ALTURA-ESCENA 2)) RADIO (- ALTURA-ESCENA RADIO))
             )]
        [else altura]
    )
)



(define ESTADO-INICIAL (/ ALTURA-ESCENA 2))

(big-bang ESTADO-INICIAL
  [to-draw grafica-circulo]
  [on-key modificador-posición]
  [on-mouse controlador-mouse]

  )