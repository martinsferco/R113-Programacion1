;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 5

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;El Estado del sistema sera una estructura del tipo posn, y representara
;las coordenadas verticales y horizontales de la figura.

(define ANCHO-ESCENA 1000)
(define ALTO-ESCENA 1000)

(define ESCENA (empty-scene ANCHO-ESCENA ALTO-ESCENA))

(define RADIO-CIRCULO 50)
(define COLOR-FIGURA "lightgreen")
(define CIRCULO (circle RADIO-CIRCULO "solid" COLOR-FIGURA))

(define DELTA 20) ;Cantidad de desplazamiento en pixeles usando el teclado

;representador-estado: Estado -> Image
;Recibe el estado actual y nos devuelve una imagen de la figura colocada
;en una escena, colocada donde indican las coordenadas del estado.

(define (representador-estado estado)
  (place-image CIRCULO (posn-x estado)(posn-y estado) ESCENA))


;desplazar-figura-teclado: Estado String -> Estado
;Dado el estado actual y la tecla presionada, nos devuelve un nuevo estado
;de acuerdo a si se presiono alguna de las flechas direccionales, las cuales
;desplazan a la figura en el sentido que sus nombres indican.

(check-expect (desplazar-figura-teclado (make-posn 50 50) "up") (make-posn 50 (- 50 DELTA)))
(check-expect (desplazar-figura-teclado (make-posn 50 50) "down") (make-posn 50 (+ 50 DELTA)))
(check-expect (desplazar-figura-teclado (make-posn 50 50) "right") (make-posn (+ 50 DELTA) 50))
(check-expect (desplazar-figura-teclado (make-posn 50 50) "left") (make-posn (- 50 DELTA) 50))
(check-expect (desplazar-figura-teclado (make-posn 50 50) "k") (make-posn 50 50))

(define (desplazar-figura-teclado estado tecla)
  (cond [(key=? tecla "up") (make-posn (posn-x estado)(- (posn-y estado) DELTA))]
        [(key=? tecla "down") (make-posn (posn-x estado)(+ (posn-y estado) DELTA))]
        [(key=? tecla "right") (make-posn (+ (posn-x estado) DELTA) (posn-y estado))]
        [(key=? tecla "left") (make-posn (- (posn-x estado) DELTA) (posn-y estado))]
        [else estado]
        )
  )


;verificador-mover-mouse: Number Number -> Boolean
;Dadas las coordenadas donde se hizo click, nos determina si podemos
;colocar la imagen sin que se salga de la escena.

(define (verificador-mover-mouse x y)
  (and (<= x (- ANCHO-ESCENA RADIO-CIRCULO))
       (>= x RADIO-CIRCULO)
       (>= y RADIO-CIRCULO)
       (<= y (- ALTO-ESCENA RADIO-CIRCULO))
       )
  )

;corrector-posicion: Number Number -> Estado
;Dada las coordenadas actuales (para saber donde se encuentra ubicada la figura),
;nos devuelve una posicion corregida la cual es adyacente a los bordes de la escena.
 

(define (corrector-posicion x y)
  
  )

;mover-figura-mouse: Estado Number Number String -> Estado
;Dado el estado actual, nos devuelve un nuevo estado de acuerdo a donde
;se presiono el mouse dentro de la escena.

(define (mover-figura-mouse estado x y tipo)
  (cond [(string=? tipo "button-down") (if (not (verificador-mover-mouse x y))
                                           (corrector-posicion x y)
                                           (make-posn x y))]
        [else estado])
  )


(define ESTADO-INICIAL (make-posn (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2)))

(big-bang ESTADO-INICIAL
  [to-draw representador-estado]
  [on-key desplazar-figura-teclado]
  [on-mouse mover-figura-mouse]
  )