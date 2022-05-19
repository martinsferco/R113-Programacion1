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

(define DELTA 23) ;Cantidad de desplazamiento en pixeles usando el teclado

(define INITIAL-STATE (make-posn (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2)))
(define FINAL-STATE (make-posn -1 -1))


;posn=?: posn posn -> Boolean
;Toma dos datos de tipo posn y nos devuelve #t si son iguales.
;En caso contrario devuelve #f.

(define (posn=? posn1 posn2)
  (and (= (posn-x posn1)(posn-x posn2)) (= (posn-y posn1)(posn-y posn2)))
  )

;render-state: Estado -> Image
;Recibe el estado actual y nos devuelve una imagen de la figura colocada
;en una escena, colocada donde indican las coordenadas del estado.

(define (render-state state)
  (if (not (posn=? state FINAL-STATE))
      (place-image CIRCULO (posn-x state)(posn-y state) ESCENA)
      (place-image (text "FINIQUITADO" 100 "black") (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2) ESCENA))
  )


;verificar-mover-teclado: Estado Tecla -> Boolean
;Dado el estado actual y para donde se quiere desplazar, nos devuelve #t
;si es posible mover en dicho sentido la figura sin que se nos salga de
;la escena. En caso contrario devuelve #f.

(define (verificar-mover-teclado estado tecla)
  (cond [(string=? tecla "up") (>= (- (posn-y estado) DELTA RADIO-CIRCULO) 0)]
        [(string=? tecla "down")(<= (+ (posn-y estado) DELTA RADIO-CIRCULO) ALTO-ESCENA)]
        [(string=? tecla "right") (<= (+ (posn-x estado) DELTA RADIO-CIRCULO) ANCHO-ESCENA)]
        [(string=? tecla "left") (>= (- (posn-x estado) DELTA RADIO-CIRCULO) 0)]
        )
  )


;corregir-posicion: Estado String -> Estado
;Dado el estado actual y la tecla que se presiono, nos devuelve la coordenada
;en la que la figura no se sale de la escena. Consideramos que esta funcion es
;llamada cuando tenemos un desplazamiento que produce que la imagen se salga de
;pantalla, por lo que no verificamos las coordenadas del estado.

(define (corregir-posicion estado tecla)
  (cond [(key=? tecla "up") (make-posn (posn-x estado) RADIO-CIRCULO)]
        [(key=? tecla "down")(make-posn (posn-x estado) (- ALTO-ESCENA RADIO-CIRCULO))]
        [(key=? tecla "left") (make-posn RADIO-CIRCULO (posn-y estado) )]
        [(key=? tecla "right") (make-posn (- ANCHO-ESCENA RADIO-CIRCULO) (posn-y estado))]
        )
  )

;keyboard-handler: Estado String -> Estado
;Dado el estado actual y la tecla presionada, nos devuelve un nuevo estado
;de acuerdo a si se presiono alguna de las flechas direccionales, las cuales
;desplazan a la figura en el sentido que sus nombres indican.

(check-expect (keyboard-handler (make-posn 100 100) "up") (make-posn 100 (- 100 DELTA)))
(check-expect (keyboard-handler (make-posn 100 100) "down") (make-posn 100 (+ 100 DELTA)))
(check-expect (keyboard-handler (make-posn 100 100) "right") (make-posn (+ 100 DELTA) 100))
(check-expect (keyboard-handler (make-posn 100 100) "left") (make-posn (- 100 DELTA) 100))
(check-expect (keyboard-handler (make-posn 100 100) "k") (make-posn 100 100))

(define (keyboard-handler estado tecla)
  (cond [(key=? tecla "up") (if (verificar-mover-teclado estado tecla)
                                (make-posn (posn-x estado)(- (posn-y estado) DELTA))
                                (corregir-posicion estado tecla))]
        
        [(key=? tecla "down") (if (verificar-mover-teclado estado tecla)
                                  (make-posn (posn-x estado)(+ (posn-y estado) DELTA))
                                  (corregir-posicion estado tecla))]
        
        [(key=? tecla "right") (if (verificar-mover-teclado estado tecla)
                                   (make-posn (+ (posn-x estado) DELTA) (posn-y estado))
                                   (corregir-posicion estado tecla))]
        
        [(key=? tecla "left") (if (verificar-mover-teclado estado tecla)
                                   (make-posn (- (posn-x estado) DELTA) (posn-y estado))
                                   (corregir-posicion estado tecla))]
        [(or (key=? tecla "q") (key=? tecla "Q")) FINAL-STATE]
        [else estado]
        )
  )

;corregir-posx: Number -> Number
;Dado donde se clickeo horizontalmente, nos devuelve una coordenada que haga que el circulo
;quede en el borde de la escena (en el sentido X).

(check-expect (corregir-posx 1) RADIO-CIRCULO)
(check-expect (corregir-posx (- ANCHO-ESCENA 1)) (- ANCHO-ESCENA RADIO-CIRCULO))

(define (corregir-posx x)
              (if (> x (/ ANCHO-ESCENA 2))
                  (- ANCHO-ESCENA RADIO-CIRCULO)
                  RADIO-CIRCULO
                  )
  )

;corregir-posy: Number -> Number
;Dado donde se clickeo verticalmente, nos devuelve una coordenada que haga que el circulo
;quede en el borde de la escena (en el sentido Y).

(check-expect (corregir-posy 1) RADIO-CIRCULO)
(check-expect (corregir-posy (- ALTO-ESCENA 1)) (- ALTO-ESCENA RADIO-CIRCULO))

(define (corregir-posy y)
              (if (> y (/ ANCHO-ESCENA 2))
                  (- ALTO-ESCENA RADIO-CIRCULO)
                  RADIO-CIRCULO
                  )
  )

;verificador-x: Number -> Boolean
;Dadas las coordenadas horizontales donde se hizo click, nos determina si podemos
;colocar la imagen sin que se salga de la escena.

(check-expect (verificador-x  0) #f)
(check-expect (verificador-x ANCHO-ESCENA) #f)
(check-expect (verificador-x (/ ANCHO-ESCENA 2)) #t)

(define (verificador-x x)
  (and (<= x (- ANCHO-ESCENA RADIO-CIRCULO))
       (>= x RADIO-CIRCULO)
       )
  )


;verificador-y: Number -> Boolean
;Dadas las coordenadas verticales donde se hizo click, nos determina si podemos
;colocar la imagen sin que se salga de la escena.

(check-expect (verificador-y  0) #f)
(check-expect (verificador-y ALTO-ESCENA) #f)
(check-expect (verificador-y (/ ALTO-ESCENA 2)) #t)

(define (verificador-y y)
  (and (<= y (- ALTO-ESCENA RADIO-CIRCULO))
       (>= y RADIO-CIRCULO)
       )
  )

;mouse-handler: Estado Number Number String -> Estado
;Dado el estado actual, nos devuelve un nuevo estado de acuerdo a donde
;se presiono el mouse dentro de la escena.

(define (mouse-handler estado x y tipo)
  (cond [(string=? tipo "button-down") (cond [(and (verificador-x x)(verificador-y y)) (make-posn x y)]
                                             [(verificador-x x) (make-posn x (corregir-posy y))]
                                             [(verificador-y y) (make-posn (corregir-posx x) y)]
                                             [else (make-posn (corregir-posx x) (corregir-posy y))]
                                             )]
        [else estado])
  )

;detect-final-state: Estado -> Boolean
;Nos devuelve #t si el estado es el FINAL-STATE, en caso contrario devuelve #f.

(check-expect (detect-final-state (make-posn 3 3)) #f)
(check-expect (detect-final-state FINAL-STATE) #t)

(define (detect-final-state state)
  (posn=? state FINAL-STATE)
  )


(big-bang INITIAL-STATE
  [to-draw render-state]
  [on-key keyboard-handler]
  [on-mouse mouse-handler]
  [stop-when detect-final-state render-state]
  )