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

(define TAM-MOSCA 30)


(define MOSCA (circle TAM-MOSCA "solid" "black"))

(define DELTA 50)
(define GAMMA 30)

;Iniciamos con la mosca en el medio de la escena.
(define ESTADO-INICIAL (make-posn (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2)))

;Definimos el estado final del programa.
(define ESTADO-FINAL (make-posn -1 -1))

;posn=?: posn posn -> Boolean
;Toma dos datos de tipo posn y nos devuelve #t si son iguales.
;En caso contrario devuelve #f.

(define (posn=? posn1 posn2)
  (and (= (posn-x posn1)(posn-x posn2)) (= (posn-y posn1)(posn-y posn2)))
  )

;mostrar-estado: Estado -> Image
;Dado el estado actual, nos devuelve una imagen en donde se coloca la mosca
;sobre la escena en las coordenadas adecuadas.
;Si el estado es el ESTADO-FINAL nos muestra un mensaje que diga "MOSCA ATRAPADA".

(define (mostrar-estado estado)
  (if (not (posn=? estado ESTADO-FINAL))
      (place-image MOSCA (posn-x estado)(posn-y estado) ESCENA)
      (place-image (text "MOSCA ATRAPADA" 50 "black") (/ ANCHO-ESCENA 2)(/ ALTO-ESCENA 2) ESCENA))
  )


;coor-random: Number -> Estado
;Recibe una coordenadas, y suma o resta aleatoriamente DELTA unidades.

(define (coor-random coordenada)
  (if (= (random 2) 0) (- coordenada DELTA) (+ coordenada DELTA)) 
  )


;new-corx: Number -> Number
;Recibe una coordenada X, y suma o resta aleatoriamente DELTA unidades.
;Si se encuentra en una poisición que provocaría que se fuera de la pantalla,
;se restringe la decisión para que vaya solo a uno de los lados.

(check-expect (new-corx 0) DELTA)
(check-expect (new-corx ANCHO-ESCENA) (- ANCHO-ESCENA DELTA))

(define (new-corx corx)
  (cond [(< (- corx TAM-MOSCA DELTA) 0) (+ corx DELTA)]
        [(> (+ corx TAM-MOSCA DELTA) ANCHO-ESCENA) (- corx DELTA)]
        [else (coor-random corx)])
  )

;new-cory: Number -> Number
;Recibe una coordenada Y, y suma o resta aleatoriamente DELTA unidades.
;Si se encuentra en una poisición que provocaría que se fuera de la pantalla,
;se restringe la decisión para que vaya solo a uno de los lados.

(check-expect (new-cory 0) DELTA)
(check-expect (new-cory ANCHO-ESCENA) (- ANCHO-ESCENA DELTA))

(define (new-cory cory)
(cond [(< (- cory TAM-MOSCA DELTA) 0) (+ cory DELTA)]
        [(> (+ cory TAM-MOSCA DELTA) ANCHO-ESCENA) (- cory DELTA)]
        [else (coor-random cory)])
  )

;mover-mosca: Estado -> Estado
;Recibe un estado, y nos devuelve un nuevo estado el cual desplaza a la mosca
;aleatoriamente delta unidades dentro de la Escena.

(define (mover-mosca estado)
  (make-posn (new-corx (posn-x estado)) (new-cory (posn-y estado)))
  )


;distancia: posn posn -> Number
;Dado el estado actual y las coordenadas del click, representados con la estructura posn,
;nos devuelve la distancia entre ellos.

(check-expect (distancia (make-posn 0 0) (make-posn 3 4)) 5)
(check-expect (distancia (make-posn 0 3) (make-posn 4 0)) 5)
(check-expect (distancia (make-posn 0 0) (make-posn 0 0)) 0)

(define (distancia p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2)))
           )
        )
  )

;aplastar-mosca: Estado Number Number String -> Boolean
;Dado el estado actual y las coordenadas donde se hizo click, nos devuelve el ESTADO-FINAL
;si la distancia entre el evento y el estado es menor a GAMMA unidades. En caso contrario,
;nos devuelve el estado actual.

(define (aplastar-mosca estado x y tipo)
  (cond [(string=? tipo "button-down") (if (< (distancia estado (make-posn x y)) GAMMA)
                                           ESTADO-FINAL
                                           estado)]
        [else estado])
  )

;estado-final?: Estado -> Boolean
;Nos devuelve #t si el estado es el ESTADO-FINAL. Caso contrario devuelve #f.

(check-expect (estado-final? (make-posn 3 3)) #f)
(check-expect (estado-final? ESTADO-FINAL) #t)

(define (estado-final? estado)
  (posn=? estado ESTADO-FINAL)
  )



(big-bang ESTADO-INICIAL
  [to-draw mostrar-estado]
  [on-tick mover-mosca 0.1]
  [on-mouse aplastar-mosca]
  [stop-when estado-final? mostrar-estado]
  )