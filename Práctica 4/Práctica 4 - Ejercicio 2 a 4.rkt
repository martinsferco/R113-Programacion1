;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 2 a 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicios 2 a 4

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;Ejercicio 2)-

;dist-origen: posn -> Number
;Dado un punto en el plano representado con una estructura posn,
;nos devuelve su distancia al origen.

(check-expect (dist-origen (make-posn 3 4)) 5)
(check-expect (dist-origen (make-posn 0 0)) 0)
(check-expect (dist-origen (make-posn -4 -3)) 5)
(check-expect (dist-origen (make-posn 10 0)) 10)
(check-error (dist-origen 1) "dist-origen: expects a posn")

(define (dist-origen p)
  (if (not (posn? p))
      (error "dist-origen: expects a posn")
      (sqrt (+ (sqr (posn-x p))(sqr (posn-y p)))))
  )

;__________________________________________
;Ejercicio 3)-

;simetrico: posn -> posn
;Dado un punto en el plano representado con una estructura posn,
;nos devuelve su simetrico respecto al origen.

(check-expect (simetrico (make-posn 3 4)) (make-posn -3 -4))
(check-expect (simetrico (make-posn 0 0)) (make-posn 0 0))
(check-expect (simetrico (make-posn -4 -3)) (make-posn 4 3))
(check-expect (simetrico (make-posn 10 0)) (make-posn -10 0))
(check-error (simetrico "Hola") "simetrico: expects a posn")

(define (simetrico p)
  (if (not (posn? p))
      (error "simetrico: expects a posn")
      (make-posn (- (posn-x p))(- (posn-y p)))
      )
  )

;__________________________________________
;Ejercicio 4)-

;distancia: posn posn -> Number
;Dados dos puntos en el plano, representados con la estructura posn,
;nos devuelve la distancia entre ellos. En caso de que uno de ellos no
;lo sea, devuelve un error.

(check-expect (distancia (make-posn 0 0) (make-posn 3 4)) 5)
(check-expect (distancia (make-posn 0 3) (make-posn 4 0)) 5)
(check-expect (distancia (make-posn 0 0) (make-posn 0 0)) 0)
(check-error (distancia 1 1) "Tipos incorrectos para la funcion")
(check-error (distancia "hola" #t) "Tipos incorrectos para la funcion")

(define (distancia p1 p2)
  (if (not (and (posn? p1)(posn? p2)))
      (error "Tipos incorrectos para la funcion")
      (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2)))
           )
        )
      )
  )








